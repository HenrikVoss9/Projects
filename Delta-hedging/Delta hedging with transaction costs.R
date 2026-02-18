library("pbapply")
library("data.table")
library("quantmod")
library("Deriv")
library("dplyr")
library("readxl")
library("stats")
library("optimx")
library("nloptr")
library("ggplot2")

# ***********************************************************************
#                        read option data
# ***********************************************************************
months = sprintf("%d%02d", rep(2022:2023, each = 12), rep(1:12, times = 2))
file_paths = file.path("/Users/henrikvoss/Desktop/Bachelorprojekt/Data/SPX_Options", paste0("spx_eod_", months, ".txt"))
ops_data_list = lapply(file_paths, read.csv, header = TRUE)
ops_data = do.call(rbind, ops_data_list)
TB4WK = read.csv("/Users/henrikvoss/Desktop/Bachelorprojekt/Data/DTB4WK.csv")

ops_data$DaysToExp = as.numeric(as.Date(ops_data$X.EXPIRE_DATE.) - as.Date(ops_data$X.QUOTE_DATE.))
ops_data$Call_Price = (ops_data$X.C_BID. + ops_data$X.C_ASK.) / 2
ops_data$Put_Price = (ops_data$X.P_BID. + ops_data$X.P_ASK.) / 2

ops_data$X.EXPIRE_DATE. = as.Date(ops_data$X.EXPIRE_DATE)
unique_expiration_dates = as.Date(unique(ops_data$X.EXPIRE_DATE.))
unique_expiration_dates = unique_expiration_dates[order(unique_expiration_dates)]
exp_dates = unique_expiration_dates[seq(1, length(unique_expiration_dates), by = 1)]
ops_dataf = ops_data[ops_data$X.EXPIRE_DATE. %in% exp_dates, ]

div = read_excel("/Users/henrikvoss/Desktop/Bachelorprojekt/Data/PerformanceGraphExport (1).xls")
names(div) = c("Date", "DivPoints")
div$Date= as.Date(div$Date)
div$DivPoints = as.numeric(div$DivPoints)
div = div[-(1:185), ]
div$Rate = as.numeric(TB4WK$DTB4WK[match(div$Date, as.Date(TB4WK$DATE))]) * 0.01
div = na.omit(div)
div$DaysUntilReset = rep(248:0, length.out = nrow(div))
div$PVDiv = div$DivPoints * exp(-div$Rate * div$DaysUntilReset)
# div$PVDiv = 0 # For tsla data

# ***********************************************************************
#                        Transaction cost
# ***********************************************************************
spx = read.csv("/Users/henrikvoss/Desktop/Bachelorprojekt/Data/HistoricalData_spx.csv")
spx$Date = as.Date(spx$Date, format = "%m/%d/%Y")
spx = spx[spx$Date >= as.Date('2022-01-01') & spx$Date <= as.Date('2023-12-31'), ]
spx$Close.Last = as.numeric(gsub("\\$", "", spx$Close.Last))
spx$Open = as.numeric(gsub("\\$", "", spx$Open))
spx$High = as.numeric(gsub("\\$", "", spx$High))
spx$Low = as.numeric(gsub("\\$", "", spx$Low))

spx$log_close = log(spx$Close.Last)
spx$log_high = log(spx$High)
spx$log_low = log(spx$Low)
spx$mid_range = (spx$log_high + spx$log_low) / 2
spx$next_mid_range = NA
spx$term = NA

for (i in 1:(nrow(spx) - 1)) {
  spx$next_mid_range[i] = spx$mid_range[i + 1]
  spx$term[i] = (spx$log_close[i] - spx$mid_range[i]) * (spx$log_close[i] - spx$next_mid_range[i])
}

spx$spread = ifelse(spx$term >= 0, 2 * sqrt(spx$term), NA)
for (i in 2:nrow(spx)) {
  if (is.na(spx$spread[i])) {
    spx$spread[i] = spx$spread[i - 1]
  }
}
spx = subset(spx, select = -c(log_close, log_high, log_low, mid_range, next_mid_range, term))

# ***********************************************************************
#                        SPX plots
# ***********************************************************************
ggplot(spx, aes(x = Date, y = Close.Last)) +
  geom_line(color = 'blue') +
  labs(title = "SPX Prices Over Time", x = "Date", y = "Close Price") +
  theme_minimal()

spx$LogReturn = c(NA, diff(log(spx$Close.Last)))
spx = na.omit(spx)
density_function = function(x, mean, sd) {
  dnorm(x, mean = mean, sd = sd)
}

ggplot(spx, aes(x = LogReturn)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.005, fill = "gray", color = "black", alpha = 0.5) +
  stat_function(fun = density_function, args = list(mean = mean(spx$LogReturn), sd = sd(spx$LogReturn)),
                color = 'red', size = 0.7) +
  labs(title = "S&P500 Log-Returns", x = "Daily Return", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

test_result = ks.test(spx$LogReturn, "pnorm", mean(spx$LogReturn), sd(spx$LogReturn))
print(test_result)

# ***********************************************************************
#                        Black-Scholes delta
# ***********************************************************************
bs_delta = function(ops2, S = ops2$UnderlyingPrice - ops2$Div, K = ops2$Strike, r = ops2$Rate, sigma, T = ops2$DaysToExp) {
  delta = rep(NA, length(ops2$Date))
  d1 = (log(S / K) + (r + sigma^2 / 2) * T / 365) / (sigma * sqrt(T / 365))
  delta[ops2$putCall == "Call"] = pnorm(d1[ops2$putCall == "Call"])
  delta[ops2$putCall == "Put"] = pnorm(d1[ops2$putCall == "Put"]) - 1
  
  return(delta)
}
# ***********************************************************************
#                        Process options
# ***********************************************************************
process_data_for_date = function(exp, ops_data, TB4WK) {
  ops_data_filtered = subset(ops_data, X.EXPIRE_DATE. == as.Date(exp))
  ops = ops_data_filtered[, c("X.QUOTE_DATE.", "X.UNDERLYING_LAST.", "X.EXPIRE_DATE.", "X.STRIKE.", "Call_Price", "X.C_BID.", "X.C_ASK.", "Put_Price", "X.P_BID.", "X.P_ASK.", "X.C_DELTA.", "X.P_DELTA.", "X.C_IV.", "X.P_IV.")]
  names(ops) = c("Date", "UnderlyingPrice", "ExpireDate", "Strike", "Call_Price", "Call_Bid", "Call_Ask", "Put_Price", "Put_Bid", "Put_Ask", "Call_Delta", "Put_Delta", "Call_IV", "Put_IV")
  ops$DaysToExp = as.numeric(as.Date(ops$ExpireDate) - as.Date(ops$Date))
  ops$Rate = as.numeric(TB4WK$DTB4WK[match(as.Date(ops$Date), as.Date(TB4WK$DATE))]) * 0.01
  ops$Div = as.numeric(div$PVDiv[match(as.Date(ops$Date), div$Date)])
  ops$Transactioncost = as.numeric(spx$spread[match(as.Date(ops$Date), spx$Date)])
  ops = na.omit(ops)
  
  process_options = function(type) {
    ops_type = ops[, c("Date", "Strike", "UnderlyingPrice", "ExpireDate", "DaysToExp", "Rate", "Div")]
    premium_col = ifelse(type == "Call", "Call_Price", "Put_Price")
    ops_type$Premium = ops[, premium_col]
    ops_type$Bid = ops[, ifelse(type == "Call", "Call_Bid", "Put_Bid")]
    ops_type$Ask = ops[, ifelse(type == "Call", "Call_Ask", "Put_Ask")]
    ops_type$IV = ops[, ifelse(type == "Call", "Call_IV", "Put_IV")]
    ops_type$IV = ifelse(ops_type$IV > 0, ops_type$IV, NA)
    ops_type$Delta = ops[, ifelse(type == "Call", "Call_Delta", "Put_Delta")]
    ops_type$TransactionCost = ops$Transactioncost
    ops_type$putCall = type
    ops_type$ID = paste0(ops_type$Strike, "-", type)
    ops_type$Delta = ops[, ifelse(type == "Call", "Call_Delta", "Put_Delta")]
    
    #black_scholes_func = ifelse(type == "Call", BlackScholesCall, BlackScholesPut)
    
    ops_type
  }
  
  opsCall = process_options("Call")
  opsPut = process_options("Put")
  ops2 = rbind(opsCall, opsPut)

  pct_diff = ops2$UnderlyingPrice / ops2$Strike
  ops2$Moneyness = with(ops2, ifelse(
    putCall == "Call",
    ifelse(
      pct_diff <= 0.94 & pct_diff > 0.8, "Deep OTM",
      ifelse(pct_diff > 0.94 & pct_diff <= 0.97, "OTM",
             ifelse(pct_diff > 0.97 & pct_diff <= 1.03, "ATM",
                    ifelse(pct_diff > 1.03 & pct_diff <= 1.06, "ITM",
                           ifelse(pct_diff > 1.06 & pct_diff < 1.2, "Deep ITM", "Exclude"))))),
    ifelse(
      pct_diff > 1.06 & pct_diff < 1.2, "Deep OTM",
      ifelse(pct_diff <= 1.06 & pct_diff > 1.03, "OTM",
             ifelse(pct_diff <= 1.03 & pct_diff > 0.97, "ATM",
                    ifelse(pct_diff <= 0.97 & pct_diff > 0.94, "ITM",
                           ifelse(pct_diff <= 0.94 & pct_diff > 0.8, "Deep ITM", "Exclude")))))
  ))
  return(ops2)
}

all_results = pblapply(exp_dates, function(x) {
  result = process_data_for_date(x, ops_data, TB4WK)
})
all_resultsdf = bind_rows(all_results, .id = "source")
all_resultsdf = na.omit(all_resultsdf)

# ***********************************************************************
#                        Filters
# ***********************************************************************
# Filter moneyness
all_resultsdf = all_resultsdf[all_resultsdf$Moneyness != "Exclude", ]

# Filter 6 < exp < 365
all_resultsdf = all_resultsdf[all_resultsdf$DaysToExp <= 365, ]

ops_data_split = split(all_resultsdf, all_resultsdf$ExpireDate)
list = list()
for (i in seq_along(ops_data_split)){
  max_days = max(ops_data_split[[i]]$DaysToExp)
  if (max_days < 6){
    list[[i]] = unique(ops_data_split[[i]]$ExpireDate)
  }
}
for (i in seq_along(list)){
  all_resultsdf = all_resultsdf[!all_resultsdf$ExpireDate == list[i], ]
}

# Filter price < 0.375
all_resultsdf = all_resultsdf[all_resultsdf$Premium >= 0.375, ]

table(all_resultsdf$putCall, all_resultsdf$Moneyness)

# ***********************************************************************
#                        Calibration
# ***********************************************************************
bs_price = function(S, K, r, T, sigma, type) {
  d1 = (log(S/K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 = d1 - sigma * sqrt(T)
  if(type == "Call") {
    price = S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  } else {
    price = K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
  }
  return(price)
}

calibrate_sigma = function(df) {
  sigmas = pbsapply(1:nrow(df), function(i) {
    objective = function(sigma) {
      model_price = bs_price(df$UnderlyingPrice[i], df$Strike[i], df$Rate[i], df$DaysToExp[i]/365, sigma, df$putCall[i])
      if (is.na(model_price)) return(Inf)
      mse = (df$Premium[i] - model_price)^2
      return(mse)
    }
    opts = list(algorithm = "NLOPT_LN_BOBYQA", xtol_rel = 1.0e-8)
    result = nloptr(x0 = c(historical_vol), eval_f = objective, lb = 0.01, ub = 1, opts = opts)
    return(result$solution)
  })
  return(sigmas)
}

historical_vol = 0.2
sigmas = calibrate_sigma(all_resultsdf)
# write.csv(sigmas, "/Users/henrikvoss/Desktop/Bachelorprojekt/Data/Calibrated_sigmas_spx.csv", row.names = FALSE)
# sigmas = read.csv("/Users/henrikvoss/Desktop/Bachelorprojekt/Data/Calibrated_sigmas_spx.csv")
all_resultsdf$CIV = sigmas
})
# Delta calcualtions with calibrated volatility
all_resultsdf$CDelta = round(bs_delta(ops2 = all_resultsdf, sigma = all_resultsdf$CIV), digits = 5)

all_results = split(all_resultsdf, all_resultsdf$source)
all_results = lapply(all_results, function(df) {
  df$source = NULL
  return(df)
})

# ***********************************************************************
#                        Hedging function
# ***********************************************************************
sprds = function(data, strk1, hedgeFreq){
  tmp = subset(data, data$ID == strk1)
  tmp = tmp[, c("Date", "putCall","Strike", "Premium", "Delta", "CDelta", "UnderlyingPrice", "DaysToExp", "ID", "TransactionCost", "Moneyness")]
  
  strks = unique(tmp$ID)
  s1 = subset(tmp,tmp$ID == strks[1])
  s1$Premium = -s1$Premium
  s1$BSDelta = s1$CDelta

  d = unique(tmp$DaysToExp)
  d = d[order(d,decreasing = TRUE)]
  d_hedge = d[seq(1, length(d), by = hedgeFreq)]                  
  non_hedged_days = setdiff(d, d_hedge)
  
  results_list = list()
  stkshares_prev = 0
  prev_delta = 0
  all_days = sort(unique(c(d_hedge, non_hedged_days)), decreasing = TRUE)
  
  for (day in all_days) {
    one = subset(s1, s1$DaysToExp == day)
    one = one[order(one$Premium), ]
    
    if (day %in% d_hedge) {
      if (day == max(d_hedge)) {
        stk = one$BSDelta * 100
        NetDelta = as.numeric(one$BSDelta * 100) - as.numeric(stk)
      } else {
        delta_change = one$BSDelta - prev_delta
        stk = delta_change * 100
        NetDelta = as.numeric(delta_change * 100) - as.numeric(stk)
      }
      stkTotal = stk + stkshares_prev
      stkshares_prev = stkTotal
      prev_delta = one$BSDelta
    } else {
      if (day == max(d_hedge)) {
        stk = 0
        NetDelta = as.numeric(one$BSDelta * 100) - as.numeric(stk)
      } else {
        delta_change = one$BSDelta
        stk = 0
        NetDelta = as.numeric(delta_change * 100) - as.numeric(stkshares_prev)
      }
      stkTotal = stk + stkshares_prev
      stkshares_prev = stkTotal 
    }
    transactionCost = one$TransactionCost * abs(stk)
    #transactionCost = 0
    one = data.frame(Date = as.character(one$Date),
                     ID = one$ID, Strike = one$Strike,
                     Premium = one$Premium, BSDelta = one$BSDelta, Delta = one$Delta,
                     StkPurchased = stk, stkTotal = stkTotal, NetDelta = NetDelta,
                     StkPrice = one$UnderlyingPrice, TransactionCost = transactionCost,
                     DaysToExp = one$DaysToExp, Moneyness = one$Moneyness)
    
    results_list[[as.character(day)]] = one
  }
  
  
  result = do.call(rbind, results_list)
  result = result[order(result$DaysToExp, decreasing = TRUE), ]
  
  StkPnL = suppressWarnings(c(0,diff(result$StkPrice)*result$stkTotal) - result$TransactionCost)
  result$StkPnL = StkPnL[1:(length(StkPnL)-1)] 
  result$OptPnL = c(0,diff(result$Premium))*100
  last = length(result$Date)
  if (last >= 2) {
    last = last-1
    if (length(result$OptPnL) >= last) {
      if (result$OptPnL[last] == 0) {
        if (grepl("-Call", result$ID[last])) {
          result$OptPnL[last] = (-result$Premium[1] - max(c(result$StkPrice[last] - result$Strike[last]), 0)) * 100
        } else if (grepl("-Put", result$ID[last])) {
          result$OptPnL[last] = (-result$Premium[1] - max(c(result$Strike[last] - result$StkPrice[last]), 0)) * 100
        }
      }
    }
  }
  
  result$TotalStkPnL = cumsum(result$StkPnL)
  result$TotalOptPnL = cumsum(result$OptPnL)
  result$HedgeCost = abs(result$TotalStkPnL + result$TotalOptPnL)
  
  return(result)
}
######### Til test ###########
indices = sapply(all_results, function(l) l$Date == as.Date("2022-01-03"))
top_level_indices = sapply(indices, function(lv) any(lv))
which(top_level_indices)

Result_test = sprds(data = all_results[[1]], strk1 = "4100-Call", hedgeFreq = 1)
write.csv(Result_test, "/Users/henrikvoss/Desktop/Bachelorprojekt/result.csv", row.names = FALSE)
Result_test2 = pblapply(seq_along(id_list), function(i) {
  sprds(data = all_results[[3]], strk1 = id_list[i], hedgeFreq = 1)
})
Result_test = bind_rows(Result_test)
Result_test2 = bind_rows(Result_test2)
############################################################################# 

id_list = lapply(all_results, function(x) unique(x$ID))

ALL = list()
start_time = Sys.time()
for (i in seq_along(all_results)) {
  comb = id_list[[i]]
  ALL[[i]] = pblapply(seq_along(comb), function(ii){
    sprds(all_results[[i]], strk1 = comb[ii], hedgeFreq = 1)
  })
}
end_time = Sys.time()
difftime(end_time, start_time, units = "secs")
ALL = bind_rows(ALL)
# write.csv(ALL, "/Users/henrikvoss/Desktop/Bachelorprojekt/Data/Resultater/spx_nocost.csv", row.names = FALSE)
# spx_cost = read.csv("/Users/henrikvoss/Desktop/Bachelorprojekt/Data/Resultater/spx_cost.csv")
# spx_nocost = read.csv("/Users/henrikvoss/Desktop/Bachelorprojekt/Data/Resultater/spx_nocost.csv")
spx_costCall = spx_cost[grep("Call$", spx_cost$ID), ]
spx_costPut = spx_cost[grep("Put$", spx_cost$ID), ]
spx_nocostCall = spx_nocost[grep("Call$", spx_nocost$ID), ]
spx_nocostPut = spx_nocost[grep("Put$", spx_nocost$ID), ]

ALL2 = list()
start_time = Sys.time()
for (i in seq_along(all_results)) {
  comb = id_list[[i]]
  ALL2[[i]] = pblapply(as.list(1:length(comb)), function(ii){
    sprds(all_results[[i]], strk1 = comb[ii], hedgeFreq = 2)
  })
}
end_time = Sys.time()
difftime(end_time, start_time, units = "secs")
ALL2 = bind_rows(ALL2)
# write.csv(ALL2, "/Users/henrikvoss/Desktop/Bachelorprojekt/Data/Resultater/spx2_nocost.csv", row.names = FALSE)
# spx2_cost = read.csv("/Users/henrikvoss/Desktop/Bachelorprojekt/Data/Resultater/spx2_cost.csv")
# spx2_nocost = read.csv("/Users/henrikvoss/Desktop/Bachelorprojekt/Data/Resultater/spx2_ncoost.csv")
spx2_costCall = spx2_cost[grep("Call$", spx2_cost$ID), ]
spx2_costPut = spx2_cost[grep("Put$", spx2_cost$ID), ]
spx2_nocostCall = spx2_nocost[grep("Call$", spx2_nocost$ID), ]
spx2_nocostPut = spx2_nocost[grep("Put$", spx2_nocost$ID), ]

ALL3 = list()
start_time = Sys.time()
for (i in seq_along(all_results)) {
  comb = id_list[[i]]
  ALL3[[i]] = pblapply(as.list(1:length(comb)), function(ii){
    sprds(all_results[[i]], strk1 = comb[ii], hedgeFreq = 5)
  })
}
end_time = Sys.time()
difftime(end_time, start_time, units = "secs")
ALL3 = bind_rows(ALL3)
# write.csv(ALL3, "/Users/henrikvoss/Desktop/Bachelorprojekt/Data/Resultater/spx3_nocost.csv", row.names = FALSE)
# spx3_cost = read.csv("/Users/henrikvoss/Desktop/Bachelorprojekt/Data/Resultater/spx3_cost.csv")
# spx3_nocost = read.csv("/Users/henrikvoss/Desktop/Bachelorprojekt/Data/Resultater/spx3_nocost.csv")
spx3_costCall = spx3_cost[grep("Call$", spx3_cost$ID), ]
spx3_costPut = spx3_cost[grep("Put$", spx3_cost$ID), ]
spx3_nocostCall = spx3_nocost[grep("Call$", spx3_nocost$ID), ]
spx3_nocostPut = spx3_nocost[grep("Put$", spx3_nocost$ID), ]

# ***********************************************************************
#                        Hedge measures
# ***********************************************************************
hedge_performance = function(..., M = "Moneyness", D = "DaysToExp", H = "HedgeCost") {
  data_list = list(...)
  results = list()
  maes_list = list()
  sds_list = list()
  
  for (i in seq_along(data_list)) {
    df = data_list[[i]]
    
    subsets = list(
      ALL_DITM = subset(df, get(M) == "Deep ITM"),
      ALL_ITM = subset(df, get(M) == "ITM"),
      ALL_ATM = subset(df, get(M) == "ATM"),
      ALL_OTM = subset(df, get(M) == "OTM"),
      ALL_DOTM = subset(df, get(M) == "Deep OTM")
    )
    
    at_exp_subsets = list(
      AtExp_DITM = subset(df, get(M) == "Deep ITM" & get(D) == 1),
      AtExp_ITM = subset(df, get(M) == "ITM" & get(D) == 1),
      AtExp_ATM = subset(df, get(M) == "ATM" & get(D) == 1),
      AtExp_OTM = subset(df, get(M) == "OTM" & get(D) == 1),
      AtExp_DOTM = subset(df, get(M) == "Deep OTM" & get(D) == 1)
    )
    
    maes = sapply(at_exp_subsets, function(subset) mean(subset[[H]], na.rm = TRUE))
    sds = sapply(at_exp_subsets, function(subset) sd(subset[[H]], na.rm = TRUE))
    results[[paste0("DataFrame_", i)]] = list(Subsets = subsets, AtExp_Subsets = at_exp_subsets, Means = maes, SDs = sds)
    maes_list[[i]] = maes
    sds_list[[i]] = sds
  }

  categories = c("Deep ITM", "ITM", "ATM", "OTM", "Deep OTM")
  hedging_errors = data.frame(
    Category = categories
  )
  
  for (i in seq_along(maes_list)) {
    hedging_errors[[paste0("MAE(", i, ")")]] = round(unlist(maes_list[[i]]), 2)
    hedging_errors[[paste0("SD(", i, ")")]] = round(unlist(sds_list[[i]]), 2)
  }
  
  print(hedging_errors)
  return(list(Results = results, HedgingErrorsTable = hedging_errors))
}

result_cost = hedge_performance(spx_cost, spx2_cost, spx3_cost)
result_costCall = hedge_performance(spx_costCall, spx2_costCall, spx3_costCall)
result_costPut = hedge_performance(spx_costPut, spx2_costPut, spx3_costPut)

result_nocost = hedge_performance(spx_nocost, spx2_nocost, spx3_nocost)
result_nocostCall = hedge_performance(spx_nocostCall, spx2_nocostCall, spx3_nocostCall)
result_nocostPut = hedge_performance(spx_nocostPut, spx2_nocostPut, spx3_nocostPut)

# ***********************************************************************
#                        T tests
# ***********************************************************************
spx_nocost$HedgingFrequency = "Daily"
spx2_nocost$HedgingFrequency = "EveryOtherDay"
spx3_nocost$HedgingFrequency = "Weekly"

# Combine the data frames into one
combined_costs = rbind(spx_nocost, spx2_nocost, spx3_nocost)
# Subset the data to only include rows where DaysToExp is 1
subset_costs = subset(combined_costs, DaysToExp == 1)
categories = unique(subset_costs$Moneyness)
t_test_results = data.frame(Category = categories)

for (category in categories) {
  category_data = subset(subset_costs, Moneyness == category)
  daily_costs = subset(category_data, HedgingFrequency == "Daily")$HedgeCost
  every_other_day_costs = subset(category_data, HedgingFrequency == "EveryOtherDay")$HedgeCost
  weekly_costs = subset(category_data, HedgingFrequency == "Weekly")$HedgeCost
  
  t_test_1_vs_2 = t.test(daily_costs, every_other_day_costs, paired = TRUE)
  t_test_1_vs_3 = t.test(daily_costs, weekly_costs, paired = TRUE)
  t_test_2_vs_3 = t.test(every_other_day_costs, weekly_costs, paired = TRUE)
  t_test_results[t_test_results$Category == category, "p_val(1 vs 2)"] = sprintf("%.2e%s", t_test_1_vs_2$p.value)
  t_test_results[t_test_results$Category == category, "p_val(1 vs 3)"] = sprintf("%.2e%s", t_test_1_vs_3$p.value)
  t_test_results[t_test_results$Category == category, "p_val(2 vs 3)"] = sprintf("%.2e%s", t_test_2_vs_3$p.value)
}

print(t_test_results)

# Other t test
spx_cost$HedgingFrequency = "Daily"
spx2_cost$HedgingFrequency = "EveryOtherDay"
spx3_cost$HedgingFrequency = "Weekly"

combined_costs = rbind(spx_cost, spx2_cost, spx3_cost)
subset_costs = subset(combined_costs, DaysToExp == 1)

spx_nocost$HedgingFrequency = "Daily"
spx2_nocost$HedgingFrequency = "EveryOtherDay"
spx3_nocost$HedgingFrequency = "Weekly"

combined_costs_no = rbind(spx_nocost, spx2_nocost, spx3_nocost)
subset_costs_no = subset(combined_costs_no, DaysToExp == 1)

categories = unique(subset_costs$Moneyness)
t_test_results = data.frame(Category = categories)

for (category in categories) {
  category_costs = subset(subset_costs, Moneyness == category)
  category_nocosts = subset(subset_costs_no, Moneyness == category)
  
  daily_costs = subset(category_costs, HedgingFrequency == "Daily")$HedgeCost
  daily_nocosts = subset(category_costs, HedgingFrequency == "Daily")$HedgeCost
  
  every_other_day_costs = subset(category_costs, HedgingFrequency == "EveryOtherDay")$HedgeCost
  every_other_day_costs_no = subset(category_nocosts, HedgingFrequency == "EveryOtherDay")$HedgeCost
  
  weekly_costs = subset(category_costs, HedgingFrequency == "Weekly")$HedgeCost
  weekly_nocosts = subset(category_nocosts, HedgingFrequency == "Weekly")$HedgeCost
  
  test_daily = t.test(daily_costs, daily_nocosts, paired = TRUE)
  test_every_other_day = t.test(every_other_day_costs, every_other_day_nocosts, paired = TRUE)
  test_weekly = t.test(weekly_costs, weekly_nocosts, paired = TRUE)
  
  test_results[t_test_results$Category == category, "p_val(Daily)"] = sprintf("%.2e%s", test_daily$p.value)
  test_results[t_test_results$Category == category, "p_val(EveryOtherDay)"] = sprintf("%.2e%s", test_every_other_day$p.value)
  test_results[t_test_results$Category == category, "p_val(Weekly)"] = sprintf("%.2e%s", test_weekly$p.value)
}

print(test_results)


# Mean percentage changes across all categories for each frequency
subset_costs = subset(combined_costs, DaysToExp == 1)
subset_nocosts = subset(combined_costs_no, DaysToExp == 1)
categories = unique(subset_costs$Moneyness)

results = data.frame(Category = categories, Frequency = rep(c("Daily", "EveryOtherDay", "Weekly"), each = length(categories)))

for (category in categories) {
  for (freq in c("Daily", "EveryOtherDay", "Weekly")) {
    costs = subset(subset_costs, Moneyness == category & HedgingFrequency == freq)$HedgeCost
    costs_no = subset(subset_nocosts, Moneyness == category & HedgingFrequency == freq)$HedgeCost
    
    mae_with_cost = mean(costs)
    sd_cost = sd(costs)
    mae_nocost = mean(costs_no)
    sd_nocost = sd(costs_no)
    mae_change = ((mae_nocost - mae_cost) / mae_cost) * 100
    sd_change = ((sd_nocost - sd_cost) / sd_cost) * 100
    
    results[results$Category == category & results$Frequency == freq, "MAE_Percent_Change"] = mae_change
    results[results$Category == category & results$Frequency == freq, "SD_Percent_Change"] = sd_change
  }
}

mean_results = results %>%
  group_by(Frequency) %>%
  summarize(Mean_MAE_Percent_Change = mean(MAE_Percent_Change, na.rm = TRUE), 
            Mean_SD_Percent_Change = mean(SD_Percent_Change, na.rm = TRUE))

print(mean_results)
