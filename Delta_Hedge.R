library("pbapply")
library("data.table")
library("quantmod")
library("Deriv")
"***********************************************************************
                             Notes
************************************************************************
Udregn PnL for portefølje der replikeres samt den hedgede portefølje
Tilføj længere t bill
Tilføj mere options data til længere løbetid
Lav lapply funktion for delta hedge results*

Husk, indlæs data helt op til experation date hvis mangler
"
# ***********************************************************************
#                        read option data
# ***********************************************************************
months = sprintf("%d%02d", rep(2023, 12), 1:12)
# months = sprintf("%d%02d", rep(2022:2023, each = 12), rep(1:12, times = 2)) If more years needed
file_paths = file.path("/Users/henrikvoss/Desktop/Bachelorprojekt/Data/", paste0("spy_eod_", months, ".txt"))
ops_data_list = lapply(file_paths, read.csv, header = TRUE)
ops_data = do.call(rbind, ops_data_list)
TB4WK = read.csv("/Users/henrikvoss/Desktop/Bachelorprojekt/Data/DTB4WK.csv")
ops_data_filtered = subset(ops_data, X.EXPIRE_DATE. == as.Date("2023-09-29")) # 2023-03-01
ops = ops_data_filtered[, c("X.QUOTE_DATE.", "X.UNDERLYING_LAST.", "X.EXPIRE_DATE.", "X.STRIKE.", "X.C_LAST.", "X.P_LAST.", "X.C_DELTA.", "X.P_DELTA.", "X.C_IV.", "X.P_IV.")]
names(ops) = c("Date", "UnderlyingPrice", "ExpireDate", "Strike", "Call_Last", "Put_Last", "Call_Delta", "Put_Delta", "Call_IV", "Put_IV")

indices = match(as.Date(ops$Date), as.Date(TB4WK$DATE))
c = TB4WK$DTB4WK[indices]

ops$Rate = as.numeric(c) * 0.01
ops$DaysToExp = as.numeric(as.Date(ops$ExpireDate) - as.Date(ops$Date))

# Making data frame ops2
opsCall = ops[, c("Date", "Strike", "UnderlyingPrice", "DaysToExp", "Rate")]
opsCall$Mark = ops$Call_Last
opsCall$IV = ops$Call_IV
opsCall$Delta = ops$Call_Delta
opsCall$putCall = rep("Call", length(opsCall$Date))
opsCall$ID = paste0(opsCall$Strike,"-",opsCall$putCall)

opsPut = ops[, c("Date", "Strike", "UnderlyingPrice", "DaysToExp", "Rate")]
opsPut$Mark = ops$Put_Last
opsPut$IV = ops$Put_IV
opsPut$Delta = ops$Put_Delta
opsPut$putCall = rep("Put", length(opsPut$Date))
opsPut$ID = paste0(opsPut$Strike,"-",opsPut$putCall)

ops2 = rbind(opsCall, opsPut)

#### TIL DEBUG #################################################################################
S = ops2$UnderlyingPrice
K = ops2$Strike
r = ops2$Rate
sigma = ops2$IV
T = ops2$DaysToExp

d1 = (log(S[1]/K[1]) + (r[1] + sigma[1]^2/2) * T[1]/365) / (sigma[1] * sqrt(T[1]/365))
pnorm(d1)
################################################################################################

# Problem med funktionen - Når delta burde være 1 regnes den til 0
bs_delta = function(S = ops2$UnderlyingPrice, K = ops2$Strike, r = ops2$Rate, sigma = ops2$IV, T = ops2$DaysToExp) {
  delta = rep(NA, (length(ops2$Date)))
  for (i in 1:length(ops2$Date)) {
    if (ops2$putCall[i] == "Call"){
      d1 = (log(S[i]/K[i]) + (r[i] + sigma[i]^2/2) * T[i]/365) / (sigma[i] * sqrt(T[i]/365))
      delta[i] = pnorm(d1)
    } else if (ops2$putCall[i] == "Put") {
      d1 = (log(S[i]/K[i]) + (r[i] + sigma[i]^2/2) * T[i]/365) / (sigma[i] * sqrt(T[i]/365))
      delta[i] = pnorm(d1) - 1
    } else {
    stop("option_type should be either 'call' or 'put'")
    }
  }
  return(delta)
}
# Calculate delta
ops2$BSDelta = round(bs_delta(), digits = 5)
#ops2 = subset(ops2,ops2$DaysToExp <= 14)     # Måske ikke nødvendig

# Combinations of all strikes
comb = unique(expand.grid(ops2$ID[1:10], ops2$ID[1:10]))           # Slice hvis nødvendigt
comb = subset(comb, Var1 != Var2)
comb$Var1 = as.character(comb$Var1)
comb$Var2 = as.character(comb$Var2)

#### Til test
strk1 = comb[38219,1]
strk2 = comb[38219,2]

# Spreads function
sprds = function(strk1, strk2, hedgeFreq = 1){
  tmp = subset(ops2, ops2$ID == strk1 | ops2$ID == strk2)
  tmp = tmp[, c("Date", "putCall","Strike", "Mark", "BSDelta", "UnderlyingPrice", "DaysToExp", "ID")]

  # Short 1st strike
  tmp$BSDelta[is.na(tmp$BSDelta)] = 0
  strks = unique(tmp$ID)
  s1 = subset(tmp,tmp$ID == strks[1])
  s1$Mark = -s1$Mark 
  s1$BSDelta = -s1$BSDelta 
  s1 = rbind(s1,subset(tmp,tmp$ID == strks[2]))
  
  # Short 2nd strike
  s2 = subset(tmp,tmp$ID == strks[2])
  s2$Mark = -s2$Mark 
  s2$BSDelta = -s2$BSDelta 
  s2 = rbind(s2,subset(tmp,tmp$ID == strks[1]))
  
  # Extract unique days to expirations
  d = unique(tmp$DaysToExp)
  d = d[order(d,decreasing = TRUE)]
  d_hedge = d[seq(1, length(d), by = hedgeFreq)]                  # Set hedging frequency
  non_hedged_days = setdiff(d, d_hedge)
  
  transactionCostRate = 0.01
  
########################################################### HEDGED DAYS short1 ########################################################
  short1 = lapply(as.list(d_hedge),function(Days2Exp){
    one = subset(s1, s1$DaysToExp == Days2Exp)
    one = as.data.frame(one[order(one$Mark),])
    prc     = sum(one$Mark)          # net price of options (negative == credit)
    dlta    = sum(one$BSDelta)*100   # net delta between strikes
    stk     = -dlta                  # num of shares to offset delta
    stkVal  = stk*one$Strike[1]      # value of position at expiration date (European option)
    transactionCost = abs(stkVal) * transactionCostRate # Transaction equal to percentage of the total share value
    
    # Combine data
    one = as.data.frame(cbind(as.character(one$Date[1]), 
          one$ID[1],one$putCall[1],one$Strike[1], 
          one$Mark[1], one$BSDelta[1],one$ID[2],
          one$putCall[2],one$Strike[2], one$Mark[2], 
          one$BSDelta[2],prc,dlta,stk,NA,one$UnderlyingPrice[1],stkVal, transactionCost,
          one$DaysToExp[1]))
    
    colnames(one) <- c("Date","ID1","Type1","Strike1","Premium1","BSDelta1",
                       "ID2","Type2","Strike2","Premium2","BSDelta2",
                       "NetPremium","OptDelta","StkShares","NetDelta",
                       "StkPrice","StkValue", "TransactionCost", "DaysToExp")

    one$NetDelta   = as.numeric(one$OptDelta) + as.numeric(one$StkShares)
    one$Strike1    = as.numeric(one$Strike1)
    one$Strike2    = as.numeric(one$Strike2)
    one$Premium1   = as.numeric(one$Premium1)
    one$Premium2   = as.numeric(one$Premium2)
    one$BSDelta1   = as.numeric(one$BSDelta1)
    one$BSDelta2   = as.numeric(one$BSDelta2)
    one$NetPremium = as.numeric(one$NetPremium)
    one$OptDelta   = as.numeric(one$OptDelta)
    one$StkShares  = as.numeric(one$StkShares)
    one$NetDelta   = as.numeric(one$NetDelta)
    one$StkPrice   = as.numeric(one$StkPrice)
    one$StkValue   = as.numeric(one$StkValue)
    one$TransactionCost   = as.numeric(one$TransactionCost)
    one$DaysToExp  = as.numeric(one$DaysToExp)
    one
  })
  
########################################################### NON HEDGED DAYS short1 ########################################################
  short1n = lapply(as.list(non_hedged_days),function(Days2Exp){
    one = subset(s1, s1$DaysToExp == Days2Exp)
    one = as.data.frame(one[order(one$Mark),])
    prc     = sum(one$Mark)          # net price of options (negative == credit)
    dlta    = sum(one$BSDelta)*100   # net delta between strikes
    stk     = 0                      # Not hedging delta
    stkVal  = stk*one$Strike[1]      # value of position at expiration date (European option)
    transactionCost = abs(stkVal) * transactionCostRate # Transaction equal to percentage of the total share value
    
    # Combine data
    one = as.data.frame(cbind(as.character(one$Date[1]), 
                              one$ID[1],one$putCall[1],one$Strike[1], 
                              one$Mark[1], one$BSDelta[1],one$ID[2],
                              one$putCall[2],one$Strike[2], one$Mark[2], 
                              one$BSDelta[2],prc,dlta,stk,NA,one$UnderlyingPrice[1],stkVal, transactionCost,
                              one$DaysToExp[1]))
    
    colnames(one) <- c("Date","ID1","Type1","Strike1","Premium1","BSDelta1",
                       "ID2","Type2","Strike2","Premium2","BSDelta2",
                       "NetPremium","OptDelta","StkShares","NetDelta",
                       "StkPrice","StkValue", "TransactionCost", "DaysToExp")
    
    one$NetDelta   = as.numeric(one$OptDelta) + as.numeric(one$StkShares)
    one$Strike1    = as.numeric(one$Strike1)
    one$Strike2    = as.numeric(one$Strike2)
    one$Premium1   = as.numeric(one$Premium1)
    one$Premium2   = as.numeric(one$Premium2)
    one$BSDelta1   = as.numeric(one$BSDelta1)
    one$BSDelta2   = as.numeric(one$BSDelta2)
    one$NetPremium = as.numeric(one$NetPremium)
    one$OptDelta   = as.numeric(one$OptDelta)
    one$StkShares  = as.numeric(one$StkShares)
    one$NetDelta   = as.numeric(one$NetDelta)
    one$StkPrice   = as.numeric(one$StkPrice)
    one$StkValue   = as.numeric(one$StkValue)
    one$TransactionCost   = as.numeric(one$TransactionCost)
    one$DaysToExp  = as.numeric(one$DaysToExp)
    one
  })
  # Row bind results
  short1 = do.call(rbind,short1)
  short1 <- na.omit(short1)
  short1n = do.call(rbind,short1n)
  short1n <- na.omit(short1n)
  
  short1 = rbind(short1,short1n)
  short1 = short1[order(short1$DaysToExp, decreasing = TRUE), ]
  
  StkPnL = suppressWarnings(c(0,diff(short1$StkPrice)*short1$StkShares) - short1$TransactionCost)
  short1$StkPnL = StkPnL[1:(length(StkPnL)-1)] 
  short1$OptPnL <- c(0,diff(short1$NetPremium))*100
  short1$GrossPnL <- round(short1$StkPnL + short1$OptPnL,2)
  short1$CumSumPnL <- cumsum(short1$GrossPnL)
  
########################################################### HEDGED DAYS short2 ########################################################
  short2 = lapply(as.list(d_hedge),function(Days2Exp){
    one = subset(s2, s2$DaysToExp == Days2Exp)
    one = as.data.frame(one[order(one$Mark),])
    prc     = sum(one$Mark)          # net price of options (negative == credit)
    dlta    = sum(one$BSDelta)*100   # net delta between strikes
    stk     = -dlta                  # num of shares to offset delta
    stkVal  = stk*one$Strike[1]      # value of position at expiration date (European option)
    transactionCost = abs(stkVal) * transactionCostRate # Transaction equal to percentage of the total share value

    one <- as.data.frame(cbind(as.character(one$Date[1]), 
                               one$ID[1],one$putCall[1],one$Strike[1], 
                               one$Mark[1], one$BSDelta[1],one$ID[2],
                               one$putCall[2],one$Strike[2], one$Mark[2], 
                               one$BSDelta[2],prc,dlta,stk,NA,one$UnderlyingPrice[1],stkVal, transactionCost,
                               one$DaysToExp[1]))

    colnames(one) <- c("Date","ID1","Type1","Strike1","Premium1","BSDelta1",
                       "ID2","Type2","Strike2","Premium2","BSDelta2",
                       "NetPremium","OptDelta","StkShares","NetDelta",
                       "StkPrice","StkValue", "TransactionCost", "DaysToExp")

    one$NetDelta   = as.numeric(one$OptDelta) + as.numeric(one$StkShares)
    one$Strike1    = as.numeric(one$Strike1)
    one$Strike2    = as.numeric(one$Strike2)
    one$Premium1   = as.numeric(one$Premium1)
    one$Premium2   = as.numeric(one$Premium2)
    one$BSDelta1   = as.numeric(one$BSDelta1)
    one$BSDelta2   = as.numeric(one$BSDelta2)
    one$NetPremium = as.numeric(one$NetPremium)
    one$OptDelta   = as.numeric(one$OptDelta)
    one$StkShares  = as.numeric(one$StkShares)
    one$NetDelta   = as.numeric(one$NetDelta)
    one$StkPrice   = as.numeric(one$StkPrice)
    one$StkValue   = as.numeric(one$StkValue)
    one$TransactionCost   = as.numeric(one$TransactionCost)
    one$DaysToExp  = as.numeric(one$DaysToExp)
    one
  })
  
########################################################### NON HEDGED DAYS 2 ########################################################
  short2n = lapply(as.list(non_hedged_days),function(Days2Exp){
    one = subset(s2, s2$DaysToExp == Days2Exp)
    one = as.data.frame(one[order(one$Mark),])
    prc     = sum(one$Mark)          # net price of options (negative == credit)
    dlta    = sum(one$BSDelta)*100   # net delta between strikes
    stk     = 0                      # Not hedging delta
    stkVal  = stk*one$Strike[1]      # value of position at expiration date (European option)
    transactionCost = abs(stkVal) * transactionCostRate # Transaction equal to percentage of the total share value
    
    one <- as.data.frame(cbind(as.character(one$Date[1]), 
                               one$ID[1],one$putCall[1],one$Strike[1], 
                               one$Mark[1], one$BSDelta[1],one$ID[2],
                               one$putCall[2],one$Strike[2], one$Mark[2], 
                               one$BSDelta[2],prc,dlta,stk,NA,one$UnderlyingPrice[1],stkVal, transactionCost,
                               one$DaysToExp[1]))
    
    colnames(one) <- c("Date","ID1","Type1","Strike1","Premium1","BSDelta1",
                       "ID2","Type2","Strike2","Premium2","BSDelta2",
                       "NetPremium","OptDelta","StkShares","NetDelta",
                       "StkPrice","StkValue", "TransactionCost", "DaysToExp")
    
    one$NetDelta   = as.numeric(one$OptDelta) + as.numeric(one$StkShares)
    one$Strike1    = as.numeric(one$Strike1)
    one$Strike2    = as.numeric(one$Strike2)
    one$Premium1   = as.numeric(one$Premium1)
    one$Premium2   = as.numeric(one$Premium2)
    one$BSDelta1   = as.numeric(one$BSDelta1)
    one$BSDelta2   = as.numeric(one$BSDelta2)
    one$NetPremium = as.numeric(one$NetPremium)
    one$OptDelta   = as.numeric(one$OptDelta)
    one$StkShares  = as.numeric(one$StkShares)
    one$NetDelta   = as.numeric(one$NetDelta)
    one$StkPrice   = as.numeric(one$StkPrice)
    one$StkValue   = as.numeric(one$StkValue)
    one$TransactionCost   = as.numeric(one$TransactionCost)
    one$DaysToExp  = as.numeric(one$DaysToExp)
    one
  })
  
  short2 = do.call(rbind,short2)
  short2 <- na.omit(short2)
  short2n = do.call(rbind,short2n)
  short2n <- na.omit(short2n)
  
  short2 = rbind(short2,short2n)
  short2 = short2[order(short2$DaysToExp, decreasing = TRUE), ]
  
  StkPnL = suppressWarnings(c(0,diff(short2$StkPrice)*short2$StkShares) - short1$TransactionCost)
  short2$StkPnL = StkPnL[1:(length(StkPnL)-1)] 
  short2$OptPnL <- c(0,diff(short2$NetPremium))*100
  short2$GrossPnL <- round(short2$StkPnL + short2$OptPnL,2)
  short2$CumSumPnL <- cumsum(short2$GrossPnL)
  
  # Combine short1 & short2
  result = rbind(short1,short2)
  return(result)
}
# Function test
df = sprds(strk1 = comb[1,1], strk2 = comb[1,2])
df = data.frame(lapply(df, function(x) {
  if (is.numeric(x)) round(x, 2) else x
}))

# Calculate results for all combinations
ALL = pblapply(as.list(1:nrow(comb)), function(ii){
  sprds(strk1 = comb[ii,1], strk2 = comb[ii,2], hedgeFreq = 1)
})

ALL = rbindlist(ALL,use.names = TRUE)
#write.csv(ALL, file = "/Users/henrikvoss/Desktop/Bachelorprojekt/ALL.csv", row.names = FALSE)
#ALL = read.csv("/Users/henrikvoss/Desktop/Bachelorprojekt/ALL.csv")
AtExp = subset(ALL, ALL$DaysToExp == 0)
AtExp = unique(AtExp[order(AtExp$CumSumPnL,decreasing = TRUE),])
bst = unique(subset(ALL, ALL$ID1 == "290-Call" & ALL$ID2 == "270-Call"))
bst = data.frame(lapply(bst, function(x) {
  if (is.numeric(x)) round(x, 2) else x
}))

# If duplicate rows run below code
first_occurrences = sapply(unique(bst$DaysToExp), function(x) which(bst$DaysToExp == x)[1])
bst = bst[first_occurrences, ]
#write.csv(bst, file = "/Users/henrikvoss/Desktop/Bachelorprojekt/bst.csv", row.names = FALSE)





