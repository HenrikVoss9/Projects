import matplotlib.pyplot as plt
import pandas_datareader as pdr
import pandas as pd
import numpy as np
from scipy.stats import norm
from scipy.optimize import minimize, Bounds, LinearConstraint

def check_input(stocks, step_size, period, gamma = None):
    assert isinstance(stocks, list) and all(isinstance(stock, str) for stock in stocks), "stocks must be a list of strings"
    
    assert isinstance(step_size, int) and step_size > 0, "step_size must be a positive integer"

    assert isinstance(period, list) and len(period) == 2, "period must be a list containing two date strings"

    if gamma is not None:
        assert isinstance(gamma, list) and all(isinstance(value, int) for value in gamma), "gamma must be a list of integers"
        
    
def plot_func(legend, title, xlabel, ylabel):
    plt.legend(legend, loc = 'center left', bbox_to_anchor = (1, 0.5))
    plt.title(title)
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.show()

    
def get_prices(stocks, step_size = None, period = None, show_plot = True, get_output = False, input_check = True):
    '''
    Returns a tuple (stocks, price), where p[j, i] represents the opening price of stock i at time step j and stocks[i] is the name of the i‘th stock.

            Parameters:
                    stocks (list): A list of stock tickers as strings
                    step_size (int): An integer that describes the index of stocks prices in days
                    period (list): A list of two dates as such "yyyy-mm-dd"

            Returns:
                    (stocks, price) (tuple): A tuple of two numpy arrays             
    '''

    if step_size is None:
        step_size = 1
        print(f"Default step size: {step_size}")

    if period is None:
        period = ["2023-01-01", None]
        print(f"Default period: {period}")

    if input_check:
        check_input(stocks, step_size, period)
               
    stock_read = pdr.stooq.StooqDailyReader(stocks, start = period[0]).read()
    stock_read = stock_read.sort_index()

    price = [[stock_read.loc[period[0]:period[1]]["Open"][stock][::step_size][i]
                for i in range(len(stock_read.loc[period[0]:period[1]]["Open"][stock][::step_size]))]
                    for stock in stocks]
    price = np.array(price)

    nans = pd.isna(price)
    for i in range(len(stocks)):
        if nans[i,0]:
            raise ValueError (f"{stocks[i]} is too new a stock for the period. Choose another start date or remove {stocks[i]}")
    
    if show_plot:
        plt.figure(figsize = (11, 6))
        plt.plot(stock_read.loc[period[0]:period[1]]["Open"][::step_size].index, np.transpose(price))
        plt.xticks(rotation = 45)
        plot_func(stocks, "Stock market prices", "Date", "USD")
        
    if not get_output:
        display(pd.DataFrame(np.transpose(price), columns = stocks, index = stock_read.loc[period[0]:period[1]]["Open"][::step_size].index))

    return (stocks, price) if get_output else None


def stats(stocks, step_size = None, period = None, show_plot = True, get_output = False, input_check = True):
    '''
    Returns a tuple (reward, mean, cov) for each given stock and plots the probability density function (pdf) of the return.

            Parameters:
                    stocks (list): A list of stock tickers as strings
                    step_size (int): An integer that describes the index of stocks prices in days
                    period (list): A list of two dates as such "yyyy-mm-dd"

            Returns:
                    (reward, mean, cov) (tuple): A tuple of three numpy arrays 
    '''

    if step_size is None:
        step_size = 1
        print(f"Default step size: {step_size}")

    if period is None:
        period = ["2023-01-01", None]
        print(f"Default period: {period}")

    if input_check:
        check_input(stocks, step_size, period)

    p = get_prices(stocks, step_size, period, show_plot = False, get_output = True, input_check = False)[1]
    
    reward = [[(p[j][i + 1] - p[j][i])/p[j][i]
               for i in range(len(p[0]) - 1)]
                  for j in range(len(stocks))]
    reward = np.array(np.transpose(reward))
    
    mean = [np.sum(reward[:, i]/len(reward)) for i in range(len(stocks))]
    mean = np.array(mean)

    s = 0
    for i in range(len(reward)):
        s += np.transpose([reward[i] - mean]) @ [reward[i] - mean]
    cov = s/len(reward)
    var = [cov[i][i] for i in range(len(stocks))]

    global upr_bound, lwr_bound
    upr_bound = sum(mean/len(stocks)) + 3 * max(var)**0.5
    lwr_bound = sum(mean/len(stocks)) - 3 * max(var)**0.5                
    if show_plot:
        plt.figure(figsize = (11, 6))
        x = np.linspace(lwr_bound, upr_bound, 1000)
        
        for i in range(len(stocks)):
            plt.plot(x, norm.pdf(x, mean[i], var[i]**0.5), lw = 2)
        plot_func(stocks, "Probability density function", "Random variable", "Probability density")
        
    if not get_output:
        display(pd.DataFrame(reward, columns = stocks))
        display(pd.DataFrame(np.transpose(mean), index = stocks, columns = ["Mean"]))
        display(pd.DataFrame(cov, index = stocks, columns = stocks))
     
    return (reward, mean, cov) if get_output else None


def optimize(stocks, step_size = None, period = None, gamma = None, get_output = False):
    '''
    Returns a list l containing a list for each gamma with it's respective optimal weight of stocks and plots the pdf of each solution.
    Finally it creates a scatter plot of how the weights change as gamma changes.

            Parameters:
                    stocks (list): A list of stock tickers as strings
                    step_size (int): An integer that describes the index of stocks prices in days
                    period (list): A list of two dates as such "yyyy-mm-dd"
                    gamma (list): A list of integers describing risk tolerance

            Returns:
                    l (list): a list l containing a list for each gamma
    '''
    if step_size is None:
        step_size = 1
        print(f"Default step size: {step_size}")

    if period is None:
        period = ["2023-01-01", None]
        print(f"Default period: {period}")

    if gamma is None:
        gamma = [1,5,10]
        print(f"Default gamma values: {gamma}")

    check_input(stocks, step_size, period, gamma)
    
    mean, cov = stats(stocks, step_size, period, show_plot = False, get_output = True, input_check = False)[1:3]
    w = np.ones(len(stocks))*(1/len(stocks))

    def object_func(w, mean, cov, gamma):
        return -(w @ mean - gamma * w @ (cov @ w))

    def constraint(w):
        return np.sum(w) - 1     
       
    def optimizer(func, w, mean, cov, gamma):
        opt_bounds = Bounds(0, 1)
        opt_constraints = [{'type': 'eq', 'fun': constraint}]

        optimal_weights = minimize(func, w, args = (mean, cov, gamma), method = "SLSQP", bounds = opt_bounds, constraints = opt_constraints)
        return optimal_weights['x']

    l = [[gamma[i], optimizer(object_func, w, mean, cov, gamma[i])] for i in range(len(gamma))]

    plt.figure(figsize = (11, 6))
    x = np.linspace(lwr_bound, upr_bound, 1000)
    for i in range(len(gamma)):
        mean_p = l[i][1] @ mean
        var_p = l[i][1] @ cov @ np.transpose(l[i][1])                                 
        plt.plot(x, norm.pdf(x, mean_p, var_p**0.5), lw = 2)
    plot_func(gamma, "Probability density function", "Random variable", "Probability density")

    plt.figure(figsize = (11, 6))
    for i in range(len(stocks)):
        y = [l[j][1][i] for j in range(len(gamma))]
        plt.scatter(gamma, y, marker = i)
    plot_func(stocks, "Scatterplot of portfolio weights vs gamma", "Gamma", "Weight")
    
    if not get_output:
        w = [optimizer(object_func, w, mean, cov, gamma[i]) for i in range(len(gamma))]
        display(pd.DataFrame(w, columns = stocks, index = gamma))
        

    return l if get_output else None

