# Programming-with-Application-in-Finance
The following code are implemented with the use of R*. 

The objectives are the following 
> obtaining and analyzing financial data;
> estimate return characteristics;
> compute efficient portfolios by means of Markowitz optimization;
> recall the principles of single index models;
> estimate index models;
> price options by means of Monte-Carlo simulation;
> compute risk measures.

In the Task1 folder I compute the implied volatility of Norsk Hydro (NHY) and 
then implement a Monte Carlo simulation to predict the stock price. Furthermore, I price a special type of 
call option on the company - "down-and-in barrier call option". 

In the Task2 folder I calculate a list of optimal allocations with different trading strategies:
1. an optimal mean-variance portfolio that promises an expected excess return of 5% p.a. No short selling is permitted
   and that no stock shall have a weight of more than 10%.
2. a minimum variance strategy is a strategy that invests in the optimal portfolio with the lowest variance. 
   Use an expanding window with initially 60 return observations to estimate a single-index model.
3. momentum strategy proposed in Jegadeesh and Titman (1993).
