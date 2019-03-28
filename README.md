<script async src="https://www.googletagmanager.com/gtag/js?id=UA-112502179-1"></script> <script> window.dataLayer = window.dataLayer || []; function gtag(){dataLayer.push(arguments);} gtag('js', new Date());
gtag('config', 'UA-112502179-1'); </script>

# VaR-Optimization-Using-Quantile-Regression

---

Value at risk (VaR) is a very useful tool to quantify extreme losses. There are many different methodologies to forecast value at risk. However, those methodologies always produce widely different VaR forecasts. In this paper, we try to combine some frequently used VaR forecasts by penalized quantile regression. We randomly choose 30 stocks from SP500 to form a equally weighted portfolio. Then we compare penalized quantile regression with other standalone VaR methods as well as two combination methods(Simple average and unpenalized quantile regression). To evaluate different forecasts methods, we perform backtesting using unconditional coverage test and conditional coverage test. The results show that penalized quantile regressions perform better than standalone methods.

More details about the project can be found in [my report](/YuxiangLi_RiskManagement_Final_Project.pdf). And the code can be found [here](/RiskFinal_YuxiangLi.R).

---

- [1. Introduction](#1-introduction)
- [2. VaR Forecasting Methods](#2-var-forecasting-methods)
  - [2.1 Standalone Methods](#21-standalone-methods)
  - [2.2 Combination Methods](#22-combination-methods)
- [3. Backtesting](#3-backtesting)
  - [3.1 Unconditional Coverage](#31-unconditional-coverage)
  - [3.2 Conditional Coverage](#32-conditional-coverage)
- [4. Results](#4-results)
  - [4.1 VaR Forecasts](#41-var-forecasts)
  - [4.2 Backtesting](#42-backtesting)
- [5. Summary](#5-summary)

---

## 1 Introduction

The value at risk (VaR) is defined as the worst possible loss over a target horizon that will not be exceeded with a given probability (Jorion, 2006).

Many standalone VaR forecasting methods have been introduced. However, no single VaR forecast outperforms others throughout the existing VaR forecasting comparisons. They all have different advantages and disadvantages. All models are prone to suffer from model misspecificationand estimation uncertainty.Thus, people begin to seek a better forecasting by combining predictions from different models.

In this paper, I used Elastic net penalized quantile regression to combine 4 different VaR forecasts. Then, by comparing with standalone VaR forecasts and other combination forecasts, we find penalized quantile regression is a better combination method for VaR forecasting.

## 2 VaR Forecasting Methods

I use the daily adjusted close price of 30 constituents of SP500 from Jan 1, 2014 to Dec 31, 2017, a total of 3524 days. The portfolio is formed by equally weighted investing on those assets. Then I use a 500-day rolling window to calculate 1-day ahead VaR forecasts with 4 different methods, which will give us 4 time series of length 3023. I keep using a 500-day rolling window to estimate combination weights by quantile regression to calculate 1-day Combo_VaR. Thus, I will lose 1000 data and end up getting different 1-day VaR forecasts from Dec 24, 2007 to Dec 29, 2017, a total of 2523 days.

### 2.1 Standalone Methods

I choose 4 widely used VaR forecasts models to estimate our standalone VaR forecasts. These models cover parametric, semi-parametric and non-parametric methods.

***Normal Distribution：*** This method assumes the return of assets is normally distributed with mean <a href="https://www.codecogs.com/eqnedit.php?latex=\mu" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mu" title="\mu" /></a> and variance <a href="https://www.codecogs.com/eqnedit.php?latex=\sigma^2" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\sigma^2" title="\sigma^2" /></a>. Thus, to calculate VaR is to calculate the quantile of a gaussian distribution.

***Historical Simulation：*** Historical simulation is a classic and popular non-parametric method. Without any assumption about return distribution, it predicts next day's VaR by the empirical <a href="https://www.codecogs.com/eqnedit.php?latex=\alpha" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\alpha" title="\alpha" /></a>-quantile of a period of past returns.

***Extreme Value Theory：*** Since I only focus on the tail part of return distribution for risk management, extreme value theory is a very useful tool that models the tail distribution without any assumption about the center of the distribution.

***GARCH Model：*** The GARCH method models the conditional variance of return. It assumes that the variance of return follows a predictable pattern. I use the GARCH(1, 1) of Bollerslev (1986).

### 2.2 Combination Methods

This section introduces the combination methods I use. There are 3 combinations, including simple mean, unpenalized quantile regression and penalized quantile regression with elastic net penalty.

***Simple Mean:*** Simple mean is a very simple and empirically successful method in combining forecasts. The combination weights of simple mean approach are equally divided to each methods.

***Unpenalized Quantile Regression:*** As a comparison between quantile regression methods, we adopt the unpenalized quantile regression by Koenker (2016).

***Elastic Net Penalized Quantile Regression:*** Since different VaR forecasts are highly correlated to each other, Bayer(2018) suggests using penalized quantile regression to deal with multicollinearity. Elastic net penalty is a linear combination of lasso penalty and ridge penalty.

## 3 Backtesting

We will assess the performance of different approached by the unconditional coverage backtest by Kupiec (1995) and the conditional coverage backtest by Engle and Manganelli (2004).

### 3.1 Unconditional Coverage

This method focuses on VaR failures. By defining failure rate as N/T , whereN is the number of exceptions and T is the number of total observations, we would expect the failure rate not far from the confidence level. Then we can apply a likelihood ratio test.

### 3.2 Conditional Coverage

The unconditional coverage model ignores time variation in the data. The VaR forecasts would also be invalid if the exceptions cluster over time. Thus, the deviation should be serially independent if the model were to be true. Christoffersen (1998) proposes the very inuential conditional coverage test.

## 4 Results

### 4.1 VaR Forecasts

The following pictures show the results of different VaR forecasts. The exceptions are shown in black dot.

![Normal](/Picture/Normal.png)
![HS](/Picture/HS.png)
![EVT](/Picture/EVT.png)
![GARCH](/Picture/GARCH.png)
![Mean](/Picture/Mean.png)
![QRU](/Picture/QRU.png)
![QRP](/Picture/QRP.png)

As we can see, the combination forecasts are much better than standalone forecasts. And from the plot, we could feel that penalized quantile regression method might be the best combination of the three. There are fewer exceptions and no sign of clustering.

### 4.2 Backtesting

The following table shows the p-value backtestng. Models rejected at the 1% significant level are shown in bold.

| |Exceptions|LR_uc|LR_ind|LR_cc|
|---|---|---|---|---|
|Normal|62|***0.000***|***0.004***|***0.000***|
|HS|32|0.193|***0.007***|0.011|
|EVT|40|***0.006***|***0.003***|***0.000***|
|GARCH|41|***0.004***|0.698|0.014|
|Simple Mean|33|0.138|0.453|0.251|
|Unpenalized QR|53|***0.000***|0.438|***0.000***|
|Penalized QR|30|0.354|0.370|0.436|

We notice that all the standalone VaR forecasts are rejected by unconditional coverage test, which means they do underestimate the risks. For combination methods, simple mean and penalized QR methods both perform well in terms of unconditional and conditional coverage. And penalized QR method produces less exceptions. We also also find unpenalized QR method fails to provide valid VaR forecasts. The poor performance of unpenalized QR method is due to the multicollinearity problem discussed by Bayer (2018), which causes unpenalized quantile regression to generate unstable combination weights.

## 5 Summary

In this paper, we try to combine VaR forecasts using elastic net penalized quantile regression and to test its performance with other standalone and combination methods. Elastic net penalized quantile regression improves unpenalized quantile regression in forecasts combination with lasso and ridge penalty, which solve the multicollinearity problem and perform variable selection. From the backtesting, we find that (1) Penalized QR method does produce a very good and valid VaR forecast in different situations; (2) Unpenalized QR method fails to provide valid VaR forecasts due to the multicollinearity; (3) Simple mean method performs unexpectedly well in conditional and unconditional coverage tests; (4) Most standalone VaR forecasts underestimate risks, except historical simulation.
