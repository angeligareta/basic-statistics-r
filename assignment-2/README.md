# Multiple Linear Regression

This project aims to answer a set of questions regarding the creation of reggression models for the prediction of diamonds price. The dataset assigned for the task was a subset of the original [Kaggle Diamonds dataset](https://www.kaggle.com/shivam2503/diamonds), which contains the prices and additional attributes of around 54,000 diamonds.

## Research Questions

The question the assignment intends to answer are:

- Plot Price vs Caratage and log(Price) vs Caratage. Decide on which response variable is better to use.
- Find a suitable way to include, besides caratage, the other categorical information available: clarity, color and certificate.
  - Comment on the model fitted.
  - Basic analysis of residuals
- Two different remedial actions (for a limitation on the model in previous question).
  - Create a new categorical explanatory variable Size with values Small, Medium, Large.
  - Interpret the interaction parameter med\*carat. What can we infer on the incremental pricing of caratage in the 3 clusters?
- Which of the two remedial actions do you prefer and why? Think on terms of interpretability and validity of the assumptions

## Findings

The complete description of the findings for each research question can be found on the [assignment report](./docs/multiple_linear_regression_report.pdf). The [implementation](./multiple_linear_regression.R) was performed using R programming language with _ggplot2_, _dplyr_, _lmtest_ and _tsoutliers_ libraries.

## Authors

- Miguel Perez Mateo
- Junhui Liang
- Angel Igareta [angel@igareta.com](mailto:angel@igareta.com)
