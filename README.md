# Churn Modeling for Bank Customers

## Project Overview
This project aims to identify the optimal method for predicting customer churn in a bank using various statistical and machine learning techniques in R. The dataset from Kaggle includes 10,000 customers and 14 variables related to banking behavior.

### Research Objectives
- Conduct exploratory analysis and graphical representation of the data.
- Apply predictive modeling techniques including logistic regression, Naive Bayes, LDA/QDA, KNN, and Poisson regression.
- Utilize LASSO and PCA for feature selection.
- Employ resampling techniques to balance binary variables for more accurate modeling.

## Dataset
Data sourced from [Kaggle's Churn Modeling Dataset](https://www.kaggle.com/datasets/).

## Methodology
- **Data Preprocessing**: Standardizing features, handling missing values, and balancing the dataset through oversampling.
- **Model Development**: Implementing various classification models and feature selection techniques.
- **Evaluation**: Comparing model performance using appropriate evaluation metrics.

## Results
The Naive Bayes classifier with kernel density estimation, applied to the oversampled data, yielded the most accurate predictions. This approach effectively models binary predictor variables and continuous predictor variables' conditional probability density functions. The oversampling technique helped balance the dataset, enhancing the model's classification performance.

## Tools and Technologies
- **Language**: R
