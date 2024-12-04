# Predicting Diabetes Risk Using Machine Learning
## Overview:
This project analyzes a dataset of 70,692 survey responses from the US in 2015 to identify key factors contributing to diabetes and develop machine learning classifiers for diabetes prediction. The goal is to propose the best classifier by comparing different models and their performance metrics.

## Key Features:
### Data Preprocessing:
Reduced the dataset from 22 variables to 15 by excluding statistically insignificant and weakly correlated features.
Treated ordinal variables (e.g., age, education, income) as quantitative for effective analysis.
### Exploratory Data Analysis (EDA):
Explored relationships between diabetes prevalence and variables such as BMI, age, and general health using visualizations.
Excluded variables like mental and physical health days, which showed no strong correlation with diabetes.
## Classification Models:
### Models Used: Decision Trees, K-Nearest Neighbors (KNN), Naïve Bayes, and Logistic Regression.
## Evaluation Metrics: Mean accuracy, type 2 error (false negative rate), and AUC-ROC.
## Findings:
KNN (K=46) emerged as the best model with the highest AUC (0.748), balancing true positives and false negatives effectively.
Logistic Regression and Decision Trees also performed well, but Naïve Bayes was excluded due to its high type 2 error and independence assumptions.
## Tools & Techniques:
### Programming Language: 
Python
### Statistical Procedures:
Feature selection using correlation and p-value analysis.
Cross-validation (5-fold) to ensure model reliability.
## Key Libraries: 
Pandas, Scikit-learn, Matplotlib, and Seaborn.
## Conclusion:
The KNN classifier demonstrated the best predictive performance but comes with computational complexity and memory requirements.
It is recommended to use KNN in conjunction with standard diagnostic methods for a robust diabetes prediction system.
## Potential Applications:
Early diabetes risk prediction for healthcare providers.
Development of personalized intervention strategies based on risk factors.
