# Airbnb-Rental-Price-Prediction-Project
Predicting Airbnb rental price using statistical and Machine Learning techniques in R

## Executive Summary
This report aims to analyze and predict the price for an Airbnb rental based on 96 variables regarding its property, host, and past reviews. Methods of analysis include both exploratory data analysis, predictive modeling, and machine learning. The report illustrates the detailed process of data cleaning and imputation, variable selection, and model building.  

The models that were built and trained include **linear regression** model, **logistic regression** model, **decision tree**, **random forest** mode, and **boost** model. The analytical process also contains feature selection including forward and backward Stepwise selection and **lasso regression**. (see the unselected models in Kaggle Project.R)

Eventually, a machine learning method - **XGBoost algorithm** - was employed to execute the prediction using 63 variables. All codes used in this project can be found in the appendices. (see the final model and report in Kaggle Project.rmd)

Results of the XGBoost model show that the price of an Airbnb rental is primarily influenced by the property's location, amenities, host service, and review scores.  
