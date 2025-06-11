# ds-capstone2-dropout-prediction

This repository contains the code and analysis for the Student Dropout and Academic Success Prediction Project, completed as part of [HarvardX PH125.9x: Data Science - Capstone](https://www.edx.org/course/data-science-capstone).

## Project Overview

The goal of this project is to build and evaluate predictive models for classifying students at risk of dropout using a comprehensive dataset from a Portuguese higher education institution. The project leverages data from the [UCI Machine Learning Repository](https://archive.ics.uci.edu/dataset/697/predict+students+dropout+and+academic+success), featuring macroeconomic context, student demographics, academic history, and program characteristics. The workflow includes data cleaning, feature engineering, exploratory analysis, systematic model building, and final model validation.

## Key Steps

- **Data Preparation:** Review, clean, and recode variables across four main categories: macroeconomic context, student demographics, academic preparation/performance, and program characteristics. Redundant and low-value features are removed to optimize model performance.
- **Exploratory Data Analysis:** Visualization and analysis of key factors influencing dropout, such as financial aid, prior grades, attendance type, major, application mode, and course load. Patterns in dropout risk are identified by subgroup.
- **Model Development:** Multiple machine learning models are trained and evaluated: Random Forest, k-Nearest Neighbors (KNN), Naive Bayes, and Gradient Boosted Trees (xgboost). Cross-validation and an independent hold-out validation set are used for robust assessment.
- **Ensemble Methods:** Two ensemble models are constructed: (1) a majority-voting ensemble of all four models, and (2) a refined ensemble using the three best-performing models (Random Forest, Naive Bayes, Gradient Boosted Trees).
- **Evaluation:** Model performance is compared using accuracy, sensitivity, and specificity. The final model is selected based on predictive accuracy and robustness.

## Main Results

- The final ensemble model (Random Forest, Naive Bayes, Gradient Boosted Trees) achieved an accuracy of 0.8684807, a sensitivity of 0.72, and a specificity of 0.94 on the validation set.
- Random Forest and Gradient Boosted Trees were the strongest individual models, but the ensemble approach delivered the highest overall accuracy.
- All work was completed in R using R Markdown.

## Impact and Limitations

- **Potential Impact:** These models enable institutions to identify and support at-risk students, improving retention and academic outcomes. By integrating diverse data sources, the approach provides a holistic early warning system for dropout prevention.
- **Limitations:** The dataset is from a single institution, and the validation set is relatively modest (441 records). The target variable is imbalanced (more "on_track" than "dropout" cases), which may influence the results. Findings may not fully generalize to other contexts.
- **Future Work:** Expand the dataset to include more institutions, explore advanced machine learning methods, and integrate additional data sources (such as student engagement and qualitative data) for even better predictive performance.

## Data Source

- [UCI Predict Students’ Dropout and Academic Success Dataset](https://archive.ics.uci.edu/dataset/697/predict+students+dropout+and+academic+success)
