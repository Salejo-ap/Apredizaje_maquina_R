# -*- coding: utf-8 -*-
"""
Created on Thu Jan 23 08:14:07 2025

@author: jorge.alvarado
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.linear_model import Ridge, Lasso, ElasticNet
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.compose import ColumnTransformer
from sklearn.metrics import mean_squared_error
from sklearn.pipeline import Pipeline

# Load data
carros = pd.read_csv("carros2011imputado.csv", sep=';', decimal=',')

# Drop irrelevant columns
carros = carros.drop(columns=['ID', 'modelo', 'precio_basico', 'precio_equipado'])

# Separate numerical and categorical features
num_features = carros.select_dtypes(include=['float64', 'int64']).columns.drop('precio_promedio')
cat_features = carros.select_dtypes(include=['object']).columns

# Create preprocessing pipeline
preprocessor = ColumnTransformer([
    ('num', StandardScaler(), num_features),
    ('cat', OneHotEncoder(drop='first'), cat_features)
])

# Split data into train and test
X = carros.drop(columns=['precio_promedio'])
y = carros['precio_promedio']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=49584)

# Ridge Regression
ridge_model = Pipeline([
    ('preprocessor', preprocessor),
    ('ridge', Ridge())
])

ridge_params = {'ridge__alpha': np.logspace(-3, 3, 50)}
ridge_grid = GridSearchCV(ridge_model, ridge_params, scoring='neg_mean_squared_error', cv=5)
ridge_grid.fit(X_train, y_train)

# Best Ridge model
print("Best Ridge Alpha:", ridge_grid.best_params_['ridge__alpha'])
ridge_best = ridge_grid.best_estimator_

# Lasso Regression
lasso_model = Pipeline([
    ('preprocessor', preprocessor),
    ('lasso', Lasso())
])

lasso_params = {'lasso__alpha': np.logspace(-3, 3, 50)}
lasso_grid = GridSearchCV(lasso_model, lasso_params, scoring='neg_mean_squared_error', cv=5)
lasso_grid.fit(X_train, y_train)

# Best Lasso model
print("Best Lasso Alpha:", lasso_grid.best_params_['lasso__alpha'])
lasso_best = lasso_grid.best_estimator_

# Elastic Net Regression
elastic_net_model = Pipeline([
    ('preprocessor', preprocessor),
    ('elasticnet', ElasticNet())
])

elastic_params = {
    'elasticnet__alpha': np.logspace(-3, 3, 50),
    'elasticnet__l1_ratio': np.linspace(0.1, 0.9, 9)
}

elastic_grid = GridSearchCV(elastic_net_model, elastic_params, scoring='neg_mean_squared_error', cv=5)
elastic_grid.fit(X_train, y_train)

# Best Elastic Net model
print("Best Elastic Net Params:", elastic_grid.best_params_)
elastic_best = elastic_grid.best_estimator_

# Evaluate models
ridge_preds = ridge_best.predict(X_test)
lasso_preds = lasso_best.predict(X_test)
elastic_preds = elastic_best.predict(X_test)

ridge_rmse = np.sqrt(mean_squared_error(y_test, ridge_preds))
lasso_rmse = np.sqrt(mean_squared_error(y_test, lasso_preds))
elastic_rmse = np.sqrt(mean_squared_error(y_test, elastic_preds))

print("Ridge RMSE:", ridge_rmse)
print("Lasso RMSE:", lasso_rmse)
print("Elastic Net RMSE:", elastic_rmse)

# Plot coefficients for Elastic Net
elastic_coefs = elastic_best.named_steps['elasticnet'].coef_
feature_names = preprocessor.named_transformers_['num'].get_feature_names_out().tolist() + \
                 preprocessor.named_transformers_['cat'].get_feature_names_out().tolist()
coef_df = pd.DataFrame({'Feature': feature_names, 'Coefficient': elastic_coefs})
coef_df = coef_df.sort_values(by='Coefficient', key=abs, ascending=False)

plt.figure(figsize=(10, 8))
sns.barplot(data=coef_df, y='Feature', x='Coefficient', palette='coolwarm')
plt.title('Feature Importance (Elastic Net)')
plt.show()
