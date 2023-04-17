Source of dataset:

This dataset is originally from the National Institute of Diabetes and Digestive and Kidney. The objective of the dataset is to diagnostically predict whether a patient has diabetes, based on certain diagnostic measurements included in the dataset. Several constraints were placed on the selection of these instances from a larger database. In particular, all patients here are females at least 21 years old of Pima Indian heritage.
Link: https://www.kaggle.com/datasets/akshaydattatraykhare/diabetes-dataset



Data Dictionary:

Pregnancies -> This field represents the number of times a person has been pregnant. It is a discrete variable with integer values ranging from 0 to a positive integer.	

Glucose -> This field represents the glucose level (measured in mg/dL) of the person. It is a continuous variable with numeric values representing the person's blood glucose concentration.

Blood Pressure -> This field represents the glucose level (measured in mg/dL) of the person. It is a continuous variable with numeric values representing the person's blood glucose concentration.

Skin Thickness	-> This field represents the skin thickness (measured in mm) of the person. It is a continuous variable with numeric values representing the thickness of the skin at a particular location on the body.	

Insulin -> This field represents the insulin level (measured in mu U/ml) of the person. It is a continuous variable with numeric values representing the person's insulin concentration.

BMI -> This field represents the Body Mass Index (BMI) of the person. It is a continuous variable with numeric values representing the person's BMI.

Diabetes Pedigree Function -> This field represents a function that scores the diabetes history of the person's ancestors. It is a continuous variable with numeric 
values representing the person's diabetes pedigree function score.

Age	-> This field represents the age (in years) of the person. It is a discrete variable with integer values representing the person's age.

Outcome	-> This field represents the outcome of the person's diabetes diagnosis. It is a binary variable with values 0 or 1, where 0 indicates no diabetes and 1 indicates diabetes.




Questions that I tried to answer (Hypothesis):

Principal Component Analysis (PCA):
1.	How many variables we can reduce to, by using Principal Component Analysis?
2.	Are we able to identify the contribution of different features towards the overall variability or explained variance in the diabetes dataset?

Clustering Analysis
1.	Does clustering analysis help in identifying any pattern in the dataset, such as difference in glucose level, age, or insulin?

Exploratory Factor Analysis
1.	Can we reduce the number of factors using EFA, by identifying the minimum number of factors that capture the majority of the variance of the dataset?

Multiple Regression
1.	We are trying to predict the age of the person by using Multiple Regression, based on the other features like pregnancies, glucose, insulin, etc.

Logistic Regression
1.	We are trying to predict the person’s outcome (diabetic or non-diabetic) by using Logistic Regression, based on other features like glucose, insulin, age, etc.

Supporting Evidence:
All the supporting evidence and interpretation are attached to the code.
