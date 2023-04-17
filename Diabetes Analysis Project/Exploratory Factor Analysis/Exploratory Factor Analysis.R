#Question we are trying to answer.
#1.	Can we reduce the number of factors using EFA, by identifying the minimum number of factors that capture the majority of the variance of the dataset?

#Libraries
library(readr)
library(psych)
library(GPArotation)

#Loading the Dataset
diabetes <- read_csv("D:/MITA/SPRING/Multivariate_Analysis/Homework/Exploratory Factor Analysis/diabetes.csv")
View(diabetes)

#Principal Component Analysis
#Rotate using Varimax Method
method_1 <- principal(diabetes[-9], nfactors=4, rotate="varimax")
method_1

#Brief Interpretation
#We passed nfactors = 4 which refers to the number of factors or PCs.
#RC2, RC1, RC3, and RC4 has some values which represents the loadings of each variables.
#Positive loading indicates that there is a positive correlation between the original variable and extracted Principal Components.
#While, negative loading indicates a negative relationship.
#h2 shows the proportion of variance in each variable explained by the principal components.
#u2 shows the proportion of variance in each variable unexplained by the principal components.
#com shows the complexity to explain the variance.
#SS Loadings shows the squared loading of each principal components.
#Proportion Variance shows the proportion of total variance explained by each principal component.
#Cumulative Variance shows the cumulative proportion of total variance explained by each principal component
#Proportion Explained shows the proportion of total variance explained by each principal component, expressed as a percentage.
#Cumulative Proportion shows the cumulative proportion of total variance explained by each principal component, expressed as a percentage.

#Rounding method_1$values (Eigen Values) to 3 decimal places.
round(method_1$values, 3)

#Rounding fir.pc$values (Eigen Values).
round(method_1$values)

#Combining columns based on loadings
method_1$loadings

#Brief Interpretation
#From the result we can make following conclusions:
# RC1 shows the relation with Blood Pressure, Skin Thickness, and BMI.
# RC2 shows the relation with Pregnancies, and Age.
# RC3 shows the relation with Glucose, and Insulin.
# RC4 shows the relation with only Diabetes Pedigree Function.
#All four RCs explains around 72% (71.6% precise) of the variance.

#Loadings with more digits
for (i in c(1,3,2,4)) { print(method_1$loadings[[1,i]])}

#Communalities
method_1$communality

#Brief Interpretation
#Pregnancies has a communality estimate of 0.6508162, meaning that approximately 65% of the variance in Pregnancies is explained by the extracted factors.
#Similarly, Glucose has a communality estimate of 0.7446147, indicating that approximately 74% of the variance in Glucose is explained by the extracted factors.

#Rotated factor scores
method_1$scores
#Notice the columns ordering: RC2, RC1, RC3 and RC4

#Factor Recommendation
fa.parallel(diabetes[-9])

#Brief Interpretation
#Parallel analysis suggests that the number of factors =  3  and the number of components =  2.

#Correlations within Factors
fa.plot(method_1)

#Visualizaing the relationship
fa.diagram(method_1)

#Brief Interpretation
#We can say that we reduced the factors from 8 to 4.
#It can be seen from the Components Analysis Plot.

#Factor Recommendation for a simple structure
vss(diabetes[-9])

#Brief Interpretation
#We can also see from Very Simple Structure that after factor number 4, we can't see any increase in the explained variance.
#Therefore, we can conclude that number of factors = 4 is a good fit.