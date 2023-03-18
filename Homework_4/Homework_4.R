#Loading the dataset
library(readr)
wine <- read.csv("D:/MITA/SPRING/Multivariate_Analysis/Homework/Homework_4/wine.csv")
View(wine)

#Loading Libraries
library(psych)
library(GPArotation)

attach(wine)

#METHOD - 1
#Using varimax rotate method
fit.pc <- principal(wine[-1], nfactors=4, rotate="varimax")
fit.pc

#The output of fit.pc describes following:
# h2 -> It is explained variance
# u2 -> It is unxplained variance
# If we add up h2 and u2 together, we get the total variance as 1.

#Rounding fit.pc$values (Eigen Values) to 3 decimal places.
round(fit.pc$values, 3)

#Rounding fir.pc$values (Eigen Values).
round(fit.pc$values)

#Combining columns
fit.pc$loadings

#From the result we can make following conclusions:
# RC1 shows the relation with Flavanoids, Total_Phenols, OD280, Proanthocyanins, and negative of Nonflavanoid_Phenols.
# RC2 shows the relation with Magnesium and Color_Intensity.
# RC3 shows the relation with Ash, and Ash_Alcanity.
# RC4 shows the relation with Hue, negative of Malic_Acid, and negative of Color_Intensity.

# Loadings with more digits
for (i in c(1,3,2,4)) { print(fit.pc$loadings[[1,i]])}

# Communalities
fit.pc$communality

# Rotated factor scores, Notice the columns ordering: RC1, RC4, RC2 and RC3
fit.pc$scores

#Factor Recommendation
fa.parallel(wine)

#Elbow can be seen at 3.

#Correlations within Factors
fa.plot(fit.pc)

#Visualizaing the relationship
fa.diagram(fit.pc)

#Factor Recommendation for a simple structure
vss(wine[-1])


#METHOD - 2
#Using oblimin rotate method
fit.pc01 <- principal(wine[-1], nfactors=4, rotate="oblimin")
fit.pc01


#Rounding fit.pc$values (Eigen Values) to 3 decimal places.
round(fit.pc01$values, 3)

#Rounding fir.pc$values (Eigen Values).
round(fit.pc01$values)

#Combining columns
fit.pc01$loadings

#From the result we can make following conclusions:
# TC1 shows the relation with Proanthocyanins, Flavanoids, Total_Phenols, OD280, and negative of Nonflavanoid_Phenols.
# TC2 shows the relation with Proline, Magnesium, and Color_Intensity.
# TC3 shows the relation with Ash, and Ash_Alcanity.
# TC4 shows the relation with Malic_Acid, Color_Intensity, and negative of Hue.

# Loadings with more digits
for (i in c(1,3,2,4)) { print(fit.pc01$loadings[[1,i]])}

# Communalities
fit.pc01$communality

# Rotated factor scores, Notice the columns ordering: RC1, RC4, RC2 and RC3
fit.pc01$scores

#Correlations within Factors
fa.plot(fit.pc01)

#Visualizaing the relationship
fa.diagram(fit.pc01)


# Computing Correlation Matrix
corrm.wine <- cor(wine[-1])
corrm.wine
plot(corrm.wine)
wine_pca <- prcomp(wine[-1], scale=TRUE)
summary(wine_pca)
plot(wine_pca)


(eigen_wine <- round(wine_pca$sdev^2,3))
round(fit.pc$values, 3)
names(eigen_wine) <- paste("PC",1:9,sep="")
eigen_wine
sumlambdas <- sum(eigen_wine)
sumlambdas
propvar <- round(eigen_wine/sumlambdas,2)
propvar
cumvar_wine <- cumsum(propvar)
cumvar_wine
matlambdas <- rbind(eigen_wine,propvar,cumvar_wine)
matlambdas
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
rownames(matlambdas)
eigvec.wine <- wine_pca$rotation
print(wine_pca)

# Taking the first four PCs
pcafactors.wine <- eigvec.wine[,1:4]
pcafactors.wine

# Multiplying each column of the eigenvector’s matrix by the square-root of the corresponding eigenvalue in order to get the factor loadings
unrot.fact.wine <- sweep(pcafactors.wine,MARGIN=2,wine_pca$sdev[1:4],`*`)
unrot.fact.wine

# Computing communalities
communalities.wine <- rowSums(unrot.fact.wine^2)
communalities.wine

# Performing the varimax rotation.
rot.fact.wine <- varimax(unrot.fact.wine)

#View(unrot.fact.wine)
rot.fact.wine

# The print method of varimax omits loadings less than abs(0.1).
fact.load.wine <- rot.fact.wine$loadings[1:9,1:4]
fact.load.wine

