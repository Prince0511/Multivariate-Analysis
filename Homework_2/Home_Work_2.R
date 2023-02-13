#Loading Data set
library(readr)
Bumpus_sparrows <- read_csv("D:/MITA/SPRING/Multivariate_Analysis/Data_Sets/Bumpus_sparrows.csv")
sparr <- data.frame(Bumpus_sparrows)
View(sparr)


###############################################################################
#STATISTICAL ANALYSIS
str(sparr)
stars(sparr,labels = sparrows$Survivorship)



################################################################################
#SCATTER PLOT MATRIX (USING GGALLY)
attach(sparr)
library(GGally)
ggscatmat(sparr, columns = 2:6, color = "Survivorship", alpha = 0.7)

#Trying different parameters to explore i.e., "corMethod"

#corMethod = Pearson
ggscatmat(sparr, columns = 2:6, color = "Survivorship", alpha = 0.7, corMethod = "pearson")

#corMethod = Kendall
ggscatmat(sparr, columns = 2:6, color = "Survivorship", alpha = 0.7, corMethod = "kendall")

#corMethod = Spearman
ggscatmat(sparr, columns = 2:6, color = "Survivorship", alpha = 0.7, corMethod = "spearman")

################################################################################
#GGPLOT VISUALIZATIONS

library(ggplot2)

#Created a new column "survive" where "S" = 1 and "NS" = 0
sparr$survive <- with(sparr, ifelse(Survivorship == "S", 1, 0))
View(sparr)

#Scatterplot using ggplot between Total_length and survive
ggplot(sparr, mapping = aes(x=Total_length, y=survive)) + geom_point()

#Histogram using ggplot of Total_length
ggplot(sparr, mapping = aes(Total_length)) + geom_histogram(binwidth = 1, fill = "blue")

#Histogram using ggplot of Total_length with survivorship
ggplot(sparr, aes(x=Total_length, fill = Survivorship)) + geom_histogram(position = 'fill', binwidth = 1) + ylab("Survival")
#Interpretation: What I think/interpret using this chart is, the birds of longer length are less likely to survive than the shorter one.

#Scatterplot using ggplot between Total_length and Humerus_length
ggplot(sparr, mapping = aes(x=Total_length, y=L_humerous)) + geom_point()

#Scatterplot and regression between Total_length and Humerus_length with survive
ggplot(sparr, mapping = aes(x=(Total_length), y=(L_humerous))) + geom_point(aes(color = survive)) + geom_smooth(method="lm", se = FALSE, color = "red")

#Just for fun: Fitting Model to gather some more insights
model1 <- glm(survive ~ Total_length + L_humerous, family = binomial, data = sparr)
summary(model1)

#Boxplot with ggplot of Survivorship and Total_length
ggplot(sparr, mapping = aes(x=Survivorship, y=Total_length)) + geom_boxplot() 

#Boxplot with ggplot of Survivorship and L_beak_head
ggplot(sparr, mapping = aes(x=Survivorship, y=L_beak_head)) + geom_boxplot() 

#Density plot using ggplot of Alar_extent filled with survivorship
ggplot(sparr, mapping = aes(x=Alar_extent, fill=Survivorship, alpha = 0.5)) + geom_density()
