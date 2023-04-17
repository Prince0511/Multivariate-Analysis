#Question trying to answer using Clustering Analysis.
#1. We are trying to find what is the optimal number of clusters on the dataset.
#2. Does clustering analysis help in identifying any pattern in the dataset, such as difference in glucose level, age, or insulin?

#Libraries
library(cluster)
library(readr)
library(factoextra)
library(magrittr)
library(NbClust)

#Loading the dataset
diabetes <- read_csv("D:/MITA/SPRING/Multivariate_Analysis/Homework/Clustering Analysis/diabetes.csv")
View(diabetes)

#Scaling the data for Standardization
diabetes_scale <- scale(diabetes[-9])
diabetes_scale

#Calculating distance of the scaled data
dist_diabetes <- dist(diabetes_scale, method = "euclidian")
dist_diabetes

#Cluster analysis by Single Linkage Method
clust_diabetes <- hclust(dist_diabetes, method = "single") 

#Plotting vertical dendrogram
plot(as.dendrogram(clust_diabetes),ylab="Distance between Patients",ylim=c(0,4.5),main="Dendrogram of Diabetes")

#Computing percentage of variation for two clusters
(kmeans2.diabetes <- kmeans(diabetes_scale, 2, nstart = 10))
perc.var.2 <- round(100*(1 - kmeans2.diabetes$betweenss/kmeans2.diabetes$totss),1)
names(perc.var.2) <- "Perc. 2 clus"
perc.var.2
#We get the variance as 83.5% by forming two clusters.

#Computing percentage of variation for three clusters
(kmeans3.diabetes <- kmeans(diabetes_scale, 3, nstart = 10))
perc.var.3 <- round(100*(1 - kmeans3.diabetes$betweenss/kmeans3.diabetes$totss),1)
names(perc.var.3) <- "Perc. 3 clus"
perc.var.3
#We get the variance as 71.0% by forming three clusters.

#Computing percentage of variation for four clusters
(kmeans4.diabetes <- kmeans(diabetes_scale, 4, nstart = 10))
perc.var.4 <- round(100*(1 - kmeans4.diabetes$betweenss/kmeans4.diabetes$totss),1)
names(perc.var.4) <- "Perc. 4 clus"
perc.var.4
#We get the variance as 63.8% by forming four clusters.

#Computing percentage of variation for five clusters
(kmeans5.diabetes <- kmeans(diabetes_scale, 5, nstart = 10))
perc.var.5 <- round(100*(1 - kmeans5.diabetes$betweenss/kmeans5.diabetes$totss),1)
names(perc.var.5) <- "Perc. 5 clus"
perc.var.5
#We get the variance as 59.0% by forming five clusters.

#Computing percentage of variation for six clusters
(kmeans6.diabetes <- kmeans(diabetes_scale, 6, nstart = 10))
perc.var.6 <- round(100*(1 - kmeans6.diabetes$betweenss/kmeans6.diabetes$totss),1)
names(perc.var.6) <- "Perc. 6 clus"
perc.var.6
#We get the variance as 54.7% by forming six clusters.

#Generating a Variance_List
Variance_List <- c(perc.var.2,perc.var.3,perc.var.4,perc.var.5, perc.var.6)
Variance_List

#Visualizing the generated Variance_List
plot(Variance_List)

#GGplot Visualization
res.dist <- get_dist(diabetes, stand = TRUE, method = "pearson")

#Visualizing the distance between each other
fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#Brief Interpretation
#We can see a dissimilarity matrix which is plotted using gradient color.
#Here, low indicates lowest dissimilarity values.
#mid indicates middle dissimilarity values.
#high indicates highest dissimilarity values.

#Finding Optimal Distance
fviz_nbclust(diabetes, kmeans, method = "gap_stat")

#Brief Interpretation
#From the graph we can see that, if number of clusters is 1, it has the gap statistics greater than 0.85.
#But for 2,3,4, and 5, the gap statistics is almost below 0.75.
#We basically prefer the number of clusters that has higher gap statistics because it indicates a better clustering structure.

#Plotting the Clusters, where cluster = 1
set.seed(123)
km.res <- kmeans(diabetes, 1, nstart = 25)
fviz_cluster(km.res, data = diabetes,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

#Plotting the Clusters, where cluster = 2
set.seed(123)
km.res <- kmeans(diabetes, 2, nstart = 25)
fviz_cluster(km.res, data = diabetes,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

#Brief Interpretation
#We can observe that, we are not able to distinguish the clusters easily when number of clusters are 2.
#We can see that yellow cluster has overlapped blue cluster, but noticeably, we can see that yellow cluster has half of the formed cluster which is not overlapped.

#Clustering using PAM Method
pam.res <- pam(diabetes, 2)

#Visualize
fviz_cluster(pam.res)

#Performing Hierarchical Clustering
res.hc <- diabetes %>% scale() %>% dist(method = "euclidean") %>% hclust(method = "ward.D2")

#Plotting Cluster dendrogram for k = 2
fviz_dend(res.hc, k = 2,
          cex = 0.5,
          k_colors = c("#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, 
          rect = TRUE 
)

#Optimal Number of Clusters
res.nbclust <- diabetes %>% scale() %>% NbClust(distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all") 

#Brief Interpretation
#We are using Hubert Index and D-Index method, which are graphical method to determine the number of clusters.
#From the majority rule, we can say that, the best number of clusters is 2 for our dataset.

#Visualization using Bar Chart
fviz_nbclust <- function (x, FUNcluster = NULL, method = c("silhouette", "wss", 
                                                           "gap_stat"), diss = NULL, k.max = 10, nboot = 100, verbose = interactive(), 
                          barfill = "steelblue", barcolor = "steelblue", linecolor = "steelblue", 
                          print.summary = TRUE, ...) 
{
  set.seed(123)
  if (k.max < 2) 
    stop("k.max must bet > = 2")
  method = match.arg(method)
  if (!inherits(x, c("data.frame", "matrix")) & !("Best.nc" %in% 
                                                  names(x))) 
    stop("x should be an object of class matrix/data.frame or ", 
         "an object created by the function NbClust() [NbClust package].")
  if (inherits(x, "list") & "Best.nc" %in% names(x)) {
    best_nc <- x$Best.nc
    if (any(class(best_nc) == "numeric") ) 
      print(best_nc)
    else if (any(class(best_nc) == "matrix") )
      .viz_NbClust(x, print.summary, barfill, barcolor)
  }
  else if (is.null(FUNcluster)) 
    stop("The argument FUNcluster is required. ", "Possible values are kmeans, pam, hcut, clara, ...")
  else if (!is.function(FUNcluster)) {
    stop("The argument FUNcluster should be a function. ", 
         "Check if you're not overriding the specified function name somewhere.")
  }
  else if (method %in% c("silhouette", "wss")) {
    if (is.data.frame(x)) 
      x <- as.matrix(x)
    if (is.null(diss)) 
      diss <- stats::dist(x)
    v <- rep(0, k.max)
    if (method == "silhouette") {
      for (i in 2:k.max) {
        clust <- FUNcluster(x, i, ...)
        v[i] <- .get_ave_sil_width(diss, clust$cluster)
      }
    }
    else if (method == "wss") {
      for (i in 1:k.max) {
        clust <- FUNcluster(x, i, ...)
        v[i] <- .get_withinSS(diss, clust$cluster)
      }
    }
    df <- data.frame(clusters = as.factor(1:k.max), y = v, 
                     stringsAsFactors = TRUE)
    ylab <- "Total Within Sum of Square"
    if (method == "silhouette") 
      ylab <- "Average silhouette width"
    p <- ggpubr::ggline(df, x = "clusters", y = "y", group = 1, 
                        color = linecolor, ylab = ylab, xlab = "Number of clusters k", 
                        main = "Optimal number of clusters")
    if (method == "silhouette") 
      p <- p + geom_vline(xintercept = which.max(v), linetype = 2, 
                          color = linecolor)
    return(p)
  }
  else if (method == "gap_stat") {
    extra_args <- list(...)
    gap_stat <- cluster::clusGap(x, FUNcluster, K.max = k.max, 
                                 B = nboot, verbose = verbose, ...)
    if (!is.null(extra_args$maxSE)) 
      maxSE <- extra_args$maxSE
    else maxSE <- list(method = "firstSEmax", SE.factor = 1)
    p <- fviz_gap_stat(gap_stat, linecolor = linecolor, 
                       maxSE = maxSE)
    return(p)
  }
}

.viz_NbClust <- function (x, print.summary = TRUE, barfill = "steelblue", 
                          barcolor = "steelblue") 
{
  best_nc <- x$Best.nc
  if (any(class(best_nc) == "numeric") )
    print(best_nc)
  else if (any(class(best_nc) == "matrix") ) {
    best_nc <- as.data.frame(t(best_nc), stringsAsFactors = TRUE)
    best_nc$Number_clusters <- as.factor(best_nc$Number_clusters)
    if (print.summary) {
      ss <- summary(best_nc$Number_clusters)
      cat("Among all indices: \n===================\n")
      for (i in 1:length(ss)) {
        cat("*", ss[i], "proposed ", names(ss)[i], 
            "as the best number of clusters\n")
      }
      cat("\nConclusion\n=========================\n")
      cat("* According to the majority rule, the best number of clusters is ", 
          names(which.max(ss)), ".\n\n")
    }
    df <- data.frame(Number_clusters = names(ss), freq = ss, 
                     stringsAsFactors = TRUE)
    p <- ggpubr::ggbarplot(df, x = "Number_clusters", 
                           y = "freq", fill = barfill, color = barcolor) + 
      labs(x = "Number of clusters k", y = "Frequency among all indices", 
           title = paste0("Optimal number of clusters - k = ", 
                          names(which.max(ss))))
    return(p)
  }
}

#Assign them to the factoextra namespace
environment(fviz_nbclust) <- asNamespace("factoextra")
assignInNamespace("fviz_nbclust",fviz_nbclust,"factoextra")
environment(.viz_NbClust) <- asNamespace("factoextra")
assignInNamespace(".viz_NbClust",.viz_NbClust,"factoextra")
fviz_nbclust(res.nbclust, ggtheme = theme_minimal())

#Brief Interpretation
#We can see that, number of clusters = 2 has the highest frequency among all indices.
#Therefore, we can say that 2 is the optimal number of clusters for our dataset.

# Quality of Clustering
set.seed(123)

# Enhanced hierarchical clustering, cut in 2 groups
res.hc <- diabetes %>% scale() %>% eclust("hclust", k = 2, graph = FALSE)

# Visualize with factoextra
fviz_dend(res.hc, palette = "jco",rect = TRUE, show_labels = FALSE)

#Inspect the silhouette plot:
fviz_silhouette(res.hc)

#Brief Interpretation
#We are using this method to measure the quality of clustering in dataset.
#It's value ranges from -1 to 1, where higher value indicate better clustering.
#From 768 values, 265 belongs to cluster 1 and 503 belongs to cluster 2.
#Since, we observed an overlap in our cluster, we are oberserving few negative silhouette value for cluster 1 and 2.

# Silhouette width of observations
sil <- res.hc$silinfo$widths[, 1:3]

# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]

#Brief Interpretation
#We can see a list of where the values with negative silhouette along with its cluster and index is listed.