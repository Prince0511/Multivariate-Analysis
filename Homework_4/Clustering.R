# Loading the dataset
library(readr)
wine <- read_csv("D:/MITA/SPRING/Multivariate_Analysis/Homework/Homework_4/wine.csv")
View(wine)

#Information about dataset
# 1) Alcohol - Content of alcohol in wine
# 2) Malic acid - Amount of Malic acid in wine
# 3) Ash - Amount of Ash in wine
# 4) Alcalinity of ash - Amount of Alcalinity of ash in wine
# 5) Magnesium - Amount of Magnesium in wine
# 6) Total phenols - Amount of phenols in wine
# 7) Flavanoids - Amount of Flavanoids in wine
# 8) Nonflavanoid phenols - Amount of Nonflavanoids phenols in wine
# 9) Proanthocyanins - Amount of Proanthocyanins in wine
# 10)Color intensity - Concentration of Color intensity in wine
# 11)Hue - Concentraion of Hue in wine
# 12)OD280/OD315 of diluted wines - Content of wine diluted
# 13)Proline - Amount of Proline in wine

# Loading Libraries
library(cluster)
library(readr)
library(factoextra)
library(magrittr)
library(NbClust)

#Scaling the data for standardization
matstd.wine <- scale(wine)
matstd.wine

#Calculating distance of the scaled data
dist.wine <- dist(matstd.wine, method = "euclidian")
dist.wine

#Cluster analysis by Single Linkage Method
clustwine <- hclust(dist.wine, method = "single") 

#Plotting vertical dendrogram
plot(as.dendrogram(clustwine),ylab="Distance between Wines",ylim=c(0,4.5),main="Dendrogram of Wine")

#Computing percentage of variation for two clusters
(kmeans2.wine <- kmeans(matstd.wine,2,nstart = 10))
perc.var.2 <- round(100*(1 - kmeans2.wine$betweenss/kmeans2.wine$totss),1)
names(perc.var.2) <- "Perc. 2 clus"
perc.var.2

#Computing percentage of variation for three clusters
(kmeans3.wine <- kmeans(matstd.wine,3,nstart = 10))
perc.var.3 <- round(100*(1 - kmeans3.wine$betweenss/kmeans3.wine$totss),1)
names(perc.var.3) <- "Perc. 3 clus"
perc.var.3

#Computing percentage of variation for four clusters
(kmeans4.wine <- kmeans(matstd.wine,4,nstart = 10))
perc.var.4 <- round(100*(1 - kmeans4.wine$betweenss/kmeans4.wine$totss),1)
names(perc.var.4) <- "Perc. 4 clus"
perc.var.4

#Computing percentage of variation for five clusters
(kmeans5.wine <- kmeans(matstd.wine,5,nstart = 10))
perc.var.5 <- round(100*(1 - kmeans5.wine$betweenss/kmeans5.wine$totss),1)
names(perc.var.5) <- "Perc. 5 clus"
perc.var.5

#Computing percentage of variation for six clusters
(kmeans6.wine <- kmeans(matstd.wine,6,nstart = 10))
perc.var.6 <- round(100*(1 - kmeans6.wine$betweenss/kmeans6.wine$totss),1)
names(perc.var.6) <- "Perc. 6 clus"
perc.var.6

#Generating a Variance_List
Variance_List <- c(perc.var.2,perc.var.3,perc.var.4,perc.var.5, perc.var.6)
Variance_List

#Visualizing the generated Variance_List
plot(Variance_List)

## GGPlot Visualization
res.dist <- get_dist(wine, stand = TRUE, method = "pearson")

#Visualizing the distance between each other
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#Finding Optimal Distance
fviz_nbclust(wine, kmeans, method = "gap_stat")
set.seed(123)
km.res <- kmeans(wine, 3, nstart = 25)

# Visualize
fviz_cluster(km.res, data = wine,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

#Using PAM Method
pam.res <- pam(wine, 3)

#Visualize
fviz_cluster(pam.res)

#Performing Hierarchical Clustering
res.hc <- wine %>% scale() %>% dist(method = "euclidean") %>%
  hclust(method = "ward.D2")


fviz_dend(res.hc, k = 3,
          cex = 0.5,
          k_colors = c("#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, 
          rect = TRUE 
)

#Optimal Number of Clusters
res.nbclust <- wine %>% scale() %>% NbClust(distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all") 

#Visualization using Bar Chart
fviz_nbclust(res.nbclust, ggtheme = theme_minimal())

#Code to solve Error of line 119
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

# assign them to the factoextra namespace
environment(fviz_nbclust) <- asNamespace("factoextra")
assignInNamespace("fviz_nbclust",fviz_nbclust,"factoextra")
environment(.viz_NbClust) <- asNamespace("factoextra")
assignInNamespace(".viz_NbClust",.viz_NbClust,"factoextra")
fviz_nbclust(res.nbclust, ggtheme = theme_minimal())




# Quality of Clustering
set.seed(123)
# Enhanced hierarchical clustering, cut in 3 groups
res.hc <- wine %>% scale() %>% eclust("hclust", k = 3, graph = FALSE)

# Visualize with factoextra
fviz_dend(res.hc, palette = "jco",rect = TRUE, show_labels = FALSE)

#Inspect the silhouette plot:
fviz_silhouette(res.hc)

# Silhouette width of observations
sil <- res.hc$silinfo$widths[, 1:3]

# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]


