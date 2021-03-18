## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(aweSOM)

full.data <- iris
## Select variables
train.data <- full.data[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
### Scale training data
train.data <- scale(train.data)

### RNG Seed (for reproducibility)
set.seed(1465)
### Initialization (PCA grid)
init <- somInit(train.data, 4, 4)
## Train SOM
iris.som <- kohonen::som(train.data, grid = kohonen::somgrid(4, 4, "hexagonal"), 
                         rlen = 100, alpha = c(0.05, 0.01), radius = c(2.65,-2.65), 
                         dist.fcts = "sumofsquares", init = init)

## -----------------------------------------------------------------------------
somQuality(iris.som, train.data)

## -----------------------------------------------------------------------------
superclust_pam <- cluster::pam(iris.som$codes[[1]], 3)
superclasses_pam <- superclust_pam$clustering

superclust_hclust <- hclust(dist(iris.som$codes[[1]]), "complete")
superclasses_hclust <- cutree(superclust_hclust, 3)

## -----------------------------------------------------------------------------
aweSOMplot(som = iris.som, type = "Cloud", data = full.data, 
           variables = c("Species", "Sepal.Length", "Sepal.Width",  
                         "Petal.Length", "Petal.Width"), 
           superclass = superclasses_pam)

## -----------------------------------------------------------------------------
aweSOMplot(som = iris.som, type = "Hitmap", superclass = superclasses_pam)

## -----------------------------------------------------------------------------
aweSOMplot(som = iris.som, type = "UMatrix", superclass = superclasses_pam)

## ---- fig.width=8, fig.height=7-----------------------------------------------
aweSOMsmoothdist(iris.som)

## -----------------------------------------------------------------------------
aweSOMplot(som = iris.som, type = "Circular", data = full.data, 
           variables = c("Sepal.Length", "Sepal.Width",  
                         "Petal.Length", "Petal.Width"), 
           superclass = superclasses_pam)

## -----------------------------------------------------------------------------
aweSOMplot(som = iris.som, type = "Barplot", data = full.data, 
           variables = c("Sepal.Length", "Sepal.Width",  
                         "Petal.Length", "Petal.Width"), 
           superclass = superclasses_pam, 
           values = "prototypes")

## -----------------------------------------------------------------------------
aweSOMplot(som = iris.som, type = "Boxplot", data = full.data, 
           variables = c("Sepal.Length", "Sepal.Width",  
                         "Petal.Length", "Petal.Width"),
           superclass = superclasses_pam, 
           scales = "same")

## -----------------------------------------------------------------------------
aweSOMplot(som = iris.som, type = "Line", data = full.data, 
           variables = c("Sepal.Length", "Sepal.Width",  
                         "Petal.Length", "Petal.Width"), 
           superclass = superclasses_pam, 
           values = "median")

## -----------------------------------------------------------------------------
aweSOMplot(som = iris.som, type = "Radar", data = full.data, 
           variables = c("Sepal.Length", "Sepal.Width",  
                         "Petal.Length", "Petal.Width"), 
           superclass = superclasses_pam)

## -----------------------------------------------------------------------------
aweSOMplot(som = iris.som, type = "Color", data = full.data, 
           variables = "Sepal.Length", superclass = superclasses_pam)

## -----------------------------------------------------------------------------
aweSOMplot(som = iris.som, type = "CatBarplot", data = full.data, 
           variables = "Species", superclass = superclasses_pam)

## -----------------------------------------------------------------------------
aweSOMplot(som = iris.som, type = "Pie", data = full.data, variables = "Species", 
           superclass = superclasses_pam)

## ---- fig.width=7, fig.height=4-----------------------------------------------
aweSOMscreeplot(som = iris.som, method = "pam", nclass = 3)

## ---- fig.width=7, fig.height=6-----------------------------------------------
aweSOMsilhouette(iris.som, superclasses_pam)

## ---- fig.width=7, fig.height=4-----------------------------------------------
aweSOMdendrogram(clust = superclust_hclust, nclass = 3)

