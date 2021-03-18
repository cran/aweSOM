## Functions to plot SOM

getPalette <- function(pal, n, reverse= FALSE) {
  if(pal == "grey") {
    res <- grey(1:n / n)
  } else if(pal == "rainbow") { 
    res <- substr(rainbow(n), 1, 7) 
  } else if(pal == "heat") { 
    res <- substr(heat.colors(n), 1, 7) 
  } else if(pal == "terrain") { 
    res <- substr(terrain.colors(n), 1, 7) 
  } else if(pal == "topo") { 
    res <- substr(topo.colors(n), 1, 7) 
  } else if(pal == "cm") { 
    res <- substr(cm.colors(n), 1, 7) 
  } else if (pal == "viridis") {
    if (n == 1) {
      res <- substr(viridis::viridis(3), 1, 7)[1]
    } else if (n == 2) {
      res <- substr(viridis::viridis(3), 1, 7)[c(1,3)]
    } else 
      res <- substr(viridis::viridis(n), 1, 7)
  } else {
    if (n == 1) {
      res <- RColorBrewer::brewer.pal(3, pal)[1]
    } else if (n == 2) {
      res <- RColorBrewer::brewer.pal(3, pal)[c(1,3)]
    } else 
      res <- RColorBrewer::brewer.pal(n, pal)
  }
  if (length(res) == 1) 
    res <- list(res)
  if (reverse) 
    res <- rev(res)
  res
}




#' Dendogram of hierarchical clustering of SOM cells
#'
#' Plots the dendogram of a hierarchical clustering of the SOM prototypes.
#'
#' @param clust an object of class \code{hclust}, the result of a hierarchical
#'   clustering performed by \code{stats::hclust}.
#' @param nclass an integer, number of superclasses
#' 
#' @return Returns \code{NULL} if \code{nclass} is 1, or else a \code{list}
#'   containing the indices of the SOM cells in each superclass.
#'
#' @examples
#' ## Build training data
#' dat <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
#' ### Scale training data
#' dat <- scale(dat)
#' ## Train SOM
#' ### Initialization (PCA grid)
#' init <- somInit(dat, 4, 4)
#' ok.som <- kohonen::som(dat, grid = kohonen::somgrid(4, 4, 'hexagonal'),
#'                        rlen = 100, alpha = c(0.05, 0.01),
#'                        radius = c(2.65,-2.65),
#'                        init = init, dist.fcts = 'sumofsquares')
#' ## Group cells into superclasses (hierarchical clustering)
#' superclust <- hclust(dist(ok.som$codes[[1]]), 'complete')
#' ## Plot superclasses dendrogram
#' aweSOMdendrogram(superclust, 2)
aweSOMdendrogram <- function(clust, nclass){
  if (is.null(clust)) return(NULL)
  if (!("hclust" %in% class(clust))) {
    warning("argument 'clust' must be of class 'hclust'")
    return(NULL)
  }
  plot(clust, xlab= "", main= "")
  if (nclass > 1)
    rect.hclust(clust, k= nclass)
}



#' Screeplot of SOM superclasses
#'
#' The screeplot, helps deciding the optimal number of superclasses. Available
#' for both PAM and hierarchical clustering.
#'
#' @param som \code{kohonen} object, a SOM created by the \code{kohonen::som}
#'   function.
#' @param nclass number of superclasses to be visualized in the screeplot.
#'   Default is 2.
#' @param method Method used for clustering. Hierarchical clustering
#'   ("hierarchical") and Partitioning around medoids ("pam") can be used.
#'   Default is hierarchical clustering.
#' @param hmethod For hierarchicical clustering, the clustering method, by
#'   default "complete". See the \code{stats::hclust} documentation for more
#'   details.
#'   
#' @return No return value, called for side effects.
#'
#' @examples
#' ## Build training data
#' dat <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
#' ### Scale training data
#' dat <- scale(dat)
#' ## Train SOM
#' ### Initialization (PCA grid)
#' init <- somInit(dat, 4, 4)
#' ok.som <- kohonen::som(dat, grid = kohonen::somgrid(4, 4, 'hexagonal'),
#'                        rlen = 100, alpha = c(0.05, 0.01),
#'                        radius = c(2.65,-2.65),
#'                        init = init, dist.fcts = 'sumofsquares')
#' ## Group cells into superclasses (PAM clustering)
#' superclust <- cluster::pam(ok.som$codes[[1]], 2)
#' superclasses <- superclust$clustering
#' aweSOMscreeplot(ok.som, method = 'hierarchical',
#'                 hmethod = 'complete', nclass = 2)
aweSOMscreeplot <- function(som, nclass= 2, 
                            method= c("hierarchical", "pam"), 
                            hmethod= c("complete", "ward.D2", "ward.D", "single", 
                                       "average", "mcquitty", "median", "centroid")){
  if (is.null(som)) return(NULL)
  method <- match.arg(method)
  hmethod <- match.arg(hmethod)
  
  if (method == "hierarchical")
    ok.hclust <- hclust(dist(som$codes[[1]]), hmethod)
  
  ncells <- nrow(som$codes[[1]])
  nvalues <- max(nclass, min(ncells, max(ceiling(sqrt(ncells)), 10)))
  clust.var <- sapply(1:nvalues, function(k) {
    if (method == "hierarchical") {
      clust <- cutree(ok.hclust, k)
    } else if (method == "pam") 
      clust <- cluster::pam(som$codes[[1]], k)$clustering
    clust.means <- do.call(rbind, by(som$codes[[1]], clust, colMeans))[clust, ]
    mean(rowSums((som$codes[[1]] - clust.means)^2))
  })
  unexpl <- 100 * round(clust.var / 
                          (sum(apply(som$codes[[1]], 2, var)) * (ncells - 1) / ncells), 3)
  plot(unexpl, t= "b", ylim= c(0, 100),
       xlab= "Nb. Superclasses", ylab= "% Unexpl. Variance")
  grid()                      
  abline(h= unexpl[nclass], col= 2)
}




#' Smooth Distance Plot for SOM
#'
#' Plots a visualization of the distances between the SOM cells. Based on the
#' U-Matrix, which is computed for each cell as the mean distance to its
#' immediate neighbors.
#'
#' @param som \code{kohonen} object, a SOM created by the \code{kohonen::som} function.
#' @param pal character, the color palette. Default is "viridis". Can be
#'   "viridis", "grey", "rainbow", "heat", "terrain", "topo", "cm", or any
#'   palette name of the RColorBrewer package.
#' @param reversePal logical, whether color palette should be reversed. Default
#'   is FALSE.
#' @param legendFontsize numeric, the font size for the legend. Default 14.
#'
#' @return Returns an object of classes \code{gg} and \code{ggplot}.
#' 
#' @examples
#' ## Build training data
#' dat <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
#' ### Scale training data
#' dat <- scale(dat)
#' ## Train SOM
#' ### Initialization (PCA grid)
#' init <- somInit(dat, 4, 4)
#' ok.som <- kohonen::som(dat, grid = kohonen::somgrid(4, 4, 'rectangular'),
#'                        init = init)
#' aweSOMsmoothdist(ok.som)
aweSOMsmoothdist <- function(som, 
                             pal = c("viridis", "grey", "rainbow", "heat", 
                                     "terrain", "topo", "cm", 
                                     rownames(RColorBrewer::brewer.pal.info)), 
                             reversePal = FALSE, 
                             legendFontsize = 14) {
  if (is.null(som)) return(NULL)
  pal <- match.arg(pal)
  
  mapdist <- aweSOM::somDist(som)
  plates <- fields::Tps(x = som$grid$pts, 
                        Y = rowMeans(mapdist$proto.data.dist.neigh,
                                     na.rm= TRUE), 
                        give.warnings = FALSE)
  interpol <- expand.grid(x = seq(min(som$grid$pts[, 1]) - 0.25, 
                                  max(som$grid$pts[, 1]) + 0.25, by = 0.1), 
                          y = seq(min(som$grid$pts[, 2]) - 0.25, 
                                  max(som$grid$pts[, 2]) + 0.25, by = 0.1))
  interpol <- data.frame(interpol, 
                         z = fields::predict.Krig(plates, interpol)[, 1])
  ggplot2::ggplot(interpol, ggplot2::aes_string(x = "x", y = "y", z = "z")) + 
    ggplot2::geom_contour_filled(binwidth = 1.2 * diff(range(interpol$z)) / 8, 
                                 na.rm = TRUE) + 
    ggplot2::geom_point(data= data.frame(som$grid$pts, z = NA), 
                        size = legendFontsize / 10) +
    ggplot2::theme_void() + ggplot2::coord_fixed() + 
    ggplot2::scale_fill_discrete(
      type = getPalette(pal, 8, reversePal), 
      guide = ggplot2::guide_legend(reverse = TRUE, title = "Distance")) + 
    ggplot2::theme(
      legend.text = ggplot2::element_text(size = legendFontsize), 
      legend.title = ggplot2::element_text(size = legendFontsize + 2))
}




#' Silhouette plot of SOM superclasses
#'
#' Plots a silhouette plot, used to assess the quality of the super-clustering
#' of SOM prototypes into superclasses. Available for both PAM and
#' hierarchical clustering.
#'
#' @param som \code{kohonen} object, a SOM created by the \code{kohonen::som} function.
#' @param clust object containing the result of the super-clustering of the SOM
#'   prototypes (either a \code{hclust} or a \code{pam} object).
#'
#' @return No return value, called for side effects.
#' 
#' @examples
#' ## Build training data
#' dat <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
#' ### Scale training data
#' dat <- scale(dat)
#' ## Train SOM
#' ### Initialization (PCA grid)
#' init <- somInit(dat, 4, 4)
#' ok.som <- kohonen::som(dat, grid = kohonen::somgrid(4, 4, 'hexagonal'),
#'                        rlen = 100, alpha = c(0.05, 0.01),
#'                        radius = c(2.65,-2.65), init = init,
#'                        dist.fcts = 'sumofsquares')
#' ## Group cells into superclasses (PAM clustering)
#' superclust <- cluster::pam(ok.som$codes[[1]], 2)
#' superclasses <- superclust$clustering
#' aweSOMsilhouette(ok.som, superclasses)
aweSOMsilhouette <- function(som, clust){
  if (is.null(som)) return(NULL)
  plot(cluster::silhouette(clust, dist(som$codes[[1]])), 
       main= "Silhouette of Cell Superclasses")
}





################################################################################
## d3-based plots
################################################################################



#################################
## Generate plot parameters to be passed to D3 functions
#################################

getPlotParams <- function(type, som, superclass, data, plotsize, 
                          varnames, cellNames, 
                          normtype, valueFormat,
                          palsc, palplot, reversePal, 
                          plotOutliers, showSC, equalSize, 
                          showAxes, transparency, showNames= TRUE, 
                          legendPos= "below", legendFontsize= 14, 
                          cloudType, cloudSeed) {
  
  ##########
  ## Common parameters for all plots
  ##########
  somsize <- nrow(som$grid$pts)
  clustering <- factor(som$unit.classif, 1:somsize)
  clust.table <- table(clustering)
  
  gridInfo <- list(nbLines= som$grid$ydim,
                   nbColumns= som$grid$xdim,
                   topology= ifelse(som$grid$topo == "rectangular", 
                                    'rectangular', "hexagonal"))
  n.sc <- length(unique(superclass))
  superclassColor <- getPalette(palsc, n.sc)
  res <- list(plotType= type, 
              sizeInfo= plotsize, 
              gridInfo= gridInfo, 
              superclass= superclass, 
              superclassColor= superclassColor, 
              cellNames= cellNames, 
              clustering= as.numeric(clustering),
              cellPop= unname(clust.table), 
              showAxes= showAxes, 
              transparency= transparency, 
              showNames= showNames,
              legendPos= legendPos, 
              legendFontsize= legendFontsize, 
              legendReverse= FALSE)

  if (type == "Cloud") {
    fulldata <- data[, varnames[-1], drop = FALSE]
    varnames <- varnames[1]
    if (varnames == "None") {
      data <- NULL
    } else data <- data[, varnames]
  }
  if (type %in% c("Pie", "CatBarplot", "Cloud")) if (!is.null(data)) {
    if (length(dim(data)) == 2) data <- data[, varnames]
    if (is.numeric(data)) {
      ## Transform Numeric variables into factors with max 8 levels
      if (length(unique(data)) > 8) {
        data <- cut(data, 8)
      } else data <- as.factor(data)
      if (legendPos == "beside") {
        res$legendReverse <- TRUE
      }
    } else data <- as.factor(data)
    unique.values <- levels(data)
    nvalues <- nlevels(data)
  }
  
  if (type %in% c("Circular", "Line", "Barplot", "Boxplot", "Color", "Radar")) {
    if (is.null(dim(data))) {
      data <- data.frame(data)
      colnames(data) <- varnames
    } else data <- as.data.frame(data)
    if (type == "Color") 
      data <- as.data.frame(sapply(data, as.numeric))
    
    nvar <- length(varnames)
    
    ##########
    ## Compute normalized values for mean/median/prototype to use in plots
    ##########
    
    if (type %in% c("Boxplot", "Circular", "Line", "Barplot", "Color", "Radar")) {
      if (valueFormat == "prototypes") {
        varnames <- intersect(varnames, colnames(som$codes[[1]]))
        nvar <- length(varnames)
        if (is.null(varnames)) return(NULL)
        prototypes <- as.matrix(as.data.frame(som$codes[[1]])[varnames])
      }
    }
    
    if (type %in% c("Circular", "Line", "Barplot", "Color", "Radar")) {
      ## realValues are displayed in the text info above the plot
      if (valueFormat == "mean") {
        realValues <- do.call(rbind, lapply(split(data, clustering), 
                                            colMeans, na.rm = TRUE))
      } else if (valueFormat == "median") { 
        realValues <- do.call(rbind, lapply(split(data, clustering), 
                                            function(x) 
                                              apply(x, 2, median, na.rm= TRUE)))
      } else if (valueFormat == "prototypes") { 
        realValues <- prototypes
        data <- prototypes
      }
      
      ## normValues are used for plotting (scaled to 0.05-0.95)
      normValues <- realValues
      if (normtype == "contrast") {
        for (i in varnames) normValues[, i] <- (realValues[, i] - min(realValues[, i], na.rm = TRUE)) /
            (max(realValues[, i], na.rm = TRUE) - min(realValues[, i], na.rm = TRUE))
      } else if (normtype == "range") {
        for (i in varnames) normValues[, i] <- (realValues[, i] - min(data[, i], na.rm = TRUE)) / 
            (max(data[, i], na.rm = TRUE) - min(data[, i], na.rm = TRUE))
      } else if (normtype == "same") {
        for (i in varnames) normValues[, i] <- (realValues[, i] - min(data, na.rm = TRUE)) / 
            (max(data, na.rm = TRUE) - min(data, na.rm = TRUE))
      }
      normValues <- .05 + .9 * normValues
      
      realValues <- round(realValues, 3)
      if (type == "Color") {
        ## 8 colors (equal-sized bins of values) of selected palette
        normValues <- apply(normValues, 2, function(x) 
          getPalette(palplot, 8, reversePal)[cut(x, seq(.049, .951, length.out= 9))])
        normValues[is.na(normValues)] <- "#FFFFFF"
        
        ## reallevels: for legend, levels of cuts in real vars
        if (normtype %in% c("same", "range")) {
          reallevels <- levels(cut(data[, 1], breaks = 8))
        } else if (normtype == "contrast") {
          if (valueFormat == "mean") {
            realcolvalues <- sapply(split(data[, 1], clustering), mean, na.rm = TRUE)
          } else if (valueFormat == "median") {
            realcolvalues <- sapply(split(data[, 1], clustering), median, na.rm = TRUE)
          } else if (valueFormat == "prototypes") {
            realcolvalues <- data
          }
          reallevels <- levels(cut(realcolvalues, breaks = 8))
        }
      }
    } else if (type == "Boxplot") {
      if (valueFormat == "prototypes") {
        normDat <- as.data.frame(prototypes[clustering, ])
        data <- as.data.frame(apply(data, 2, as.numeric))
      } else {
        data <- as.data.frame(apply(data, 2, as.numeric))
        normDat <- data
      }
      
      if (normtype == "same") {
        normDat <- (normDat - min(normDat, na.rm = TRUE)) / 
          (max(normDat, na.rm = TRUE) - min(normDat, na.rm = TRUE))
      } else {
        normDat <- as.data.frame(sapply(normDat, function(x) (x - min(x, na.rm = TRUE)) / 
                                          (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))))
      }
      
    }
  } else if (type == "UMatrix") {
    proto.gridspace.dist <- kohonen::unit.distances(som$grid)
    proto.dataspace.dist <- as.matrix(dist(som$codes[[1]]))
    proto.dataspace.dist[round(proto.gridspace.dist, 3) > 1] <- NA
    proto.dataspace.dist[proto.gridspace.dist == 0] <- NA
    realValues <- round(unname(rowMeans(proto.dataspace.dist, na.rm= TRUE)), 4)
    normValues <- (realValues - min(realValues)) / (max(realValues) - min(realValues))
    normValues <- getPalette(palplot, 8, reversePal)[cut(normValues , seq(-.001, 1.001, length.out= 9))]
    varnames <- "Mean distance to neighbors"
    type <- "Color"
    reallevels <- levels(cut(realValues, breaks= 8))
    res$plotType <- "Color"
  }
  
  
  ##########
  ## Generate plot-type specific list of arguments
  ##########
  
  if (type == "Hitmap") {
    res$normalizedValues <- unname(.9 * sqrt(clust.table) / sqrt(max(clust.table)))
    res$realValues <- unname(clust.table)
  } else if (type %in% c("Circular", "Line", "Barplot", "Color", "Radar")) {
    res$label <- varnames
    res$normalizedValues <- unname(normValues)
    res$realValues <- unname(realValues)
    res$isCatBarplot <- FALSE
    res$showSC <- showSC
    if (type != "Color") {
      res$nVars <- nvar
      res$labelColor <- getPalette(palplot, nvar, reversePal)
    }
    if (type == "Line") {
      res$labelColor <- rep("#808080", nvar)
    }
    if (type == "Color") {
      res$labelColor <- getPalette(palplot, 8, reversePal)
      res$label <- reallevels
      res$colorVarName <- varnames
      if (legendPos == "beside") {
        res$labelColor <- rev(res$labelColor)
        res$label <- rev(res$label)
      }
    }
  } else if (type == "Boxplot") {
    res$nVars <- nvar
    res$label <- varnames
    res$labelColor <- getPalette(palplot, nvar, reversePal)
    
    boxes.norm <- lapply(split(normDat, clustering), boxplot, plot= FALSE)
    boxes.real <- lapply(split(data, clustering), boxplot, plot= FALSE)
    res$normalizedValues <- unname(lapply(boxes.norm, function(x) unname(as.list(as.data.frame(round(x$stats, 3))))))
    res$realValues <- unname(lapply(boxes.real, function(x) unname(as.list(as.data.frame(round(x$stats, 3))))))

    if (plotOutliers) {
      res$normalizedExtremesValues <- unname(lapply(boxes.norm, function(x) unname(split(round(x$out, 3), factor(x$group, levels= 1:nvar)))))
      res$realExtremesValues <- unname(lapply(boxes.real, function(x) as.list(unname(split(round(x$out, 3), factor(x$group, levels= 1:nvar))))))
    } else {
      res$normalizedExtremesValues <- unname(lapply(boxes.norm, function(x) lapply(1:nvar, function(y) numeric(0))))
      res$realExtremesValues <- unname(lapply(boxes.real, function(x) lapply(1:nvar, function(y) numeric(0))))
    }
  } else if (type == "Pie") {
    res$nVars <- nvalues
    res$label <- unique.values
    res$labelColor <- getPalette(palplot, nvalues, reversePal)
    if (equalSize) {
      res$normalizedSize <- rep(.9, length(clust.table))
      res$normalizedSize[clust.table == 0] <- 0
    } else
      res$normalizedSize <- unname(.9 * sqrt(clust.table) / sqrt(max(clust.table)))
    res$realSize <- unname(clust.table)
    res$normalizedValues <- unname(lapply(split(data, clustering), 
                                          function(x) {
                                            if (!length(x)) return(rep(1/nvalues, nvalues))
                                            unname(table(x) / length(x))
                                          }))
    res$realValues <- unname(lapply(split(data, clustering), 
                                    function(x) unname(table(x))))
    
  } else if (type == "CatBarplot") {
    res$nVars <- nvalues
    res$isCatBarplot <- TRUE
    res$label <- unique.values
    res$labelColor <- getPalette(palplot, nvalues, reversePal)
    res$realValues <- unname(lapply(split(data, clustering), 
                                    function(x) unname(table(x))))
    if (normtype == "contrast") {
      maxValue <- max(do.call(c, lapply(split(data, clustering), 
                                        function(x) {
                                          if (!length(x)) return(rep(0, nvalues))
                                          unname(table(x) / length(x))
                                        })))
    } else maxValue <- 1
    res$normalizedValues <- unname(lapply(split(data, clustering), 
                                          function(x) {
                                            if (!length(x)) return(rep(0, nvalues))
                                            .05 + .9 * unname(table(x) / length(x)) / maxValue
                                          }))
    res$plotType <- "Barplot"
  } else if (type == "Cloud") {
    if (cloudType == "cellPCA") {
      cloudtraindata <- apply(som$data[[1]], 2, 
                              function(x) {x[is.na(x)] <- mean(x, na.rm = T); x})
      clouddata <- matrix(NA, nrow(som$data[[1]]), 2)
      for (iCell in unique(som$unit.classif)) {
        theObs <- som$unit.classif == iCell
        if (sum(theObs) == 1) {
          clouddata[theObs, ] <- c(0, 0)
        } else if (sum(theObs) == 2) { ## If only two points, include prototype
          clouddata[theObs, ] <- stats::prcomp(rbind(som$codes[[1]][iCell, ], 
                                                     cloudtraindata[theObs, ]))$x[-1, 1:2]
        } else {
          clouddata[theObs, ] <- stats::prcomp(cloudtraindata[theObs, ])$x[, 1:2]
        }
        if (sum(theObs) > 1) 
          clouddata[theObs, ] <- 0.5 * clouddata[theObs, ] / max(abs(clouddata[theObs, ]))
      }
    } else if (cloudType == "kPCA") {
      clouddata <- som$data[[1]] - som$codes[[1]][som$unit.classif, ]
      clouddata <- apply(clouddata, 2,
                         function(x) {x[is.na(x)] <- mean(x, na.rm = T); x})
      clouddata <- kernlab::kpca(clouddata)@rotated[, 1:2]
      clouddata <- apply(clouddata, 2, function(x) (x - min(x)) / (max(x) - min(x)) - 0.5)
    } else if (cloudType == "PCA") {
      clouddata <- som$data[[1]] - som$codes[[1]][som$unit.classif, ]
      clouddata <- apply(clouddata, 2,
                         function(x) {x[is.na(x)] <- mean(x, na.rm = T); x})
      clouddata <- stats::prcomp(clouddata)$x[, 1:2]
      clouddata <- apply(clouddata, 2, function(x) (x - min(x)) / (max(x) - min(x)) - 0.5)
    } else if (cloudType == "proximity") {
      theSomDist <- aweSOM::somDist(som)
      dist1 <- rowSums((som$codes[[1]][som$unit.classif, , drop = FALSE] -
                          som$data[[1]])^2, na.rm = TRUE)
      bmu2 <- t(sapply(1:nrow(som$data[[1]]), function(iObs) {
        theProtos <- c(which(theSomDist$neigh.matrix[som$unit.classif[iObs], ]))
        protodists <- rowSums(t(t(som$codes[[1]][theProtos, , drop = FALSE]) -
                                  som$data[[1]][iObs, ])^2, na.rm = TRUE)
        c(theProtos[which.min(protodists)], min(protodists))
      }))
      wmat <- matrix(0, nrow(som$data[[1]]), nrow(som$grid$pts))
      wmat[cbind(1:nrow(som$data[[1]]), som$unit.classif)] <- dist1
      wmat[cbind(1:nrow(som$data[[1]]), bmu2[, 1])] <- bmu2[, 2]
      wmat <- wmat / rowSums(wmat)
      clouddata <- wmat %*% som$grid$pts
      clouddata <- .6 * (clouddata - som$grid$pts[som$unit.classif, ])
      clouddata[, 2] <- -clouddata[, 2]
    } else if (cloudType == "random") {
      if (!is.na(cloudSeed)) set.seed(cloudSeed)
      clouddata <- matrix(ncol = 2, stats::runif(2 * nrow(som$data[[1]]), 
                                                 min = -0.5, max = 0.5))
    }
    
    res$normalizedValues <- clouddata
    res$realValues <- cellNames
    res$cellNames <- unname(lapply(split(cellNames, clustering),
                                   function(x) paste(x, collapse= ", ")))
    if (is.null(data)) {
      res$legendPos <- "none"
      res$label <- ""
      res$labelColor <- getPalette(palplot, 2, reversePal)[1]
      res$cloudColor <- rep(0, length(clustering))
    } else {
      res$label <- levels(data)
      res$labelColor <- getPalette(palplot, nvalues, reversePal)
      res$cloudColor <- as.numeric(data) - 1
    }
    fulldata <- sapply(fulldata, function(x) as.character(x))
    res$fullData <- as.matrix(fulldata)
    res$fullDataNames <- colnames(fulldata)
  }
  
  res
}


#################################
## Widget function for shiny interface, render D3 plot in an htmlwidget
#################################

aweSOMwidget <- function(ok.som, ok.sc, ok.data, ok.trainrows, 
                         graphType, plotNames, plotVarMult, plotVarOne, 
                         plotSize, plotOutliers, plotEqualSize, plotShowSC,
                         contrast, average_format, palsc, palplot, plotRevPal,
                         plotAxes, plotTransparency, legendPos, legendFontsize,
                         cloudType, cloudSeed,
                         width = NULL, height = NULL, elementId = NULL) {
  
  if (is.null(ok.som))
    return(NULL)
  
  ok.clust <- factor(ok.som$unit.classif, levels= 1:nrow(ok.som$codes[[1]]))
  plot.data <- ok.data[ok.trainrows, ]
  if(is.null(plot.data)) return(NULL)
  
  # Obs names per cell for message box
  if (is.null(plotNames)) return(NULL) 
  if (plotNames == "(rownames)") {
    plotNames.var <- rownames(plot.data)
  } else {
    plotNames.var <- as.character(plot.data[, plotNames])
  }
  cellNames <- unname(lapply(split(plotNames.var, ok.clust),
                             function(x) paste(x, collapse= ", "))) # "&#13;&#10;" "<br />"
  
  
  if (graphType %in% c("Circular", "Radar", "Barplot", "Boxplot", "Line")) {
    if (is.null(plotVarMult)) return(NULL)
    plotVar <- plotVarMult
    data <- plot.data[, plotVar]
  } else if (graphType %in% c("Color", "Pie", "CatBarplot")) {
    if (is.null(plotVarOne)) return(NULL)
    plotVar <- plotVarOne
    data <- plot.data[, plotVar]
  } else if (graphType %in% c("Hitmap")) {
    plotVar <- NULL
    data <- NULL
  } else if (graphType == "Cloud") {
    if (is.null(plotVarOne)) return(NULL)
    plotVar <- c(plotVarOne, plotVarMult)
    data <- plot.data
    # if (plotVar == "None") {
    #   data <- NULL
    # } else {
    #   data <- plot.data[, plotVar]
    # }
    cellNames <- plotNames.var
  }
  
  plotParams <- getPlotParams(graphType, ok.som, ok.sc, data, plotSize, 
                              plotVar, cellNames,
                              contrast, average_format,
                              palsc, palplot, plotRevPal, 
                              plotOutliers, plotShowSC, plotEqualSize, 
                              plotAxes, plotTransparency, 
                              legendPos= legendPos, legendFontsize= legendFontsize, 
                              cloudType = cloudType, cloudSeed = cloudSeed)

  # create the widget
  htmlwidgets::createWidget("aweSOMwidget", plotParams, elementId = elementId, 
                            width = plotSize, height = plotSize, package = "aweSOM", 
                            sizingPolicy = htmlwidgets::sizingPolicy(defaultWidth = "100%", defaultHeight = "auto", padding= 0))
}

## htmlwidgets - shiny binding
aweSOMoutput <- function(outputId, width = "100%", height = "auto") {
  htmlwidgets::shinyWidgetOutput(outputId, "aweSOMwidget", width, height, package = "aweSOM")
}
renderaweSOM <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, aweSOMoutput, env, quoted = TRUE)
}

aweSOMwidget_html = function(id, style, class, ...){
  htmltools::tags$div(id = id, class = class, 
                      style= paste0(style, "display:block; margin:auto; margin-top:5px; margin-bottom:5px;"))
}

#################################
## Console-callable function for interactive plots
#################################


#' Interactive SOM plots
#'
#' Plot interactive visualizations of self-organizing maps (SOM), as an html
#' page. The plot can represent general map informations, or selected
#' categorical or numeric variables (not necessarily the ones used during
#' training). Hover over the map to focus on the selected cell or variable, and
#' display further information.
#'
#' @param som \code{kohonen} object, a SOM created by the \code{kohonen::som} function.
#' @param type character, the plot type. The default "Hitmap" is a population
#'   map. "Cloud" plots the observations as a scatterplot within each cell (see
#'   Details). "UMatrix" plots the average distance of each cell to its
#'   neighbors, on a color scale. "Circular" (barplot), "Barplot", "Boxplot",
#'   "Radar" and "Line" are for numeric variables. "Color" (heat map) is for a
#'   single numeric variable. "Pie" (pie chart) and "CatBarplot" are for a
#'   single categorical (factor) variable.
#' @param data data.frame containing the variables to plot. This is typically
#'   not the training data, but rather the unscaled original data, as it is
#'   easier to read the results in the original units, and this allows to plot
#'   extra variables not used in training. If not provided, the training data is
#'   used.
#' @param variables character vector containing the names of the variable(s) to
#'   plot. See Details. 
#' @param superclass integer vector, the superclass of each cell of the SOM.
#' @param obsNames character vector, names of the observations to be displayed
#'   when hovering over the cells of the SOM. Must have a length equal to the
#'   number of data rows. If not provided, the row names of data will be used.
#' @param scales character, controls the scaling of the variables on the plot.
#'   See Details.
#' @param values character, the type of value to be displayed. The default
#'   "mean" uses the observation means (from data) for each cell. Alternatively,
#'   "median" uses the observation medians for each cell, and "prototypes" uses
#'   the SOM's prototypes values.
#' @param size numeric, plot size, in pixels. Default 400.
#' @param palsc character, the color palette used to represent the superclasses
#'   as background of the cells. Default is "Set3". Can be "viridis", "grey",
#'   "rainbow", "heat", "terrain", "topo", "cm", or any palette name of the
#'   RColorBrewer package.
#' @param palvar character, the color palette used to represent the variables.
#'   Default is "viridis", available choices are the same as for palsc.
#' @param palrev logical, whether color palette for variables is reversed.
#'   Default is FALSE.
#' @param showAxes logical, whether to display the axes (for "Circular",
#'   "Barplot", "Boxplot", "Star", "Line", "CatBarplot"), default TRUE.
#' @param transparency logical, whether to use transparency when focusing on a
#'   variable, default TRUE.
#' @param boxOutliers logical, whether outliers in "Boxplot" are displayed,
#'   default TRUE.
#' @param showSC logical, whether to display superclasses as labels in the
#'   "Color" and "UMatrix" plots, default TRUE.
#' @param pieEqualSize logical, whether "Pie" should display pies of equal size.
#'   The default FALSE displays pies with areas proportional to the number of
#'   observations in the cells.
#' @param showNames logical, whether to display the observations names in a box
#'   below the plot.
#' @param legendPos character, whether and where to display the legend (if
#'   applicable). Possible values are "beside", "below" or "none".
#' @param legendFontsize numeric, font size to use for the legend, and for the
#'   tooltip information of the "Cloud" plot. Default is 14.
#' @param cloudType character, for "Cloud" type, controls how the point
#'   coordinates are computed, see Details.
#' @param cloudSeed numeric, for "random Cloud" type, seed for the pseudo-random
#'   placement of the points. If NA (the default), no seed will be set.
#' @param elementId character, user-defined elementId of the widget. Can be
#'   useful for user extensions when embedding the result in an html page.
#'
#' @details 
#' The selected \code{variables} must be numeric for types "Circular",
#'   "Barplot", "Boxplot", "Radar", "Color" and "Line", or factor for types
#'   "Pie" and "CatBarplot". If not provided, all columns of data will be
#'   selected. If a numeric variable is provided to a "Cloud", "Pie" or
#'   "CatBarplot", it will be split into a maximum of 8 classes. For "Cloud"
#'   plots, the first element of \code{variables} is used to color the points
#'   (and can be "None" for no coloring), the following elements (if any) are
#'   used in the information box of each point.
#' 
#' Variables scales: All values that are used for the plots (means,
#'   medians, prototypes) are scaled to 0-1 for display (minimum height to
#'   maximum height). The \code{scales} parameter controls how this scaling is
#'   done.
#' \itemize{
#' \item{"contrast"}: for each variable, the minimum height is the minimum
#' observed mean/median/prototype on the map, the maximum height is the maximum
#' on the map. This ensures maximal contrast on the plot.
#' \item{"range"}: observation range; for each variable, the minimum height
#' corresponds to the minimum of that variable over the whole dataset, the
#' maximum height to the maximum of the variable on the whole dataset.
#' \item{"same"}: same scales; all heights are displayed on the same scale,
#' using the global minimum and maximum of the dataset.
#' }
#' 
#' Cloud plot: three types of cloud plots are available, controlled by the
#' \code{cloudType} argument:
#' \itemize{
#' \item{"cellPCA"}: (default) the point coordinates are computed cell by cell,
#' by computing a PCA on the training data of that cell only. Points close to
#' the center of the cell are close to the mean of its observations. Points far
#' apart within a cell are likely to have different characteristics.
#' \item{"kPCA"}: the point coordinates are computed globally, by a kernel PCA
#' performed on all the differences between the training data and their winning
#' prototypes. Points close to the center of their cell are close to their
#' prototype, and points with similar placements in the clouds thus have a
#' similar difference to their prototype. Not recommended for large datasets
#' (eg. > 1000 observations), as it tends to take too much memory.
#' \item{"PCA"}: the point coordinates are computed globally, by a PCA
#' performed on all the differences between the training data and their winning
#' prototypes. Points close to the center of their cell are close to their
#' prototype, and points with similar placements in the clouds thus have a
#' similar difference to their prototype.
#' \item{"proximity"}: the point coordinates are computed one by one, based on
#' the distances of the observation's training data to its cell's prototype and
#' to its second best matching prototypes among its cell's neighbors. Points
#' close to their cell's center are close to their closest prototype, while
#' points close to another cell are close to that cell's prototype.
#' \item{"random"}: the point coordinates are random samples from a uniform
#' distribution.
#' }
#' 
#' 
#' @return Returns an object of class \code{htmlwidget}.
#'
#' @examples
#' ## Build training data
#' dat <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
#' ### Scale training data
#' dat <- scale(dat)
#' ## Train SOM
#' ### Initialization (PCA grid)
#' init <- somInit(dat, 4, 4)
#' ok.som <- kohonen::som(dat, grid = kohonen::somgrid(4, 4, 'hexagonal'),
#'                        rlen = 100, alpha = c(0.05, 0.01),
#'                        radius = c(2.65,-2.65), init = init,
#'                        dist.fcts = 'sumofsquares')
#' ## Group cells into superclasses (PAM clustering)
#' superclust <- cluster::pam(ok.som$codes[[1]], 2)
#' superclasses <- superclust$clustering
#'
#' ## Observations cloud ('Cloud')
#' variables <- c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width")
#' aweSOMplot(som = ok.som, type = 'Cloud', data = iris, 
#'            variables = c("Species", variables), superclass = superclasses)
#' 
#' ## Population map ('Hitmap')
#' aweSOMplot(som = ok.som, type = 'Hitmap', superclass = superclasses)
#'
#' ## Plots for numerical variables
#' ## Circular barplot
#' aweSOMplot(som = ok.som, type = 'Circular', data = iris,
#'            variables= variables, superclass = superclasses)
#' ## Barplot (numeric variables)
#' aweSOMplot(som = ok.som, type = 'Barplot', data = iris,
#'            variables= variables, superclass = superclasses)
#'
#' ## Plots for categorial variables (iris species, not used for training)
#' ## Pie
#' aweSOMplot(som = ok.som, type = 'Pie', data = iris,
#'            variables= "Species", superclass = superclasses)
#' ## Barplot (categorical variables)
#' aweSOMplot(som = ok.som, type = 'CatBarplot', data = iris,
#'            variables= "Species", superclass = superclasses)

aweSOMplot <- function(som, type= c("Hitmap", "Cloud", "UMatrix", "Circular", 
                                    "Barplot", "Boxplot", "Radar", "Line", 
                                    "Color", "Pie", "CatBarplot"), 
                       data= NULL, variables= NULL, superclass= NULL, 
                       obsNames= NULL,
                       scales= c("contrast", "range", "same"), 
                       values= c("mean", "median", "prototypes"),
                       size= 400, 
                       palsc=  c("Set3", "viridis", "grey", "rainbow", "heat", "terrain", 
                                 "topo", "cm", rownames(RColorBrewer::brewer.pal.info)), 
                       palvar= c("viridis", "grey", "rainbow", "heat", "terrain", 
                                 "topo", "cm", rownames(RColorBrewer::brewer.pal.info)), 
                       palrev= FALSE,
                       showAxes= TRUE,
                       transparency= TRUE,
                       boxOutliers= TRUE, 
                       showSC = TRUE,
                       pieEqualSize= FALSE,
                       showNames= TRUE,
                       legendPos= c("beside", "below", "none"),
                       legendFontsize= 14,
                       cloudType= c("cellPCA", "kPCA", "PCA", "proximity", "random"),
                       cloudSeed= NA,
                       elementId= NULL) {

  type <- match.arg(type)
  scales <- match.arg(scales)
  values <- match.arg(values)
  palsc <- match.arg(palsc)
  palvar <- match.arg(palvar)
  legendPos <- match.arg(legendPos)
  cloudType <- match.arg(cloudType)
  
  if (!("kohonen" %in% class(som)))
    stop("`som` argument must be a `kohonen` object, created by `kohonen::som`")
  
  if (type %in% c("Circular", "Barplot", "Boxplot", "Radar", "Line", "Color", "Cloud", "Pie", "CatBarplot")) {
    if (is.null(data)) ## If no data, fall back on training data
      data <- som$data[[1]]
    
    if (nrow(data) != nrow(som$data[[1]])) 
      stop("`data` must have the same number of rows as the training data.")
    
    if (is.null(variables)) {
      if (type == "Cloud") {
        variables <- "None"
      } else {
        ## If no variables, fall back on all columns
        variables <- colnames(data)
      }
    }
    
    if (! all(variables %in% c("None", colnames(data))))
      stop(paste0("Variables < ", 
                  paste(variables[! (variables %in% colnames(data))], collapse= " , "),
                  " > not found in data"))
    
    if (type %in% c("Color", "Pie", "CatBarplot")) {
      if (length(variables) > 1) {
        warning(paste0(type, " : Multiple variables provided, only the first one will be plotted."))
        variables <- variables[1]
      }
    }
    
    if (type %in% c("Circular", "Barplot", "Boxplot", "Radar", "Line", "Color")) {
      var.num <- sapply(variables, function(i) is.numeric(data[, i]))
      if (!all(var.num)) {
        warning(paste0("Only numeric variables can be plotted by ", type, 
                       ", variables < ", paste(variables[!var.num], collapse= " , "), 
                       " > will be dropped."))
        variables <- variables[var.num]
      }
    }
      
  }
  
  if (!(type %in% c("Hitmap", "UMatrix"))) {
    data <- as.data.frame(data)[variables[variables != "None"]]
  }
  
  if (is.null(superclass)) {
    superclass <- rep(1, nrow(som$grid$pts))
  } else if (length(superclass) != nrow(som$grid$pts)) {
    warning("`superclass` must have a length equal to the number of cells on the map. \
            No superclass will be plotted.")
    superclass <- rep(1, nrow(som$grid$pts))
  }
  superclass <- unname(superclass)

  obsClust <- som$unit.classif

  if (is.null(obsNames)) {
    if (is.null(rownames(data))) {
      obsNames <- as.character(1:nrow(som$data[[1]]))
    } else obsNames <- rownames(data)
  } else if (length(obsNames) != nrow(data)) {
    warning("`obsNames` must have a length equal to the number of rows of `data`.")
    if (is.null(rownames(data))) {
      obsNames <- as.character(1:nrow(som$data[[1]]))
    } else obsNames <- rownames(data)
  }
  if (type != "Cloud") {
    obsNames <- unname(lapply(split(obsNames, factor(obsClust, levels= 1:nrow(som$codes[[1]]))), 
                              function(x) paste(x, collapse= ", ")))
  }
  
  plotParams <- getPlotParams(type, som, superclass, data, size, 
                              variables, obsNames,
                              scales, values, palsc, palvar, palrev, 
                              boxOutliers, showSC, pieEqualSize, 
                              showAxes, transparency, showNames, 
                              legendPos, legendFontsize, cloudType, cloudSeed)

  ## Compute widget dimensions
  if (som$grid$topo == "rectangular") {
    cellSize <- size / max(som$grid$xdim, som$grid$ydim)
    widWidth <- min(size, cellSize * som$grid$xdim)
    widHeight <- min(size, cellSize * som$grid$ydim)
  } else {
    hexRadius <- min(size / (sqrt(3) * (som$grid$xdim + 0.5)), 
                     size / (1.5 * som$grid$ydim + 0.5))
    widWidth <- min(size, hexRadius * (sqrt(3) * (som$grid$xdim + 0.5)))
    widHeight <- min(size, hexRadius * (1.5 * som$grid$ydim + 0.5))
  }
  
  ## Create the widget
  res <- htmlwidgets::createWidget(
    "aweSOMwidget", plotParams, elementId = elementId, 
    width = widWidth, height = "auto", package = "aweSOM")

  ## Add elements for messages
  if(is.null(res$elementId)) {
    res$elementId <- paste0(
      'aweSOMwidget-', paste(format(as.hexmode(sample(256, 10, replace = TRUE) - 1), 
                                    width = 2), collapse = ""))
  }
  res <- htmlwidgets::prependContent(res, htmltools::tag("h4", list(id= paste0(res$elementId, "-info"))))
  res <- htmlwidgets::prependContent(res, htmltools::tag("h4", list(id= paste0(res$elementId, "-message"))))
  res <- htmlwidgets::appendContent(res, htmltools::tag("p", list(id= paste0(res$elementId, "-names"))))
  res <- htmlwidgets::appendContent(res, htmltools::tag("svg", list(id= paste0(res$elementId, "-placeHolder"), height= res$sizeInfo / 4)))
  
  res
}


#' Reorder variables for SOM plot
#'
#' Reorders a set of variables for prettier display on SOM plots. Variables that
#' have similar variations along the cell plots while be ordered close together.
#' Reordering is computed from the first component of a kernel PCA performed on
#' the matrix of displayed values (with the variables as rows, and the cells as
#' columns).
#'
#' @param som \code{kohonen} object, a SOM created by the \code{kohonen::som}
#'   function.
#' @param data \code{data.frame} containing the variables to plot. This is
#'   typically not the training data, but rather the unscaled original data, as
#'   it is easier to read the results in the original units, and this allows to
#'   plot extra variables not used in training. If not provided, the training
#'   data is used.
#' @param variables character vector containing the names of the variables to
#'   plot. If not provided, all columns of data will be selected. All variables
#'   must be numeric.
#' @param scales character, controls the scaling of the variables on the plot.
#'   The default "constrast" maximizes the displayed contrast by scaling the
#'   displayed heights of each variable from minimum to maximum of the displayed
#'   value. Alternatively, "range" uses the minimum and maximum of the
#'   observations for each variable, and "same" displays all variables on the
#'   same scale, using the global minimum and maximum of the data.
#' @param values character, the type of value to be displayed. The default
#'   "mean" uses the observation means (from data) for each cell. Alternatively,
#'   "median" uses the observation medians for each cell, and "prototypes" uses
#'   the SOM's prototypes values.
#'
#' @return Returns a character vector containing the reordered variables names.
#' 
#' @examples
#' ## Build training data
#' dat <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
#' ### Scale training data
#' dat <- scale(dat)
#' ## Train SOM
#' ### Initialization (PCA grid)
#' init <- somInit(dat, 4, 4)
#' ok.som <- kohonen::som(dat, grid = kohonen::somgrid(4, 4, 'hexagonal'),
#'                        rlen = 100, alpha = c(0.05, 0.01),
#'                        radius = c(2.65,-2.65), init = init,
#'                        dist.fcts = 'sumofsquares')
#' ## Reorder variables
#' ordered.vars <- aweSOMreorder(ok.som)
#' ## Plot with reordered variables
#' aweSOMplot(som = ok.som, type = 'Circular', data = iris,
#'            variables= ordered.vars)
aweSOMreorder <- function(som, data = NULL, variables = NULL, 
                          scales = c("contrast", "range", "same"), 
                          values = c("mean", "median", "prototypes")) {
  if (!("kohonen" %in% class(som)))
    stop("`som` argument must be a `kohonen` object, created by `kohonen::som`")
  
  if (is.null(data)) ## If no data, fall back on training data
    data <- som$data[[1]]
  if (nrow(data) != nrow(som$data[[1]])) 
    stop("`data` must have the same number of rows as the training data.")
  data <- as.data.frame(data)
  
  if (is.null(variables)) ## If no variables, fall back on all columns
    variables <- colnames(data)
  if (! all(variables %in% colnames(data))) 
    stop(paste0("Variables < ", 
                paste(variables[! (variables %in% colnames(data))], collapse= " , "),
                " > not found in data"))
  if (any(sapply(variables, function(x) !is.numeric(data[, x]))))
    stop("All variables must be numeric.")

  if (length(variables) < 2) return(variables)
  scales <- match.arg(scales)
  values <- match.arg(values)

  if (values == "mean") {
    cellValues <- do.call(rbind, lapply(split(data[, variables], som$unit.classif), 
                                        colMeans, na.rm = TRUE))
  } else if (values == "median") { 
    cellValues <- do.call(rbind, lapply(split(data[, variables], som$unit.classif), 
                                        function(x) apply(x, 2, median, na.rm = TRUE)))
  } else if (values == "prototypes") { 
    if (! all(variables %in% colnames(som$codes[[1]]))) return(NULL)
    cellValues <- som$codes[[1]][, variables]
  }
  cellValues <- apply(cellValues, 2, function(x) {x[is.na(x)] <- mean(x, na.rm= TRUE); x})
  
  if (scales == "range") {
    for (i in variables) cellValues[, i] <- (cellValues[, i] - min(data[, i], na.rm = TRUE)) / 
        (max(data[, i], na.rm = TRUE) - min(data[, i], na.rm = TRUE))
  } else if (scales == "contrast") {
    for (i in variables) cellValues[, i] <- (cellValues[, i] - min(cellValues[, i])) / 
        (max(cellValues[, i]) - min(cellValues[, i]))
  }
  
  arrange <- kernlab::kpca(t(as.matrix(cellValues)))@rotated[, 1]
  variables[order(arrange)]
}
