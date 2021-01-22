################################################################################
################################################################################
## aweSOM funtion: launch shiny interface

#' aweSOM interface
#'
#' Launches the (offline) web-based interface for training and visualizing
#' self-organizing maps.
#' 
#' @return No return value, used for side effects.
#'
#' @details If the interface does not open automatically, open the printed link
#'   in a web browser.
#'   
#'   To open large files within the interface, use
#'   \code{options(shiny.maxRequestSize=2^30)} (or a suitably large file size)
#'   before lauching the interface.
#'
#' @references Kohonen T. (2001) \emph{Self-Organizing Maps}, 3rd edition,
#'   Springer Press, Berlin. <doi:10.1007/978-3-642-56927-2>
#'   
aweSOM <- function() {
  shiny::runApp(system.file('shiny', package='aweSOM'))
}

################################################################################
################################################################################
## S3 methods

print.somQual <- function(x, ...) {
  cat("\n## Quality measures:\n", 
      "* Quantization error     : ", x$err.quant, "\n",
      "* (% explained variance) : ", x$err.varratio, "\n",
      "* Topographic error      : ", x$err.topo, "\n",
      "* Kaski-Lagus error      : ", x$err.kaski, "\n", 
      "\n## Number of obs. per map cell:")
  print(x$cellpop)
  
}

