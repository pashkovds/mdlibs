#' @include roxygen.config.R
NULL


#' MDataSet
#' @export
MDataSet <- R6Class(
  classname = "MDataSet",
  inherit = MSet,
  cloneable = F,
  public = list(
    add = function(name, data) {
      stopifnot("MData" %in% class(data))
      private$env[[name]] <- data
    }
  )
)


