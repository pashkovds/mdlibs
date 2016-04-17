#' @include roxygen.config.R
NULL


#' UpdaterSet
#' @export
UpdaterSet <- R6Class(
  classname = "UpdaterSet",
  inherit = MSet,
  cloneable = F,
  public = list(
    add = function(name, data) {
      stopifnot("MDataUpdater" %in% class(data))
      private$env[[name]] <- data
    }
  )
)
