#' @include roxygen.config.R
NULL

#' MStack
#' @export
MStack <- R6Class(
  classname = "MStack",
  cloneable = F,
  public = list(
    initialize = function() {
      private$env<- list()
    },

    get = function(id) {
      stopifnot(
        id %in% self$ids()
      )
      private$env[[id]]
    },

    add = function(data) {
      private$env[[length(private$env) + 1]] <- data
    },

    ids = function() {
      1:length(private$env)
    }
  ),
  private = list(
    env = NA
  )
)
