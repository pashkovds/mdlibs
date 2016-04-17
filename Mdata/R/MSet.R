#' @include roxygen.config.R
NULL

#' MSet
#' @export
MSet <- R6Class(
  classname = "MSet",
  cloneable = F,
  public = list(
    initialize = function() {
      private$env<- list()
    },

    get = function(name) {
      stopifnot(
        name %in% self$list.names()
      )
      private$env[[name]]
    },

    add = function(name, data) {
      private$env[[name]] <- data
    },

    list.names = function() {
      names(private$env)
    },

    subset = function(ids){
      private$env <-  private$env[ids]
      T
    }
  ),
  private = list(
    env = NA
  )
)


