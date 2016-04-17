#' @include roxygen.config.R
NULL

#' MParameterSet
#' @export
MParameterSet <- R6Class(
  classname = "MParameterSet",
  inherit = MSet,
  cloneable = F,
  public = list(
    initialize = function(...) {
      input.list <- list(...)
      private$env <- list()
      for(nms in names(input.list)){
        private$env[[nms]] <- MParameter$new(input.list[[nms]])
      }
    },

    add = function(name, data) {
      stopifnot("MParameter" %in% class(data))
      private$env[[name]] <- data
    },

    eval = function(name, ...) {
      if(! name %in% self$list.names()){
         cat(name," is not in MParameterSet! \n")
      }
      stopifnot(name %in% self$list.names())
      private$env[[name]]$eval()
    }


  ),
  private = list(
    env = NA
  )
)


