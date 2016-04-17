#' @include roxygen.config.R
NULL

#' MParameter
#' @export
MParameter <- R6Class(
  classname = "MParameter",
  cloneable = F,
  public = list(
      initialize = function(value) {
         self$set.value(value)
       },

      set.value = function(value) {
        if(is.function(value)){
          private$evaluate <- value
        }else{
          private$evaluate <- function() { value }
        }
      },

       eval = function(...){
          private$evaluate()
       }
  ),
  private = list(
    evaluate = NA
  )
)


