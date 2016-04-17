#' @include roxygen.config.R
NULL

#' MStack
#' @export
MInstructionStack <- R6Class(
  classname = "MInstructionStack",
  inherit = MStack,
  cloneable = F,
  public = list(
    add = function(data) {
      stopifnot("Instruction" %in% class(data))
      private$env[[length(private$env) + 1]] <- data
    }
  )
)
