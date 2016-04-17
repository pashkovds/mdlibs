#' @include roxygen.config.R
NULL

#' Instruction
#' @export
Instruction <- R6Class(
  classname = "Instruction",
  cloneable = F,
  public = list(
    initialize = function(fnc, ...) {
      stopifnot(is.function(fnc))
      private$fnc <- fnc
      private$pars <- MParameterSet$new(...)
      if("description" %in% private$pars$list.names()){
        description <- private$pars$eval("description")
        if(!is.data.frame(private$pars$eval("description"))){
          private$pars$add(
            name = "description",
            data = data.frame(
              desc.char = data.frame(
                desc.char = as.character(private$pars$eval("description"))[1]
              )
            )
          )
        }
      }
    },
    execute = function(...) {
      inputs <- list(...)
      inputs <- inputs[intersect(names(formals(private$fnc)), names(inputs))]
      do.call(private$fnc, inputs)
    },
    get.description = function(){
      private$pars$eval("description")
    }
  ),
  private = list(
    fnc = NA,
    pars = NA
  )
)
