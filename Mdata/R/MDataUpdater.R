#' @include roxygen.config.R
NULL

#' MDataUpdater
#' @export
MDataUpdater <- R6Class(
  classname = "MDataUpdater",
  cloneable = F,
  public = list(
    initialize = function(mdata.set, mdata, ...) {
      private$pars <- MParameterSet$new(...)
      private$mdata.set <- mdata.set
      private$pars$add(name = "updater.name", data = MParameter$new(private$pars$eval("mdata.name")))

      if(private$pars$eval("mdata.name") %in% private$mdata.set$list.names()){
        cat("Warning!!! Attempt to override",private$pars$eval("mdata.name")," data  was rejected by default \n")
      }else{
        private$mdata.set$add(name = private$pars$eval("mdata.name"), data =  mdata)
      }

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
      }else{
        private$pars$add(name = "description", data = MParameter$new(data.frame(desc.char = "EMPTY")))
      }
      private$actual.flag <- F
      private$updatable.flag <- T
    },
    update = function(){
        private$actual.flag <- T
    },

    protected.update = function(){
      tryCatch(
        {
          self$update()
        },
       error = function(e){
         cat("MDataUpdater ",private$pars$eval("mdata.name")," failed to update data! \n")
         private$updatable.flag <- F
       }
      )
    },

    get.data = function(){
       if(!self$is.actual()){
         self$protected.update()
       }
      if(private$updatable.flag){
        private$get.mdata()
      } else {
        NULL
      }
    },
    is.actual = function(){
       private$actual.flag
    },

    is.updatable = function(){
      private$updatable.flag
    },

    get.params = function(){
      private$pars
    },

    get.updater.description = function(){
      description <- private$pars$eval("description")
      description$Name.Updater = private$pars$eval("updater.name")
      description$Name.Mdata = private$pars$eval("mdata.name")
      description
    }

  ),
  private = list(
     actual.flag = NA,
     updatable.flag = NA,
     pars = NA,
     mdata.set = NA,
     get.mdata = function(){
        private$mdata.set$get(name = private$pars$eval("mdata.name"))
     }
  )
)
