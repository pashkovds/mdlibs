#' @include roxygen.config.R
NULL

#' MDataUpdater.CustomTaxonomy
#' @export
MDataUpdater.CustomTaxonomy <- R6Class(
  classname = "MDataUpdater.CustomTaxonomy",
  inherit =  MDataUpdater,
  cloneable = F,
  public = list(
    update = function(){
      #browser()

      data.frame.input = private$pars$eval("data.frame.input")

      ### get mdata
      mdata <- private$get.mdata()
      mdata$activate(num.obs = nrow(data.frame.input))

      if("new.taxonomy" %in% private$pars$list.names()){
        mdata$smart.df.filling(
          df.data =   data.frame.input,
          df.taxonomy = private$pars$eval("new.taxonomy")
        )
      }else{
        mdata$smart.df.filling(
          df.data =   data.frame.input
        )
      }

      if("description" %in% private$pars$list.names()){
        description = private$pars$eval("description")
        if(is.data.frame(description)){
          description[["UpdateTime"]] <- Sys.Date()
          mdata$set.description(description)
        }
      }
      private$actual.flag <- T
    }
  )
)
