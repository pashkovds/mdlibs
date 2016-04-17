#' @include roxygen.config.R
NULL

#' MDataUpdater.Custom
#' @export
MDataUpdater.Custom <- R6Class(
  classname = "MDataUpdater.Custom",
  inherit =  MDataUpdater,
  cloneable = F,
  public = list(
    update = function(){
      #browser()

          data.frame.input = private$pars$eval("data.frame.input")

          ### get mdata
          mdata <- private$get.mdata()
          mdata$activate(num.obs = nrow(data.frame.input))
          for(nms in colnames(data.frame.input)){
            #nms = colnames(data.frame.input)[2]
            mdata$set.field.values(
              values = data.frame.input[[nms]],
              fname = nms,
              new.taxonomy = data.frame(type = "custom", desc = nms)
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

