#' @include roxygen.config.R
NULL

#' MDataUpdater.MultipleMerge
#' @export
MDataUpdater.MultipleMerge <- R6Class(
  classname = "MDataUpdater.MultipleMerge",
  inherit =  MDataUpdater,
  cloneable = F,
  public = list(
    update = function(){
      suppressWarnings(
        {

          md.names  = private$pars$eval("md.names")
          merging.fields = private$pars$eval("merging.fields")

          df <- mdata.man$get.mdata(md.names[1])$get.field.values()
          df.tax <- mdata.man$get.mdata(md.names[1])$get.taxonomy()
          for(md.nms in md.names[-1]){
            df <- merge(df, mdata.man$get.mdata(md.nms)$get.field.values(), by = merging.fields)
            df.tax.cur <- mdata.man$get.mdata(md.nms)$get.taxonomy()
            df.tax <- rbind(df.tax, df.tax.cur[setdiff(1:nrow(df.tax.cur), match(merging.fields, df.tax.cur$fname)),])
          }


          update.time = Sys.Date()
          mdata <- private$get.mdata()
          mdata$smart.df.filling(
            df.data =  df,
            df.taxonomy =  df.tax,
            df.description =  data.frame(
              SnapshotType = "point.in.time",
              UpdateTime = update.time)
          )
          private$actual.flag <- T
        }
      )

    }
  )
)
