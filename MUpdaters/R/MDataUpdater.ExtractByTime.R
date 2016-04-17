#' @include roxygen.config.R
NULL

#' MDataUpdater.ExtractByTime
#' @export
MDataUpdater.ExtractByTime <- R6Class(
  classname = "MDataUpdater.ExtractByTime",
  inherit =  MDataUpdater,
  cloneable = F,
  public = list(
    update = function(){
      suppressWarnings(
        {
          rule.to.find.tables <- private$pars$eval("rule.to.find.tables")
          descr.df <- private$pars$eval("descriptions")
          md.ids <- Reduce(
            intersect,
            lapply(
              names(rule.to.find.tables),
              function(nms){
                stopifnot(nms%in% colnames(descr.df))
                which( descr.df[[nms]] %in% rule.to.find.tables[[nms]])
              }
            )
          )
          dname.set <- descr.df$Name.Updater[md.ids]

          extraction.result <- lapply(
              dname.set,
              function(d.name){
                #d.name <- dname.set[1]
                mdata <- mdata.man$get.mdata(name = d.name)
                tline <- mdata$get.field.values(fids = mdata$get.fids(fname = private$pars$eval("date.field.name")))
                tid <- findInterval(private$pars$eval("date.to.find"), tline)
                requested.fids <- mdata$get.fids(fname = private$pars$eval("fields.to.extract"))
                new.df <- mdata$get.field.values(tids = tid, fids = requested.fids)
                cur.desc <- mdata$get.description()
                tax.df <- mdata$get.taxonomy()[requested.fids,]
                for(nms in private$pars$eval("add.from.desc")){
                  new.df[[nms]] <- cur.desc[[nms]]
                  tax.df <- smartbind(tax.df, data.frame(fname = nms))
                }
                list(
                  new.df = new.df,
                  tax.df = tax.df
                )
              }
            )
            df.res <- do.call(
              rbind,
              lapply(
                extraction.result,
                function(l) {
                  l$new.df
                }
              )
            )

          update.time = Sys.Date()
          mdata <- private$get.mdata()
          mdata$smart.df.filling(
            df.data = df.res,
            df.taxonomy =  extraction.result[[1]]$tax.df,
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
