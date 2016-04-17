#' @include roxygen.config.R
NULL

#' MDataUpdater.Universe.YF.IndexComponents
#' @export
MDataUpdater.Universe.YF.IndexComponents <- R6Class(
  classname = "MDataUpdater.Universe.YF.IndexComponents",
  inherit =  MDataUpdater,
  cloneable = F,
  public = list(
    update = function(){
      suppressWarnings(
        {
          df.data.list <- list()
          for(page.id in 0:20){
            link = paste("http://finance.yahoo.com/q/cp?s=",private$pars$eval("index.ticker"),"+Components&c=",page.id,sep="")
            html.list <- readHTMLTable(link)
            df.data <- html.list[[9]]
            cat("Updating ",private$pars$eval("index.ticker"),nrow(df.data),"\n")
            if(is.null(df.data)){
              break
            }
            df.data.list[[length(df.data.list)+1]] <- df.data
          }

          df.data <- do.call(rbind,df.data.list)
          df.data$Index.Country <- private$pars$eval("country")
          df.data$Index.Type <- private$pars$eval("index.type")

          update.time = Sys.Date()
          mdata <- private$get.mdata()
          mdata$smart.df.filling(
            df.data =  df.data,
            df.description =  data.frame(
              Country = private$pars$eval("country"),
              Index.Type = private$pars$eval("index.type"),
              Source = "YF",
              SnapshotType = "point.in.time",
              UpdateTime = update.time)
          )
          private$actual.flag <- T
        }
      )

    }
  )
)
