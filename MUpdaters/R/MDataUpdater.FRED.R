#' @include roxygen.config.R
NULL

#' MDataUpdater.FRED
#' @export
MDataUpdater.FRED <- R6Class(
  classname = "MDataUpdater.FRED",
  inherit =  MDataUpdater,
  cloneable = F,
  public = list(
    update = function(){
      suppressWarnings(
        {

          df.data <- private$fred.get.series(ticker=private$pars$eval("ticker") ,newname=NULL)

          update.time = Sys.Date()
          mdata <- private$get.mdata()
          mdata$smart.df.filling(
            df.data =  df.data,
            df.description =  data.frame(
              Source = "FRED",
              SnapshotType = "historical",
              UpdateTime = update.time)
          )
          private$actual.flag <- T

          private$actual.flag <- T
        }
      )

    }
  ),
  private = list(
    fred.get.series = function (ticker="GOLDAMGBD228NLBM" ,newname=NULL) {
      if(is.null(newname)){
        newname<-ticker
      }
      FRED.URL <- "http://research.stlouisfed.org/fred2/series"
      tmp <- tempfile()
      download.file(paste(FRED.URL, "/", ticker, "/","downloaddata/", ticker, ".csv", sep = ""), destfile = tmp, method="wget", quiet =T)
      data.df <- read.csv(tmp, na.string = ".",stringsAsFactors=F)
      unlink(tmp)

      colnames(data.df)<-c("DATADATE","VAL")
      data.df$EFFDATE<-data.df$DATADATE
      data.df$TICKER<-newname
      data.df[,c("EFFDATE","DATADATE","TICKER","VAL")]

    }
  )
)





