#' @include roxygen.config.R
NULL

#' MDataUpdater.YF.CompanyProfile
#' @export
MDataUpdater.YF.CompanyProfile <- R6Class(
  classname = "MDataUpdater.YF.CompanyProfile",
  inherit =  MDataUpdater,
  cloneable = F,
  public = list(
    update = function(){
      suppressWarnings(
        {

          ticker <- private$pars$eval("ticker")

          link=str_c("http://finance.yahoo.com/q/pr?s=",ticker,"+Profile")
          tables <- readHTMLTable(link)
          df.data <- data.frame(Symbol=ticker,Descr=strsplit(as.character(tables[[5]][[1]][2]),"Business Summary")[[1]][2],stringsAsFactors = F)

          update.time = Sys.Date()
          mdata <- private$get.mdata()
          mdata$smart.df.filling(
            df.data =   df.data,
            df.description =  data.frame(
              Source = "YahooFinance",
              SnapshotType = "point.in.time",
              Type = "CompanyProfile",
              UpdateTime = update.time)
          )
          private$actual.flag <- T
        }
      )

    }
  )
)
