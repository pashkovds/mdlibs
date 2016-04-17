#' @include roxygen.config.R
NULL

#' MDataUpdater.YF.MarketData
#' @export
MDataUpdater.YF.MarketData <- R6Class(
  classname = "MDataUpdater.YF.MarketData",
  inherit =  MDataUpdater,
  cloneable = F,
  public = list(
    update = function(){
      suppressWarnings(
        {


          tmp.ts <- get.hist.quote(
            instrument = private$pars$eval("ticker"),
            start = private$pars$eval("from"),
            end = as.Date(timeDate()),
            quote =c("Open", "High", "Low", "Close", "AdjClose","Volume"),
            provider =c("yahoo"),
            method = NULL,
            origin="1899-12-30",
            compression = "d",
            retclass=c("zoo"),
            quiet = FALSE,
            drop=FALSE
          )

          data.df<-data.frame(
            DATADATE = as.Date(time(tmp.ts)),
            EFFDATE = as.Date(time(tmp.ts)),
            Symbol = private$pars$eval("ticker"),
            Open = as.numeric(tmp.ts[,"Open"]),
            High = as.numeric(tmp.ts[,"High"]),
            Low = as.numeric(tmp.ts[,"Low"]),
            Close = as.numeric(tmp.ts[,"Close"]),
            AdjClose = as.numeric(tmp.ts[,"AdjClose"]),
            Volume = as.numeric(tmp.ts[,"Volume"])
          )

          update.time = Sys.Date()
          mdata <- private$get.mdata()
          mdata$smart.df.filling(
            df.data =   data.df,
            df.description =  data.frame(
              Source = "YahooFinance",
              SnapshotType = "historical",
              Type = "MarketData",
              UpdateTime = update.time)
          )
          private$actual.flag <- T
        }
      )

    }
  )
)
