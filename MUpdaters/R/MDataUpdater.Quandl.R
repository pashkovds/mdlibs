#' @include roxygen.config.R
NULL

#' MDataUpdater.GoogleFinance.News
#' @export
MDataUpdater.Quandl <- R6Class(
  classname = "MDataUpdater.Quandl",
  inherit =  MDataUpdater,
  cloneable = F,
  public = list(
    update = function(){
      suppressWarnings(
        {
          ticker <- private$pars$eval("ticker")
          api.key <- private$pars$eval("Keys.Quandl")
          Quandl.api_key(api.key)
          data <- Quandl(ticker, order = "asc")

          update.time = Sys.Date()
          mdata <- private$get.mdata()
          mdata$smart.df.filling(
            df.data =   data,
            df.description =  data.frame(
              Source = "Quandl",
              SnapshotType = "historical",
              UpdateTime = update.time)
          )
          private$actual.flag <- T
        }
      )

    }
  )
)
