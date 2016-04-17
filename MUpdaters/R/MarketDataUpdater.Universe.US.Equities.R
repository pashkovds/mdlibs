#' @include roxygen.config.R
NULL

#' MDataUpdater.Universe.US.Equities
#' @export
MDataUpdater.Universe.US.Equities <- R6Class(
  classname = "taUpdater.Universe.US.Equities",
  inherit =  MDataUpdater,
  cloneable = F,
  public = list(
    update = function(){
      suppressWarnings(
        {

          df.symbols <- stockSymbols()
          df.symbols <- df.symbols[!str_detect(df.symbols$Symbol,"-"), ]
          df.symbols$Index.Country <- "US"
          df.symbols <- df.symbols[!is.na(df.symbols$MarketCap),]
          sub.mlt <- 1e6
          df.symbols$mlt <- 1/sub.mlt
          df.symbols$mlt[grep("K",df.symbols$MarketCap)] <- 1e3/sub.mlt
          df.symbols$mlt[grep("M",df.symbols$MarketCap)] <- 1e6/sub.mlt
          df.symbols$mlt[grep("B",df.symbols$MarketCap)] <- 1e9/sub.mlt
          df.symbols$MarketCap.num <- as.numeric(str_replace_all(df.symbols$MarketCap,"M|B|K|\\$","")) * df.symbols$mlt

          mdata <- private$get.mdata()
          mdata$activate(num.obs = nrow(df.symbols))
          for(nms in colnames(df.symbols)){
            mdata$set.field.values(values = df.symbols[[nms]], fname = nms)
          }

          update.time = Sys.Date()
          mdata$set.description(data.frame(SnapshotType = "point.in.time",
                                           Country = "US",
                                           Source = "Nasdaq",
                                           UpdateTime = update.time))
          private$actual.flag <- T
        }
      )

    }
  )
)
