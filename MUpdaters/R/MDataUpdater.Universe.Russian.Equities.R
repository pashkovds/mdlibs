#' @include roxygen.config.R
NULL

#' MDataUpdater.Universe.Russian.Equities
#' @export
MDataUpdater.Universe.Russian.Equities <- R6Class(
  classname = "MDataUpdater.Universe.Russian.Equities",
  inherit =  MDataUpdater,
  cloneable = F,
  public = list(
    update = function(){
      suppressWarnings(
        {

          link.list <- c(
            "http://moex.com/a3457",
            "http://moex.com/a2904",
            "http://moex.com/a2147",
            "http://moex.com/a1588",
            "http://moex.com/a1620"
          )

          industry.set <- c(OilAndGas=24, ConsumerGoods = 25, Chemicals=26, MetalsAndMining = 27, AutoIndustrial= 28, Telecom = 29, Utilities = 30, Financials = 31, Transportation = 32)

          df.micex.universe.list <- lapply(
            link.list,
            function(link){
              #link = "http://moex.com/a1588"
              html.list<-readHTMLTable(link)
              df.cur.com <- do.call(
                rbind,
                lapply(
                  names(industry.set),
                  function(nms){
                    #nms = "Transportation"
                    cat(link,"  ",nms,"\n")
                    df <- html.list[[industry.set[nms]]][-1,2:3]
                    if(!is.null(df)){
                      colnames(df)<-c("Symbol.loc","Name")
                      df$Industry <- nms
                      df
                    }else{
                      NULL
                    }
                  }
                )
              )
              df.cur.com
            }
          )
          df.micex.universe <- do.call(rbind, df.micex.universe.list)
          df.micex.universe <- df.micex.universe[match(unique(df.micex.universe$Symbol.loc),df.micex.universe$Symbol.loc),]
          rownames(df.micex.universe) <- NULL
          df.micex.universe$Preffered <- 1
          df.micex.universe$Preffered[grep(" ао",df.micex.universe$Name)] <-0
          df.micex.universe$Symbol<- paste(str_replace_all(df.micex.universe$Symbol.loc,"\\*",""),"ME",sep=".")
          df.micex.universe$Symbol<- str_replace_all(df.micex.universe$Symbol,"\t","")
          df.micex.universe$Country <- "Russia"
          df.micex.universe

          update.time = Sys.Date()
          mdata <- private$get.mdata()
          mdata$smart.df.filling(
            df.data =  df.micex.universe,
            df.description =  data.frame(
              Country = "Russia",
              Source = "Micex",
              SnapshotType = "point.in.time",
              UpdateTime = update.time)
          )
          private$actual.flag <- T

          private$actual.flag <- T
        }
      )

    }
  )
)
