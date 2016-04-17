#' @include roxygen.config.R
NULL

#' MDataUpdater.Universe.Russian.Equities
#' @export
MDataUpdater.Universe.Russian.Equities.ENG <- R6Class(
  classname = "MDataUpdater.Universe.Russian.Equities.ENG",
  inherit =  MDataUpdater,
  cloneable = F,
  public = list(
    update = function(){
      suppressWarnings(
        {
         # library(RSelenium)

          sector.df <- rbind(
            data.frame(Sector = "OilAndGas", link = "http://moex.com/en/index/MICEXO%26G/constituents/"),
            data.frame(Sector = "Electric", link = "http://moex.com/en/index/MICEXPWR/constituents/"),
            data.frame(Sector = "Telecom", link = "http://moex.com/en/index/MICEXTLC/constituents/"),
            data.frame(Sector = "MetalsAndMining", link = "http://moex.com/en/index/MICEXM%26M/constituents/"),
            data.frame(Sector = "Manufacturing", link = "http://moex.com/en/index/MICEXMNF/constituents/"),
            data.frame(Sector = "Financials", link = "http://moex.com/en/index/MICEXFNL/constituents/"),
            data.frame(Sector = "ConsumerGoods", link = "http://moex.com/en/index/MICEXCGS/constituents/"),
            data.frame(Sector = "Chemicals", link = "http://moex.com/en/index/MICEXCHM/constituents/"),
            data.frame(Sector = "Transportation", link = "http://moex.com/en/index/MICEXTRN/constituents/")
          )

          checkForServer()
          startServer()
          remDr <- remoteDriver$new()

          remDr$open()

          res.list <- lapply(1:nrow(sector.df),function(i){
            remDr$navigate(as.character(sector.df$link[i]))
            if(i==1){
              webElem <- remDr$findElements("xpath","//button[@type='button']")
              webElem[[1]]$clickElement()
            }
            html.table <- readHTMLTable(remDr$getPageSource()[[1]])
            df.out <-  html.table[[17]]
            df.out <- df.out[-1,c(2,5)]
            colnames(df.out) <- c("Symbol","Capitalization.RUB")
            df.out$Capitalization.RUB <- as.numeric(str_replace_all(str_replace_all(as.character(df.out$Capitalization.RUB)," ",""),",","."))
            df.out$Sector <- as.character(sector.df$Sector[i])
            df.out
          })
          remDr$close()

          df.securities <- do.call(rbind,res.list)
          rownames(df.securities) <- NULL
          df.securities$Country <- "Russia"
          df.securities$Symbol <- paste(df.securities$Symbol,"ME",sep=".")

          update.time = Sys.Date()
          mdata <- private$get.mdata()
          mdata$smart.df.filling(
            df.data =  df.securities,
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
