#' @include roxygen.config.R
NULL

#' MDataUpdater.InvestFunds.FundHistory
#' @export
MDataUpdater.InvestFunds.FundHistory <- R6Class(
  classname = "MDataUpdater.InvestFunds.FundHistory",
  inherit =  MDataUpdater,
  cloneable = F,
  public = list(
    update = function(){
      suppressWarnings(
        {

          date.start ="01.01.2006"
          date.end =strftime(timeDate(), format = "%d.%m.%Y")
          fid = private$pars$eval("fund.id")

          df.list <- lapply(
            1:80,
            function(page.num){
              tryCatch({
                cat(page.num,"\n")
                link <- paste("http://eng.investfunds.ru/funds/",fid,"/statistics/?dateStart=",date.start,"&dateEnd=",date.end,"&p=",page.num,sep="")
                doc.list <- readHTMLTable(link)
                pif.df <- doc.list[[4]]
                pif.df <- pif.df[-1,]
                pif.df <- apply(pif.df,2, function(s){ gsub(" ","",as.character(s))})
                colnames(pif.df) <- c("Date","Price","NAV")
                pif.df <- as.data.frame(pif.df, stringsAsFactors = F)
                pif.df$Price <- as.numeric(pif.df$Price)
                pif.df$NAV <- as.numeric(pif.df$NAV)
                pif.df$Date <- as.character(timeDate(pif.df$Date, format = "%d.%m.%Y"))
                pif.df
              }, error = function(e){
                NULL
              })
            }
          )
          df.list <- df.list[!unlist(lapply(df.list, is.null))]
          df.pif <- do.call(rbind, df.list)
          df.pif <- df.pif[order(df.pif$Date),]
          df.pif$Date <- as.Date(df.pif$Date)


          update.time = Sys.Date()
          mdata <- private$get.mdata()
          mdata$smart.df.filling(
            df.data =   df.pif,
            df.description =  data.frame(
              Source = "InvestFunds",
              SnapshotType = "historical",
              Type = "MarketData",
              UpdateTime = update.time,
              Currency = "USD",
              fundId = private$pars$eval("fund.id"))
          )
          private$actual.flag <- T
        }
      )

    }
  )
)
