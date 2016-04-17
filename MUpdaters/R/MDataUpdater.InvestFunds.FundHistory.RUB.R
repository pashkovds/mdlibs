#' @include roxygen.config.R
NULL

#' MDataUpdater.InvestFunds.FundHistory
#' @export
MDataUpdater.InvestFunds.FundHistory.RUB <- R6Class(
  classname = "MDataUpdater.InvestFunds.FundHistory.RUB",
  inherit =  MDataUpdater,
  cloneable = F,
  public = list(
    update = function(){
      suppressWarnings(
        {


          date.start =timeDate("2006-01-01")
          date.end =timeDate()
          fid = private$pars$eval("fund.id")

          df.list <- lapply(
            1:80,
            function(page.num){
              tryCatch({
                cat(page.num,"\n")
                link <- paste("http://pif.investfunds.ru/funds/",fid,"/detail/1/?&day1=",strftime(date.start, "%d"),
                              "&month1=",strftime(date.start, "%m"),
                              "&year1=",strftime(date.start, "%Y"),
                              "&day2=",strftime(date.end, "%d"),
                              "&month2=",strftime(date.end, "%m"),
                              "&year2=",strftime(date.end, "%Y"),
                              "&date1=",strftime(date.start, "%d.%m.%Y"),
                              "&date2=",strftime(date.end, "%d.%m.%Y"),
                              "&start=",30*page.num,
                              "#beginf",sep="")
                doc.list <- readHTMLTable(link)
                #doc.list[[7]]
                pif.df <- doc.list[[7]]
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
              Currency = "RUB",
              fundId = private$pars$eval("fund.id"))
          )
          private$actual.flag <- T
        }
      )

    }
  )
)
