#' @include roxygen.config.R
NULL

#' MDataUpdater.InvestFunds.FundHistory
#' @export
MDataUpdater.InvestFunds.FundList <- R6Class(
  classname = "MDataUpdater.InvestFunds.FundList",
  inherit =  MDataUpdater,
  cloneable = F,
  public = list(
    update = function(){
      suppressWarnings(
        {

          xmlFun<-function(x){
            y<-xpathSApply(x,'./a',xmlAttrs)
            if(length(y)>0){
              list(href=y,orig=xmlValue(x))
            }else{
              xmlValue(x)
            }
          }


          df.list <- lapply(1:30, function(page.num){
            tryCatch({
              cat(page.num,"\n")
              link <- paste("http://eng.investfunds.ru/funds/",
                            "?ukId=0&fundId=0&type1=1&type2=2&type3=3&",
                            "statusId=0&ratingId=0&valueId=0&specId=0&",
                            "qualCheck=1&liquidatedCheck=0&liquidatedCheck=1&",
                            "sizeId=0&perfValue=&p=",page.num,sep="")
              doc.list <- readHTMLTable(link,elFun = xmlFun)
              tab <- doc.list[[4]]
              links.to.funds <- sapply(tab$Fund, function(l){
                eval(parse(text=as.character(l)))$href[1]
              })

              tab1 <- tab[,3:6]
              tab1$AK.id <- as.numeric(unlist(lapply(strsplit(links.to.funds,"/"), function(l){l[length(l)]})))
              tab1$Fund <- sapply(tab$Fund, function(l){ eval(parse(text=as.character(l)))$orig })
              tab1$Perf.3y<- as.numeric(unlist(lapply(strsplit(as.character(tab1$`3-Year Performance`),"%"),function(l){l[1]})))
              tab1$Type <- as.character(tab1$Type)
              tab1$Category <- as.character(tab1$Category)
              tab1 <- tab1[,c("Fund","Perf.3y","AK.id","Type","Category")]
              tab1
            }, error = function(e){
              NULL
            })
          })
          df.list <- df.list[!unlist(lapply(df.list, is.null))]
          df.pif.screener <- do.call(rbind, df.list)




          update.time = Sys.Date()
          mdata <- private$get.mdata()
          mdata$smart.df.filling(
            df.data =   df.pif.screener,
            df.description =  data.frame(
              Source = "InvestFunds",
              SnapshotType = "point.in.time",
              Type = "MarketData",
              UpdateTime = update.time)
          )
          private$actual.flag <- T
        }
      )

    }
  )
)
