#' @include roxygen.config.R
NULL

#' MDataUpdater.YF.fundamentals
#' @export
MDataUpdater.YF.fundamentals <- R6Class(
  classname = "MDataUpdater.YF.fundamentals",
  inherit =  MDataUpdater,
  cloneable = F,
  public = list(
    update = function(){
      suppressWarnings(
        {
          country = private$pars$eval("country")
          ticker = private$pars$eval("ticker")
          fnd.type = private$pars$eval("fnd.type")
          is.annual = private$pars$eval("is.annual")
          stopifnot(fnd.type %in% c("is", "bs", "cf"))
          cat("Updating ", ticker," \n")

          Sys.setenv(LANG='C')
          Sys.setlocale("LC_TIME", 'C')
          if(is.annual){
            u = paste("http://finance.yahoo.com/q/",fnd.type,"?s=",ticker,"&annual",sep="")
          } else {
            u = paste("http://finance.yahoo.com/q/",fnd.type,"?s=",ticker,sep="")
          }
          doc = htmlParse(u)
          tableNodes = getNodeSet(doc, "//table")
          tab = readHTMLTable(tableNodes[[9]])
          data.header = readHTMLList(tableNodes[[9]][[1]])
          data.currency = tail(readHTMLList(tableNodes[[5]]),1)
          currency.name = str_sub(data.currency,-4,-2)

          timeline <- as.Date(timeDate(data.header[-1],format = "%b %d, %Y"))
          statement <- apply(tab,1, function(s){paste(s, collapse=";")})
          statement <- str_replace_all(statement,"\\,","")
          statement <- str_replace_all(statement,"\\(","-")
          statement <- str_replace_all(statement,"\\)","")
          statement <- str_replace_all(statement,"\\-\n","NA")
          statement.list <- lapply(strsplit(statement,";"),
                                   function(l){
                                     if(l[1]==""){l1 <-c(l[-1],NA)}else{l1 <- l}
                                     l1
                                   }
          )
          statement.matr <- do.call(cbind, statement.list)
          statement.matr <- statement.matr[,!statement.matr[1,] %in% c("NA","")]
          statement.names.init <- statement.matr[1,]
          statement.names <- str_replace_all(statement.names.init," ",".")
          statement.names <- str_replace_all(statement.names,"\\/",".")
          statement.matr.num  <- apply(statement.matr[-1,],2, function(s){ as.numeric(str_replace_all(s,"\\s+",""))})
          statement.matr.num  <- statement.matr.num[1:length(timeline),]
          statement.df <- as.data.frame(statement.matr.num)
          colnames(statement.df) <- statement.names
          statement.df$DATA.DATE <- timeline
          statement.df <- statement.df[order(statement.df$DATA.DATE, decreasing = F),]

          #########
          mdata <- private$get.mdata()
          mdata$activate(num.obs = nrow(statement.df))
          mdata$set.field.values(values = statement.df$DATA.DATE,
                                 fname = KEY.VALUES.DATE,
                                 new.taxonomy = data.frame(type="time", desc = "Period.of.reporting.end"))

          for(nms in setdiff(colnames(statement.df), KEY.VALUES.DATE)){
            new.name = str_c(str_sub(strsplit(nms,"\\.")[[1]],1,3),collapse=".")
            mdata$set.field.values(values = statement.df[[nms]], fname = new.name, new.taxonomy = data.frame(type = "fnd",fnd.type = fnd.type , desc = nms))
          }

          if(is.annual){
            freq.value = "A"
          }else{
            freq.value = "Q"
          }
          update.time = Sys.Date()
          mdata$set.description(data.frame(SnapshotType = "historical",
                                           Country = country,
                                           Source = "finance.yahoo",
                                           AssetClass = "EQ",
                                           Level = "Corporate",
                                           Info = "fundamentals",
                                           Company = ticker,
                                           Curncy = currency.name,
                                           FundamentalType = fnd.type,
                                           FundamentalFrequency = freq.value,
                                           UpdateTime = update.time))


          private$actual.flag <- T
        }
      )

    }
  )
)



