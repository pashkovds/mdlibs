#' @include roxygen.config.R
NULL

#' MDataUpdater.Universe.YF.CorpKeyStat
#' @export
MDataUpdater.Universe.YF.CorpKeyStat <- R6Class(
  classname = "MDataUpdater.Universe.YF.CorpKeyStat",
  inherit =  MDataUpdater,
  cloneable = F,
  public = list(
    update = function(){
      suppressWarnings(
        {
            ticker <- private$pars$eval("ticker")
            cat("Key statistics updating ",ticker,"\n")

            link <- paste("http://finance.yahoo.com/q/ks?s=",ticker,"+Key+Statistics", sep="")
            html.list <- readHTMLTable(link)

            link1 = paste("http://finance.yahoo.com/q/pr?s=",ticker,"+Profile")
            html.list1 <-  readHTMLTable(link1)



            new.list <- list(
              html.list[[8]],
              html.list[[11]][-1,],
              html.list[[13]][-1,],
              html.list[[15]][-1,],
              html.list[[17]][-1,],
              html.list[[19]][-1,],
              html.list[[21]][-1,],
              html.list[[25]][-1,],
              html.list[[27]][-1,],
              html.list[[29]][-1,],
              html.list1[[8]][-1,]
            )

            tmp.string <- html.list[[31]][,1]
            place.of.crncy <- str_locate(tmp.string, "Currency in ")[,2]
            Currncy <- str_sub(tmp.string,place.of.crncy + 1 ,place.of.crncy+3)


            df.tmp <-t(do.call(cbind, lapply(new.list,function(l){df <- t(l[,2]); colnames(df)<- l[,1]; df})))
            df.tmp <- data.frame(
              Names  =  as.character(rownames(df.tmp)),
              Value = as.character(df.tmp[,1])
            )

            df.tmp <- rbind(
              df.tmp,
              data.frame(
                Names  =  "Currency",
                Value = Currncy
              )
            )

            df.tmp$mlt <- 1
            df.tmp$mlt[grep("T",df.tmp$Value)] <- 1e12
            df.tmp$mlt[grep("B",df.tmp$Value)] <- 1e9
            df.tmp$mlt[grep("M",df.tmp$Value)] <- 1e6
            df.tmp$mlt[grep("K",df.tmp$Value)] <- 1e3


            df.tmp$format <- "ratio"
            df.tmp$format[grep("52-Week", df.tmp$Names)] <- "crncy"
            df.tmp$format[grep("50-Day", df.tmp$Names)] <- "crncy"
            df.tmp$format[grep("200-Day", df.tmp$Names)] <- "crncy"

            df.tmp$format[grep("%",df.tmp$Value)] <- "prc"
            df.tmp$format[grep("T",df.tmp$Value)] <- "crncy"
            df.tmp$format[grep("B",df.tmp$Value)] <- "crncy"
            df.tmp$format[grep("M",df.tmp$Value)] <- "crncy"
            df.tmp$format[grep("K",df.tmp$Value)] <- "crncy"
            df.tmp$format[grep("Most Recent Quarter", df.tmp$Names)] <- "date"
            df.tmp$format[grep("Fiscal Year Ends", df.tmp$Names)] <- "date"
            df.tmp$format[grep("Dividend Date", df.tmp$Names)] <- "date"

            df.tmp$format[grep("Sector", df.tmp$Names)] <- "desc"
            df.tmp$format[grep("Industry", df.tmp$Names)] <- "desc"
            df.tmp$format[grep("Currency", df.tmp$Names)] <- "desc"
            df.tmp$format[grep("Full Time Employees", df.tmp$Names)] <- "abs"

            df.tmp$format[grep("Shares Short", df.tmp$Names)] <- "abs"
            df.tmp$format[grep("Avg Vol", df.tmp$Names)] <- "abs"
            df.tmp$format[grep("Shares Outstanding", df.tmp$Names)] <- "abs"
            df.tmp$format[grep("Float", df.tmp$Names)] <- "abs"


            tmp.val <- df.tmp$Value
            tmp.val <- str_replace_all(tmp.val,"T$","")
            tmp.val <- str_replace_all(tmp.val,"B$","")
            tmp.val <- str_replace_all(tmp.val,"M$","")
            tmp.val <- str_replace_all(tmp.val,"K$","")
            tmp.val <- str_replace_all(tmp.val,"%","")
            tmp.val <- str_replace_all(tmp.val,",","")
            df.tmp$clear.Value <- tmp.val

            tmp.val <- df.tmp$Names
            tmp.val <- str_replace_all(tmp.val, "/|,|-|%",".")
            tmp.val <- str_replace_all(tmp.val, " ",".")
            tmp.val <- str_replace_all(tmp.val, "\\)|\\(|:|\\&","")
            tmp.val <- str_replace_all(tmp.val, "^50.Day","D52")
            tmp.val <- str_replace_all(tmp.val, "^200.Day","D200")
            tmp.val <- str_replace_all(tmp.val, "^SP500","SnP")
            tmp.val <- str_replace_all(tmp.val, "52.Week","Year")
            tmp.val <- str_replace_all(tmp.val, "1$|2$|3$|4$|5$|6$|7$|8$|9$|0$","")
            tmp.val <- str_replace_all(tmp.val, "5.Year","Y5")
            tmp.val[2] <- "Enterprise.Value"
            tmp.val[4] <- "Forward.P.E"
            tmp.val[35] <- "Year.High"
            tmp.val[36] <- "Year.Low"
            tmp.val[37] <- "50.Day.Moving.Average"
            tmp.val[45] <- "Shares.Short"
            tmp.val[46] <- "Short.Ratio"
            tmp.val[47] <- "Short.of.Float"
            tmp.val <- unlist(lapply(strsplit(tmp.val,"\\."),function(l){l<-l[l!=""]; paste(l,sep="",collapse=".")}))
            df.tmp$new.Names<-tmp.val
            df.tmp <- smartbind(data.frame(new.Names = "Company" , clear.Value = private$pars$eval("ticker"), format = 'desc'), df.tmp)

            df.output <- as.data.frame(t(df.tmp$clear.Value))
            colnames(df.output) <- df.tmp$new.Names

            ids.numeric <- which(df.tmp$format %in% c("ratio","prc","crncy"))
            for(i in ids.numeric){
              df.output[[i]] <-  as.numeric(df.output[[i]]) * df.tmp$mlt[i]
            }




          update.time = Sys.Date()
          mdata <- private$get.mdata()
          mdata$smart.df.filling(
            df.data =  df.output,
            df.taxonomy = df.tmp[,c("format"),drop=F],
            df.description =  data.frame(
              Country = private$pars$eval("country"),
              Source = "YF",
              SnapshotType = "point.in.time",
              UpdateTime = update.time)
          )
          private$actual.flag <- T
        }
      )

    }
  )
)
