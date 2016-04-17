#' @include roxygen.config.R
NULL

#' MDataUpdater.GoogleFinance.News
#' @export
MDataUpdater.GoogleFinance.News <- R6Class(
  classname = "MDataUpdater.GoogleFinance.News",
  inherit =  MDataUpdater,
  cloneable = F,
  public = list(
    update = function(){
      suppressWarnings(
        {


            Sys.setlocale("LC_MESSAGES", 'en_GB.UTF-8')
            Sys.setenv(LANG = "en_US.UTF-8")
            Sys.setlocale("LC_TIME",  'en_GB.UTF-8')


            ticker <- private$pars$eval("ticker")
            number <- private$pars$eval("news.number")
            if (number>300) {
              warning("May only get 300 stories from google")
            }

            # construct url to news feed rss and encode it correctly
            url.b1 = 'http://www.google.com/finance/company_news?q='
            url    = paste(url.b1, ticker, '&output=rss', "&start=", 1,
                           "&num=", number, sep = '')
            url    = URLencode(url)

            # parse xml tree, get item nodes, extract data and return data frame
            doc   = xmlTreeParse(url, useInternalNodes = TRUE)
            nodes = getNodeSet(doc, "//item")
            mydf  = ldply(nodes, as.data.frame(xmlToList))
            #mydf  = lapply(nodes, as.data.frame(xmlToList))

            # clean up names of data frame
            names(mydf) = str_replace_all(names(mydf), "value\\.", "")

            # convert pubDate to date-time object and convert time zone
            pubDate = strptime(mydf$pubDate,format = '%a, %d %b %Y %H:%M:%S', tz = 'GMT')
            #pubDate = with_tz(pubDate, tz = 'America/New_york')
            mydf$pubDate = NULL

            #Parse the description field
            mydf$description <- as.character(mydf$description)
            parseDescription <- function(x) {
              out <- html2text(x)$text
              out <- strsplit(out,'\n|--')[[1]]

              #Find Lead
              TextLength <- sapply(out,nchar)
              Lead <- out[TextLength==max(TextLength)]

              #Find Site
              Site <- out[3]

              #Return cleaned fields
              out <- c(Site,Lead)
              names(out) <- c('Site','Lead')
              out
            }
            description <- lapply(mydf$description,parseDescription)
            description <- as.data.frame(do.call(rbind, description))
            mydf$Site <- description$Site
            mydf$Lead <- description$Lead

            mydf$EFFDATE<-as.Date(pubDate)
            mydf$TICKER<-ticker


            my.df <- mydf[,c("EFFDATE","TICKER","title","Site","Lead","link")]
            colnames( my.df) <- c("EFFDATE","Symbol","Title","Site","Lead","Link")
            my.df <- my.df[order(my.df$EFFDATE),]


          update.time = Sys.Date()
          mdata <- private$get.mdata()
          mdata$smart.df.filling(
            df.data =   my.df,
            df.description =  data.frame(
              Source = "GoogleFinance",
              SnapshotType = "historical",
              Type = "News",
              UpdateTime = update.time)
          )
          private$actual.flag <- T
        }
      )

    }
  )
)
