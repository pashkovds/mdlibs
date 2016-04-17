
library(Mdata)
library(MUpdaters)
library(TTR)
library(gtools)
options(stringsAsFactors = F)


###################
mdata.man <- MDataManager$new()


mdata.man$add.updater(
  MDataUpdater.Universe.Russian.Equities$new(
    mdata.set = mdata.man$get.mdata.set(),
    mdata = MData$new(),
    mdata.name = "universe"
  )
)

desc.df <- mdata.man$get.mdata("universe")$get.field.values()
desc.df <- desc.df[desc.df$Preffered==0,]




for(i in 1:nrow(desc.df)){
  ticker = desc.df$Symbol[i]
  Industry <- desc.df$Industry[i]
  Country <- desc.df$Country[i]
  mdata.man$add.updater(
    MDataUpdater.Universe.YF.CorpKeyStat$new(
      mdata.set = mdata.man$get.mdata.set(),
      mdata = MData$new(),
      mdata.name = paste(ticker,".kstat",sep=""),
      country = Country ,
      Industry = Industry,
      ticker = ticker,
      is.annual = T
    )
  )
}
mdata.man$update.all()
mdata.man$remove.non.updatable()

descr.df <- mdata.man$get.descriptions()

z <- do.call(rbind,lapply(descr.df$Name.Updater[-1], function(nm){mdata.man$get.mdata(nm)$get.field.values()}))
z1 <- z[order(z$Market.Cap.intraday,decreasing =T),c("Company","Market.Cap.intraday","Trailing.P.E.ttm.intraday","Price.Sales.ttm","Price.Book.mrq","Operating.Margin.ttm","Profit.Margin.ttm",
                                               "Return.on.Assets.ttm","Return.on.Equity.ttm","Qtrly.Revenue.Growth.yoy","Qtrly.Earnings.Growth.yoy","Total.Debt.Equity.mrq")]


z1 <- merge(desc.df[,c("Symbol","Industry")],z1, by.x="Symbol",by.y="Company")


z2 <- do.call(
  rbind,
  by(z1, z1$Industry, function(l){
    score.value <-  rank(l$Price.Sales.ttm,na.last=T) + rank(l$Price.Book.mrq,na.last=T)
    score.quality <- rank(l$Operating.Margin.ttm,na.last=F) +rank(l$Profit.Margin.ttm,na.last=F)
    score.value <- (score.value - mean(score.value))/sd(score.value)
    score.quality <- (score.quality - mean(score.quality))/sd(score.quality)
    score.qmj <- score.quality - score.value
    l$QMJ <- score.qmj
    l
  })
)

rownames(z2) <- NULL
z2 <- z2[order(z2$QMJ,decreasing=T),]
z2
