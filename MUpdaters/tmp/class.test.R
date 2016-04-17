library(Mdata)
library(MUpdaters)
library(TTR)
library(gtools)
options(stringsAsFactors = F)


###################
mdata.man <- MDataManager$new()
upd <- MDataUpdater.Custom$new(
  mdata.set = mdata.man$get.mdata.set(),
  mdata = MData$new(),
  mdata.name = "test",
  data.frame.input = function(){data.frame(a=1:10)}
  )
mdata.man$get.mdata("test")

###################

###################
mdata.man <- MDataManager$new()
mdata.man$add.updater(
  MDataUpdater.FRED$new(
    mdata.set = mdata.man$get.mdata.set(),
    mdata = MData$new(),
    mdata.name = "test",
    ticker ="CPIAUCSL"
  )
)
mdata.man$get.mdata("test")$get.field.values()


###################
mdata.man <- MDataManager$new()
mdata.man$add.updater(
  MDataUpdater.FRED$new(
    mdata.set = mdata.man$get.mdata.set(),
    mdata = MData$new(),
    mdata.name = "test",
    ticker ="CPIAUCSL"
  )
)
mdata.man$get.mdata("test")$get.field.values()



###################
mdata.man <- MDataManager$new()
mdata.man$add.updater(
  MDataUpdater.GoogleFinance.News$new(
    mdata.set = mdata.man$get.mdata.set(),
    mdata = MData$new(),
    mdata.name = "test",
    ticker ="SBER",
    news.number = 20
  )
)
mdata.man$get.mdata("test")$get.field.values()


###################
mdata.man <- MDataManager$new()
mdata.man$add.updater(
  MDataUpdater.YF.MarketData$new(
    mdata.set = mdata.man$get.mdata.set(),
    mdata = MData$new(),
    mdata.name = "test",
    ticker ="ORCL",
    from = "2016-01-01"
  )
)
mdata.man$get.mdata("test")$get.field.values()
###################


