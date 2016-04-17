library(Mdata)
if(F){
  mdata <- MData$new()
  mdata$activate(num.obs = 10)
  mdata$set.field.values(values = rnorm(10), fname = "a")
  mdata$get.taxonomy()
  mdata$set.field.values(values = 1:10, fname = "b",new.taxonomy = data.frame(QBC = 23))
  mdata$get.taxonomy()
  mdata$set.field.values(values = 1:10, fname = "a",new.taxonomy = data.frame(QBC = 1, B= 2))
  mdata$get.taxonomy()

  mdata$get.field.names()
  mdata$get.field.values()
  mdata$get.taxonomy()
  mdata$get.fids(QBC=2)
  mdata$get.field.values(fids = mdata$get.fids(fname = "a"))
  mdata$get.field.names()
  mdata$get.field.values()
  mdata$get.field.values(fids = mdata$get.fids(fname =  mdata$get.field.names()))

  ###
  mdata <- MData$new()
  mdata$smart.df.filling(df.data = data.frame(a = 1:3, b = rnorm(3)))
  mdata$get.field.values()
  mdata$get.taxonomy()
  mdata$smart.df.filling(df.data = data.frame(a = 1:3, b = rnorm(3)),df.taxonomy = data.frame(QBC = "a"))
  mdata$get.field.values()
  mdata$get.taxonomy()
}


if(F){
  prm1 <- MParameter$new(function(){ 10 } )
  prm2 <- MParameter$new(function(){ prm1$eval()^2 } )
  prm2$eval()
  prm1$set.value(3)
  prm2$eval()
}


if(F){
  mpset <- MParameterSet$new(b = 2)
  mpset$add("a",MParameter$new(function(){ 5 }))
  mpset$eval("a")
  mpset$add("a.sq",MParameter$new(function(){ mpset$eval("a")^2 }))
  mpset$eval("a.sq")
  mpset$eval("b")
  mpset$eval("Q")
}
