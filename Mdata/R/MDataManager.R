#' @include roxygen.config.R
NULL


#' UpdaterSet
#' @export
MDataManager <- R6Class(
  classname = "MDataManager",
  cloneable = F,
  public = list(
     initialize = function() {
       private$mdata.set = MDataSet$new()
       private$updater.set = UpdaterSet$new()
       private$instruction.stack <- MInstructionStack$new()
       private$global.params <- MParameterSet$new()
     },

     add.updater = function(updater, force = F) {
       if((updater$get.params()$eval("updater.name") %in% private$updater.set$list.names())){
         cat("Updater",updater$get.params()$eval("updater.name"), "is already exists! \n")
         if(force){
           cat("Creating new updater",updater$get.params()$eval("updater.name"), " (force option enabled) \n")
           private$updater.set$add(updater$get.params()$eval("updater.name"), updater)
         }
       }else{
         cat("Creating new updater",updater$get.params()$eval("updater.name"), " \n")
         private$updater.set$add(updater$get.params()$eval("updater.name"), updater)
       }


     },


     add.instruction = function(instr.function, ...) {
       private$instruction.stack$add(Instruction$new(fnc = instr.function, ...))
     },

     get.mdata = function(name) {
       private$updater.set$get(name)$get.data()
     },

     update.all = function() {
       upd.set <- self$get.updater.set()
       lapply(
         upd.set$list.names(),
         function(nms){
           upd.flag <- !upd.set$get(nms)$is.actual() & upd.set$get(nms)$is.updatable()
           if(upd.flag){
              upd.set$get(nms)$protected.update()
           }else{
              F
           }
         }
       )
     },

     remove.non.updatable = function() {
       upd.set <- self$get.updater.set()
       is.updatable.df <- Reduce(
         smartbind,
         lapply(
           upd.set$list.names(),
           function(nms){
             data.frame(
              upd.flag = upd.set$get(nms)$is.updatable(),
              mdata.name = upd.set$get(nms)$get.params()$eval("mdata.name"),
              updater.name =upd.set$get(nms)$get.params()$eval("updater.name")
             )
           }
         )
       )
       is.updatable.df <- is.updatable.df[is.updatable.df$upd.flag,]
       private$mdata.set$subset(is.updatable.df$mdata.name)
       private$updater.set$subset(is.updatable.df$updater.name)
     },

     get.updater.descriptions = function() {
       self$remove.non.updatable()
       upd.set <- self$get.updater.set()
       Reduce(
         smartbind,
         lapply(
           upd.set$list.names(),
           function(nms){
               upd.set$get(nms)$get.updater.description()
           }
         )
       )
     },

     list.names = function() {
       self$get.updater.set()$list.names()
     },

      execute.all.instructions = function(){
       lapply(
         self$get.instruction.stack()$ids(),
         function(i){
           self$get.instruction.stack()$get(i)$execute()
         }
       )
     },


     get.updater.set = function(){
       private$updater.set
     },

     get.mdata.set = function(){
       private$mdata.set
     },

     get.instruction.stack = function(){
       private$instruction.stack
     },

     get.global.params = function(){
       private$global.params
     }
  ),
  private = list(
    mdata.set = NA,
    updater.set = NA,
    instruction.stack = NA,
    global.params = NA
  )
)
