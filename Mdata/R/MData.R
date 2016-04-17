#' @include roxygen.config.R
NULL

#' MData
#' @export
MData <- R6Class(
  classname = "MData",
  cloneable = T,
  public = list(
    initialize = function(...) {
      private$is.activated = F
    },
    activate = function(num.obs, ...){
      if(!private$is.activated){
        private$num.obs <- num.obs
        private$mdata = data.frame(tid = 1:num.obs)
        private$set.taxonomy(data.frame(fid = 1, fname = "tid"))
        private$is.activated = T
      }else{
        cat("Has already activated! \n")
      }
    },
    get.num.obs = function(){
      private$num.obs
    },

    get.field.values = function(fids = NULL, tids = NULL){
      if(is.null(fids)){
        fids = 1:length(self$get.field.names())
      }
      if(is.null(tids)){
         tids = 1:self$get.num.obs()
      }
      private$get.mdata()[tids, fids]
    },

    get.fids = function(...){
      inputs <- list(...)
      ids.res <- Reduce(
        intersect,
        lapply(
          names(inputs),
          function(nms){
            stopifnot(nms%in% colnames(self$get.taxonomy()))
            match(inputs[[nms]],self$get.taxonomy()[[nms]])
          }
        )
      )
      ids.res[!is.na(ids.res)]
    },

    get.taxonomy = function(){
      private$taxonomy
    },

    get.field.names = function(){
      self$get.taxonomy()$fname
    },


    set.field.values = function(values, fname, new.taxonomy = NULL){
      stopifnot(nrow(new.taxonomy) == 1)
      if(!fname %in% self$get.taxonomy()$fname){
         new.fid <- max(self$get.taxonomy()$fid) + 1
         new.field.flag = T
      }else{
        existed.fid <- which(fname == self$get.taxonomy()$fname)
        new.fid <- self$get.taxonomy()$fid[existed.fid]
        new.field.flag = F
      }

      if(is.null(new.taxonomy)){
        new.taxonomy<- data.frame(fid = new.fid, fname = fname)
      }else{
        new.taxonomy <- as.data.frame(new.taxonomy)
        new.taxonomy[["fname"]] <- fname
        new.taxonomy[["fid"]] <- new.fid
      }
      current.taxonomy <- self$get.taxonomy()
      if(new.field.flag){
        private$set.taxonomy(suppressWarnings(smartbind(current.taxonomy, new.taxonomy)))
      }else{
        for(cname in setdiff(colnames(new.taxonomy), colnames(current.taxonomy))){
          current.taxonomy[[cname]] <- NA
        }
        for(cname in colnames(new.taxonomy)){
          current.taxonomy[[cname]][existed.fid]  <- new.taxonomy[[cname]]
        }
        private$set.taxonomy(current.taxonomy)
      }

      private$set.one.field(values, fname)
    },


    set.description = function(description){
      description <- as.data.frame(description)
      stopifnot(nrow(description) == 1)
      private$description <- description
    },

    get.description = function(){
      private$description
    },

    smart.df.filling = function(df.data, df.taxonomy = NULL, df.description = NULL){
      #browser()
         df.data<-as.data.frame(df.data)
         stopifnot(nrow(df.data)>0)

         if(!is.null(df.taxonomy)){
           df.taxonomy <- as.data.frame(df.taxonomy)
           if(nrow(df.taxonomy)==1){
              df.taxonomy.prc <- as.data.frame(
                matrix(rep(NA, ncol(df.taxonomy)*ncol(df.data)), ncol(df.data), ncol(df.taxonomy))
                )
              colnames(df.taxonomy.prc) <- colnames(df.taxonomy)
              for(nms in colnames(df.taxonomy)){
                df.taxonomy.prc[[nms]] <- as.character(df.taxonomy[[nms]])
              }
           }else if(nrow(df.taxonomy)!=ncol(df.data)){
                cat("Number of rows for df.taxonomy must be ncol(df.data) or 1! \n")
                stopifnot(nrow(df.taxonomy)==ncol(df.data))
           }else{
             df.taxonomy.prc <- df.taxonomy
           }
         }else{
           df.taxonomy.prc <- NULL
         }

         if( private$is.activated){
            stopifnot(nrow(df.data) == self$get.num.obs())
         }else{
            self$activate(nrow(df.data))
         }

         if(!is.null(df.description )){
           self$set.description(df.description)
         }

         for(I in 1:ncol(df.data)){
           if(!is.null(df.taxonomy.prc)){
             self$set.field.values(
                 values = df.data[[I]],
                 fname =  colnames(df.data)[I],
                 new.taxonomy = df.taxonomy.prc[I, , drop=F]
               )
           }else{
             self$set.field.values(
               values = df.data[[I]],
               fname =  colnames(df.data)[I]
             )
           }
         }
    }

  ),
  private = list(
     num.obs = NA,
     mdata = NA,
     is.activated = NA,
     description = NA,
     taxonomy = NA,
     set.taxonomy = function(df){
       private$taxonomy <- df
     },
     get.mdata = function(){
       private$mdata
     },
     set.one.field = function(values, fname){
       private$mdata[[fname]] <- values
     }
  )
)







