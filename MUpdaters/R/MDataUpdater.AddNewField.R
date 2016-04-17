#' @include roxygen.config.R
NULL

#' MDataUpdater.AddNewField
#' @export
MDataUpdater.AddNewField <- R6Class(
  classname = "MDataUpdater.AddNewField",
  inherit =  MDataUpdater,
  cloneable = F,
  public = list(

    initialize = function(mdata.set, mdata, ...) {
      private$pars <- MParameterSet$new(...)
      private$mdata.set <- mdata.set
      stopifnot("updater.name" %in% private$pars$list.names())
      stopifnot("mdata.name" %in% private$pars$list.names())
      private$actual.flag <- F
      private$updatable.flag <- T
    },

    update = function(){

      mdata <- private$get.mdata()
      df <- mdata$get.field.values()
      list.of.new.fields <-  private$pars$eval(list.of.new.fields)
      #
      # list.of.new.fields <- list(
      #   Leverage = list(
      #      def = "Tot.Lia/Tot.Ass"
      #       ),
      #   Profit.Margin = list(
      #      def = "Net.Inc/Tot.Rev",
      #      tax = data.frame(desc = "Profit Margin"))
      # )


      for(I in 1:length(list.of.new.fields)){
        nms <- names(list.of.new.fields)[I]
        cur.lst <- list.of.new.fields[[nms]]
        mdata$set.field.values(
          values = eval(expr = parse(text = cur.lst$def), envir = df),
          fname = nms,
          new.taxonomy = cur.lst$tax
        )
      }
      private$actual.flag <- T
    }
  )
)



