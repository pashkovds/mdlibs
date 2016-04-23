
#' @export
search.fids <- function(taxonomy, ...){
  lst.input <- list(...)
  id.lst <- lapply(
    names(lst.input),
    function(nms){
      which(taxonomy[[nms]] %in% lst.input[[nms]] )
    }
  )

  id.fin <- Reduce(intersect, id.lst)
  if(!is.null(id.fin)){
    tax.tmp <- taxonomy[id.fin,]
    id.fin.order <- match(lst.input[[1]], tax.tmp[[names(lst.input)[1]]])
    id.fin <- id.fin[id.fin.order]
  }
  id.fin
}

