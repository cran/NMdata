##' Extract column labels as defined in SAS
##' @param ... See `?compareCols`
##' @seealso compareCols NMinfo 
##' @export 

colLabels <- function(...){
    compareCols(...,fun.class=function(x)attributes(x)$label)
}
