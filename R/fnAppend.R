##' paste something before file name extension.
##' 
##' Append a file name like file.mod to file_1.mod or file_pk.mod. If
##' it's a number, we can pad some zeros if wanted. The separator
##' (default is underscore) can be modified.
##' @param fn The file name or file names to modify.
##' @param x A character string or a numeric to add to the file name
##' @param pad0 In case x is numeric, a number of zeros to pad before
##'     the appended number. This is useful if you are generating say
##'     more than 10 files, and your counter will be 01, 02,..,
##'     10,... and not 1, 2,...,10,...
##' @param sep The separator between the existing file name (until
##'     extension) and the addition.
##' @return A character (vector)
##'
##' @export


fnAppend <- function(fn,x,pad0=0,sep="_"){
        
    if((!is.numeric(x)&&!is.character(x))) stop("x must be numeric or character vector.")

    has.ext <- grepl(".*[^\\.]\\.[a-zA-Z0-9]+",fn)
    if(!all(has.ext)) stop("No file name extension found. Cannot append string.")

    
    fnext <- sub(".*[^\\.]\\.([a-zA-Z0-9]+)$","\\1",fn)
    ## fnroot <- sub(paste0("\\.",fnext,"$"),"",fn)
    fnroot <- sub(paste0("\\.[a-zA-Z0-9]+$"),"",fn)
    
    if(is.numeric(x)){
        string <- sprintf(fmt=paste("%0",pad0,"d",sep=""),x)
    } else {
        string <- x
    }
    string <- paste(string,collapse=sep)
    
    if(nchar(string)>0){
        paste0(fnroot,sep,string,".",fnext)
    } else {
        fn
    }
    
}
