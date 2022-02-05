## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
 ,fig.width=7)

knitr::opts_chunk$set(tidy.opts=list(width.cutoff=60), tidy=TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  library(remotes)
#  install_github("philipdelff/NMdata")
#  library(NMdata)

## ----eval=FALSE---------------------------------------------------------------
#  library(remotes)
#  install_github("philipdelff/NMdata@vX.Y.Z")
#  library(NMdata)

## ----eval=F-------------------------------------------------------------------
#  NMscanData(...,as.fun=tibble::as_tibble)

## ----eval=F-------------------------------------------------------------------
#  NMscanData(...,as.fun="data.table")

## ----eval=F-------------------------------------------------------------------
#  ## for tibbles
#  NMdataConfig(as.fun=tibble::as_tibble)
#  ## for data.table
#  NMdataConfig(as.fun="data.table")

## ----eval=FALSE---------------------------------------------------------------
#  NMdataConf(file.mod=identity)

## ----eval=FALSE---------------------------------------------------------------
#  out2in <- function(file) file.path(dirname(file),"input.txt")
#  res <- NMscanData("path/to/output.txt",file.mod=out2in)

## ----eval=FALSE---------------------------------------------------------------
#  NMdataConf(file.mod=out2in)

## ----eval=FALSE---------------------------------------------------------------
#  NMdataConfig(modelname=function(file) basename(dirname(normalizePath(file))))

