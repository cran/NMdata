## ---- include = FALSE---------------------------------------------------------
##knitr::opts_chunk$set(dev = "cairo_pdf")
knitr::opts_chunk$set(
                      collapse = TRUE
                     ,comment = "#>"
                     ,fig.width=7
                     ,cache=FALSE
                     ,class.source="Code"
                  )
library(data.table)
library(NMdata)
## library(devtools)
## load_all()

NMdataConf(as.fun="data.table"
          ,check.time=F)

## this change data.table syntax. I think we can do without.
## knitr::opts_chunk$set(tidy.opts=list(width.cutoff=60), tidy=TRUE)

pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
## pk[,trtact:=NULL]
covs <- unique(pk[,.(ID,WEIGHTB)])
pk[,WEIGHTB:=NULL]
set.seed(1)
covs2 <- covs[,.(ID,cov2=sample(c("caucasian","black"),size=.N,replace=T))]


## ---- echo=FALSE--------------------------------------------------------------
htmltools::img(src = knitr::image_uri(## file.path(R.home("pkgdown"), "favicon", "apple-touch-icon-152x152.png")),
                                ## system.file("pkgdown/favicon/apple-touch-icon-152x152.png",package="NMdata")),
                                "apple-touch-icon-180x180.png"),
               alt = 'logo', 
               style = 'position:absolute; top:15px; right:70px; padding:0px; width:150px')

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("NMdata")
#  library(NMdata)

## -----------------------------------------------------------------------------
compareCols(covs,covs2)

## ----eval=FALSE---------------------------------------------------------------
#  ## Append an "N" to columns that NONMEM can read (as numeric)
#  pk <- renameByContents(data=pk,
#                         fun.test = NMisNumeric,
#                         fun.rename = function(x)paste0(x,"N"))
#  ## lowercase names of columns that NONMEM cannot read as numeric
#  pk <- renameByContents(data=pk,
#                         fun.test = NMisNumeric,
#                         fun.rename = tolower,
#                         invert.test = TRUE)

## -----------------------------------------------------------------------------
pk2 <- mergeCheck(pk,covs2,by="ID")

## ----include=FALSE------------------------------------------------------------
pk[,(cc(FLAG,flag)):=NULL]

## -----------------------------------------------------------------------------
dt.flags <- fread(text="FLAG,flag,condition
10,Below LLOQ,BLQ==1
100,Negative time,TIME<0")

pk <- flagsAssign(pk,tab.flags=dt.flags,subset.data="EVID==0")
pk <- flagsAssign(pk,subset.data="EVID==1",flagc.0="Dosing")
flagsCount(pk[EVID==0],tab.flags=dt.flags)[,.( flag, N.left, Nobs.left, N.discard, Nobs.discard)]

## ----include=FALSE------------------------------------------------------------
pk <- NMorderColumns(pk)

## -----------------------------------------------------------------------------
text.nm <- NMwriteData(pk,file="derived/pkdata.csv",script="NMdata-cheat.Rmd",args.stamp=list(Description="PK data for the NMdata Cheatsheet"))

## ----include=FALSE------------------------------------------------------------
## writing version 2 rds so we don't need to depend on R 3.5
NMwriteData(pk,file="derived/pkdata.csv",script="NMdata-cheat.Rmd",args.stamp=list(Description="PK data for the NMdata Cheatsheet"),args.rds=list(version=2))

## ----eval=FALSE---------------------------------------------------------------
#  NMwriteSection(dir="nonmem",
#                 file.pattern="run1.*\\.mod",
#                 list.sections=text.nm["INPUT"])

## -----------------------------------------------------------------------------
res.debug <- NMcheckData(file="nonmem/run201.mod",quiet=T)
## we will only show some of what is available here
names(res.debug)
## Meta data on input data file:
res.debug$tables

## -----------------------------------------------------------------------------
## Comparison of variable naming:
res.debug$input.colnames[c(1:2)]
res.debug$input.colnames[c(9:12)]

## -----------------------------------------------------------------------------
res.debug$NMcheckData$summary

## ----include=F----------------------------------------------------------------
##NMscanData <- function(x)NMdata::NMscanData(file.path(system.file(paste0("examples/nonmem/",x), package="NMdata")))
## res1 <- NMscanData(system.file("examples/nonmem/xgxr001.lst", package="NMdata"))

## -----------------------------------------------------------------------------
res1 <- NMscanData("nonmem/run101.lst")
class(res1)

## ----include=F----------------------------------------------------------------
## rm(NMscanData)

## -----------------------------------------------------------------------------
library(ggplot2)
## tell NMdata functions to return data.tables
NMdataConf(as.fun="data.table")
res1.dt <- NMscanData("nonmem/run101.lst",recover.rows=TRUE)
ggplot(res1.dt[ID==135&EVID==0],aes(TIME))+
    geom_point(aes(y=DV,colour=flag))+
    geom_line(aes(y=PRED))+
    labs(y="Concentration (unit)",subtitle=unique(res1.dt$model))

## -----------------------------------------------------------------------------
levels(res1.dt$trtact)

## ----eval=FALSE---------------------------------------------------------------
#  NMdataConf(as.fun=tibble::as_tibble)
#  NMdataConf(as.fun="data.table")

## ----eval=FALSE---------------------------------------------------------------
#  NMdataConf(col.row="REC")

## ----eval=FALSE---------------------------------------------------------------
#  NMdataConf(file.mod=identity)

## -----------------------------------------------------------------------------
names(NMinfo(res1.dt))

## -----------------------------------------------------------------------------
NMinfo(res1.dt,"dataCreate")

## -----------------------------------------------------------------------------
NMinfo(res1.dt,"columns")[1:8]

## -----------------------------------------------------------------------------
NMinfo(res1.dt,"columns")[30:33]

