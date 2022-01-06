## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
 ,fig.width=7)

knitr::opts_chunk$set(tidy.opts=list(width.cutoff=60), tidy=TRUE)

## ----eval=TRUE,include=FALSE--------------------------------------------------
## library(devtools)
## load_all("C:/Users/delff/working_copies/NMdata")

## ----setup,include=F----------------------------------------------------------
library(NMdata)
NMdataConf(check.time=FALSE)
## NMdataConf(as.fun="data.table")
library(data.table)
library(ggplot2)
theme_set(theme_bw()+theme(legend.position="bottom"))


## -----------------------------------------------------------------------------
pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
class(pk)

## ----include=FALSE------------------------------------------------------------
pk.reduced <- copy(pk)
pk.reduced <- pk.reduced[1:(.N%/%2)]
pk.reduced[,CYCLE:=NULL]
pk.reduced[,AMT:=as.character(AMT)]

## ----eval=TRUE----------------------------------------------------------------
compareCols(pk,pk.reduced)

## ----include=FALSE------------------------------------------------------------
pktmp <- copy(pk)
pktmp[,TRTACT:=NULL]

## -----------------------------------------------------------------------------
pk.renamed <- renameByContents(data=pktmp,fun.test=NMisNumeric,fun.rename = tolower,
                               invert.test = TRUE)

## ----eval=TRUE----------------------------------------------------------------
compareCols(pktmp,pk.renamed)

## ----include=FALSE------------------------------------------------------------
## dt.cov <- pk[,.(ID=unique(ID)[1:10])]
## dt.cov[,COV:=sample(1:5,size=10,replace=TRUE)]
dt.cov <- pk[,.(ID=unique(ID))]
dt.cov[,COV:=sample(1:5,size=.N,replace=TRUE)]
dt.cov <- dt.cov[c(1,1:(.N-1))]

## -----------------------------------------------------------------------------
pk2 <- merge(pk,dt.cov,by="ID")
dims(pk,dt.cov,pk2)

## -----------------------------------------------------------------------------
pk[ID==31,.N]
pk2[ID==31,.N]

## -----------------------------------------------------------------------------
try(mergeCheck(pk,dt.cov,by="ID"))

## -----------------------------------------------------------------------------
dt.cov2 <- dt.cov[ID!=31]
pk2.check <- mergeCheck(pk,dt.cov2,by="ID",all.x=TRUE)

## ----include=F----------------------------------------------------------------
pk <- readRDS(file=system.file("examples/data/xgxr2.rds", package="NMdata"))
pk[,`:=`(FLAG=NULL,flag=NULL)]

## -----------------------------------------------------------------------------
dt.flags <- fread(text="FLAG,flag,condition
10,Below LLOQ,BLQ==1
100,Negative time,TIME<0")

pk <- flagsAssign(pk,tab.flags=dt.flags,subset.data="EVID==0")
pk <- flagsAssign(pk,subset.data="EVID==1",flagc.0="Dosing")

## -----------------------------------------------------------------------------
tab.count <- flagsCount(data=pk[EVID==0],tab.flags=dt.flags)
print(tab.count)

## -----------------------------------------------------------------------------
pk <- NMorderColumns(pk)

## -----------------------------------------------------------------------------
NMwriteData(pk)

## ----eval=FALSE---------------------------------------------------------------
#  NMwriteSection("run001.mod","INPUT","$INPUT ROW ID TIME EVID CMT AMT DV FLAG STUDY BLQ CYCLE DOSE FLAG2 NOMTIME PART PROFDAY PROFTIME WEIGHTB eff0")

## -----------------------------------------------------------------------------
text.nm <- NMwriteData(pk)

## ----eval=FALSE---------------------------------------------------------------
#  NMwriteSection("run001.mod",list.sections=text.nm["INPUT"])

## -----------------------------------------------------------------------------
pk <- NMstamp(pk,script="vignettes/DataCreate.Rmd")
NMinfo(pk)

## -----------------------------------------------------------------------------
pk <- NMstamp(pk,script="vignettes/DataCreate.Rmd",Description="A PK dataset used for examples.")
NMinfo(pk)

