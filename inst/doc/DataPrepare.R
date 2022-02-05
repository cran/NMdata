## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
 ,fig.width=7)

knitr::opts_chunk$set(tidy.opts=list(width.cutoff=60), tidy=TRUE)

## ----eval=TRUE,include=FALSE--------------------------------------------------
## library(devtools)
## load_all()

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

## -----------------------------------------------------------------------------
special.columns <- cc(ID,TIME,CYCLE,STUDY,BW)
compareCols(pk,pk.reduced,cols.wanted=special.columns)

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

## ----error=TRUE---------------------------------------------------------------
mergeCheck(pk,dt.cov,by="ID")

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
pk <- flagsAssign(pk,subset.data="EVID==1",flagc.0="Dose")

## -----------------------------------------------------------------------------
tab.count <- flagsCount(data=pk[EVID==0],tab.flags=dt.flags)
names(tab.count)
tab.count[,.( flag, N.discard, Nobs.discard,N.left, Nobs.left)]

## -----------------------------------------------------------------------------
pk <- NMorderColumns(pk)

## ----include=FALSE------------------------------------------------------------
pk.copy <- copy(pk)
pk[1500,WEIGHTB:=30]
pk[1480,EVID:=5]  
pk[ROW==1403,AMT:=0]  

## -----------------------------------------------------------------------------
findings <- NMcheckData(pk,covs=c("DOSE","WEIGHTB"))

## -----------------------------------------------------------------------------
findings

## -----------------------------------------------------------------------------
pk <- copy(pk.copy)

## -----------------------------------------------------------------------------
text.nm <- NMwriteData(pk,file="derived/pkdata.csv",script="DataPrepare.Rmd",write.csv=TRUE,args.stamp=list(Description="PK data for the Data Preparation vignette."))

## -----------------------------------------------------------------------------
list.files("derived")

## -----------------------------------------------------------------------------
dat.inp <- NMreadCsv("derived/pkdata.csv")
NMinfo(dat.inp)

## -----------------------------------------------------------------------------
dat.inp.rds <- readRDS("derived/pkdata.rds")
NMinfo(dat.inp.rds)

## ----eval=FALSE---------------------------------------------------------------
#  NMwriteSection(dir="nonmem",
#                 file.pattern="run1.*\\.mod",
#                 list.sections=text.nm["INPUT"])

## -----------------------------------------------------------------------------
pk <- NMstamp(pk,script="vignettes/DataCreate.Rmd")
NMinfo(pk)

## -----------------------------------------------------------------------------
pk <- NMstamp(pk,script="vignettes/DataCreate.Rmd",Description="A PK dataset used for examples.",Source.Files="/path/to/adpc.sas7bdat,/path/to/adsl.sas7bdat")
NMinfo(pk)

