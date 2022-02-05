---
title: "NMdata Cheatsheet"
output:
rmarkdown::html:
    toc: true
Suggests: markdown
VignetteBuilder: knitr
vignette: >
  %\VignetteIndexEntry{NMdata Cheatsheet}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
header-includes:
- \usepackage{ae}
---





<!-- The NMdata logo -->
`<img src="data:image/png;base64,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" alt="logo" style="position:absolute; top:15px; right:70px; padding:0px; width:150px"/>`{=html}


:::: {style="display: flex;" class="column-container"}
::: {class="column-container__column"}

Built 2022-01-11 using NMdata 0.0.10.

Please make sure to see latest version available
[here](https://philipdelff.github.io/NMdata/). 

This cheat sheet is
intented to provide an overview and remind of command names. Please
refer to other vignettes for more details on specefic topics and
individual manual pages for details on the functions.

### Get started

```{.r .Code}
install.packages("NMdata")
library(NMdata)
```


### Data preparation
In building the data set, key steps are stacking data sets (like doses, samples, and simulation records) and adding additional information such as covariates. We often use `rbind` and merge or join operations for these steps. `NMdata` helps explore how to do these steps and ensure the results are as expected.

<!-- `dims` - compare dimensions of multiple data sets -->
<!-- ```{r} -->
<!-- ## In these examples we use a PK dataset and a set of covariates -->
<!-- dims(pk,covs) -->
<!-- ``` -->

`compareCols` - Compare presence and classes of columns across data sets before merging or stacking. 

```{.r .Code}
compareCols(covs,covs2)
#> Dimensions:
#>     data nrows ncols
#> 1:  covs   150     2
#> 2: covs2   150     2
#> 
#> Columns that differ:
#>     column    covs     covs2
#> 1: WEIGHTB numeric      <NA>
#> 2:    cov2    <NA> character
```
Use the `cols.wanted` argument for the overview to especially focus on the columns you need in your final data set. 


`renameByContents` - Keep track of what columns are compatible with Nonmem by renaming those that aren't. Rename columns dependent on whether Nonmem cannot interpret as numeric:

```{.r .Code}
## Append an "N" to columns that NONMEM can read (as numeric)
pk <- renameByContents(data=pk,
                       fun.test = NMisNumeric,
                       fun.rename = function(x)paste0(x,"N"))
## lowercase names of columns that NONMEM cannot read as numeric
pk <- renameByContents(data=pk,
                       fun.test = NMisNumeric,
                       fun.rename = tolower,
                       invert.test = TRUE)
```

`mergeCheck(x1,x2,...)` - Merges data and only accept results if all that happened was that columns from `x1` were added to `x1`. Row order of `x1` is retained. Arguments are passed to data.table which does the actual merge. This automates the checks we need to do after say merging covariates onto data.


```{.r .Code}
pk2 <- mergeCheck(pk,covs2,by="ID")
#> The following columns were added: cov2
```
We did not get an error from `mergeCheck` so we know that the rows in `pk2` are exactly identical to those in `pk`, except the addition of a column called `cov2`.

`flagsAssign` - Assign exclusion flags to a dataset based on specified table

`flagsCount` - Create an overview of number of retained and discarded datapoints

This is a simple example where we use only two exclusion flags. If time is negative, we assign exclusion flag `FLAG=100`. If (time is non-negative and) `BLQ==1` we assign `FLAG=10`. If none of these conditions are met, `FLAG=0`, and the row will be included in the analysis. `fread` is just a way to write the table row-wise for readability.




```{.r .Code}
dt.flags <- fread(text="FLAG,flag,condition
10,Below LLOQ,BLQ==1
100,Negative time,TIME<0")

pk <- flagsAssign(pk,tab.flags=dt.flags,subset.data="EVID==0")
#> Coding FLAG = 100, flag = Negative time
#> Coding FLAG = 10, flag = Below LLOQ
```

```{.r .Code}
pk <- flagsAssign(pk,subset.data="EVID==1",flagc.0="Dosing")
flagsCount(pk[EVID==0],tab.flags=dt.flags)[,.( flag, N.left, Nobs.left, N.discard, Nobs.discard)]
#>                  flag N.left Nobs.left N.discard Nobs.discard
#> 1: All available data    150      1352        NA           NA
#> 2:      Negative time    150      1350         0            2
#> 3:         Below LLOQ    131       755        19          595
#> 4:       Analysis set    131       755        NA           NA
```

`NMorderColumns` - Standardize column order. Columns that can be read
by NONMEM are prioritized towards left.


`NMcheckData` - Extensive data checks for NONMEM compatibility and common issues.


:::

::: {class="column-container__column"}

### Write data and update NONMEM control streams accordingly
`NMwriteData` - Write data ensuring compatibility with NONMEM. By defaults saves both a csv (for NONMEM) and an rds (for R, retaining factor levels etc). Text for optional use in `$INPUT` and `$DATA` NONMEM sections is returned. `script` and `args.stamp` are optional arguments, see "Traceability" section for their purpose.


```{.r .Code}
text.nm <- NMwriteData(pk,file="derived/pkdata.csv",script="NMdata-cheat.Rmd",args.stamp=list(Description="PK data for the NMdata Cheatsheet"))
#> Data written to file(s):
#> derived/pkdata.csv
#> derived/pkdata.rds
#> For NONMEM:
#> $INPUT ROW ID NOMTIME TIME EVID CMT AMT DV FLAG STUDY BLQ CYCLE DOSE
#> PART PROFDAY PROFTIME eff0
#> $DATA derived/pkdata.csv
#> IGN=@
#> IGNORE=(FLAG.NE.0)
```



`NMwriteSection` - Replace sections of a nonmem control stream. `NMwriteSection` can use the text generated by `NMwriteData` to update NONMEM runs to match the newly generated input data. Update INPUT section (and not DATA) for all control streams in directory "nonmem" which file names start with "run1" and end in ".mod" (say "run101.mod" to "run199.mod"):

```{.r .Code}
NMwriteSection(dir="nonmem",
               file.pattern="run1.*\\.mod",
               list.sections=text.nm["INPUT"])
```
`NMwriteSection` has the argument `data.file` to further limit the scope of files to update based on what data file the control streams use. It only makes sense to use the auto-generated text for control streams that use this data set.

The text for NONMEM can be generated without saving data using `NMgenText`. You can tailor the generation of the text to copy `(DV=CONC)`, drop `(COL=DROP)`, rename (`DV` instead of `CONC`) and more.

### Debugging input data (and control stream)
`NMcheckData` was mentioned under "Data preparation" because it can check a data set before it's written to file. However, it can also be on a path to a control stream, in which case it checks column names in INPUT section against data and then runs a full check of the data set as read by NONMEM (according to column names in $INPUT and ACCEPT/IGNORE statements in $DATA). We suppress the default print to terminal (`quiet=T`) and provide selected parts of the results here.


```{.r .Code}
res.debug <- NMcheckData(file="nonmem/run201.mod",quiet=T)
## we will only show some of what is available here
names(res.debug)
#> [1] "datafile"       "tables"         "dataCreate"     "input.filters" 
#> [5] "input.colnames" "NMcheckData"
```

```{.r .Code}
## Meta data on input data file:
res.debug$tables
#>    source       name nrow ncol nid filetype          file.mtime
#> 1:  input pkdata.csv 1502   23 150     text 2022-01-11 16:35:57
#>                            file has.col.row has.col.id
#> 1: nonmem/../derived/pkdata.csv        TRUE       TRUE
```
In this model we forgot to update the control stream INPUT section after adding a column to data ("off" means that INPUT text can be reorganized to match data file better):

```{.r .Code}
## Comparison of variable naming:
res.debug$input.colnames[c(1:2)]
#>    datafile INPUT nonmem result compare
#> 1:      ROW   ROW    ROW    ROW      OK
#> 2:       ID    ID     ID     ID      OK
```

```{.r .Code}
res.debug$input.colnames[c(9:12)]
#>    datafile INPUT nonmem result compare
#> 1:     FLAG  FLAG   FLAG   FLAG      OK
#> 2:    STUDY   BLQ    BLQ    BLQ     off
#> 3:      BLQ CYCLE  CYCLE  CYCLE     off
#> 4:    CYCLE  DOSE   DOSE   DOSE     off
```
We have some findings on the data set too. But since `res.debug$input.colnames` tells us we are reading the data incorrectly, we have to address that before interpreting findings on the data. 

```{.r .Code}
res.debug$NMcheckData$summary
#>    column              check  N Nid
#> 1:   EVID Subject has no obs 19  19
#> 2:    MDV   Column not found  1   0
```
If you are preparing a data set, run `NMcheckData` directly on the data (using the `data` argument) insted of on a control stream.

:::
::: {class="column-container__column"}

### Retrieve Nonmem results
`NMscanData` - Automatically find Nonmem input and output tables and organize data. By default, available column names are taken from the NONMEM control stream. Additional column names (columns not read by NONMEM) are taken from input data file.






















