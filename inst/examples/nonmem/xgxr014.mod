$PROBLEM    Like 001, with xgxr2, advan2

$INPUT ROW ID NOMTIME TIME EVID CMT AMT DV FLAG STUDY
BLQ CYCLE DOSE PART PROFDAY PROFTIME WEIGHTB eff0

$DATA     ../data/xgxr2.csv IGNORE=@ IGNORE=(FLAG.NE.0)

$SUBROUTINE ADVAN2 TRANS2

$PK
TVKA=THETA(1)
TVV=THETA(2)
TVCL=THETA(3)
                   
KA=TVKA*EXP(ETA(1))
V=TVV*EXP(ETA(2))
CL=TVCL*EXP(ETA(3))

$ERROR
  IPRED=F
  IRES=DV-IPRED

  IF (IPRED.GT.1) THEN
    W = SQRT(IPRED**2*SIGMA(1,1)**2 + SIGMA(2,2)**2)
  ELSE
    W=1
  ENDIF

  IWRES=IRES/W
  Y=F+F*ERR(1)+ERR(2)

;-----------------------INITIAL ESTIMATES---------------------------------
$THETA  (0,0.9)             ; POPKA
$THETA  (0,4.2)             ; POPCL
$THETA  (0,3.6)             ; POPV2

$OMEGA 0.1
$OMEGA 0.1
$OMEGA 0.1

$SIGMA .2
$SIGMA .2

$ESTIMATION METHOD=1 POSTHOC INTER MAXEVAL=9999 NSIG=2 SIGL=9
            PRINT=10 NOABORT


$TABLE ROW TVKA TVV TVCL KA V CL PRED IPRED NOPRINT FILE=xgxr014_res.txt
