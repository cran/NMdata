$PROBLEM    031 SAEM

$INPUT NMROW ID TIME EVID CMT AMT DV MDV BBW

$DATA NMsimData_xgxr032_simpletab.csv
IGN=@

$SUBROUTINE ADVAN4 TRANS4

$PK
LTVKA=THETA(1)
LTVV2=THETA(2)
LTVCL=THETA(3)
LTVV3=THETA(4)
LTVQ=THETA(5)

MU_1=LTVKA
KA=EXP(MU_1+ETA(1))
MU_2=LTVV2
V2=EXP(MU_2+ETA(2))
MU_3=LTVCL
CL=EXP(MU_3+ETA(3))
MU_4=LTVV3
V3=EXP(MU_4+ETA(4))
MU_5 = LTVQ
Q =EXP(MU_5+ETA(5))
S2=V2

$ERROR
  IPRED=F
  IRES=DV-IPRED

  IF (IPRED.GT.1) THEN
    W = SQRT(IPRED**2*SIGMA(1,1) + SIGMA(2,2))
  ELSE
    W=1
  ENDIF

  IWRES=IRES/W
  Y=F+F*ERR(1)+ERR(2)

;-----------------------INITIAL ESTIMATES---------------------------------
$THETA 0.817093  ; LTVKA
$THETA 4.32671   ; LTVV2
$THETA 2.46238   ; LTVCL
$THETA 5.39387   ; LTVV3
$THETA 2.21507   ; LTVQ


$OMEGA 0 FIX    
$OMEGA 0.173132 
$OMEGA 0.324497 
$OMEGA 0 FIX    
$OMEGA 0 FIX    


$SIGMA 0.0809353 
$SIGMA 0 FIX     

;; $ESTIMATION METHOD=1 POSTHOC INTER MAXEVAL=9999 NSIG=2 SIGL=9
;;            PRINT=10 NOABORT MSFO=xgxr032.msf





$SIMULATION ONLYSIM (1086284251) 

$TABLE NMROW PRED IPRED NOPRINT NOAPPEND ONEHEADERALL NOTITLE FILE=xgxr032_simpletab.tab

