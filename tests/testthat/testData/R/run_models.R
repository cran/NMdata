library(devtools)

load_all("~/wdirs/NMsim",export_all = FALSE)

NMexec("../nonmem/xgxr033.mod")
