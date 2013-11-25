 TODO: Add comment
# 
# Author: hehuixin
###############################################################################

source("fn.R")

irdts=read.table("ia.DAT",sep=",")
irdtc=DelBlank(irdts)
irAcy=TestSVM(irdtc,irdtc)
irAcy
#
#waters=read.table("water-treatment.data",sep=",")
#waterc=DelBlank(waters)
#watercSamp=waterc[,2:length(waterc)]
#watercCls=waterc[,1]
#watercD=cbind(watercSamp,watercCls)
#waterAcy=TestSVM(watercD,watercD)

waters=read.table("wine.data",sep=",")
waterc=DelBlank(waters)
watercSamp=waterc[,2:length(waterc)]
watercCls=as.character(waterc[,1])
watercD=cbind(watercSamp,watercCls)
waterAcy=TestSVM(watercD,watercD)
waterAcy