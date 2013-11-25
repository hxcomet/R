setwd("arff")
lsname=dir();
setwd("..")
library(RWeka)
funcAll=list_Weka_interfaces()
funcName=funcAll[[3]]
r=length(lsname)
c=length(funcName)
# AcyMat=array(dim=c(r,c))
# SdMat=data.frame(dim=c(r,c))
# fileUse=c(4:11,15:17)4:7,9:11
AcyMat=logical()
sdMat=logical()
fileUse=c(11)
funUse=c(2,4:7,10:12,16:19)
# 5:7
# funUse=c(18)
for(i in fileUse){
  library(RWeka)
  df=read.arff(system.file("arff",lsname[i],package="RWeka"))
  for(j in funUse){
    accy=CroAccy(df,5,funcName[j])
    AcyMat=c(AcyMat,accy[1])
#     SdMat=c(sdMat,accy[2])
  }
 }
b=t(as.matrix(AcyMat))

# c=t(as.matrix(sdMat))
m=logical()
for(i in 1:12){
  bc=b[,c(11,i,13)]
  tmp=TestRWekaCls(bc,bc,"DecisionStump")
  m=cbind(m,tmp)
}
m

# 初步试验
b=read.table("test.csv",sep=",")
a=read.table("data2.csv",sep=",",heard=T)
View(a)
b=a[,2:33]
b
ac=TestSVM(b,b)

Attaching package: ‘e1071’

  svm, write.svm


Warning message:
  In svm.default(trainSamp, trainLable) :
> ac
[1] 0.87454464 0.09869138
> ad=TestRWekaCls(b,b,"J48")
> ad
[1] 0.8874805 0.2369138
> ad=TestRWekaCls(b,b,"OneR");ad
[1] 0.87075310 0.06515812
> ad=TestRWekaCls(b,b,"SMO");ad
[1] 0.8761059 0.1823882

> dat=b
> source("fn.R")
There were 20 warnings (use warnings() to see them)
> bb=ResampDat(b,2)
> dat=b
> ratio=2
> rowcol=dim(dat)
> rowcol
[1] 26902    32
> lable=dat[,rowcol[2]]
> label
> lableAll=levels(lable)
> lable=dat[,rowcol[2]]
> lable=as.factor(lable)
> lableAll=levels(lable)
> lableAll
[1] "0" "1"
> result=list()
> blank=logical()
> idSig1=(lable==lableAll[1])
> idSig2=(lable==lableAll[2])
if(nrow(datSig1)>nrow(datSig2)){
  + tmp=datSig1
  + datSig1=datSig2
  + datSig2=tmp
  + 
    + 
    + n1=nrow(datSig1)
  + n2=nrow(datSig2)
  + rnum=runif(n2)
  + n2use=n1*ratio
  + if(n2use>n2){
    + print("Large class is less the ratio, calculate with region data")
    + result=dat
    + 
      + 
      + idRunP=(rnum<(1/ratio))
    + 
      + 
      + result=rbind(datSig1,datRunP)
    + }
 result
  
#   数据平衡实验
  bb=ResampDat(b,2)
  > ad=TestRWekaCls(bb,bb,"J48")
  > result
  list()
  > ad
  [1] 0.7547565 0.3778626
  > ad=TestRWekaCls(b,b,"OneR");ad
  [1] 0.87075310 0.06515812
  > ad=TestRWekaCls(bb,bb,"OneR");ad
  [1] 0.6886664 0.1523991
  > ad=TestRWekaCls(b,b,"J48");ad
  [1] 0.8874805 0.2369138
  > bb=ResampDat(b,1)
  > ad=TestRWekaCls(bbb,bbb,"J48");ad
  [1] 0.7116887 0.6202290
  > ad=TestRWekaCls(bb,bb,"J48");ad
  [1] 0.7174148 0.6853871
  > source("fn.R")
  There were 20 warnings (use warnings() to see them)
  > bb=ResampDat(b,1)
  > ad=TestRWekaCls(bbb,bbb,"J48");ad
  [1] 0.7116887 0.6202290
  > ad=TestRWekaCls(bb,bb,"J48");ad
  [1] 0.7174148 0.6853871
  > source("fn.R")
  There were 20 warnings (use warnings() to see them)
  > bb=ResampDat(b,2)
  > ad=TestRWekaCls(bb,bb,"J48");ad
  [1] 0.7562494 0.3238822
  > bbb=ResampDat(b,1)
  > ad=TestRWekaCls(bb,bb,"J48");ad
  [1] 0.7562494 0.3238822
  > ad=TestRWekaCls(bbb,bbb,"J48");ad
  [1] 0.7022755 0.6404035
  > #   数据平衡实验
    > ad=TestSVM(bbb,bbb);ad
    In svm.default(trainSamp, trainLable) :
  [1] 0.7003679 0.7178299
  > ad=TestSVM(bbb,b);ad
  
  Attaching package: ‘e1071’
  
    svm, write.svm
    [1] 0.6642629 0.7178299
  > bb=ResampDat(b,2)
  > ac=TestSVM(b,b)
  
  Attaching package: ‘e1071’