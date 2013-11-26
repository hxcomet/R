
# df=read.arff(system.file("arff","glass.arff",package="RWeka"))
#asdfsdafsd
#Tess
TestRWekaCls=function(trdat,tsdat,func){
  library(RWeka)
  cName=colnames(trdat)
  clsName=cName[length(cName)]
  formu=paste(clsName,"~.")
  cmdStr=paste("model=",func,"(formu,data=trdat)",sep="")
  eval(parse(text=cmdStr))
#   model=IBk(formu,data=trdat)
  prd=predict(model,tsdat,type="class")
  testLable=tsdat[,length(cName)]
  rightLable=which(prd==testLable)
  accy=length(rightLable)/length(testLable)
#   detach("package:RWeka")
  return(accy)
}

# TestSVM：用支持向量机计算分类准确率
# 基于：e1071; class
# 输入
#   dat:训练数据集，n*m形式的数据框，其中第m列为分类类别，是factor
#   test：测试数据集，n*m形式的数据框，其中第m列为分类类别，是factor
# 输出
#   accy:分类准确率
TestSVM = function(dat,testDat){
  library("class")
  library("e1071")
  col=length(dat)
  trainSamp=dat[,1:col-1]
  trainLable=dat[,col]
  model=svm(trainSamp,trainLable)
  testSamp=testDat[,1:col-1]
  testLable=testDat[,col]
  prd=predict(model,testSamp)
  rightLable=which(prd==testLable)
  accy=length(rightLable)/length(testLable)
  return(accy)
  detach("package:e1071")
  detach("package:class")
  #   table(prd,testLable)  
}

# TestRWekaCls：用RWeka的分类算法计算分类准确率
# 基于：RWeka
# 输入
#   trdat:训练数据集，n*m形式的数据框，其中第m列为分类类别，是factor
#   tsdat：测试数据集，n*m形式的数据框，其中第m列为分类类别，是factor
#   func:调用的函数名，为String形式 
# 输出
#   accy:分类准确率
TestRWekaCls=function(trdat,tsdat,func){
  library(RWeka)
  cName=colnames(trdat)
  clsName=cName[length(cName)]
  formu=paste(clsName,"~.")
  cmdStr=paste("model=",func,"(formu,data=trdat)",sep="")
  eval(parse(text=cmdStr))
  prd=predict(model,tsdat,type="class")
  testLable=tsdat[,length(cName)]
  rightLable=which(prd==testLable)
  accy=length(rightLable)/length(testLable)
  detach("package:RWeka")
  return(accy)
}

# DelNaSym：清除数据中的 sym 样本，转换为NA形式，再直接删除的方法
# 输入
#   sdat：n*m形式的数据框
#   sym:符号
# 输出
#   newDat：n*m形式的数据框
DelNaSym=function(sdat,sym){
  index=which(sdat==sym,arr.ind=T)
  sdat[index]=NA
  newDat=na.omit(sdat)
  return(newDat)
}

# MeanNa：将数据中的样本的属性NA值， 用同列均值的方法替换
# 输入
#   sdat：n*m形式的数据框,都是numric类型
# 输出
#   sdat：n*m形式的数据框
MeanNa=function(sdat){
  sdat=apply(sdat,2,Ttp)
  return(sdat)
  #   内嵌函数Ttp,在向量中，用此向量的均值替代NA值
  Ttp=function(sdat){
    naMat=is.na(sdat)
    meanUse=mean(sdat,na.rm=T)
    sdat[naMat]=meanUse
    return(sdat)
  }
}

# NormDat：将数据中的样本各属性进行归一化
# 输入
#   sdat：n*m形式的数据框,都是numric类型
# 输出
#   sdat：n*m形式的数据框
NormDat=function(sdat){
  mindt=apply(sdat,2,min)
  maxdt=apply(sdat,2,max)
  t1=t(sdat)-mindt
  t2=t1/(maxdt-mindt)
  newDat=t(t2)
  return(newDat)
}

# File2Mem：将数据文件读入内存，并将类别属性列排到最后一列
# 输入
#   filename：以文件形式保存的数据，n*m形式，用","分割，其中第colCs列为类别标签
#   colCs: 类别标签列     
# 输出
#   sdatcD：转换后的数据
File2Mem=function(filename,colCs){
  sdatc=read.table(filename,sep=",")
  if(colCs==1){
    s1=t(logical())
  }else{
    s1=sdatc[,1:colCs-1]
  }
  if(colCs==ncol(sdatc)){
    s2=t(logical())
  }else{
    s2=sdatc[,(colCs+1):ncol(sdatc)]
  }
  #   tmp=rbind(t(s1),t(s2))
  sdatcSamp=cbind(s1,s2)
  sdatcCls=as.character(sdatc[,colCs])    
  sdatcD=cbind(sdatcSamp,sdatcCls)
  return(sdatcD)
}

# CroDat:等分dat矩阵为numDit份，各份保持和原矩阵一样的类别比例
# 输入
#   dat：n*m的矩阵，第m列为factor形式的类别标签
#   numDit：份数
# 输出
#   result：list形式，其中result[[1]]~~result[[num]]为data.frame形式的各小份
CroDat=function(dat,numDit) {
  rowcol=dim(dat)
  lable=dat[,rowcol[2]]
  lableAll=levels(lable)
  result=list()
  blank=logical()
  for(i in 1:numDit){
    result[[i]]=list(blank)
  }
  for(i in 1:length(lableAll)){
    idSig=lable==lableAll[i]
    datSig=dat[idSig,]
    tmpCt=nrow(datSig)
    tmp=runif(tmpCt)
    idRunP=logical()
    for(j in 1:numDit){
      idRunP=(((j-1)/numDit<tmp)&(tmp<=j/numDit))
      datRunP=datSig[idRunP,]
      pieD=rbind(result[[j]],datRunP)
      result[[j]]=pieD
    }
  }
  return(result)
}

# CroAccy:交叉验证 
# 输入
#   dat:数据，n*m类型，第n列为类别，factor
#   numDit:交叉的分成份数
#   meth:采用的算法
#         1：libsvm
#         2：weka的J48
#         3：weka的IBk
# 输出
#   eff:eff[1] 分类准确度均值
#       eff[2] 分类准确度标准差
CroAccy=function(dat,numDit,meth){
  datMat=CroDat(dat,numDit)
  accyMat=matrix()
  datAll=logical()
  ct=c(0)
  for(i in 1:numDit){
    datAll=rbind(datAll,datMat[[i]])
    tmp=dim(datMat[[i]])      
    ct[i+1]=ct[i]+tmp[1]
  }
  acyM=c(0)
  for(i in 1:numDit){
    tsDat=datAll[(ct[i]+1):ct[i+1],]
    if(i==1){
      st1=logical()
    }else{
      st1=datAll[1:ct[i],]
    }
    if(i==numDit){
      st2=logical()
    }else{
      st2=datAll[(ct[i+1]+1):nrow(dat),]      
    }    
    trDat=rbind(st1,st2)
    if(meth==1){
      acyM[i]=TestSVM(trDat,tsDat)      
    }else{
      acyM[i]=TestRWekaCls(trDat,tsDat,meth)        
    }
  }
  accy=mean(acyM)
  std=sd(acyM)
  eff=c(accy,std)
  return(eff)
}

SelFeatur=function(data){
  for(i in 1:ncol){
    for(j in 1:col-1)
      
  }
  
}