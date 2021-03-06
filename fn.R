# TestSVM：用支持向量机计算分类准确率
# 基于：e1071; class
# 输入
#   dat:训练数据集，n*m形式的数据框，其中第m列为分类类别，是factor
#   test：测试数据集，n*m形式的数据框，其中第m列为分类类别，是factor
# 输出
#  result[1]: accy:--类准确率
#  result[2]: recall:--召回率
TestSVM = function(dat,testDat){
	library("class")
	library("e1071")
	col=ncol(dat)
	trainSamp=dat[,1:col-1]
	trainLable=as.factor(dat[,col])
	model=svm(trainSamp,trainLable)
	testSamp=testDat[,1:col-1]
	testLable=as.factor(testDat[,col])
	prd=predict(model,testSamp)
	rightLable=which(prd==testLable)
	accy=length(rightLable)/length(testLable)
# 以标号为“1”的作为正例，计算召回率
	recall=GetRecall(prd,testLable,"1")
# 
	result=c(accy,recall)
	detach("package:e1071")
	detach("package:class")
	return(result)
	#   table(prd,testLable)  
}


# GetRecall:计算查全率
# 输入：tlable：用以测试的lable,为一行或一列
#       rlable：真实lable，为一行或一列
#       selt：欲计算的类别名，为rlable中某个值
# 输出：recall:返回召回率，无此类别时为-99
GetRecall=function(tlable,rlable,selt){
	useId=which(rlable==selt)
	if(length(useId)==1){
		print(paste("No class:",selt))
		recall=-99
	}else{
		rightId=which(tlable[useId]==selt)
		recall=length(rightId)/length(useId)
	}
	return(recall)    
}

# TestRWekaCls：用RWeka的分类算法计算分类准确率
# 基于：RWeka
# 输入
#   trdat:训练数据集，n*m形式的数据框，其中第m列为分类类别，是factor
#   tsdat：测试数据集，n*m形式的数据框，其中第m列为分类类别，是factor
#   func:调用的函数名，为String形式 
# 输出
#  result[1]: accy:--类准确率
#  result[2]: recall:--召回率
TestRWekaCls=function(trdat,tsdat,func){
	library(RWeka)
	cName=colnames(trdat)
	trdat[,length(cName)]=as.factor(trdat[,length(cName)])
	clsName=cName[length(cName)]
	formu=paste(clsName,"~.")
	cmdStr=paste("model=",func,"(formu,data=trdat)",sep="")
	eval(parse(text=cmdStr))
	prd=predict(model,tsdat,type="class")
	testLable=as.factor(tsdat[,length(cName)])
	rightLable=which(prd==testLable)
	accy=length(rightLable)/length(testLable)
	# 以标号为“1”的作为正例，计算召回率
	recall=GetRecall(prd,testLable,"1")
	# 
	result=c(accy,recall)
	detach("package:RWeka")
	return(result)
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

# DelSame：清除数据中属性列相同的样本，转换为NA形式，再直接删除的方法
# 输入
#   sdat：n*m形式的数据框
#   sym:符号
# 输出
#   newDat：n*m形式的数据框
DelSame=function(sdat){
	matVar=apply(sdat,2,var)
	newDat=sdat[,matVar!=0]
	dimS=dim(sdat)
	tmp=c(1:dimS[2])
	sameLable=tmp[matVar==0]
	result=list()
	result[[1]]=newDat
	result[[2]]=sameLable
	return(result)
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
	nmCol=dim(dat)
	dat[,nmCol[2]]=as.factor(dat[,nmCol[2]])
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
	recall=c(0)
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
			sresult=TestSVM(trDat,tsDat)
			acyM[i]=sresult[1]      
		}
		if(meth==2){
			sresult=TestRWekaCls(trDat,tsDat,"J48")        
			acyM[i]=sresult[1]
			recall[i]=sresult[2]
		}
		if(meth==3){
			sresult=TestRWekaCls(trDat,tsDat,"IBk")        
			acyM[i]=sresult[1]
		}
	}
	accy=mean(acyM)
#	std=sd(acyM)
#	eff=c(accy,std)
	rec=mean(recall)
	eff=c(accy,rec)
	return(eff)
}

# ResampDat:数据平衡，将2类样本按比例重采样（下采样方法） 
# 输入
#   dat:数据，n*m类型，第n列为类别，factor
#   ratio:欲得到的大类数据/小类数据的比例
# 输出
#   result:得到的数据
ResampDat=function(dat,ratio){
	rowcol=dim(dat)
	lable=dat[,rowcol[2]]
	lable=as.factor(lable)
	lableAll=levels(lable)
	result=list()
	blank=logical()
	idSig1=(lable==lableAll[1])
	datSig1=dat[idSig1,]
	idSig2=(lable==lableAll[2])
	datSig2=dat[idSig2,]
# 类datSig1为小类
	if(nrow(datSig1)>nrow(datSig2)){
		tmp=datSig1
		datSig1=datSig2
		datSig2=tmp
	}
	n1=nrow(datSig1)
	n2=nrow(datSig2)    
	rnum=runif(n2)
	n2use=n1*ratio
	if(n2use>n2){
		print("Large class is less the ratio, calculate with region data")
		result=dat
	}else{
		idRunP=(rnum<(n2use/n2))
		datRunP=datSig2[idRunP,]
		result=rbind(datSig1,datRunP)
	}
	return(result)
}  

# OneFeatCls:按数据到各单一特征，评判特征的分类能力和召回率 
# 输入
#   data:数据，n*m类型，第n列为类别，factor
# 输出
#   result:得到的结果，为准确率+召回率
OneFeatCls=function(data){
	col=dim(data)
	result=logical()
	numc=col[2]-1
	for(i in 1:numc){
		datause=data[,c(i,col[2])]
		tmp=TestRWekaCls(datause,datause,"J48")
		result=cbind(result,tmp)
	}
	return(result)
}


# ResampOneFeatCls:重采样后，按数据到各单一特征，评判特征的分类能力和召回率 
# 输入
#   data:数据，n*m类型，第n列为类别，factor
#   ratio:欲得到的大类数据/小类数据的比例
# 输出
#   result:得到的结果，为准确率+召回率
ResampOneFeatCls=function(data,ratio){
	col=dim(data)
	result=logical()
	numc=col[2]-1
	for(i in 1:numc){
		datause=data[,c(i,col[2])]
		datause=ResampDat(datause,ratio)
		tmp=TestRWekaCls(datause,datause,"J48")
		result=cbind(result,tmp)
	}
	return(result)
}

# FetSelect:按数据的特征做特征选择，前向贪心算法 
# 输入
#   data:数据，n*m类型，第n列为类别，factor
#   func:调用的函数名，为String形式 
# 输出
#   result:得到的结果，为特征+准确率+召回率
FetSelect=function(data,func){
	col=dim(data)
	result=logical()
	numc=col[2]-1
	selc=c()
	acMat=c()
	recMat=c()
	mrecall=0
	allFet=1:numc
	yu=1:numc
	for(i in 1:numc){
		mrecall=0
		for(j in 1:(numc-i)){
			datause=data[,c(selc,yu[j],col[2])]
			tmp=TestRWekaCls(datause,datause,func)
			if(tmp[2]>mrecall){
				thisSel=yu[j]
				sigac=tmp[1]
				mrecall=tmp[2]
			}
		}
		selc=c(selc,thisSel)
		acMat=c(acMat,sigac)
		recMat=c(recMat,mrecall)
		yu=allFet[-selc]
	}
	result=cbind(selc,acMat,recMat)
	return(result)
}

# ReductPcaM:PCA降维处理 
# 输入
#   data:数据，n*m类型
# 输出
#   result:得到的结果，
#		result[[1]]为降维结果
#		result[[2]]为对应特征矢量矩阵
ReductPcaM=function(data){
	data=scale(data)
	dimdt=dim(data)
	col=dimdt[2]
	dtno=data[,1:col]
#   matCor=cor(data)
	matCor=t(dtno)%*%dtno
	eigTr=eigen(matCor)
	eigvalue=eigTr$values
	loadDt=eigTr$vectors
	alt=matrix(loadDt,col,col)
	redt=as.matrix(dtno)%*%alt
	result=list()
	result[[1]]=redt
	result[[2]]=alt
	return(result)
}

# TePcaClsClose:PCA降维分类封闭测试 
# 输入
#   data:数据，n*m类型，第n列为类别，factor
#	num:选取的主成分个数，0 为全部
# 输出
#   result:得到的结果
#		result[1]为准确率
#		result[2]为召回率
TePcaClsClose=function(data,num){
	dimdt=dim(data)
	col=dimdt[2]
	redt0=ReductPcaM(data[,1:(col-1)])
	redt=redt0[[1]]
	if(num==0){
		num=col-1
	}
	redt=redt[,1:num]
	redata=cbind(redt,data[,col])
	rsdt=ResampDat(redata,1)
#	result=TestRWekaCls(data.frame(rsdt),data.frame(redata),'J48')
	result=TestRWekaPrd(data.frame(rsdt),data.frame(redata),'J48')
	return(result)
}

# TePcaClsHalf:PCA降维分类半封闭测试 
# 输入
#   data:数据，n*m类型，第n列为类别，factor
# 输出
#   result:得到的结果
#		result[1]为准确率
#		result[2]为召回率
TePcaClsHalf=function(data){
	dimdt=dim(data)
	col=dimdt[2]
	redt0=ReductPcaM(data[,1:(col-1)])
	redt=redt0[[1]]
	redata=cbind(redt,data[,col])
	rsdt=ResampDat(redata,1)
	result=CroAccy(data.frame(rsdt),5,2)
	return(result)
}

# CroAccyPcaOpen:PCA降维分类开放测试，交叉验证 
# 输入
#   dat:数据，n*m类型，第n列为类别，factor
#   numDit:交叉的分成份数
#   meth:采用的算法
#         1：libsvm
#         2：weka的J48
#         3：weka的IBk
#	num:选取的主成分个数，0 为全部
# 输出
#   eff:eff[1] 分类准确度均值
#       eff[2] 分类召回率
CroAccyPcaOpen=function(dat,numDit,meth,num){
	nmCol=dim(dat)
	dat[,nmCol[2]]=as.factor(dat[,nmCol[2]])
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
	recall=c(0)
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
####################################################
#		trDat=ResampDat(data.frame(trDat),1)
####################################################
		rePca=PcaTrTs(trDat[,1:(nmCol[2]-1)],tsDat[,1:(nmCol[2]-1)])
		tr1=rePca[[1]]
		ts1=rePca[[2]]
		####################################################
		if(num==0){
			num=nmCol[2]-1
		}
		tru=tr1[,1:num]
		tsu=ts1[,1:num]
		####################################################
		trDat=cbind(tru,trDat[,nmCol[2]])
		tsDat=cbind(tsu,tsDat[,nmCol[2]])
		trDat=data.frame(trDat)
		tsDat=data.frame(tsDat)
####################################################
		trDat=ResampDat(data.frame(trDat),1)
####################################################
		
####################################################
		if(meth==1){
			sresult=TestSVM(trDat,tsDat)
			acyM[i]=sresult[1]      
		}
		if(meth==2){
			sresult=TestRWekaCls(trDat,tsDat,"J48")        
			acyM[i]=sresult[1]
			recall[i]=sresult[2]
		}
		if(meth==3){
			sresult=TestRWekaCls(trDat,tsDat,"IBk")        
			acyM[i]=sresult[1]
		}
	}
	accy=mean(acyM)
	rec=mean(recall)
	eff=c(accy,rec)
	return(eff)
}


# PcaTrTs:对trDat进行PCA降维，并用trDat的过程系数对tsDat做PCA降维
# 输入
#   trDat:数据，n*m类型
#   tsDat:数据，n*m类型
# 输出
#   result:result[[1]] trDat的降维结果
#          result[[2]] tsDat的降维结果
PcaTrTs=function(trDat,tsDat){
	dimTs=dim(tsDat)
	meanTr=apply(trDat,2,mean)
	sdTr=apply(trDat,2,sd)
	matMean=t(matrix(meanTr,dimTs[2],dimTs[1]))
	matSd=t(matrix(sdTr,dimTs[2],dimTs[1]))
	tsScale=(tsDat-matMean)*(matSd^(-1))
	matP=ReductPcaM(trDat)
	trRe=matP[[1]]
	tsRe=as.matrix(tsScale)%*%as.matrix(matP[[2]])
	result=list()
	result[[1]]=trRe
	result[[2]]=tsRe
	return(result)
}


# TestRWekaCls：用RWeka的分类算法预测结果
# 基于：RWeka
# 输入
#   trdat:训练数据集，n*m形式的数据框，其中第m列为分类类别，是factor
#   tsdat：测试数据集，n*m形式的数据框，其中第m列为分类类别，是factor
#   func:调用的函数名，为String形式 
# 输出
#  result[1]: accy:--类准确率
#  result[2]: recall:--召回率
TestRWekaPrd=function(trdat,tsdat,func){
	library(RWeka)
	cName=colnames(trdat)
	trdat[,length(cName)]=as.factor(trdat[,length(cName)])
	clsName=cName[length(cName)]
	formu=paste(clsName,"~.")
	cmdStr=paste("model=",func,"(formu,data=trdat)",sep="")
	eval(parse(text=cmdStr))
	prd=predict(model,tsdat,type="class")
	testLable=as.factor(tsdat[,length(cName)])
	# 
	result=cbind(testLable,prd)
	detach("package:RWeka")
	return(result)
}