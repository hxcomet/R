DelBlank=function(sdat){
	sizeDat=dim(sdat)
	for(i in 1:sizeDat[1]){
		sigBlank=which(sdat[i,]=="?")
		sdat[i,sigBlank]=NA
	}
	newDat=na.omit(sdat)
	return(newDat)
}