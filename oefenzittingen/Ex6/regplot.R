regplot<-function(y,X,show.reg=TRUE){
	old.par<-par(no.readonly=TRUE)
	on.exit(par(old.par))
	ncolM<-6
	nrowM<-ifelse(ncol(X)<6,1,2)
	nelem<-min(ncolM*nrowM,ncol(X))
	ncolA<-ceiling(nelem/nrowM)
	par(mfrow=c(ceiling(nelem/ncolM)*2,ncolA),oma=c(0,2,2,0),mar=c(2,2,1,1))
	for(i in 1:ncolA){
		plot(X[,i],y,col="light blue",pch=16,xlab="",main=colnames(X)[i],ylab=names(y))
		if(show.reg){
			modl<-lm(y~X[,names(X)[i]])
			if(is.numeric(X[,i]))	abline(modl$coef,col="red")
		}
	}
	for(i in 1:ncolA){
		if(is.numeric(X[,i])){	
			boxplot(X[,i],horizontal=TRUE,pch=16,col="light blue",cex=2)
		} else {plot.new()}
	}
	if(nrowM>1){
		for(i in (ncolA+1):nelem){
			plot(X[,i],y,col="light blue",pch=16,xlab="",main=colnames(X)[i],ylab=names(y))
			if(show.reg){
				modl<-lm(y~X[,names(X)[i]])
				if(is.numeric(X[,i]))	abline(modl$coef,col="red")
			}
		}
		for(i in (nelem+1):(ncolA*nrowM)) plot.new()
		for(i in (ncolA+1):nelem){
			if(is.numeric(X[,i])){
				boxplot(X[,i],horizontal=TRUE,pch=16,col="light blue",cex=2)
			} else {plot.new()}
		}
	}
}
#try it:
#regplots(y=iris[,"Sepal.Length"],X=iris[,names(iris)!="Sepal.Length"])
