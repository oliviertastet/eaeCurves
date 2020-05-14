#'  Plot the mean curves for the groups 
#'
#' 	@param dataf Do you love cats? Defaults to TRUE.
#' 	@param days Do you love cats? Defaults to TRUE.
#' 	@param show Do you love cats? Defaults to TRUE.
#' 	@param min.score Do you love cats? Defaults to TRUE.
#' 	@param by Do you love cats? Defaults to TRUE.
#' 	@keywords plotCurve
#' 	@examples
#' plotCurve()
#' 	@export
plotCurve<-function(dataf,days=1:263,show='None',min.score=0,by='sex'){
	if(by=='sex'){
		col='condition'
	}else{
		col='sex'
	}
	dataf=subset(dataf, max_score>min.score)
	dataf$group = paste0(dataf$sex, dataf$condition)
	first=T
	survival = list()
	for(g in rev(unique(dataf$group))){
		sub = subset(dataf, group==g)
		for(i in 1:263){
			t = as.numeric(as.character(sum(na.omit(as.numeric(!is.na(sub[,as.character(i)]))))/nrow(sub)))
			vec=c(t, i,g)
			if(first){
				first=F 
				df = vec
			}else{
				df=rbind(df,  vec)
			}
		}
	}
	m=melt(dataf[,c('condition','sex','souris',days)],id.vars=c('condition','sex','souris'))
	m$value=as.numeric(as.character(m$value))
	m$variable=as.numeric(as.character(m$variable))
	m$value=as.numeric(as.character(m$value))
	m$var=paste0(m[,2],m[,1])
	m$souris.id = paste(m$souris,m$sex,m$condition,sep='.')

	a=aggregate(m$value,by=list(m$condition,m$sex,m$variable),FUN=mean,na.rm=T)
	a$key = paste0(a[,2],a[,1],a[,3])
	rownames(a)=a$key
	df=data.frame(df)
	rownames(df)=paste0(df[,3],df[,2])
	a$group=paste0(a[,1],a[,2])

	a$survival = as.numeric(as.character(df[rownames(a),1]))

	a$Group.3=as.numeric(as.character(a$Group.3))
	colnames(a)=c('condition','sex','day','score','key','group','survival')
	a$score = as.numeric(as.character(a$score))
	sums=c()
	a=a[order(a$group,a$day),]
	a=na.omit(a)
	for(souris in unique(a$group)){
		sub = a[which(a$group==souris),]
		sum.cum = 0
		for(day in 1:nrow(sub)){
				sum.cum=sum.cum+sub[day,'score']
				sums=c(sums, sum.cum)
			}
	}
	a$sums = sums
	if(col == 'sex'){
		cols=c('lightpink2','deepskyblue')	 
	}else{
		cols=c('darkorange','olivedrab2')
	}
	if(show=='None'){
	g=ggplot(a,aes(day))+geom_path(aes(group=group,y=score,col=a[,col]),size=1)+facet_grid(a[,by]~.)+theme_bw()+scale_color_manual(values=cols)+xlab('Days')+ylab('Clinical Score')
	}else if(show=='survival'){
	a[is.nan(a$score),'score']=NA
	scaleFactor <- max(na.omit(a$score)) / max(a$survival)
	g=ggplot(a,aes(day))+geom_path(aes(group=group,y=score,col=a[,col]),size=1)+facet_grid(a[,by]~.)+theme_bw()+scale_color_manual(values=cols)+xlab('Days')+ylab('Clinical Score')+geom_path(aes(y=survival*scaleFactor,linetype=a[,col]))+scale_y_continuous(name="score", sec.axis=sec_axis(~./scaleFactor, name="survival"))
	}else if(show=='sums'){
		a[is.nan(a$score),'score']=NA
		scaleFactor <- max(na.omit(a$score)) / max(a$sums)
		g=ggplot(a,aes(day))+geom_path(aes(group=group,y=score,col=a[,col]),size=1)+facet_grid(a[,by]~.)+theme_bw()+scale_color_manual(values=cols)+xlab('Days')+ylab('Clinical Score')+geom_path(aes(y=sums*scaleFactor,linetype=a[,col]))+scale_y_continuous(name="score", sec.axis=sec_axis(~./scaleFactor, name="cumsum"))
	}else if(show=='log.sums'){
		a[is.nan(a$score),'score']=NA
		scaleFactor <- max(na.omit(a$score)) / max(log1p(a$sums))
		g=ggplot(a,aes(day))+geom_path(aes(group=group,y=score,col=a[,col]),size=1)+facet_grid(a[,by]~.)+theme_bw()+scale_color_manual(values=cols)+xlab('Days')+ylab('Clinical Score')+geom_path(aes(y=log1p(sums)*scaleFactor,linetype=a[,col]))+scale_y_continuous(name="score", sec.axis=sec_axis(~./scaleFactor, name="cumsum"))
	}
	g
}
