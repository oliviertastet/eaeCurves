#'  Align the curves 
#'
#' 	@param data Do you love cats? Defaults to TRUE.
#' 	@param days Do you love cats? Defaults to TRUE.
#' 	@param k Do you love cats? Defaults to TRUE.
#' 	@keywords alignCurves
#' 	@examples
#' alignCurves()
#' 	@export
alignCurves<-function(data, days = 1:263, k=3, n=5){
	title='Mean clinical score'

	m=melt(data[,c('condition','sex','souris',days)],id.vars=c('condition','sex','souris'))
	m=na.omit(m)
	m$key = paste(m$souris,m$sex,m$condition,m$variable,sep='.')
	m$souris.id = paste(m$souris,m$sex,m$condition,sep='.')
	
	vec=c()
	ages = c()
	vars = c()
	sums=c()
	m=m[!duplicated(m$key),]
	m=m[order(m$souris.id,m$variable),]
	for(mouse in unique(m$souris.id)){
		sub = subset(m, souris.id==mouse)	
		sick = F
		check = T
		for(day in unique(as.character(sub$variable))){
			if(check){
				if(is.na(sub[which(sub$variable==day),'value'])){
					if(!is.na(sub[which(sub$variable==as.character(as.numeric(day)+1)),'value'])){
						sub[which(sub$variable==day),'value']=0	
					}
				}
				if(sub[which(sub$variable==day),'value']>0){
					ages=c(ages,day)
					sick=T
					check=F
					vec[(length(vec)-n):(length(vec))]=1
				}
			}
			if(sick){
				vec=c(vec,1)
			}else{
				vec=c(vec,0)
			}
		}
	}
	m$sick = vec
	m.sick=subset(m, sick==1)
	m.sick$variable=as.numeric(as.character(m.sick$variable))
	df=data.frame(matrix(ncol=264))
	m.sick$souris.id=paste(m.sick$souris,m.sick$sex,m.sick$condition,sep='.')
	for(mouse in unique(m.sick$souris.id)){
		vec=rep(NA,264)
		names(vec)=1:263
		colnames(df)=names(vec)
		sub = subset(m.sick, souris.id==mouse)
		sums=c(sums, sum(as.numeric(sub$value)))
		vars=c(vars, var(as.numeric(sub$value)))
		sub$variable=(sub$variable-min(sub$variable))+1
		vec[sub$variable]=sub$value
		df=rbind(df,vec)
	
	}
	df=df[2:nrow(df),]
	df=data.frame(cbind(unique(m.sick$souris.id),df))
	colnames(df)=gsub('X','',colnames(df))
	df$souris = unlist(strsplit(as.character(df[,1]),'\\.'))[seq(1,nrow(df)*3,3)]
	df$sex = unlist(strsplit(as.character(df[,1]),'\\.'))[seq(2,nrow(df)*3,3)]
	df$condition = unlist(strsplit(as.character(df[,1]),'\\.'))[seq(3,nrow(df)*3,3)]
	df=df[,c('souris','sex','condition',1:263)]
	df$max_score = as.numeric(apply(df[,4:266],1,FUN=max,na.rm=T))
	df$age.at.onset = ages
	rownames(df)=paste(df[,1],df[,2],df[,3],sep='.')
	hc=hclust(dist(df[,as.character(1:263)]))
	df$cluster = cutree(hc,k=k)
	df$var = vars
	df$score_sum=sums
	#df[is.na(df)]=5
	df
}

