#!bin/env Rscript

## Generic function to scale numeric and categorical variables for multi-model approaches

## Function definitions
scaler<-function(df, ID, centered=TRUE, scaled=TRUE,...){

## Function parameters
#	df = dataframe containing all response, explanatory, and grouping factors 
#	ID = names of grouping variables that should not be scaled; provide as c("Var1", "Var2")
#	centered = should the function center the mean at 0?
#	scaled = should the function scale each variable by its standard deviation?

###--------------------Begin function--------------------###


	# strip NAs and drop NA factor levels
	df<-df[complete.cases(df),]
	df<-droplevels(df)

	## extract ID variables
	ID.vars<-df[,colnames(df)%in%ID] 

#--------------------scale the numeric variables--------------------#
	
	numerics<-sapply(df, is.numeric)
	dat_cont<-df[, numerics]
	dat_cont<-dat_cont[,!colnames(dat_cont)%in%ID]
	scaled_cont<-scale(dat_cont, center=TRUE)

#--------------------scale the categorical variables-----------------#
	cats<-df[,!numerics]
	cats<-cats[,!colnames(cats)%in%ID]
	cats<-as.data.frame(cats)
	
	## if you only have 1 categorical variable, do this...
	if(dim(cats)[2]==1){

		i.levels<-levels(cats[,1])
		cats[, 2]<-0
		cats[, 2][cats[,1]==i.levels[2]]<-1
		colnames(cats)[2]<-paste(i.levels[1],i.levels[2],"dummy", sep=".")

	} else if (dim(cats)[2]>1){

		## if you have more than 1 categorical variable, do this...
		for(i in 1:dim(cats)[2]){
		
		nvars <- dim(cats)[2]
		i.levels<-levels(cats[,i])
		counter<-1
		
		cats[, 1+nvars]<-0
		cats[, 1+nvars][cats[,i]==i.levels[2]]<-1
		colnames(cats)[1+nvars]<-paste(i.levels[1],i.levels[2],"dummy", sep=".")

		## for variables with more than 2 levels we need to add more than 2 dummy variables
		if(length(i.levels)>2){
		
		for(j in 3:length(i.levels)){
		cats[, counter+nvars+(j-2)]<-0
		cats[, counter+nvars+(j-2)][cats[,i]==i.levels[j]]<-1
		colnames(cats)[counter+nvars+(j-2)]<-paste(i.levels[1],i.levels[j],"dummy", sep=".")
		
		}}}
	}	



#--------------------center categorical dummy variables--------------------

	if(dim(cats)[2]==2){
				cats[,2]<-cats[,2] - mean(cats[,2])} else if(dim(cats)[2]>2){

		## remove unscaled categorical
		cats.num<-sapply(cats, is.numeric)
		cats<-cats[,cats.num]
		## center each scaled categorical
		for(i in 1:dim(cats)[2]){
			cats[,i]<-cats[,i] - mean(cats[,i])
		}}

#--------------------bind numeric and categorical together with ID.vars-----------------#
	scaled.df<-cbind(ID.vars, scaled_cont, cats)
	return(scaled.df)

	## END
	}

