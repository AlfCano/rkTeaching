frequencyTableIntervals <- function(data, variable, breaks=NULL, right=FALSE, include.lowest=TRUE, center=FALSE, width=FALSE, groups=NULL, decimals=4){
	require(plyr)
	if (!is.data.frame(data)) {
		stop("data must be a data frame")
	}
	if (!variable %in% colnames(data)) {
		stop(paste(variable, "is not a column of data frame"))
	}
	if (!is.numeric(data[[variable]])) {
		stop(paste(variable, "must be numeric"))
	}
	if (!is.null(groups)) {
		for (i in 1:length(groups)) {
			if (!groups[i] %in% colnames(data)) {
				stop(paste(groups[i], "is not a column of data frame", data))
			}
			if (!is.factor(data[[groups[i]]])) {
				stop(paste(groups[i], "is not a factor"))
			}
		}
	}
	if (is.null(groups)){
		result <- tabulateFrequenciesIntervals(data=data, variable=variable, breaks=breaks, right=right, include.lowest=include.lowest, center=center, width=width, decimals=decimals)
	}
	else {
		f <- function(df){
			tabulateFrequenciesIntervals(data=df, variable=variable, breaks=breaks, right=right, include.lowest=include.lowest, center=center, width=width, decimals=decimals)
		}
		result <- dlply(data, groups, f)
	}
	return(result)
}

tabulateFrequenciesIntervals <- function(data, variable, breaks=NULL, right=FALSE, include.lowest=TRUE, center=FALSE, width=FALSE, decimals=4) {
	require(plyr)
	result <- na.omit(data[[variable]])
	if (is.null(breaks)){
		breaks <- pretty(range(result), nclass.Sturges(result))
	}
	centers <- (breaks[-1]+breaks[-length(breaks)])/2
	#for (i in 1:length(centers)) {
		#result[nrow(result)+1,] <- NA
		#result[nrow(result),variable] <- centers[i]
	#}
	result <- data.frame(c(result,centers))
	colnames(result) <- variable
	result <- transform(result, classes=cut(result[[variable]], breaks=breaks, right=right, include.lowest=include.lowest))
	result <- count(result, "classes")
	colnames(result)[1] <- paste(variable,"classes",sep=" ")
	colnames(result)[2] <- "Abs.Freq."
	result[["Abs.Freq."]] <- result[["Abs.Freq."]] - rep(1,length(centers))
	if (center) {
		result[["Center"]] <- centers
	}
	if (width) {
		result[["Width"]] <- breaks[-1]-breaks[-length(breaks)]
	}
	if (center | width) {
		result <- cbind(result[,names(result)!="Abs.Freq."], Abs.Freq.=result[["Abs.Freq."]])
	}
	result <- mutate(result, Rel.Freq.=round(Abs.Freq./sum(Abs.Freq.),decimals), Cum.Abs.Freq.=cumsum(Abs.Freq.), Cum.Rel.Freq.=round(Cum.Abs.Freq./sum(Abs.Freq.),decimals))
	return(result)
}
