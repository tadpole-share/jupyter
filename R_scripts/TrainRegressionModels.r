#' @param AdjustedTrainFileName the filename of the adjusted data frame
#' @param numberOfRandomSamples the number of models to be created
#' @param ImputedTrainFileName the filename of the imputed dataset
#' @return
#  filename of the RDATA object with classification models
#'
#' @examples
TrainRegressionModels <- function(AdjustedTrainFileName,
									ImputedTrainFileName,
									numberOfRandomSamples=50)
{
	AdjustedTrainFrame <- read.csv(AdjustedTrainFileName, na.strings=c("NA",-4,"-4.0",""," ","NaN"))
	if (!is.null(AdjustedTrainFrame$X)) 
	{
		AdjustedTrainFrame$X <- NULL
	}
	rownames(AdjustedTrainFrame) <- paste(AdjustedTrainFrame$RID,AdjustedTrainFrame$VISCODE,sep="_")

	ImputedTrainFrame <- read.csv(ImputedTrainFileName, na.strings=c("NA",-4,"-4.0",""," ","NaN"))
	if (!is.null(ImputedTrainFrame$X)) 
	{
		ImputedTrainFrame$X <- NULL
	}
	rownames(ImputedTrainFrame) <- paste(ImputedTrainFrame$RID,ImputedTrainFrame$VISCODE,sep="_")

	predictors=c("AGE","PTGENDER",colnames(AdjustedTrainFrame)[-c(1:22)])
	print(predictors)

	AdjustedTrainFrame$Ventricles <- ImputedTrainFrame[rownames(AdjustedTrainFrame),"Ventricles"]/ImputedTrainFrame[rownames(AdjustedTrainFrame),"ICV"]
	AdjustedTrainFrame$ADAS13 <- ImputedTrainFrame[rownames(AdjustedTrainFrame),"ADAS13"]

	## Train Regression models 

	source('R_scripts/TADPOLE_Train_ADAS_ICV.R')
	RegressionModels <- TrainTadpoleRegresionModels(AdjustedTrainFrame,
														   predictors=predictors,
														   numberOfRandomSamples=numberOfRandomSamples,
														   MLMethod=BSWiMS.model,
														   NumberofRepeats = 1)
	RegressionModelsFileName <- c("data/_RegressionModels_50_Nolog.RDATA")
	
	save(RegressionModels,file=RegressionModelsFileName)
	return(RegressionModelsFileName)
}
