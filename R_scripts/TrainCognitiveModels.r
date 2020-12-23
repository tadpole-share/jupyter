#' @param AdjustedTrainFileName the filename of the adjusted data frame
#' @param numberOfRandomSamples the number of models to be created
#' @param delta if longitudinal data will be used for predictoin
#' @return
#  filename of the RDATA object with classification models
#'
#' @examples
TrainCognitiveModels <- function(AdjustedTrainFileName,
									numberOfRandomSamples=25,
									delta=TRUE)
{
	AdjustedTrainFrame <- read.csv(AdjustedTrainFileName, na.strings=c("NA",-4,"-4.0",""," ","NaN"))
	if (!is.null(AdjustedTrainFrame$X)) 
	{
		AdjustedTrainFrame$X <- NULL
	}
	rownames(AdjustedTrainFrame) <- paste(AdjustedTrainFrame$RID,AdjustedTrainFrame$VISCODE,sep="_")


	predictors=c("AGE","PTGENDER",colnames(AdjustedTrainFrame)[-c(1:22)])
	print(predictors)

	source('R_scripts/TADPOLE_Train.R')
	#Train 25 Models for the D2 subjects
	CognitiveClassModels <- TrainTadpoleClassModels(AdjustedTrainFrame,
													predictors=predictors,
													numberOfRandomSamples=numberOfRandomSamples,
													delta=delta,
													MLMethod=BSWiMS.model,
													NumberofRepeats = 1)
	CognitiveClassModelsFileName <- c("data/_CognitiveClassModels_25.RDATA.RDATA")						
	save(CognitiveClassModels,file=CognitiveClassModelsFileName)
	return (CognitiveClassModelsFileName)
}
