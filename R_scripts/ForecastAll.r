#' @param CognitiveModelFileName the filename of the cognitive models
#' @param RegressionModelFileName the filename of the regression models
#' @param AdjustedTestFileName the filename of the imputed test dataset
#' @param ImputedTestFileName the filename of the imputed test dataset
#' @param submissionTemplateFileName the filename of the submission template
#' @return
#  filename of the RDATA object with forecast data
#'
#' @examples
ForecastAll <- function(CognitiveModelFileName,
						RegressionModelFileName,
						AdjustedTestFileName,
						ImputedTestFileName,
						submissionTemplateFileName
						)
{
	suppressMessages(library("FRESA.CAD"))
	suppressMessages(library(readxl))
	source('R_scripts/predictCognitiveStatus.R')
	source('R_scripts/predictTADPOLERegresions.R')
	source('R_scripts/FiveYearForecast.R')


	submissionTemplate <- as.data.frame(read_excel(submissionTemplateFileName))
	submissionTemplate$`Forecast Date` <- as.Date(paste(submissionTemplate$`Forecast Date`,"-01",sep=""))
	submissionTemplate$`CN relative probability` <- as.numeric(nrow(submissionTemplate))
	submissionTemplate$`MCI relative probability` <-  as.numeric(nrow(submissionTemplate))
	submissionTemplate$`AD relative probability` <-  as.numeric(nrow(submissionTemplate))
	submissionTemplate$ADAS13 <-  as.numeric(nrow(submissionTemplate))
	submissionTemplate$`ADAS13 50% CI lower` <-  as.numeric(nrow(submissionTemplate))
	submissionTemplate$`ADAS13 50% CI upper` <-  as.numeric(nrow(submissionTemplate))
	submissionTemplate$Ventricles_ICV <-  as.numeric(nrow(submissionTemplate))
	submissionTemplate$`Ventricles_ICV 50% CI lower` <-  as.numeric(nrow(submissionTemplate))
	submissionTemplate$`Ventricles_ICV 50% CI upper` <-  as.numeric(nrow(submissionTemplate))
	submissionTemplate <- submissionTemplate[order(submissionTemplate$`Forecast Month`),]


	AdjustedTestFrame <- read.csv(AdjustedTestFileName, na.strings=c("NA",-4,"-4.0",""," ","NaN"))
	if (!is.null(AdjustedTestFrame$X)) 
	{
		AdjustedTestFrame$X <- NULL
	}
	rownames(AdjustedTestFrame) <- paste(AdjustedTestFrame$RID,AdjustedTestFrame$VISCODE,sep="_")

	ImputedTestFrame <- read.csv(ImputedTestFileName, na.strings=c("NA",-4,"-4.0",""," ","NaN"))
	if (!is.null(ImputedTestFrame$X)) 
	{
		ImputedTestFrame$X <- NULL
	}
	rownames(ImputedTestFrame) <- paste(ImputedTestFrame$RID,ImputedTestFrame$VISCODE,sep="_")


	load(file=CognitiveModelFileName)
	predictADNI <- forecastCognitiveStatus(CognitiveClassModels,AdjustedTestFrame)

	AdjustedTestFrame$Ventricles <- ImputedTestFrame[rownames(AdjustedTestFrame),"Ventricles"]/ImputedTestFrame[rownames(AdjustedTestFrame),"ICV"]
	AdjustedTestFrame$ADAS13 <- ImputedTestFrame[rownames(AdjustedTestFrame),"ADAS13"]

	## THe last time point required for forecasting ADAS13 and Ventricles
	ltptf <- AdjustedTestFrame
	ltptf <- ltptf[order(ltptf$EXAMDATE),]
	ltptf <- ltptf[order(as.numeric(ltptf$RID)),]
	rids <- ltptf$RID
	ltptf <- ltptf[c(rids[1:(length(rids)-1)] != rids[-1],TRUE),]
	rownames(ltptf) <- ltptf$RID

	 ### Forecasting 

	load(file=RegressionModelFileName)
	forecast <- FiveYearForeCast(predictADNI,
								   testDataset=ltptf,
								   ADAS_Ventricle_Models=RegressionModels,
								   Subject_datestoPredict=submissionTemplate)


	ForeCastingFileName <- c("data/_ForecastFRESACAD.csv")
	write.csv(forecast,file=ForeCastingFileName)

	return(ForeCastingFileName)
}
