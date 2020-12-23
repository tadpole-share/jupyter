#' @param train_fname train data frame to be imputed and adjusted
#' @param test_fname test data matrix to be imputed and adjusted
#' @param dictionary_fname the data dictionary
#' @param colImpute The minumum observation in months for column imputation
#' @param dictionary the data dictionary
#' @param colImputeThreshold: The minimum fraction of missing data per column
#' @param rowImputeThreshold: The minimum fraction of missing data per patient
#' @param includeID: For longitudianl data imputation
#' @return
#  matrixes with imputed and adjusted data
#'
#' @examples
dataTADPOLEPreprocesingPy <- function(train_fname,
                                    test_fname,
                                    dictionary_fname,
                                    MinVisit=36,
                                    colImputeThreshold=0.25,
                                    rowImputeThreshold=0.10,
                                    includeID=TRUE)
{
	train_frame <-  read.csv(train_fname, na.strings=c("NA",-4,"-4.0",""," ","NaN"))
	test_frame <- read.csv(test_fname, na.strings=c("NA",-4,"-4.0",""," ","NaN"))
	dictionary <- read.csv(dictionary_fname, na.strings=c("NA",-4,"-4.0",""," ","NaN"))
	source('R_scripts/dataPreprocessing.R')

	train_frame$X <- NULL
	test_frame$X <- NULL
	rownames(train_frame) <- paste(train_frame$RID,train_frame$VISCODE,sep="_")
	rownames(test_frame) <- paste(test_frame$RID,test_frame$VISCODE,sep="_")


	dataTadpole <- dataTADPOLEPreprocesing(train_frame,
											test_frame,
											dictionary,
											MinVisit,
											colImputeThreshold,
											rowImputeThreshold,
											includeID)

	write.csv(dataTadpole$AdjustedTrainFrame,"data/_tmp_dataTadpole$AdjustedTrainFrame.csv")
	write.csv(dataTadpole$testingFrame,"data/_tmp_dataTadpole$testingFrame.csv")
	write.csv(dataTadpole$Train_Imputed,"data/_tmp_dataTadpole$Train_Imputed.csv")
	write.csv(dataTadpole$Test_Imputed,"data/_tmp_dataTadpole$Test_Imputed.csv")
	
	return (c(0))
}