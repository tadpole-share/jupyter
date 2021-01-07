#' @param D1D2file the filename of the D1D2 Tadpole data
#' @param D3File the filename of the D3 Dataset
#' @return
#'
#' @examples


#Create Training and Testing Sets from TADPOLE data sets
ExtractTrainTest_tadpole_D1_D2 <- function(D1D2file,D3File)
{  
  TADPOLE_D1_D2 <- read.csv(D1D2file, na.strings=c("NA",-4,"-4.0",""," ","NaN"))
  TADPOLE_D3 <- read.csv(D3File, na.strings=c("NA",-4,"-4.0",""," ","NaN"))

  TADPOLE_D1_D2$X <- NULL
  TADPOLE_D3$X <- NULL

  TADPOLE_D1_D2$EXAMDATE <- as.Date(TADPOLE_D1_D2$EXAMDATE)
  #Train Test Split
  TrainingSet <- subset(TADPOLE_D1_D2,D1==1)
  TesingSet <- subset(TADPOLE_D1_D2,D2==1)
  rownames(TrainingSet) <- paste(TrainingSet$RID,TrainingSet$VISCODE,sep="_")
  rownames(TesingSet) <- paste(TesingSet$RID,TesingSet$VISCODE,sep="_")

  ### Data sorting

  TrainingSet <- TrainingSet[order(TrainingSet$EXAMDATE),]
  TrainingSet <- TrainingSet[order(as.numeric(TrainingSet$RID)),]

  TesingSet <- TesingSet[order(TesingSet$EXAMDATE),]
  TesingSet <- TesingSet[order(as.numeric(TesingSet$RID)),]

  TADPOLE_D3$EXAMDATE <- as.Date(TADPOLE_D3$EXAMDATE)
  rownames(TADPOLE_D3) <- paste(TADPOLE_D3$RID,TADPOLE_D3$VISCODE,sep="_")

  #D3 Cross sectional
  ## First Remove D2 subjects from Training Set
  D3IDS <- TADPOLE_D3$RID
  D3TrainingSet <- TrainingSet[!(TrainingSet$RID %in% D3IDS),]

  write.csv(TrainingSet,"data/_tmp_D1TrainingSet.csv")
  write.csv(TesingSet,"data/_tmp_D2TesingSet.csv")
  write.csv(TADPOLE_D3,"data/_tmp_D3TesingSet.csv")
  write.csv(D3TrainingSet,"data/_tmp_D3TrainingSet.csv")
  result <- numeric();
#  result <- list(TrainingSet,TesingSet,TADPOLE_D3,D3TrainingSet)
  return (result)
}
    

  




