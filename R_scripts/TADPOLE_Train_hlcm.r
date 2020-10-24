#AdjustedFrame The datatoPredict
#Predictors the predictors
#The months for training
#numberOfRandomSamples the number of samples from the adjustedFrame
#MLMethod the Machine Learning method
#asFactor is the class should be treated as a factor
#... parameters to be passed to the ML method

#It will return models that predict if a subject will convert to MCI to AD
#it will return models that predict the time to conversion

TrainTadpoleClassModels <- function(AdjustedFrame,
                                    predictors,
                                    months=NULL,
                                    numberOfRandomSamples=5,
                                    delta=FALSE,
                                    MLMethod=BSWiMS.model,
                                    asFactor=FALSE,
                                    ...){
  #args
 #AdjustedFrame= AdjustedTrainFrame
 #predictors = predictors=c("AGE","PTGENDER",colnames(AdjustedTrainFrame)[-c(1:22)])
 #months=NULL
 #numberOfRandomSamples=numberOfRandomSamples=25
 #delta=TRUE
 #MLMethod=HLCM_EM
 #asFactor=FALSE
 #hysteresis = 0.1
#
  AdjustedFrame$RID <- as.character(AdjustedFrame$RID)
  library("FRESA.CAD")
  if (is.null(months)){
    months <- as.numeric(names(table(AdjustedFrame$M)))
    print(months)
  }
  cpredictors <- predictors
  
  AdjustedFrame <- AdjustedFrame[order(AdjustedFrame$Years_bl),]
  AdjustedFrame <- AdjustedFrame[order(as.numeric(AdjustedFrame$RID)),]
  
  pdis <- AdjustedFrame$RID
  lastTimepointSet <- AdjustedFrame[c(pdis[1:(length(pdis)-1)] != pdis[-1],TRUE),]
  rownames(lastTimepointSet) <- lastTimepointSet$RID

  BaseTimepointSet <- AdjustedFrame[c(TRUE,pdis[-1] != pdis[1:(length(pdis)-1)]),]
  rownames(BaseTimepointSet) <- BaseTimepointSet$RID
  deltaFeaturepredictors <- predictors[regexpr('_bl', predictors) < 0][-(c(1:2))]
  
  TimePointsSubset <- list();
  Orderbytimepoint <- NULL
  m <- 0
  i <- 1;
  for (m in months) {
    TimePointsSubset[[i]] <- subset(AdjustedFrame,M == m)
    rownames(TimePointsSubset[[i]]) <- TimePointsSubset[[i]]$RID
    TimePointsSubset[[i]]$Year_bl_LastVisit <- lastTimepointSet[TimePointsSubset[[i]]$RID,"Years_bl"]
    TimePointsSubset[[i]]$Last_DX <- lastTimepointSet[TimePointsSubset[[i]]$RID,"DX"]
    TimePointsSubset[[i]]$TimeToLastVisit <- TimePointsSubset[[i]]$Year_bl_LastVisit - TimePointsSubset[[i]]$Years_bl
    if (delta)
    {
      deltaObservations <- TimePointsSubset[[i]][,deltaFeaturepredictors] - BaseTimepointSet[rownames(TimePointsSubset[[i]]),deltaFeaturepredictors]
      colnames(deltaObservations) <- paste("Delta",colnames(deltaObservations),sep="_")
      TimePointsSubset[[i]] <- cbind(TimePointsSubset[[i]],deltaObservations)
    }
    TimePointsSubset[[i]] <- TimePointsSubset[[i]][complete.cases(TimePointsSubset[[i]][,predictors]),]
    Orderbytimepoint <- rbind(Orderbytimepoint,TimePointsSubset[[i]])
    i <- i + 1
  }
  
  AdjustedFrame <- Orderbytimepoint
  AdjustedFrame <- AdjustedFrame[order(AdjustedFrame$Years_bl),]
  AdjustedFrame <- AdjustedFrame[order(as.numeric(AdjustedFrame$RID)),]
  
  Orderbytimepoint <- NULL
  if (delta){
    predictors <- c(predictors,colnames(deltaObservations))
  }
  

  ## Get All the MCI subjects that progressed to AD
  
  print(table(AdjustedFrame$DX))
  MCISubset <- subset(AdjustedFrame,DX == "NL to MCI" | DX == "MCI" | DX == "Dementia to MCI")
  MCIIDS <- unique(MCISubset$RID)
  print(length(MCIIDS))
  
  subsetMCIADConversion <-  subset(AdjustedFrame,DX == "MCI to Dementia" | DX == "Dementia")
  print(nrow(subsetMCIADConversion))
  MCIConverters <- subsetMCIADConversion$RID %in% MCIIDS
  subsetMCIADConversion <- subsetMCIADConversion[MCIConverters,]
  print(nrow(subsetMCIADConversion))
  pdis <- subsetMCIADConversion$RID
  subsetMCIADConversion <- subsetMCIADConversion[c(TRUE,pdis[-1] != pdis[1:(length(pdis)-1)]),]
  print(nrow(subsetMCIADConversion))



  rownames(subsetMCIADConversion) <- subsetMCIADConversion$RID
  
  ### MCI Subset by time points
  
  MCItoADorderbytimepoint <- NULL
  for (m in months){
    TimePointsMCISubset <- subset(MCISubset,M == m)
    rownames(TimePointsMCISubset) <-  TimePointsMCISubset$RID
    TimePointsMCISubset$TimeToEvent <- subsetMCIADConversion[TimePointsMCISubset$RID,"Years_bl"] - TimePointsMCISubset$Years_bl
    MCItoADorderbytimepoint <- rbind(MCItoADorderbytimepoint,TimePointsMCISubset)
  }
  
  controlMCIToADset <- MCItoADorderbytimepoint[is.na(MCItoADorderbytimepoint$TimeToEvent),]
  controlMCIToADset <- subset(controlMCIToADset,TimeToLastVisit > 3)
  hist(controlMCIToADset$TimeToLastVisit)
  controlMCIToADset$TimeToEvent <- controlMCIToADset$TimeToLastVisit
  
  caseMCIToADset <- MCItoADorderbytimepoint[!is.na(MCItoADorderbytimepoint$TimeToEvent),]
  caseMCIToADset <- subset(caseMCIToADset,TimeToEvent > 0 & TimeToEvent < 5.0 )
  hist(caseMCIToADset$TimeToEvent)
  
  ## MCI Modeling Set

  controlMCIToADset$class <- 0
  caseMCIToADset$class <- 1
  MCI_to_AD_set <- rbind(controlMCIToADset,caseMCIToADset)
  MCI_to_AD_set$TimeToLastVisit <- NULL
  
  MCI_to_AD_TrainSet <- MCI_to_AD_set[MCI_to_AD_set$D1==1,]
  
  print(table(MCI_to_AD_TrainSet$class))
  
  
  ## Modeling MCI conversion
  
  print(table(MCI_to_AD_TrainSet$VISCODE))
  
  MCI_to_ADSets <- list();
  MCI_TO_AD_Model <- list();
  MCI_TO_AD_TimeModel <- list();
  n=1
  pMCItoADEvent <- 0;
  
  for (n in 1:numberOfRandomSamples){
    randomnumber <- sample(1:nrow(MCI_to_AD_TrainSet),nrow(MCI_to_AD_TrainSet))
    MCI_to_AD_RandomSet <- MCI_to_AD_TrainSet[randomnumber,]
    MCI_to_AD_RandomSet <- MCI_to_AD_RandomSet[order(as.numeric(MCI_to_AD_RandomSet$RID)),]
    RID <- MCI_to_AD_RandomSet$RID
    set1 <- MCI_to_AD_RandomSet[c(RID[1:length(RID)-1] != RID[-1],TRUE),]
    rownames(set1) <- set1$RID
    set1 <- set1[complete.cases(set1),]
    print(nrow(set1))
    print(table(set1$class))
    pMCItoADEvent <- pMCItoADEvent + sum(set1$class)/nrow(set1)
    MCI_to_ADSets[[n]] <- set1[,c("class",predictors)]
    if (asFactor){
      MCI_to_ADSets[[n]]$class <- as.factor(MCI_to_ADSets[[n]]$class)
    }
    
    MCI_TO_AD_Model[[n]] <- MLMethod(class ~ .,MCI_to_ADSets[[n]],hysteresis=hysteresis)

    sm <- summary(MCI_TO_AD_Model[[n]])
    #print(sm$tAUC)
    MCI_TO_AD_Model[[n]]$BSWiMS.model$bootCV$data <- NULL
    MCI_TO_AD_Model[[n]]$BSWiMS.model$bootCV$testOutcome <- NULL
    MCI_TO_AD_Model[[n]]$BSWiMS.model$bootCV$testPrediction <- NULL
    

    set1 <- subset(set1,class==1)
    print(nrow(set1))
    
    MCI_to_ADSets[[n]] <- set1[,c("TimeToEvent",predictors)]
    MCI_to_ADSets[[n]]$TimeToEvent <- set1$TimeToEvent
    MCI_TO_AD_TimeModel[[n]] <- MLMethod(TimeToEvent ~ .,MCI_to_ADSets[[n]],hysteresis=hysteresis)
    MCI_TO_AD_TimeModel[[n]]$univariate <- NULL
    MCI_TO_AD_TimeModel[[n]]$BSWiMS.model$bootCV$data <- NULL
    MCI_TO_AD_TimeModel[[n]]$BSWiMS.model$bootCV$testOutcome <- NULL
    MCI_TO_AD_TimeModel[[n]]$BSWiMS.model$bootCV$testPrediction <- NULL
  }
  pMCItoADEvent <- pMCItoADEvent/numberOfRandomSamples
  

  ## Get All the MCI subjects that progressed to NC
  

  subsetMCINCConversion <-  subset(AdjustedFrame,DX == "MCI to NL" | DX == "NL")
  print(nrow(subsetMCIADConversion))
  MCIConverters <- subsetMCINCConversion$RID %in% MCIIDS
  subsetMCINCConversion <- subsetMCINCConversion[MCIConverters,]
  print(nrow(subsetMCINCConversion))
  pdis <- subsetMCINCConversion$RID
  subsetMCINCConversion <- subsetMCINCConversion[c(TRUE,pdis[-1] != pdis[1:(length(pdis)-1)]),]
  print(nrow(subsetMCINCConversion))
  
  
  rownames(subsetMCINCConversion) <- subsetMCINCConversion$RID
  
  ### MCI Subset by time points
  
  MCIToNCorderbytimepoint <- NULL
  for (m in months)
  {
    TimePointsMCISubset <- subset(MCISubset,M == m)
    rownames(TimePointsMCISubset) <-  TimePointsMCISubset$RID
    TimePointsMCISubset$TimeToEvent <- subsetMCINCConversion[TimePointsMCISubset$RID,"Years_bl"] - TimePointsMCISubset$Years_bl
    MCIToNCorderbytimepoint <- rbind(MCIToNCorderbytimepoint,TimePointsMCISubset)
  }
  
  controlMCIToNCset <- MCIToNCorderbytimepoint[is.na(MCIToNCorderbytimepoint$TimeToEvent),]
  controlMCIToNCset <- subset(controlMCIToNCset,TimeToLastVisit > 3)
  hist(controlMCIToNCset$TimeToLastVisit)
  controlMCIToNCset$TimeToEvent <- controlMCIToNCset$TimeToLastVisit
  
  caseMCIToNCset <- MCIToNCorderbytimepoint[!is.na(MCIToNCorderbytimepoint$TimeToEvent),]
  caseMCIToNCset <- subset(caseMCIToNCset,TimeToEvent > 0 & TimeToEvent < 5.0 )
  hist(caseMCIToNCset$TimeToEvent)
  
  ## MCI Modeling Set
  
  controlMCIToNCset$class <- 0
  caseMCIToNCset$class <- 1
  MCI_to_NC_set <- rbind(controlMCIToNCset,caseMCIToNCset)
  MCI_to_NC_set$TimeToLastVisit <- NULL
  
  MCI_to_NC_TrainSet <- MCI_to_NC_set[MCI_to_NC_set$D1==1,]
  
  print(table(MCI_to_NC_TrainSet$class))
  
  
  ## Modeling MCI conversion
  
  print(table(MCI_to_NC_TrainSet$VISCODE))
  
  MCI_to_NCSets <- list();
  MCI_TO_NC_Model <- list();
  MCI_TO_NC_TimeModel <- list();
  n=1
  pMCItoNCEvent <- 0;
  
  for (n in 1:numberOfRandomSamples)
  {
    randomnumber <- sample(1:nrow(MCI_to_NC_TrainSet),nrow(MCI_to_NC_TrainSet))
    MCI_to_NC_RandomSet <- MCI_to_NC_TrainSet[randomnumber,]
    MCI_to_NC_RandomSet <- MCI_to_NC_RandomSet[order(as.numeric(MCI_to_NC_RandomSet$RID)),]
    RID <- MCI_to_NC_RandomSet$RID
    set1 <- MCI_to_NC_RandomSet[c(RID[1:length(RID)-1] != RID[-1],TRUE),]
    rownames(set1) <- set1$RID
    set1 <- set1[complete.cases(set1),]
    print(nrow(set1))
    print(table(set1$class))
    pMCItoNCEvent <- pMCItoNCEvent + sum(set1$class)/nrow(set1)
    MCI_to_NCSets[[n]] <- set1[,c("class",predictors)]
    if (asFactor)
    {
      MCI_to_NCSets[[n]]$class <- as.factor(MCI_to_NCSets[[n]]$class)
    }
    
    MCI_TO_NC_Model[[n]] <- MLMethod(class ~ .,MCI_to_NCSets[[n]],...)
    MCI_TO_NC_Model[[n]]$BSWiMS.model$bootCV$data <- NULL
    MCI_TO_NC_Model[[n]]$BSWiMS.model$bootCV$testOutcome <- NULL
    MCI_TO_NC_Model[[n]]$BSWiMS.model$bootCV$testPrediction <- NULL
    
    sm <- summary(MCI_TO_NC_Model[[n]])
    print(sm$tAUC)
    
    set1 <- subset(set1,class==1)
    print(nrow(set1))
    
    MCI_to_NCSets[[n]] <- set1[,c("TimeToEvent",predictors)]
    MCI_to_NCSets[[n]]$TimeToEvent <- set1$TimeToEvent
    MCI_TO_NC_TimeModel[[n]] <- MLMethod(TimeToEvent ~ .,MCI_to_NCSets[[n]],...)
    MCI_TO_NC_TimeModel[[n]]$BSWiMS.model$bootCV$data <- NULL
    MCI_TO_NC_TimeModel[[n]]$BSWiMS.model$bootCV$testOutcome <- NULL
    MCI_TO_NC_TimeModel[[n]]$BSWiMS.model$bootCV$testPrediction <- NULL
    
  }
  pMCItoNCEvent <- pMCItoNCEvent/numberOfRandomSamples
 
  
  
  ## Get All the NC subjects that progressed
  print(table(AdjustedFrame$DX))
  NCSubset <- subset(AdjustedFrame,DX == "NL" | DX == "MCI to NL")
  NCIDS <- unique(NCSubset$RID)
  print(length(NCIDS))
  
  subsetNCConvConversion <-  subset(AdjustedFrame,DX == "NL to Dementia" | DX == "NL to MCI" | DX == "MCI")
  print(nrow(subsetNCConvConversion))
  MCIConverters <- subsetNCConvConversion$RID %in% NCIDS
  subsetNCConvConversion <- subsetNCConvConversion[MCIConverters,]
  print(nrow(subsetNCConvConversion))
  pdis <- subsetNCConvConversion$RID
  subsetNCConvConversion <- subsetNCConvConversion[c(TRUE,pdis[-1] != pdis[1:(length(pdis)-1)]),]
  print(nrow(subsetNCConvConversion))
  

  rownames(subsetNCConvConversion) <- subsetNCConvConversion$RID
  
  ### NC Subset by time points
  
  NCConvorderbytimepoint <- NULL
  for (m in months)
  {
    TimePointsNCSubset <- subset(NCSubset,M == m)
    rownames(TimePointsNCSubset) <-  TimePointsNCSubset$RID
    TimePointsNCSubset$TimeToEvent <- subsetNCConvConversion[TimePointsNCSubset$RID,"Years_bl"] - TimePointsNCSubset$Years_bl
    NCConvorderbytimepoint <- rbind(NCConvorderbytimepoint,TimePointsNCSubset)
  }
  
  controlNCConvset <- NCConvorderbytimepoint[is.na(NCConvorderbytimepoint$TimeToEvent),]
  controlNCConvset <- subset(controlNCConvset, TimeToLastVisit > 3)
  hist(controlNCConvset$TimeToLastVisit)
  controlNCConvset$TimeToEvent <- controlNCConvset$TimeToLastVisit
  caseNCConvset <- NCConvorderbytimepoint[!is.na(NCConvorderbytimepoint$TimeToEvent),]
  caseNCConvset <- subset(caseNCConvset,TimeToEvent > 0 & TimeToEvent < 5.0)
  hist(caseNCConvset$TimeToEvent)
  
  ## Modeling Nomal congitive Set
  controlNCConvset$class <- 0
  caseNCConvset$class <- 1
  NCConv_set <- rbind(controlNCConvset,caseNCConvset)
  
  NCConv_TrainSet <- NCConv_set[NCConv_set$D1==1,]
  
  table(NCConv_TrainSet$class)
  
  table(NCConv_TrainSet$VISCODE)
  
  NCConvSets <- list();
  NCConv_Model <- list();
  NL_TO_OTHER_TimeModel <- list();
  n=1
  
  pNCtoMCIEvent <- 0;
  for (n in 1:numberOfRandomSamples)
  {
    randomnumber <- sample(1:nrow(NCConv_TrainSet),nrow(NCConv_TrainSet))
    NCConv_RandomSet <- NCConv_TrainSet[randomnumber,]
    NCConv_RandomSet <- NCConv_RandomSet[order(as.numeric(NCConv_RandomSet$RID)),]
    RID <- NCConv_RandomSet$RID
    set1 <- NCConv_RandomSet[c(RID[1:length(RID)-1] != RID[-1],TRUE),]
    rownames(set1) <- set1$RID
    set1 <- set1[complete.cases(set1),]
    print(nrow(set1))
    print(table(set1$class))
    pNCtoMCIEvent <- pNCtoMCIEvent + sum(set1$class)/nrow(set1)
    
    NCConvSets[[n]] <- set1[,c("class",predictors)]
    if (asFactor)
    {
      NCConvSets[[n]]$class <- as.factor(NCConvSets[[n]]$class)
    }
    NCConv_Model[[n]] <- MLMethod(class ~ .,NCConvSets[[n]],...)
    NCConv_Model[[n]]$BSWiMS.model$bootCV$data <- NULL 
    NCConv_Model[[n]]$BSWiMS.model$bootCV$testOutcome <- NULL
    NCConv_Model[[n]]$BSWiMS.model$bootCV$testPrediction <- NULL
    
    sm <- summary(NCConv_Model[[n]])
    print(sm$tAUC)
    
    set1 <- subset(set1,class==1)
    print(nrow(set1))
    NCConvSets[[n]] <- set1[,c("TimeToEvent",predictors)]
    NCConvSets[[n]]$TimeToEvent <- set1$TimeToEvent
    NL_TO_OTHER_TimeModel[[n]] <- MLMethod(TimeToEvent ~ .,NCConvSets[[n]],...)
    NL_TO_OTHER_TimeModel[[n]]$BSWiMS.model$bootCV$data <- NULL 
    NL_TO_OTHER_TimeModel[[n]]$BSWiMS.model$bootCV$testOutcome <- NULL
    NL_TO_OTHER_TimeModel[[n]]$BSWiMS.model$bootCV$testPrediction <- NULL
  }
  pNCtoMCIEvent <- pNCtoMCIEvent/numberOfRandomSamples


  ## Cross Sectional Modeling
  
  ### Baseline Modeling Set


  class <- 
    2*(AdjustedFrame$DX == "Dementia" | AdjustedFrame$DX == "MCI to Dementia" | AdjustedFrame$DX == "NL to Dementia") +
    1*(AdjustedFrame$DX == "Dementia to MCI" | AdjustedFrame$DX == "MCI" | AdjustedFrame$DX == "NL to MCI")
  
  AdjustedFrame$class <- class
  

  AllADNISets <- list();
  AllADNI_Model <- list();
  n=1
  
  for (n in 1:numberOfRandomSamples)
  {
    randomnumber <- sample(1:nrow(AdjustedFrame),nrow(AdjustedFrame))
    AllADNI_RandomSet <- AdjustedFrame[randomnumber,]
    AllADNI_RandomSet <- AllADNI_RandomSet[order(as.numeric(AllADNI_RandomSet$RID)),]
    RID <- AllADNI_RandomSet$RID
    set1 <- AllADNI_RandomSet[c(RID[1:length(RID)-1] != RID[-1],TRUE),]
    rownames(set1) <- set1$RID
    AllADNISets[[n]] <- set1[,c("class",cpredictors)]
    AllADNISets[[n]] <- AllADNISets[[n]][complete.cases(AllADNISets[[n]]),]
    print(nrow(AllADNISets[[n]]))
    print(table(set1$DX_bl,set1$class))
    if (asFactor)
    {
      AllADNISets[[n]]$class <- as.factor(AllADNISets[[n]]$class)
    }
    AllADNI_Model[[n]] <- MLMethod(class ~ .,AllADNISets[[n]],...)
    AllADNI_Model[[n]]$BSWiMS.model$bootCV$data <- NULL 
    AllADNI_Model[[n]]$BSWiMS.model$bootCV$testOutcome <- NULL
    AllADNI_Model[[n]]$BSWiMS.model$bootCV$testPrediction <- NULL
    AllADNI_Model[[n]]$oridinalModels$data <- NULL
    AllADNI_Model[[n]]$oridinalModels$theClassBaggs[[1]]$bagged.model$model <- NULL
    AllADNI_Model[[n]]$oridinalModels$theClassBaggs[[2]]$bagged.model$model <- NULL
    AllADNI_Model[[n]]$oridinalModels$theClassBaggs[[3]]$bagged.model$model <- NULL
    AllADNI_Model[[n]]$oridinalModels$theBaggedModels[[1]]$bagged.model$model <- NULL
    AllADNI_Model[[n]]$oridinalModels$theBaggedModels[[2]]$bagged.model$model <- NULL
    AllADNI_Model[[n]]$oridinalModels$redBaggedModels[[1]]$bagged.model$model <- NULL
    AllADNI_Model[[n]]$oridinalModels$redBaggedModels[[2]]$bagged.model$model <- NULL
    AllADNI_Model[[n]]$oridinalModels$polr <- NULL
  }
  
  predicitionModels <- list(CrossModels = AllADNI_Model,
                            MCIToADModels=MCI_TO_AD_Model,
                            MCIToADTimeModel = MCI_TO_AD_TimeModel,
                            MCIToNCModels=MCI_TO_NC_Model,
                            MCIToNCTimeModel = MCI_TO_NC_TimeModel,
                            NCToMCIModel=NCConv_Model,
                            NCToMCITimeModel=NL_TO_OTHER_TimeModel,
                            predictors = cpredictors,
                            pMCItoADEvent = pMCItoADEvent,
                            pNCtoMCIEvent = pNCtoMCIEvent,
                            pMCItoNCEvent = pMCItoNCEvent
  )
  
  return (predicitionModels)
}