#' FiveYearForeCast
#'
#' @param Classpredictions Subject wise predictions of NC, MCI or AD
#' @param ADAS_Predictions Subject wise predictions of ADAS score
#' @param Ventricle_Predictions Subject wise predicitons of Ventricle Volumes
#' @param Subject_datestoPredict Frame of Subject and date to Predict
#'
#' @return
#  The data frame with all the predictions
#' @export
#'
#' @examples
FiveYearForeCast <- function(Classpredictions=NULL,testDataset=NULL,ADAS_Ventricle_Models=NULL,logt=FALSE,Subject_datestoPredict=NULL)
{
  predictedFrame <-NULL
  RID <- as.character(Subject_datestoPredict$RID)
  sdMIC <- sd(Classpredictions$MCITOADTimeprediction)
  sdNC <- sd(Classpredictions$NCToMCITimeprediction)
  Forecastdates <- Subject_datestoPredict$`Forecast Date`
  
  statusLO <- (Classpredictions$lastKownDX == "NL" | Classpredictions$lastKownDX == "MCI to NL") + 
    2*(Classpredictions$lastKownDX == "Dementia to MCI" | Classpredictions$lastKownDX == "NL to MCI" | Classpredictions$lastKownDX == "MCI") + 
    3*(Classpredictions$lastKownDX == "MCI to Dementia" | Classpredictions$lastKownDX == "Dementia")
  names(statusLO) <- names(Classpredictions$lastKownDX)
  print(sum(is.na(statusLO)))
  print(table(statusLO))
  
  thrNCMCI <- max( (1.5-Classpredictions$NCMCIAUC), (1.0-Classpredictions$pNCtoMCIEvent) )
  thrMCINAD <- max( (1.5-Classpredictions$MCIADAUC), (1.0-Classpredictions$pMCItoADEvent) )           
  thrMCINC <- max ( (1.5-Classpredictions$MCINCAUC), (1.0-Classpredictions$pMCItoNCEvent) )

  print(c(Classpredictions$MCIADAUC,Classpredictions$NCMCIAUC,Classpredictions$MCINCAUC))
  print(c(Classpredictions$pMCItoADEvent,Classpredictions$pNCtoMCIEvent,Classpredictions$pMCItoNCEvent))
    
  W_MCIAD = 2*(Classpredictions$MCIADAUC-0.5)
  W_MCINC = 2*(Classpredictions$MCINCAUC-0.5)
  W_NCMCI = 2*(Classpredictions$NCMCIAUC-0.5)
  
  print(c(W_MCIAD,W_NCMCI,W_MCINC))
  
  ida <- RID[1]
  afdate=Forecastdates[1]
  VentricleAdas <- forecastRegressions(ADAS_Ventricle_Models,testDataset,futuredate=afdate)
  m = 0
  hist(as.numeric(VentricleAdas$ADAS13_MCI[2,]))
  hist(as.numeric(VentricleAdas$Ventricles_MCI[2,]))
  
  print(as.numeric(VentricleAdas$ADAS13_MCI[2,]))
  print(as.numeric(VentricleAdas$Ventricles_MCI[2,]))
  
  for (n in 1:nrow(Subject_datestoPredict))
  {
      id <- RID[n]
      fdate <- Forecastdates[n] 
      if (fdate != afdate)
      {
        print(fdate)
        VentricleAdas <- forecastRegressions(ADAS_Ventricle_Models,testDataset,futuredate=Forecastdates[n])
        m = 0
      }
      m = m + 1
      afdate <- fdate

      if (is.na(statusLO[id]))
      {
        BaseCN_prob <- Classpredictions$crossprediction[id,"0"]
        BaseMCI_prob <- Classpredictions$crossprediction[id,"1"]
        BaseAD_prob <- Classpredictions$crossprediction[id,"2"]
      }
      else
      {
        BaseCN_prob <- 1.0*(statusLO[id] == 1)
        BaseMCI_prob <- 1.0*(statusLO[id] == 2)
        BaseAD_prob <- 1.0*(statusLO[id] == 3)
      }
      TimeToAD <-  Classpredictions$MCITOADTimeprediction[id]
      TimeToMCI <-  Classpredictions$NCToMCITimeprediction[id]
      TimeToNC <-  Classpredictions$MCITONCTimeprediction[id]
      

      timeInterval <- as.numeric(fdate-as.Date(Classpredictions$predictedTimePointData[id,"EXAMDATE"]))/365.25

      NCMCITimeLine <- exp(-(timeInterval-TimeToMCI)/(0.25*TimeToMCI))
        NCMCITimeLine <- 1.0/(1.0+NCMCITimeLine)
        MCIADTimeLine <- exp(-(timeInterval-TimeToAD)/(0.25*TimeToAD))
        MCIADTimeLine <- 1.0/(1.0+MCIADTimeLine)
        MCINCTimeLine <- exp(-(timeInterval-TimeToNC)/(0.25*TimeToNC))
        MCINCTimeLine <- 1.0/(1.0+MCINCTimeLine)

      NCTOMCIprob <- Classpredictions$NCToMCIprediction[id]*NCMCITimeLine

      MCITOADprob <- Classpredictions$MCITOADprediction[id]*MCIADTimeLine

      MCITONCprob <- Classpredictions$MCITONCprediction[id]*MCINCTimeLine

      finalNCProb <- BaseCN_prob
      finalMCIProb <-BaseMCI_prob
      finalADProb <- BaseAD_prob
      

      finalNCProb_a <- BaseCN_prob*(1.0-W_NCMCI) + W_NCMCI*BaseCN_prob*(1.0 - NCTOMCIprob)
      finalNCProb_b <- BaseCN_prob*(1.0-W_MCINC) + W_MCINC*BaseMCI_prob*MCITONCprob
      finalNCProb <- (finalNCProb_a*W_NCMCI + finalNCProb_b*W_MCINC)/(W_MCINC+W_MCINC)
      finalMCIProb_a <- BaseMCI_prob*(1.0-W_MCIAD) + W_MCIAD*BaseMCI_prob*(1.0 - MCITOADprob)
      finalMCIProb_b <- BaseMCI_prob*(1.0-W_NCMCI) + W_NCMCI*BaseCN_prob*NCTOMCIprob
      finalMCIProb_c <- BaseMCI_prob*(1.0-W_MCINC) + W_MCINC*BaseMCI_prob*(1.0 - MCITONCprob)
      finalMCIProb <- (finalMCIProb_a*W_MCIAD + finalMCIProb_b*W_NCMCI + finalMCIProb_c*W_MCINC)/(W_MCIAD+W_NCMCI+W_MCINC)
      finalADProb <- BaseAD_prob*(1.0-W_MCIAD) +  W_MCIAD*BaseMCI_prob*MCITOADprob
      
      totalProb <- finalNCProb + finalMCIProb + finalADProb
      finalNCProb <- finalNCProb/totalProb
      finalMCIProb <- finalMCIProb/totalProb
      finalADProb <- finalADProb/totalProb
      
      Subject_datestoPredict[n,4] <- as.numeric(finalNCProb)
      Subject_datestoPredict[n,5] <- as.numeric(finalMCIProb)
      Subject_datestoPredict[n,6] <- as.numeric(finalADProb)

      
      
      Adas13 <- BaseCN_prob*VentricleAdas$ADAS13_NC[id,] + BaseMCI_prob*VentricleAdas$ADAS13_MCI[id,] + BaseAD_prob*VentricleAdas$ADAS13_AD[id,]
      if (logt)
      {
        Adas13 <- exp(Adas13)-1
      }
      ADSAS13pred <- as.vector(quantile(Adas13, probs = c(0.25, 0.5, 0.75), na.rm = TRUE,names = FALSE, type = 7));

      Subject_datestoPredict[n,7] <- ADSAS13pred[2]
      Subject_datestoPredict[n,8] <- ADSAS13pred[1]
      Subject_datestoPredict[n,9] <- ADSAS13pred[3]

      Ventricle <- BaseCN_prob*VentricleAdas$Ventricles_NC[id,] + BaseMCI_prob*VentricleAdas$Ventricles_MCI[id,] + BaseAD_prob*VentricleAdas$Ventricles_AD[id,]
      if (logt)
      {
        Ventricle <- exp(Ventricle)
      }
      Ventriclepred <- as.vector(quantile(Ventricle, probs = c(0.25, 0.5, 0.75), na.rm = TRUE,names = FALSE, type = 7));
      Subject_datestoPredict[n,10] <- Ventriclepred[2]
      Subject_datestoPredict[n,11] <- Ventriclepred[1]
      Subject_datestoPredict[n,12] <- Ventriclepred[3]
  }
    
  return (Subject_datestoPredict)
}