library(tidyverse)
library(lubridate)
library(dplyr)
library(MASS)

models <- function(area_data, pyear){
  CombineData <- function(df,pyear){
    df$DISTRICT <- ifelse(is.na(df$DISTRICT), 0, df$DISTRICT)
    # get number of sales per neighborhood group/value area.
    df$new_dist <- NA
    summary <- df %>%
      group_by(DISTRICT) %>%
      dplyr::summarize(AVG_RCNLD = mean(LAND_VAL_RW, na.rm = T), Count = sum(!is.na(SOLD_PRICE)), sales = sum(SYEAR_old == pyear, na.rm = T)) %>%
      arrange(Count) 
    
    # get the large neighborhood groups (more than 80 sales)
    largegroup <- summary %>%
      dplyr::filter(Count >= 100, sales != 0)
    # get the small neighborhood groups (less than 80 sales)
    smallgroup<- summary %>%
      dplyr::filter(Count < 100 | sales == 0)
    #only run if there are small groups to combine
    if(nrow(smallgroup > 0)){ 
      
      # Create an empty matrix to be filled with old and new value areas
      X <- matrix(rep(NA, nrow(smallgroup)*2), ncol = 2)
      # Create data frame from matrix
      new_ngrp <- data.frame(X)
      
      # Running a loop the number of small group times to fill the empty data frame.
      
      for (i in 1:nrow(smallgroup)){
        difference <- c()
        for (k in 1:nrow(largegroup)){
          # Getting the difference in mean RCNLD of the small group with each large group.
          difference = append(difference, abs(smallgroup[i, ]$AVG_RCNLD - largegroup[k, ]$AVG_RCNLD))
        }
        
        # Location of the large group with the smallest difference in the average RCNLD
        dif = which.min(difference)
        # Put the old group number in the first column of data frame
        new_ngrp[i, ]$X1 <- smallgroup[i, ]$DISTRICT
        # Put the new group number in the second column of the data frame
        new_ngrp[i, ]$X2 <- largegroup[dif, ]$DISTRICT
      }
      # Loop through the city data and assign the new group numbers.
      for(j in 1:nrow(df)){
        for(k in 1:nrow(new_ngrp)){
          # Checking if DISTRICT is equal to any of the old groups that need to be combined
          if(df$DISTRICT[j] == new_ngrp$X1[k]){
            # updates the group number.
            df$new_dist[j] <- new_ngrp$X2[k]
          }
        }
      }
      # Return organized data frame
      colnames(new_ngrp) = c("old", "new")
    }
    df$new_dist <- ifelse(is.na(df$new_dist), df$DISTRICT, df$new_dist)
    return(list(df, new_ngrp))
  }
  
  
  # Add all variables to the area_data Data
  area_data <- area_data %>%
    # filter(IMP_VAL != 0) %>% # getting rid of vacant lots
    # filter(STATUS == 'A' | STATUS == "K") %>% # keeping only valid accounts
    # Create Variables for modeling
    mutate( SYEAR = as.numeric(format(as.Date(SOLD_DATE, format="%d-%h-%y"), "%Y")),
            SMONTH = as.numeric(format(as.Date(SOLD_DATE, format="%d-%h-%y"), "%m")),
            TIME_ADJ = (1.005)^((12*(pyear - SYEAR))+(13-SMONTH)),
            LAND_SIZE_RW = LOT_VALUE,
            LAND_VAL_RW = LOT_VALUE,
            AGE = max(SYEAR, na.rm = T) - YEAR_BUILT,
            FAIR = QUALITY_DESCR == "Fair" | QUALITY_DESCR == "Fair Plus" | 
              QUALITY_DESCR == "Poor" | QUALITY_DESCR == "Low",
            AVERAGE = QUALITY_DESCR == 'Average' | QUALITY_DESCR == 'Average Plus' 
            | QUALITY_DESCR == "Average P",
            GOOD = QUALITY_DESCR == "Good" | QUALITY_DESCR == 'Good Plus',
            VERYGOOD = QUALITY_DESCR == "Very Good" | QUALITY_DESCR == "Very Good Plus",
            EXCELLENT = QUALITY_DESCR == "Excellent",
            TWOSTORY = STYLE_DESCR == 'Two Story' | STYLE_DESCR == "Two and One Half",
            ONESTORY = STYLE_DESCR == 'One Story' | STYLE_DESCR == "One and One Half",
            MULTISTORY = STYLE_DESCR == 'Bi-Level' | 
              STYLE_DESCR == 'Bi Level 2 Story' | 
              STYLE_DESCR == 'Split Level',
            OTHERSTORY = (STYLE_DESCR != "Two Story ") & 
              (STYLE_DESCR != 'Two and One Half') &  
              (STYLE_DESCR != 'One Story') &  
              (STYLE_DESCR != 'One and One Half') &  
              (STYLE_DESCR != 'Bi-Level'),
            UPDATED = REMODEL_YEAR >= 2004 & REMODEL_PCT >= .75,
            LT2000 = GLA < 2000,
            SPLINE1 = ifelse(LT2000 == F, 2000, GLA * LT2000),
            SPLINE2 = ifelse(LT2000 == F, GLA - 2000, 0),
            NET_SOLD_PRICE = SOLD_PRICE - CONCESSIONS,
            TIME_ADJ_NET_SOLD_PRICE = NET_SOLD_PRICE * TIME_ADJ,
            NET_SOLD_PRICE_IMP = NET_SOLD_PRICE - LOT_VALUE,
            TA_NET_SOLD_PRICE_IMP = TIME_ADJ_NET_SOLD_PRICE - LAND_VAL_RW,
            VALID = SALE_VALID == 'Y' | SALE_VALID == '',
            SPPSF = TIME_ADJ_NET_SOLD_PRICE / GLA,
            FIN_AREA = GLA + FIN_BSMT,
            DET_RCNLD = DS_IRP_RCNLD + DS_IRS_RCNLD + DS_IAS_RCNLD + DS_ICS_RCNLD,
            TOT_RCNLD = RES_RCNLD + DET_RCNLD,
            BATH_3_4 = THREE_QTR_BATHS_3 + FULL_BATHS_3 + FULL_BATHS_4,
            NO_BSMT = ifelse((UNFIN_BSMT == 0 & FIN_BSMT == 0), T, F),
            REMOD_20 = ifelse(REMODEL_YEAR < 2002, 0, 1),
            SMONTH_old = SMONTH,
            SYEAR_old =SYEAR,
            YrMon = SYEAR + (SMONTH -.5)/12)  
  
  area_data$SYEAR = pyear
  area_data$SMONTH = 12
  area_data$REMODEL_PCT = ifelse(area_data$REMODEL_YEAR < 2002, 0, area_data$REMODEL_PCT)
  
  
  # Apply the function to the data
  combined <- CombineData(area_data, pyear)
  area_data <- combined[[1]]
  new_district <- combined[[2]]
  
  maxyear <- max(as.numeric(area_data$SYEAR), na.rm = T)
  
  # Filter data to create the model
  LinearData <- area_data %>%
    dplyr::filter(SOLD_PRICE != 0, NET_SOLD_PRICE_IMP > 0) # only using actual sales to model
  
  # create a vector for the years of sales data available.
  yearsofdata <- unique(LinearData$SYEAR)
  
  # Split the data on the neighborhood group to make individual models
  splitdfLinear <- group_split(LinearData, new_dist)
  
  # Create empty list for the models
  modelsLin <- list()
  
  # Empty lists to collect values for ratio analysis
  PREDVALUES_Lin <- c()
  SOLDPRICE_Lin <- c()
  
  # Create a model for each DISTRICT
  for(i in 1:length(unique(LinearData$new_dist))){
    # Create model and save to list
    modelsLin[[i]] <- glm(sqrt(NET_SOLD_PRICE_IMP) ~ SPLINE1 + SPLINE2 +
                            #(FIN_AREA) + 
                            LOT_ACRES + #CONDITION + #STYLE_DESCR + 
                            OTHERSTORY + MULTISTORY + ONESTORY  +
                            EXCELLENT + VERYGOOD + KITCHENS + GLA_BEDROOMS + 
                            GOOD + FAIR + AVERAGE  +
                            (AGE) + (HALF_BATHS_2) + 
                            (BATH_3_4) + (DELUXE_BATHS_5) + (LUXURY_BATHS_6) + (ATT_GAR_AREA) + 
                            (DET_GAR_AREA) + 
                            (REMODEL_PCT) + (FIN_BSMT) + (UNFIN_BSMT) +  NO_BSMT + YrMon + FLA:SPLINE1 + FLA:SPLINE2 + FLA, 
                          family = "Gamma",
                         data = splitdfLinear[[i]])
    # Use backward selection based on AIC to find best predictive model
    modelsLin[[i]] <- step(modelsLin[[i]], method = "backward", trace = F)
    
    # Collect Values for ratio analysis
    
    
    
    #PREDVALUES_Lin <- append(PREDVALUES_Lin, modelsLin[[i]]$fitted.values + splitdfLinear[[i]]$LAND_VAL_RW)
    #SOLDPRICE_Lin <- append(SOLDPRICE_Lin, splitdfLinear[[i]]$NET_SOLD_PRICE)
    
    
  }
  
  ## Get predictions
  
  # Get a list of the unique neighborhood groups
  DISTRICTS <- unique(area_data$new_dist)
  
  LinearPredsData <- area_data %>%
    dplyr::mutate(SMONTH_old = SMONTH,
           SYEAR_old = SYEAR,
           YrMon = SYEAR + (SMONTH - .5)/12)
  # Split the data by neighborhood group to run prediction with the appropriate model
  splitedPredictions <- group_split(LinearPredsData, new_dist)
  # Create an empty list to populate with the predictions
  linearpredictions <- list()
  
  # Populate the prediction list
  for(i in 1:length(DISTRICTS)){
    linearpredictions[[i]] <- predict(modelsLin[[i]],splitedPredictions[[i]])  
  }
  
  # Create a list to collect the predictions connected with the associated variables
  finalarea_dataLinear <- list()
  
  # Add predictions onto the existing data tables 
  for(i in 1:length(DISTRICTS)){
    finalarea_dataLinear[[i]] <- cbind(splitedPredictions[[i]], linearpredictions[[i]])
  }
  
  
  ##########################
  ## Multiplicative Model ##
  ##########################
  
  # Create the denominator to calculate the age ratio
  den_ageratio <- mean(maxyear - area_data$YEAR_BUILT)
  
  min_year <- min(as.numeric(area_data$SYEAR))
  
  
  # Create the variables for the multiplicative model
  Multmodel1 <- area_data %>%
    # filter(IMP_VAL != 0) %>%
    # filter(STATUS == 'A' | STATUS == "K") %>%
    dplyr::filter(SOLD_PRICE != 0, NET_SOLD_PRICE_IMP > 0) %>%
    dplyr::mutate(SYEAR = as.numeric(format(as.Date(SOLD_DATE, format="%d-%h-%y"), "%Y")),
           SMONTH = as.numeric(format(as.Date(SOLD_DATE, format="%d-%h-%y"), "%m")),
           MONTHS = ((SYEAR - (pyear - 3)) * 12) + SMONTH,
           MONTHSAWAY = 36 + 1 - MONTHS,
           LN_MONTHSAWAY = log(max(MONTHS, na.rm = T) + 1 - MONTHS),
           LN_AGERATIO = log(1+(maxyear-YEAR_BUILT)/den_ageratio),
           LN_DETGARRATIO = log(1 + (DET_GAR_AREA / (mean(DET_GAR_AREA)))),
           # LN_IMPVALRATIO = log(1+(IMP_VAL/mean(IMP_VAL))),
           LN_ATGARRATIO = log(1+ (ATT_GAR_AREA / ( mean(ATT_GAR_AREA)))),
           LN_RCNLDRATIO = log(1+ TOT_RCNLD / ( mean(TOT_RCNLD))),
           LN_DETRCNLDRATIO = log(1 + ((DET_RCNLD)/mean(DET_RCNLD))),
           LN_BLTASSFRATIO = log(1+ (GLA / ( mean(GLA)))),
           # LN_TOTVALRATIO = log(1+ TOT_VAL / ( mean(TOT_VAL))),
           LN_BSMT_FIN_RATIO = log(1+ (FIN_BSMT / ( mean(FIN_BSMT)))),
           LN_TOTAREARATIO = log(1+ (GLA + FIN_BSMT + UNFIN_BSMT) / ( mean(GLA + 
                                                                             FIN_BSMT + UNFIN_BSMT))),
           LN_FINAREA = log(1 + ((GLA + FIN_BSMT)/mean(GLA + FIN_BSMT))),
           LN_LOT_VALUE <- log(LAND_VAL_RW),
           NET_SOLD_PRICE = SOLD_PRICE - CONCESSIONS,
           # TA_NET_SOLD_PRICE_IMP = T - LOT_VALUE,
           LN_NET_SOLD_PRICE = log(TIME_ADJ_NET_SOLD_PRICE),
           LN_NET_SOLD_PRICE_IMP = log(NET_SOLD_PRICE_IMP),
           LN_SPLINE_1 = log(1 + SPLINE1),
           LN_SPLINE_2 = log(1 + SPLINE2)) %>%
    filter(NET_SOLD_PRICE_IMP > 0)
  
  
  
  
  
  # Split the data on neighborhood group/ value area
  splitdfMult <- group_split(Multmodel1, new_dist)
  # create empty lists
  modelsMult <- list()
  PREDVALUES_Mult <- c()
  SOLDPRICE_Mult <- c()
  
  
  
  # Create the models
  for(i in 1:length(unique(Multmodel1$new_dist))){
    modelsMult[[i]] <- glm( (LN_NET_SOLD_PRICE_IMP) ~ (LN_SPLINE_1 + LN_SPLINE_2 + 
                            OTHERSTORY + MULTISTORY + ONESTORY + TWOSTORY + CONDITION + log(LOT_ACRES + 1) +
                             EXCELLENT + VERYGOOD + GOOD + FAIR +
                             LN_TOTAREARATIO + LN_BSMT_FIN_RATIO + 
                             LN_BLTASSFRATIO + LN_DETRCNLDRATIO + LN_RCNLDRATIO + 
                             LN_ATGARRATIO + LN_DETGARRATIO + 
                             LN_AGERATIO + (YrMon))*FIN_AREA,
                          family = "Gamma",
                           data = splitdfMult[[i]] )
    
    
    
    modelsMult[[i]] <- step(modelsMult[[i]], method = "backward", trace = F)
    
    #PREDVALUES_Mult <- append(PREDVALUES_Mult, exp(modelsMult[[i]]$fitted.values) + splitdfMult[[i]]$LAND_VAL_RW)
    #SOLDPRICE_Mult <- append(SOLDPRICE_Mult, exp(splitdfMult[[i]]$LN_NET_SOLD_PRICE))
    
  }
  
  
  
  
  # Collect Predictions
  
  # Create the denominator for the detgar ratio
  den_detgar <- mean(area_data$DET_GAR_AREA)
  
  # Create the data to collect the multiplicative predictions
  Multmodeldata <-area_data %>%
    #  filter(IMP_VAL != 0) %>%
    # filter(STATUS == 'A' | STATUS == "K") %>%
    #filter(SOLD_PRICE != 0) %>%
    dplyr::mutate(NET_SOLD_PRICE = SOLD_PRICE - CONCESSIONS,
           NET_SOLD_PRICE_IMP = NET_SOLD_PRICE - LAND_VAL_RW,
           #SYEAR = as.numeric(SYEAR),
           #SMONTH = as.numeric(format(as.Date(SOLD_DATE, format="%d-%h-%y"), "%m")),
           MONTHS = ((SYEAR - min_year) * 12) + SMONTH,
           MONTHSAWAY = max(MONTHS, na.rm = T) + 1 - MONTHS,
           LN_MONTHSAWAY = log(max(MONTHS, na.rm = T) + 1 - MONTHS),
           LN_AGERATIO = log(1+(maxyear-YEAR_BUILT)/den_ageratio),
           LN_DETGARRATIO = log(1 + (DET_GAR_AREA / (mean(DET_GAR_AREA)))),
           # LN_IMPVALRATIO = log(1+(IMP_VAL/mean(IMP_VAL))),
           LN_ATGARRATIO = log(1+ (ATT_GAR_AREA / ( mean(ATT_GAR_AREA)))),
           LN_RCNLDRATIO = log(1+ TOT_RCNLD / ( mean(TOT_RCNLD))),
           LN_DETRCNLDRATIO = log(1 + ((DET_RCNLD)/mean(DET_RCNLD))),
           LN_BLTASSFRATIO = log(1+ (GLA / ( mean(GLA)))),
           # LN_TOTVALRATIO = log(1+ TOT_VAL / ( mean(TOT_VAL))),
           LN_BSMT_FIN_RATIO = log(1+ (FIN_BSMT / ( mean(FIN_BSMT)))),
           LN_TOTAREARATIO = log(1+ (GLA + FIN_BSMT + UNFIN_BSMT) / ( mean(GLA + 
                                                                             FIN_BSMT + UNFIN_BSMT))),
           LN_FINAREA = log(1 + ((GLA + FIN_BSMT)/mean(GLA + FIN_BSMT))),
           SMONTH_old = SMONTH,
           SYEAR_old = SYEAR,
           YrMon = SYEAR + (SMONTH - .5)/12,
           LN_SPLINE_1 = log(1 + SPLINE1),
           LN_SPLINE_2 = log(1 + SPLINE2))
  
  #LN_NETSOLDPRICEIMP = log(NET_SOLD_PRICE_IMP)) %>%
  # mutate(
  #NET_SOLD_PRICE = SOLD_PRICE - CONCESSIONS,
  # NET_SOLD_PRICE_IMP = NET_SOLD_PRICE - LOT_VALUE,
  #LN_NET_SOLD_PRICE_IMP = log(TA_NET_SOLD_PRICE_IMP))
  
  # Split the data on neighborhood groups to collect the predictions
  splitedPredictions <- group_split(Multmodeldata, new_dist)
  # Create empty list to collect the predictions
  multpredictions <- list()
  
  # Collect the multiplicative predictions
  for(i in 1:length(DISTRICTS)){
    multpredictions[[i]] <- predict(modelsMult[[i]],splitedPredictions[[i]])  
  }
  
  
  # Create a list to put the final data frames with multiplicative predictions in
  finalarea_dataMult <- list()
  # Add predictions to data frames
  for(i in 1:length(DISTRICTS)){
    finalarea_dataMult[[i]] <- cbind(splitedPredictions[[i]],multpredictions[[i]])
  }
  
  
  
  # Organize data into one final data frame (undo splits)
  final_lin <- bind_rows(finalarea_dataLinear)
  final_mult <- bind_rows(finalarea_dataMult)
  final <- area_data
  final$lin_pred <- final_lin$`linearpredictions[[i]]`
  final$mult_pred <- final_mult$`multpredictions[[i]]`
  
  
  
  weight_lin = 1
  weight_mult = 0
  
  final$TOT_VAL2022 <- final$X2022_RE_RES + final$X2022_RE_AGR + final$X2022_RE_COM + final$X2022_IM_RES + final$X2022_IM_AGR + final$X2022_IM_COM
  final$IMP_VAL2021 <-  final$X2022_IM_RES + final$X2022_IM_AGR + final$X2022_IM_COM
  
  # Calculate the final predictions using the weights found above
  final <- final %>%
    dplyr::mutate(TV_IMP = weight_lin * (lin_pred)^2 + weight_mult * exp(mult_pred),
           TV_IMP_PCT = (TV_IMP - IMP_VAL2021) / IMP_VAL2021,
           TV_IMP_cap = ifelse(TV_IMP_PCT > .4 & YEAR_BUILT <= pyear - 2 & UPDATED == FALSE | TV_IMP_PCT > 10, yes = 1.4 * IMP_VAL2021, ifelse(TV_IMP_PCT < 0, yes = IMP_VAL2021, no = TV_IMP)),
           TVPred = TV_IMP_cap + LAND_VAL_RW,
           TVPctChange = (TVPred - TOT_VAL2022) / TOT_VAL2022)
  
  return(final)
}