library(tidyverse)
library(lubridate)
library(dplyr)
library(MASS)



lcoefs <- function(area_data, pyear) {
  # Create a function to organize new n groups
  CombineData <- function(df,pyear){
    df$DISTRICT <- ifelse(is.na(df$DISTRICT), 0, df$DISTRICT)
    # get number of sales per neighborhood group/value area.
    df$new_dist <- NA
    summary <- df %>%
      group_by(DISTRICT) %>%
      dplyr::summarize(AVG_LAND =median(LOT_VALUE/LOT_ACRES, na.rm = T) , Count = sum(!is.na(SOLD_PRICE)), sales = sum(SYEAR_old == pyear, na.rm = T)) %>%
      arrange(Count) 
    
    # get the large neighborhood groups (more than 80 sales)
    largegroup <- summary %>%
      dplyr::filter(Count >= 50)
    # get the small neighborhood groups (less than 80 sales)
    smallgroup<- summary %>%
      dplyr::filter(Count < 50)
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
          difference = append(difference, abs(smallgroup[i, ]$AVG_LAND - largegroup[k, ]$AVG_LAND))
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
            YrMon = SYEAR + (SMONTH -.5)/12,
            log_impval = log(NET_SOLD_PRICE_IMP + 1)) 
  # Set year and month to the value for predictions
  area_data$SYEAR = pyear
  area_data$SMONTH = 12
  # Calculate remodel year
  area_data$REMODEL_PCT = ifelse(area_data$REMODEL_YEAR < 2002, 0, area_data$REMODEL_PCT)
  # Calculate previous years improvement value
  area_data$IMP_VAL2021 <-  area_data$X2022_IM_RES + area_data$X2022_IM_AGR + area_data$X2022_IM_COM
  
  
  # Combine all the data and collect new district list
  combined <- CombineData(area_data, pyear)
  area_data <- combined[[1]]
  new_district <- combined[[2]]
  #saveRDS(new_district, "M:/shiny_trial/new_district.rds")
  
  yearsofdata <- unique(area_data$SYEAR)
  
  # Split the data on the neighborhood group to make individual models
  splitdf <- group_split(area_data, new_dist)
  
  # Create empty list for the models
  lcoefs <- list()
  lamdas <- list()
  predsdf <- list()
  
  #saveRDS(sort(unique(area_data$DISTRICT)), "M:/shiny_trial/dist_list.rds")
  
  # Create a model for each DISTRICT
  for(i in 1:length(unique(area_data$new_dist))){
    
    model_mat <- model.matrix(NET_SOLD_PRICE_IMP ~ SPLINE1 + SPLINE2 +
                                #(FIN_AREA) + 
                                LOT_ACRES + #CONDITION + #STYLE_DESCR + 
                                OTHERSTORY + MULTISTORY + ONESTORY  +
                                EXCELLENT + VERYGOOD + KITCHENS + GLA_BEDROOMS + 
                                GOOD + FAIR + AVERAGE  +
                                (AGE) + (HALF_BATHS_2) + 
                                (BATH_3_4) + (DELUXE_BATHS_5) + (LUXURY_BATHS_6) + (ATT_GAR_AREA) + 
                                (DET_GAR_AREA) + 
                                (REMODEL_PCT) + (FIN_BSMT) + (UNFIN_BSMT) +  NO_BSMT + YrMon + FLA:SPLINE1 + FLA:SPLINE2 + FLA
                              , data = splitdf[[i]][!is.na(splitdf[[i]]$SOLD_PRICE) & splitdf[[i]]$NET_SOLD_PRICE_IMP > 0 & splitdf[[i]]$VALID == TRUE, ])[,-c(1)]
    
    
    splitpred <- splitdf[[i]] %>% 
      dplyr::mutate(SMONTH_old = SMONTH,
                    SYEAR_old = SYEAR,
                    YrMon = SYEAR + (SMONTH - .5)/12)
    
    preds_mat <- model.matrix( ~ SPLINE1 + SPLINE2 +  
                                 #(FIN_AREA) + 
                                 LOT_ACRES + #CONDITION + #STYLE_DESCR + 
                                 OTHERSTORY + MULTISTORY + ONESTORY  +
                                 EXCELLENT + VERYGOOD + KITCHENS + GLA_BEDROOMS + 
                                 GOOD + FAIR + AVERAGE  +
                                 (AGE) + (HALF_BATHS_2) + 
                                 (BATH_3_4) + (DELUXE_BATHS_5) + (LUXURY_BATHS_6) + (ATT_GAR_AREA) + 
                                 (DET_GAR_AREA) + 
                                 (REMODEL_PCT) + (FIN_BSMT) + (UNFIN_BSMT) +  NO_BSMT + YrMon + FLA:SPLINE1 + FLA:SPLINE2 + FLA, data = splitpred)[,-c(1)]
    
    
    lambda = exp(seq(0, 20, length = 1000))
    lasso_lm = glmnet::glmnet(model_mat, sqrt(splitdf[[i]][!is.na(splitdf[[i]]$SOLD_PRICE) & splitdf[[i]]$NET_SOLD_PRICE_IMP > 0 & splitdf[[i]]$VALID == TRUE, ]$NET_SOLD_PRICE_IMP), alpha=1, lambda = lambda)
    models = lasso_lm
    #plot(lasso_lm)
    lasso_cv = glmnet::cv.glmnet(model_mat, sqrt(splitdf[[i]][!is.na(splitdf[[i]]$SOLD_PRICE) & splitdf[[i]]$NET_SOLD_PRICE_IMP > 0 & splitdf[[i]]$VALID == TRUE, ]$NET_SOLD_PRICE_IMP), alpha=1, lambda = lambda)
    lbestlam = lasso_cv$lambda.min
    lamdas[[i]] = lbestlam
    #plot(lasso_cv)
    lcoefs[[i]] = predict(lasso_lm, s=lbestlam, type="coefficient")
    preds = predict(lasso_lm, newx = preds_mat, s=lbestlam)
    
    predsdf[[i]] <- cbind(splitdf[[i]], preds)
  }
  
  #saveRDS(models, "M:/shiny_trial/models.rds")
  #saveRDS(lamdas, "M:/shiny_trial/lamdas.rds")
  
  final <- do.call(rbind.data.frame, predsdf)
  
  final$TOT_VAL2022 <- final$X2022_RE_RES + final$X2022_RE_AGR + final$X2022_RE_COM + final$X2022_IM_RES + final$X2022_IM_AGR + final$X2022_IM_COM
  final$IMP_VAL2021 <-  final$X2022_IM_RES + final$X2022_IM_AGR + final$X2022_IM_COM
  
  final <- final %>%
    mutate(TV_IMP = ((s1)^2)*.92,
           TV_IMP_PCT = (TV_IMP - IMP_VAL2021) / IMP_VAL2021,
           TV_IMP_cap = ifelse(TV_IMP_PCT > .6 & YEAR_BUILT <= pyear - 2 & UPDATED == FALSE, yes = IMP_VAL2021*1.6, ifelse(TV_IMP_PCT < 0, yes = IMP_VAL2021, no = TV_IMP)),
           TVPred = TV_IMP_cap + LOT_VALUE,
           SOLD_pct = ((NET_SOLD_PRICE) - TOT_VAL2022) / TOT_VAL2022,
           TVPctChange = (TVPred - TOT_VAL2022) / TOT_VAL2022)
  
  
  
  return(lcoefs)
  
}


