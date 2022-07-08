library(glmnet)
library(tidyverse)
library(lubridate)
library(plyr)
library(MASS)




combined_ridge <- function(area_data, pyear){
# Create a function to organize new n groups
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
    dplyr::filter(Count >= 40, sales != 0)
  # get the small neighborhood groups (less than 80 sales)
  smallgroup<- summary %>%
    dplyr::filter(Count < 40 | sales == 0)
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

# Filter data to create the model
LinearData <- area_data %>%
  filter(SOLD_PRICE != 0) # only using actual sales to model

# create a vector for the years of sales data available.
yearsofdata <- unique(LinearData$SYEAR)

# Split the data on the neighborhood group to make individual models
splitdfLinear <- group_split(area_data, new_dist)

DISTRICTS <- unique(area_data$new_dist)

# Create empty list for the models
lin_models <- list()
lin_lamdas <- list()
lin_predsdf <- list()

#saveRDS(sort(unique(area_data$DISTRICT)), "M:/shiny_trial/dist_list.rds")

# Create a model for each DISTRICT
for(i in 1:length(unique(area_data$new_dist))){
  model_mat <- model.matrix(NET_SOLD_PRICE_IMP ~ SPLINE1 + SPLINE2 +  
                              OTHERSTORY + MULTISTORY + ONESTORY + EXCELLENT + VERYGOOD + 
                              GOOD + FAIR + AVERAGE + AGE + HALF_BATHS_2 + 
                              BATH_3_4 + DELUXE_BATHS_5 + LUXURY_BATHS_6 + ATT_GAR_AREA + 
                              DET_GAR_AREA + 
                              REMODEL_PCT + FIN_BSMT + UNFIN_BSMT + NO_BSMT + YrMon, data = splitdfLinear[[i]][!is.na(splitdfLinear[[i]]$SOLD_PRICE), ])[,-c(1)]
  
  splitdfLinear[[i]] <- splitdfLinear[[i]] %>% 
    dplyr::mutate(SMONTH_old = SMONTH,
           SYEAR_old = SYEAR,
           YrMon = SYEAR + (SMONTH - .5)/12)
  
  preds_mat <- model.matrix(SERNO ~ SPLINE1 + SPLINE2 +  
                              OTHERSTORY + MULTISTORY + ONESTORY + EXCELLENT + VERYGOOD + 
                              GOOD + FAIR + AVERAGE + AGE + HALF_BATHS_2 + 
                              BATH_3_4 + DELUXE_BATHS_5 + LUXURY_BATHS_6 + ATT_GAR_AREA + 
                              DET_GAR_AREA + 
                              REMODEL_PCT + FIN_BSMT + UNFIN_BSMT + NO_BSMT + YrMon, data = splitdfLinear[[i]])[,-c(1)]
  
  
  
  lambda = exp(seq(-15, 15, length = 1000))
  lasso_lm = glmnet(model_mat, .95*splitdfLinear[[i]][!is.na(splitdfLinear[[i]]$SOLD_PRICE), ]$NET_SOLD_PRICE_IMP, alpha=1, lambda = lambda)
  lin_models[[i]] = lasso_lm
  #plot(lasso_lm)
  lasso_cv = cv.glmnet(model_mat, .95*splitdfLinear[[i]][!is.na(splitdfLinear[[i]]$SOLD_PRICE), ]$NET_SOLD_PRICE_IMP, alpha=1, lambda = lambda)
  lbestlam = lasso_cv$lambda.min
  lin_lamdas[[i]] = lbestlam
  #plot(lasso_cv)
  lcoefs = predict(lasso_lm, s=lbestlam, type="coefficient")
  preds = predict(lasso_lm, newx = preds_mat, s=lbestlam)
  
  lin_predsdf[[i]] <- cbind(splitdfLinear[[i]], preds)
}

# Create a list to collect the predictions connected with the associated variables
finalarea_dataLinear <- bind_rows(lin_predsdf)


##########################
## Multiplicative Model ##
##########################

# Create the denominator to calculate the age ratio
den_ageratio <- mean(pyear - area_data$YEAR_BUILT)

min_year <- min(as.numeric(area_data$SYEAR))


# Create the variables for the multiplicative model

# Create the denominator for the detgar ratio
den_detgar <- mean(area_data$DET_GAR_AREA)

# Create the data to collect the multiplicative predictions
Multmodeldata <-area_data %>%
  #  filter(IMP_VAL != 0) %>%
  # filter(STATUS == 'A' | STATUS == "K") %>%
  #filter(SOLD_PRICE != 0) %>%
  dplyr::mutate(NET_SOLD_PRICE = SOLD_PRICE - CONCESSIONS,
                NET_SOLD_PRICE_IMP = NET_SOLD_PRICE - LAND_VAL_RW,
                LN_NET_SOLD_PRICE_IMP = log(NET_SOLD_PRICE_IMP),
                #SYEAR = as.numeric(SYEAR),
                #SMONTH = as.numeric(format(as.Date(SOLD_DATE, format="%d-%h-%y"), "%m")),
                MONTHS = ((SYEAR - min_year) * 12) + SMONTH,
                MONTHSAWAY = max(MONTHS, na.rm = T) + 1 - MONTHS,
                LN_MONTHSAWAY = log(max(MONTHS, na.rm = T) + 1 - MONTHS),
                LN_AGERATIO = log(1+(pyear-YEAR_BUILT)/den_ageratio),
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


Multmodeldata$LN_NET_SOLD_PRICE_IMP <- ifelse(is.na(Multmodeldata$LN_NET_SOLD_PRICE_IMP), 0, as.numeric(Multmodeldata$LN_NET_SOLD_PRICE_IMP))

splitdfMult <- group_split(Multmodeldata, new_dist)

mult_models <- list()
mult_lamdas <- list()
mult_predsdf <- list()

#saveRDS(sort(unique(area_data$DISTRICT)), "M:/shiny_trial/dist_list.rds")

# Create a model for each DISTRICT
for(i in 1:length(unique(area_data$new_dist))){
  model_mat <- model.matrix(LN_NET_SOLD_PRICE_IMP ~ VALID + LN_SPLINE_1 + LN_SPLINE_2 +
                              OTHERSTORY + MULTISTORY + ONESTORY + TWOSTORY + 
                              EXCELLENT + VERYGOOD + GOOD + FAIR +
                              LN_TOTAREARATIO + LN_BSMT_FIN_RATIO + 
                              LN_BLTASSFRATIO + LN_DETRCNLDRATIO + LN_RCNLDRATIO + 
                              LN_ATGARRATIO + LN_DETGARRATIO + 
                              LN_AGERATIO + YrMon, data = splitdfMult[[i]][!is.na(splitdfMult[[i]]$SOLD_PRICE), ])[,-c(1)]
  
  splitdfMult[[i]] <- splitdfMult[[i]] %>% 
    mutate(SMONTH_old = SMONTH,
           SYEAR_old = SYEAR,
           YrMon = SYEAR + (SMONTH - .5)/12)
  
  preds_mat <- model.matrix(SERNO ~ VALID + LN_SPLINE_1 + LN_SPLINE_2 + 
                              OTHERSTORY + MULTISTORY + ONESTORY + TWOSTORY + 
                              EXCELLENT + VERYGOOD + GOOD + FAIR +
                              LN_TOTAREARATIO + LN_BSMT_FIN_RATIO + 
                              LN_BLTASSFRATIO + LN_DETRCNLDRATIO + LN_RCNLDRATIO + 
                              LN_ATGARRATIO + LN_DETGARRATIO + 
                              LN_AGERATIO + YrMon, data = splitdfMult[[i]])[,-c(1)]
  
  
  
  lambda = exp(seq(-15, 15, length = 1000))
  lasso_lm = glmnet(model_mat, splitdfMult[[i]][!is.na(splitdfMult[[i]]$SOLD_PRICE), ]$LN_NET_SOLD_PRICE_IMP, alpha=1, lambda = lambda)
  mult_models[[i]] = lasso_lm
  #plot(lasso_lm)
  lasso_cv = cv.glmnet(model_mat, splitdfMult[[i]][!is.na(splitdfMult[[i]]$SOLD_PRICE), ]$LN_NET_SOLD_PRICE_IMP, alpha=1, lambda = lambda)
  lbestlam = lasso_cv$lambda.min
  mult_lamdas[[i]] = lbestlam
  #plot(lasso_cv)
  lcoefs = predict(lasso_lm, s=lbestlam, type="coefficient")
  preds = predict(lasso_lm, newx = preds_mat, s=lbestlam)
  
  mult_predsdf[[i]] <- cbind(splitdfMult[[i]], preds)
}




# Create a list to put the final data frames with multiplicative predictions in
finalarea_dataMult <- do.call(rbind.data.frame, mult_predsdf)





# Organize data into one final data frame (undo splits)
finalarea_dataLinear$multPred <- finalarea_dataMult$s1
final <- finalarea_dataLinear


weight_lin = 0
weight_mult = 1

final$TOT_VAL2022 <- final$X2022_RE_RES + final$X2022_RE_AGR + final$X2022_RE_COM + final$X2022_IM_RES + final$X2022_IM_AGR + final$X2022_IM_COM
final$IMP_VAL2021 <-  final$X2022_IM_RES + final$X2022_IM_AGR + final$X2022_IM_COM

# Calculate the final predictions using the weights found above
final <- final %>%
  mutate(TV_IMP = weight_lin * s1 + weight_mult * exp(multPred),
         TV_IMP_PCT = (TV_IMP - IMP_VAL2021) / IMP_VAL2021,
         TV_IMP_cap = ifelse(TV_IMP_PCT > .4 & YEAR_BUILT <= pyear - 2 & UPDATED == FALSE | TV_IMP_PCT > 10, yes = 1.4 * IMP_VAL2021, ifelse(TV_IMP_PCT < 0, yes = IMP_VAL2021, no = TV_IMP)),
         TVPred = TV_IMP_cap + LAND_VAL_RW,
         TVPctChange = (TVPred - TOT_VAL2022) / TOT_VAL2022)

return(final)
}
