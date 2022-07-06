# Source the code for the model
source("data/model.R")
source("data/combine.R")
source("data/lcoefs.R")
# Read in data for forecast graph and edit data
forecast <- (read.csv("data/SalesPerMonth_export.csv"))
forecast$date <- as.numeric(format(as.Date(paste("01-",forecast$Month, sep = ""), format = "%d-%b-%y"), "%Y")) + (as.numeric(format(as.Date(paste("01-",forecast$Month, sep = ""), format = "%d-%b-%y"), "%m")) - .5)/12
# Read in required libraries
library(tidyverse)
library(dplyr)
library(shiny)
library(ggplot2)
library(MASS)





function(input, output, session){
  ## Code File input
  final <- eventReactive(input$run, {
    req(input$new_file)
    new_df <- read.csv(input$new_file$datapath)
    model(new_df,input$pyear)
  })
  
  observeEvent(input$run, {
    updateSelectInput(session, "dist", choices = final()[[1]]$new_dist)
  }
  )
  
 # new_dist <- eventReactive(input$run, {
   # req(input$new_file)
   # new_df <- read.csv(input$new_file$datapath)
  #  combine(new_df, input$pyear)
  #})
  
  #coefs <- eventReactive(input$run, {
  #  req(input$new_file)
  #  new_df <- read.csv(input$new_file$datapath)
 #   lcoefs(new_df, input$pyear)
 # })
  
  output$new_dist <- renderTable({
    #new_dist()
    final()[[2]]
  })
 
  output$coefs <- renderTable({
    final_df <- final()[[1]]
    coefs <- final()[[3]]
    districts <- unique(final_df$new_dist)
    num <- match(input$dist, districts)
    cbind(names((coefs[[num]][,1])), as.matrix(coefs[[num]]))
  })
  ## Code Forecast graph
  output$forecast <- renderPlot({
    ## Data from the current year
    #cur_data <- final_df() %>%
     # dplyr::filter(SYEAR_old == 2022) %>%
      #dplyr::mutate(date = 2022 + (SMONTH_old - .5)/12) %>%
      #dplyr::group_by(SMONTH_old) %>%
      #dplyr::summarize(SOLD_PRICE = median(SOLD_PRICE, na.rm = T), date = mean(date, na.rm = T))
    cur_data <- forecast %>% dplyr::filter(date >= 2022)
    
    forecast <- forecast %>% dplyr::filter(date < 2022)
    ## Build linear model for trend from 2012 - 2021 (Pre Jump)
    linear.lm <- lm(SOLD_PRICE ~ date, data = forecast[forecast$date > 2012 & forecast$date < 2021,])
    pred_value <- c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)
    preds <- predict(linear.lm, newdata = data.frame(date = pred_value))
    linear_data <- data.frame("date" = pred_value, "SOLD_PRICE" = preds)
    ## Set names for the legend
    colors <- c("Linear Trend (Pre Covid)" = "red", "Future Linear Forecast" = "blue", "Current Trend" = "green")
    ## Code forecast graph 
    ggplot(data = forecast, mapping = aes(date, SOLD_PRICE))+
      geom_point()+
      xlim(2012, 2023)+
      geom_line(mapping = aes(color = "Linear Trend (Pre Covid)"), data = linear_data)+
      forecast::geom_forecast(aes(color = "Future Linear Forecast"), h = 12)+
      geom_smooth(aes(color = "Current Trend"), data = cur_data)+ 
      labs(x = "Year",
           y = "Median Sold Price Per Month",
           color = "Legend") +
      scale_color_manual(values = colors)
    
    })
  
  ## Code Download button
  output$download <- downloadHandler(
    filename = function(){
     paste("preds.csv", sep = "")
    },
    content = function(file){
    write.csv(final()[[1]] %>% dplyr::select(SERNO, TV_IMP_cap, LOT_VALUE, TVPred), file, row.names = FALSE)
    }
  )
  
  ## Code Overall ratio statistics
  output$ratio_stats <- renderTable({
    ## Pull data to calculate ratio statistics
    final_df <- final()[[1]]
    ratio_data <- c()
    ratio_data <- final_df %>%
      dplyr::filter(TIME_ADJ_NET_SOLD_PRICE != 0) %>%
      dplyr::filter(SYEAR_old == input$pyear) %>%
      dplyr::filter(YEAR_BUILT < input$pyear - 1) %>%
      dplyr::filter(SALE_VALID == 'Y') %>%
      dplyr::mutate(ratio = TVPred/ TIME_ADJ_NET_SOLD_PRICE)
    ## Calculate ratio statistics
    ratio <- ratio_data$ratio
    weightedmean <- sum(ratio_data$TVPred, na.rm = T) / sum(ratio_data$TIME_ADJ_NET_SOLD_PRICE, na.rm = T)
    COD <- sum(abs(ratio - median(ratio, na.rm = T))/length(ratio)/median(ratio, na.rm = T))
    COV <- sd(ratio, na.rm = T)/mean(ratio, na.rm = T)
    PRD <- mean(ratio, na.rm = T)/ weightedmean
    
    stats <- data.frame(COD, COV, PRD, "Weighted Mean" = weightedmean, "Median" = median(ratio))
    stats
  })
  output$ratio_hist <- renderPlot({
    ## Pull data to calculate ratio statistics
    final_df <- final()[[1]]
    ratio_data <- c()
    ratio_data <- final_df %>%
      dplyr::filter(TIME_ADJ_NET_SOLD_PRICE != 0) %>%
      dplyr::filter(SYEAR_old == input$pyear) %>%
      dplyr::filter(YEAR_BUILT < input$pyear - 1) %>%
      dplyr::filter(SALE_VALID == 'Y') %>%
      dplyr::mutate(ratio = TVPred/ TIME_ADJ_NET_SOLD_PRICE)
    ## Calculate ratio
    ratio <- ratio_data$ratio
    hist(ratio, main = "Histogram of Ratio", breaks = input$br, xlim = input$xlim, col = "darkgrey", border = "white")
  })
  
  ## Code ratio table by Value Area
  output$table_nbhgrp <- renderTable({
    ## Pull data to calculate ratio statistics
    final_df <- final()[[1]]
    ratio_data <- c()
    ratio_data <- final_df %>%
      dplyr::filter(TIME_ADJ_NET_SOLD_PRICE != 0) %>%
      dplyr::filter(SYEAR_old == input$pyear) %>%
      dplyr::filter(YEAR_BUILT < input$pyear - 1) %>%
      dplyr::filter(SALE_VALID == 'Y') %>%
      dplyr::mutate(ratio = TVPred/ TIME_ADJ_NET_SOLD_PRICE)
    ## Calculate ratios grouped by Value Area
    dist <- ratio_data %>%
      #filter(ratio_post_cap < 1.25, ratio_post_cap > .72) %>%
      dplyr::group_by(NBHD_CODE) %>%
      dplyr::summarize(COD = sum(abs(ratio - median(ratio))/n()/median(ratio)),
                       COV = sd(ratio)/mean(ratio),
                       weightedmean = sum(TVPred) / sum(TIME_ADJ_NET_SOLD_PRICE),
                       PRD = mean(ratio)/ weightedmean,
                       median = median(ratio),
                       count = n()) 
    dist
    
  })
  
  ## Code ratio table by District
  output$table_dist <- renderTable({
    final_df <- final()[[1]]
    ## Pull data to calculate ratio statistics
    ratio_data <- c()
    ratio_data <- final_df %>%
      dplyr::filter(TIME_ADJ_NET_SOLD_PRICE != 0) %>%
      dplyr::filter(SYEAR_old == input$pyear) %>%
      dplyr::filter(YEAR_BUILT < input$pyear - 1) %>%
      dplyr::filter(SALE_VALID == 'Y') %>%
      dplyr::mutate(ratio = TVPred/ TIME_ADJ_NET_SOLD_PRICE)
    ## Calculate statistics by district
    dist <- ratio_data %>%
      #filter(ratio_post_cap < 1.25, ratio_post_cap > .72) %>%
      dplyr::group_by(DISTRICT) %>%
      dplyr::summarize(COD = sum(abs(ratio - median(ratio))/n()/median(ratio)),
                       COV = sd(ratio)/mean(ratio),
                       weightedmean = sum(TVPred) / sum(TIME_ADJ_NET_SOLD_PRICE),
                       PRD = mean(ratio)/ weightedmean,
                       median = median(ratio),
                       count = n()) 
    dist
    
  })
  
  ## Code for outlier table
  output$outlier_table <- renderTable(
    {
      final_df <- final()[[1]]
      ratio_data <- c()
      ratio_data <- final_df %>%
        dplyr::filter(TIME_ADJ_NET_SOLD_PRICE != 0) %>%
        dplyr::filter(SYEAR_old == input$pyear) %>%
        dplyr::filter(YEAR_BUILT < input$pyear - 1) %>%
        dplyr::filter(SALE_VALID == 'Y') %>%
        dplyr::mutate(ratio = TVPred/ TIME_ADJ_NET_SOLD_PRICE)
     
      
      outlier <- ratio_data %>%
        dplyr::filter(ratio > input$range[[2]] | ratio < input$range[[1]])
      outlier
    }
  )
  output$outliercount <- renderTable(
    {
      final_df <- final()[[1]]
      ratio_data <- c()
      ratio_data <- final_df %>%
        dplyr::filter(TIME_ADJ_NET_SOLD_PRICE != 0) %>%
        dplyr::filter(SYEAR_old == input$pyear) %>%
        dplyr::filter(YEAR_BUILT < input$pyear - 1) %>%
        dplyr::filter(SALE_VALID == 'Y') %>%
        dplyr::mutate(ratio = TVPred/ TIME_ADJ_NET_SOLD_PRICE)
      
      
      outlier <- ratio_data %>%
        dplyr::filter(ratio > input$range[[2]] | ratio < input$range[[1]])
      data.frame("Count" = nrow(outlier))
    }
  )
  output$pct_change_data <- renderTable(
    {
      final_df <- final()[[1]]
      data <- data.frame("Median" = median(final_df$TVPctChange, na.rm = T), "Mean" = mean(final_df$TVPctChange, na.rm = T), "Max" = max(final_df$TVPctChange, na.rm = T), "Min" = min(final_df$TVPctChange, na.rm = T), "Standard Deviation" = sd(final_df$TVPctChange, na.rm = T))
      data
    }
  )
  output$pct_hist <- renderPlot(
    {
      final_df <- final()[[1]]
      hist(final_df$TVPctChange, breaks = input$brks, xlim = input$xlim2, main = "Histogram of Percent Change", col = "darkgrey", border = "white")
    }
  )
  output$value_pct <- renderTable(
    {
      final_df <- final()[[1]]
      data <- final_df %>%
        dplyr::group_by(NBHD_CODE) %>%
        dplyr::summarise(Median = median(TVPctChange, na.rm = T),
                         Mean = mean(TVPctChange, na.rm = T),
                         Max = max(TVPctChange, na.rm = T),
                         Min = min(TVPctChange, na.rm = T),
                         SD = sd(TVPctChange, na.rm = T))
      data
    }
  )
  output$dist_pct <- renderTable(
    {
      final_df <- final()[[1]]
      data <- final_df %>%
        dplyr::group_by(DISTRICT) %>%
        dplyr::summarise(Median = median(TVPctChange, na.rm = T),
                         Mean = mean(TVPctChange, na.rm = T),
                         Max = max(TVPctChange, na.rm = T),
                         Min = min(TVPctChange, na.rm = T),
                         SD = sd(TVPctChange, na.rm = T))
      data
    }
  )
}