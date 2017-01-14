
library(shiny)
library(googleVis)
library(plotly)
library(readxl)
library(forecast)

shinyServer(function(input, output) {

    ## transation sheet
    
    # transaction = read.csv("./data/HK_Domestic_1.csv")
    # sales = read.csv("./data/HK_Domestic_2.csv")
    # stock = read.csv("./data/HK_Domestic_3.csv")
    # full = read.csv("./data/HK_Domestic_4.csv")
    # stock2 = read.csv("./data/HK_Domestic_5.csv")
    # rent = read.csv("./data/HK_Domestic_6.csv")
    # salesIndice = read.csv("./data/HK_Domestic_7.csv")
    # stock2 = read.csv("./data/HK_Domestic_8.csv")
    # salesIndiceFormat = read.csv("./data/HK_Domestic_9.csv")
    # salesIndiceFormatSize = read.csv("./data/HK_Domestic_10.csv")
    
        filename = "./data/HK_Domestic.xlsx"

    transaction = read_excel(filename, sheet = 1, na = "-")
    sales = read_excel(filename, sheet = 2, na = "-")
    stock = read_excel(filename, sheet = 3, na = "-")
    full = read_excel(filename, sheet = 4, na = "-")
    stock2 = read_excel(filename, sheet = 5, na = "-")
    rent = read_excel(filename, sheet = 6, na = "-")
    salesIndice = read_excel(filename, sheet = 7, na = "-")
    stock2 = read_excel(filename, sheet = 8, na = "-")
    salesIndiceFormat = read_excel(filename, sheet = 9, na = "-")
    salesIndiceFormatSize = read_excel(filename, sheet = 10, na = "-")
    rentIndiceFormat = read_excel(filename, sheet = 11, na = "-")


    for2ggplot <- function(forec.obj, data.color = 'blue', fit.color = 'red', forec.color = 'black',
                           lower.fill = 'darkgrey', upper.fill = 'grey', format.date = F)
    {
        serie.orig = forec.obj$x
        serie.fit = forec.obj$fitted
        pi.strings = paste(forec.obj$level, '%', sep = '')

        if(format.date)
            dates = as.Date(time(serie.orig))
        else
            dates = time(serie.orig)

        serie.df = data.frame(date = dates, serie.orig = serie.orig, serie.fit = serie.fit)

        forec.M = cbind(forec.obj$mean, forec.obj$lower[, 1:2], forec.obj$upper[, 1:2])
        forec.df = as.data.frame(forec.M)
        colnames(forec.df) = c('forec.val', 'l0', 'l1', 'u0', 'u1')

        if(format.date)
            forec.df$date = as.Date(time(forec.obj$mean))
        else
            forec.df$date = time(forec.obj$mean)

        p = ggplot() +
            geom_line(aes(date, serie.orig, colour = 'data'), data = serie.df) +
            geom_line(aes(date, serie.fit, colour = 'fit'), data = serie.df) +
            scale_y_continuous() +
            geom_ribbon(aes(x = date, ymin = l0, ymax = u0, fill = 'lower'), data = forec.df, alpha = I(0.4)) +
            geom_ribbon(aes(x = date, ymin = l1, ymax = u1, fill = 'upper'), data = forec.df, alpha = I(0.3)) +
            geom_line(aes(date, forec.val, colour = 'forecast'), data = forec.df) +
            scale_color_manual('Series', values=c('data' = data.color, 'fit' = fit.color, 'forecast' = forec.color)) +
            scale_fill_manual('P.I.', values=c('lower' = lower.fill, 'upper' = upper.fill))

        if (format.date)
            p = p + scale_x_date()

        p
    }

    datasetInput <- reactive({
        perm.vector.size <- as.vector(input$size)
        perm.vector.district <- as.vector(input$district)
    })
    # call as a text output
    output$textOut <- renderText({
        input$action # makes sure nothing moves till the button is hit
        # isolate prevents datasetInput from reactively evaluating
        isolate(datasetInput())
    })

    output$plot1 <- renderPlotly({

        temp = transaction[,3:6]
        names(temp) = c("YearMonth", "SalesNum", "SalesAmount", "AvgSalesNum")
        temp$YearMonth = as.Date(paste(temp$YearMonth, "01", sep="-"), "%Y-%m-%d")


        plot_ly(temp) %>%
            add_trace(x = ~YearMonth, y = ~SalesNum, type = 'bar', name = '# Sales', orientation = 'v',
                      marker = list(color = '#C9EFF9'),
                      hoverinfo = "text",
                      text = ~SalesNum) %>%
            add_trace(x = ~YearMonth, y = ~AvgSalesNum/1000, type = 'scatter', mode = 'lines', name = 'Avg k $ by transation', yaxis = 'y2',
                      line = list(color = "red"),
                      hoverinfo = "text",
                      text = ~paste(AvgSalesNum, '$')) %>%
            layout(title = 'Evolution of the property sales since 2002 in HK',
                   xaxis = list(title = ""),
                   yaxis = list(side = 'left', title = '# of sales', showgrid = FALSE, zeroline = FALSE),
                   yaxis2 = list(side = 'right', overlaying = "y", title = 'Amount in Millions HKD', showgrid = FALSE, zeroline = FALSE))

    })



  output$plot3 <- renderPlotly({
      #input.size = c("40m","40-69.9m","70-99.9m","100-159.9m","160")
      #input.district = c("HK","KWL","NT")
      stock2 = read_excel(filename, sheet = 5, na = "-")
      perm.vector.size <- as.vector(input$size)
      nbSize = length(perm.vector.size)

      tempStockYear = subset(stock2[,1])
      if (nbSize == 1) {
          tempStock1 = stock2[,grepl(perm.vector.size[1], names(stock2), ignore.case=TRUE)]
          tempStock1 = subset(tempStock1[,3])
          tempStock <- cbind(tempStockYear, tempStock1)
          tempStock$Year = as.Date(paste(tempStock$Year, "01-01", sep="-"), "%Y-%m-%d")

          plot_ly(tempStock, x = ~Year, y = tempStock[,names(tempStock[2])]*100, type = 'scatter', name = perm.vector.size[1], mode = 'none', fill = 'tozeroy') %>%
          layout(title = 'Count',
                 xaxis = list(title = "",
                              showgrid = FALSE),
                 yaxis = list(title = "",
                              showgrid = FALSE,
                              ticksuffix = '%'))
      } else if (nbSize == 2) {
          tempStock1 = stock2[,grepl(perm.vector.size[1], names(stock2), ignore.case=TRUE)]
          tempStock1 = subset(tempStock1[,3])
          tempStock2 = stock2[,grepl(perm.vector.size[2], names(stock2), ignore.case=TRUE)]
          tempStock2 = subset(tempStock2[,3])
          tempStock <- cbind(tempStockYear, tempStock1, tempStock2)
          tempStock$Year = as.Date(paste(tempStock$Year, "01-01", sep="-"), "%Y-%m-%d")

          for (i in 3:2) {
              tempStock[,i-1] <- tempStock[,i-1] + tempStock[,i]
          }

          plot_ly(tempStock, x = ~Year, y = tempStock[,names(tempStock[2])]*100, type = 'scatter', name = perm.vector.size[1], mode = 'none', fill = 'tozeroy') %>%
              add_trace(y = tempStock[,names(tempStock[3])]*100, type = 'scatter', name = perm.vector.size[2], mode = 'none', fill = 'tozeroy') %>%
              layout(title = 'Count',
                     xaxis = list(title = "",
                                  showgrid = FALSE),
                     yaxis = list(title = "",
                                  showgrid = FALSE,
                                  ticksuffix = '%'))
      } else if (nbSize == 3) {
          tempStock1 = stock2[,grepl(perm.vector.size[1], names(stock2), ignore.case=TRUE)]
          tempStock1 = subset(tempStock1[,3])
          tempStock2 = stock2[,grepl(perm.vector.size[2], names(stock2), ignore.case=TRUE)]
          tempStock2 = subset(tempStock2[,3])
          tempStock3 = stock2[,grepl(perm.vector.size[3], names(stock2), ignore.case=TRUE)]
          tempStock3 = subset(tempStock3[,3])
          tempStock <- cbind(tempStockYear, tempStock1, tempStock2, tempStock3)

              for (i in 4:2) {
                  tempStock[,i-1] <- tempStock[,i-1] + tempStock[,i]
              }

          plot_ly(tempStock, x = ~Year, y = tempStock[,names(tempStock[2])]*100, type = 'scatter', name = perm.vector.size[1], mode = 'none', fill = 'tozeroy') %>%
              add_trace(y = tempStock[,names(tempStock[3])]*100, type = 'scatter', name = perm.vector.size[2], mode = 'none', fill = 'tozeroy') %>%
              add_trace(y = tempStock[,names(tempStock[4])]*100, type = 'scatter', name = perm.vector.size[3], mode = 'none', fill = 'tozeroy') %>%
              layout(title = 'Count',
                     xaxis = list(title = "",
                                  showgrid = FALSE),
                     yaxis = list(title = "",
                                  showgrid = FALSE,
                                  ticksuffix = '%'))
      } else if (nbSize == 4) {
          tempStock1 = stock2[,grepl(perm.vector.size[1], names(stock2), ignore.case=TRUE)]
          tempStock1 = subset(tempStock1[,3])
          tempStock2 = stock2[,grepl(perm.vector.size[2], names(stock2), ignore.case=TRUE)]
          tempStock2 = subset(tempStock2[,3])
          tempStock3 = stock2[,grepl(perm.vector.size[3], names(stock2), ignore.case=TRUE)]
          tempStock3 = subset(tempStock3[,3])
          tempStock4 = stock2[,grepl(perm.vector.size[4], names(stock2), ignore.case=TRUE)]
          tempStock4 = subset(tempStock4[,3])
          tempStock <- cbind(tempStockYear, tempStock1, tempStock2, tempStock3, tempStock4)

              for (i in 5:2) {
                  tempStock[,i-1] <- tempStock[,i-1] + tempStock[,i]
              }

          plot_ly(tempStock, x = ~Year, y = tempStock[,names(tempStock[2])]*100, type = 'scatter', name = perm.vector.size[1], mode = 'none', fill = 'tozeroy') %>%
              add_trace(y = tempStock[,names(tempStock[3])]*100, type = 'scatter', name = perm.vector.size[2], mode = 'none', fill = 'tozeroy') %>%
              add_trace(y = tempStock[,names(tempStock[4])]*100, type = 'scatter', name = perm.vector.size[3], mode = 'none', fill = 'tozeroy') %>%
              add_trace(y = tempStock[,names(tempStock[5])]*100, type = 'scatter', name = perm.vector.size[4], mode = 'none', fill = 'tozeroy') %>%
              layout(title = 'Count',
                     xaxis = list(title = "",
                                  showgrid = FALSE),
                     yaxis = list(title = "",
                                  showgrid = FALSE,
                                  ticksuffix = '%'))
      } else if (nbSize == 5) {
          tempStock1 = stock2[,grepl(perm.vector.size[1], names(stock2), ignore.case=TRUE)]
          tempStock1 = subset(tempStock1[,3])
          tempStock2 = stock2[,grepl(perm.vector.size[2], names(stock2), ignore.case=TRUE)]
          tempStock2 = subset(tempStock2[,3])
          tempStock3 = stock2[,grepl(perm.vector.size[3], names(stock2), ignore.case=TRUE)]
          tempStock3 = subset(tempStock3[,3])
          tempStock4 = stock2[,grepl(perm.vector.size[4], names(stock2), ignore.case=TRUE)]
          tempStock4 = subset(tempStock4[,3])
          tempStock5 = stock2[,grepl(perm.vector.size[5], names(stock2), ignore.case=TRUE)]
          tempStock5 = subset(tempStock5[,3])
          tempStock <- cbind(tempStockYear, tempStock1, tempStock2, tempStock3, tempStock4, tempStock5)

              for (i in 6:2) {
                  tempStock[,i-1] <- tempStock[,i-1] + tempStock[,i]
              }

          plot_ly(tempStock, x = ~Year, y = tempStock[,names(tempStock[2])]*100, type = 'scatter', name = perm.vector.size[1], mode = 'none', fill = 'tozeroy') %>%
              add_trace(y = tempStock[,names(tempStock[3])]*100, type = 'scatter', name = perm.vector.size[2], mode = 'none', fill = 'tozeroy') %>%
              add_trace(y = tempStock[,names(tempStock[4])]*100, type = 'scatter', name = perm.vector.size[3], mode = 'none', fill = 'tozeroy') %>%
              add_trace(y = tempStock[,names(tempStock[5])]*100, type = 'scatter', name = perm.vector.size[4], mode = 'none', fill = 'tozeroy') %>%
              add_trace(y = tempStock[,names(tempStock[6])]*100, type = 'scatter', name = perm.vector.size[5], mode = 'none', fill = 'tozeroy') %>%
              layout(title = 'Count',
                     xaxis = list(title = "",
                                  showgrid = FALSE),
                     yaxis = list(title = "",
                                  showgrid = FALSE,
                                  ticksuffix = '%'))
      }

  })


  output$plot3old <- renderPlotly({
      tempStockYear = subset(stock2[,1])
      if (!is.na(perm.vector[1])) {
          tempStock1 = stock2[,grepl(perm.vector[1], names(stock2), ignore.case=TRUE)]
          tempStock1 = subset(tempStock1[,3])
          a = 2
          if (!is.na(perm.vector[2])) {
              tempStock2 = stock2[,grepl(perm.vector[2], names(stock2), ignore.case=TRUE)]
              tempStock2 = subset(tempStock2[,3])
              a = a + 1
              if (!is.na(perm.vector[3])) {
                  tempStock3 = stock2[,grepl(perm.vector[3], names(stock2), ignore.case=TRUE)]
                  tempStock3 = subset(tempStock3[,3])
                  a = a + 1
                  if (!is.na(perm.vector[4])) {
                      tempStock4 = stock2[,grepl(perm.vector[4], names(stock2), ignore.case=TRUE)]
                      tempStock4 = subset(tempStock4[,3])
                      a = a + 1
                      if (!is.na(perm.vector[5])) {
                          tempStock5 = stock2[,grepl(perm.vector[5], names(stock2), ignore.case=TRUE)]
                          tempStock5 = subset(tempStock5[,3])
                          tempStock <- cbind( tempStock1, tempStock2, tempStock3, tempStock4, tempStock5)
                          a = a + 1
                      } else {
                          tempStock <- cbind( tempStock1, tempStock2, tempStock3, tempStock4)
                      }
                  } else {
                      tempStock <- cbind( tempStock1, tempStock2, tempStock3)
                  }
              } else {
                  tempStock <- cbind( tempStock1, tempStock2)
              }
          }
          else {
              tempStock <- cbind(tempStock1)
          }
      }
      tempStock <- cbind(tempStockYear, tempStock)
      tempStock$Year = as.Date(paste(tempStock$Year, "01-01", sep="-"), "%Y-%m-%d")
      for (i in c(a:2)) {
          tempStock[,i-1] <- tempStock[,i-1] + tempStock[,i]
      }
      if (!is.na(perm.vector[1])) {
          p <- plot_ly(tempStock, x = ~Year, y = tempStock[,names(tempStock[2])]*100, type = 'scatter', name = perm.vector[1], mode = 'none', fill = 'tozeroy')
          for (i in 2:3) {
              p <- add_trace(tempStock, x = ~Year, y = tempStock[,names(tempStock[i])]*100, type = 'scatter', name = perm.vector[i-1], mode = 'none', fill = 'tozeroy')
          }
      }
      plot(p)
  })


  output$plot2 <- renderPlotly({

      perm.vector.size <- as.vector(input$size)
      nbSize = length(perm.vector.size)

      perm.vector.district <- as.vector(input$district)
      nbDistrict = length(perm.vector.district)

      temp = sales[c(3,5,6,7)]
      temp$YearMonth = as.Date(paste(temp$`Year-Month`, "01", sep="-"), "%Y-%m-%d")

      pal <- c("red", "blue", "green")


      a=b=c=d=e=0

      if (nbDistrict == 3) {
          if(sum(grepl("40m", perm.vector.size)) > 0){
          tempSize1 = subset(temp, Size == "0-39.9m")
          tempSize1$SizeDistrict = paste(tempSize1$Size, tempSize1$District, sep=" - ")
          tempSize1 = tempSize1[,c(5,2,3,6,4)]
          names(tempSize1) = c("YearMonth", "Size", "District", "SizeDistrict", "Avg")
          a=1
          }
          if(sum(grepl("40-69.9m", perm.vector.size)) > 0){
          tempSize2 = subset(temp, Size == "40-69.9m")
          tempSize2$SizeDistrict = paste(tempSize2$Size, tempSize2$District, sep=" - ")
          tempSize2 = tempSize2[,c(5,2,3,6,4)]
          names(tempSize2) = c("YearMonth", "Size", "District", "SizeDistrict", "Avg")
          b=1
          }
          if(sum(grepl("70-99.9m", perm.vector.size)) > 0){
          tempSize3 = subset(temp, Size == "70-99.9m")
          tempSize3$SizeDistrict = paste(tempSize3$Size, tempSize3$District, sep=" - ")
          tempSize3 = tempSize3[,c(5,2,3,6,4)]
          names(tempSize3) = c("YearMonth", "Size", "District", "SizeDistrict", "Avg")
          c=1
          }
          if(sum(grepl("100-159.9m", perm.vector.size)) > 0){
          tempSize4 = subset(temp, Size == "100-159.9m")
          tempSize4$SizeDistrict = paste(tempSize4$Size, tempSize4$District, sep=" - ")
          tempSize4 = tempSize4[,c(5,2,3,6,4)]
          names(tempSize4) = c("YearMonth", "Size", "District", "SizeDistrict", "Avg")
          d=1
          }
          if(sum(grepl("160", perm.vector.size)) > 0){
          tempSize5 = subset(temp, Size == "160+m")
          tempSize5$SizeDistrict = paste(tempSize5$Size, tempSize5$District, sep=" - ")
          tempSize5 = tempSize5[,c(5,2,3,6,4)]
          names(tempSize5) = c("YearMonth", "Size", "District", "SizeDistrict", "Avg")
          e=1
          }
      } else if (nbDistrict == 2) {
          temp = subset(temp, District == perm.vector.district[1] | District == perm.vector.district[2])
          if(sum(grepl("40m", perm.vector.size)) > 0){
              tempSize1 = subset(temp, Size == "0-39.9m")
              tempSize1$SizeDistrict = paste(tempSize1$Size, tempSize1$District, sep=" - ")
              tempSize1 = tempSize1[,c(5,2,3,6,4)]
              names(tempSize1) = c("YearMonth", "Size", "District", "SizeDistrict", "Avg")
              a=1
          }
          if(sum(grepl("40-69.9m", perm.vector.size)) > 0){
              tempSize2 = subset(temp, Size == "40-69.9m")
              tempSize2$SizeDistrict = paste(tempSize2$Size, tempSize2$District, sep=" - ")
              tempSize2 = tempSize2[,c(5,2,3,6,4)]
              names(tempSize2) = c("YearMonth", "Size", "District", "SizeDistrict", "Avg")
              b=1
          }
          if(sum(grepl("70-99.9m", perm.vector.size)) > 0){
              tempSize3 = subset(temp, Size == "70-99.9m")
              tempSize3$SizeDistrict = paste(tempSize3$Size, tempSize3$District, sep=" - ")
              tempSize3 = tempSize3[,c(5,2,3,6,4)]
              names(tempSize3) = c("YearMonth", "Size", "District", "SizeDistrict", "Avg")
              c=1
          }
          if(sum(grepl("100-159.9m", perm.vector.size)) > 0){
              tempSize4 = subset(temp, Size == "100-159.9m")
              tempSize4$SizeDistrict = paste(tempSize4$Size, tempSize4$District, sep=" - ")
              tempSize4 = tempSize4[,c(5,2,3,6,4)]
              names(tempSize4) = c("YearMonth", "Size", "District", "SizeDistrict", "Avg")
              d=1
          }
          if(sum(grepl("160", perm.vector.size)) > 0){
              tempSize5 = subset(temp, Size == "160+m")
              tempSize5$SizeDistrict = paste(tempSize5$Size, tempSize5$District, sep=" - ")
              tempSize5 = tempSize5[,c(5,2,3,6,4)]
              names(tempSize5) = c("YearMonth", "Size", "District", "SizeDistrict", "Avg")
              e=1
          }
      } else if (nbDistrict == 1) {
          temp = subset(temp, District == perm.vector.district[1])
          if(sum(grepl("40m", perm.vector.size)) > 0){
              tempSize1 = subset(temp, Size == "0-39.9m")
              tempSize1$SizeDistrict = paste(tempSize1$Size, tempSize1$District, sep=" - ")
              tempSize1 = tempSize1[,c(5,2,3,6,4)]
              names(tempSize1) = c("YearMonth", "Size", "District", "SizeDistrict", "Avg")
              a=1
          }
          if(sum(grepl("40-69.9m", perm.vector.size)) > 0){
              tempSize2 = subset(temp, Size == "40-69.9m")
              tempSize2$SizeDistrict = paste(tempSize2$Size, tempSize2$District, sep=" - ")
              tempSize2 = tempSize2[,c(5,2,3,6,4)]
              names(tempSize2) = c("YearMonth", "Size", "District", "SizeDistrict", "Avg")
              b=1
          }
          if(sum(grepl("70-99.9m", perm.vector.size)) > 0){
              tempSize3 = subset(temp, Size == "70-99.9m")
              tempSize3$SizeDistrict = paste(tempSize3$Size, tempSize3$District, sep=" - ")
              tempSize3 = tempSize3[,c(5,2,3,6,4)]
              names(tempSize3) = c("YearMonth", "Size", "District", "SizeDistrict", "Avg")
              c=1
          }
          if(sum(grepl("100-159.9m", perm.vector.size)) > 0){
              tempSize4 = subset(temp, Size == "100-159.9m")
              tempSize4$SizeDistrict = paste(tempSize4$Size, tempSize4$District, sep=" - ")
              tempSize4 = tempSize4[,c(5,2,3,6,4)]
              names(tempSize4) = c("YearMonth", "Size", "District", "SizeDistrict", "Avg")
              d=1
          }
          if(sum(grepl("160", perm.vector.size)) > 0){
              tempSize5 = subset(temp, Size == "160+m")
              tempSize5$SizeDistrict = paste(tempSize5$Size, tempSize5$District, sep=" - ")
              tempSize5 = tempSize5[,c(5,2,3,6,4)]
              names(tempSize5) = c("YearMonth", "Size", "District", "SizeDistrict", "Avg")
              e=1
          }
      }


      if (a==1 & b==0 & c==0 & d==0 & e==0) {
          plot_ly(data = tempSize1, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==1 & b==0 & c==0 & d==0 & e==1) {
          plot_ly(data = tempSize1, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize5, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==1 & b==0 & c==0 & d==1 & e==0) {
          plot_ly(data = tempSize1, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize4, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==1 & b==0 & c==0 & d==1 & e==1) {
          plot_ly(data = tempSize1, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize4, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize5, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==1 & b==0 & c==1 & d==0 & e==0) {
          plot_ly(data = tempSize1, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize3, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==1 & b==0 & c==1 & d==0 & e==1) {
          plot_ly(data = tempSize1, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize3, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize5, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==1 & b==0 & c==1 & d==1 & e==0) {
          plot_ly(data = tempSize1, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize3, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize4, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==1 & b==0 & c==1 & d==1 & e==1) {
          plot_ly(data = tempSize1, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize3, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize4, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize5, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==1 & b==1 & c==0 & d==0 & e==0) {
          plot_ly(data = tempSize1, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize2, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==1 & b==1 & c==0 & d==0 & e==1) {
          plot_ly(data = tempSize1, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize2, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize5, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==1 & b==1 & c==0 & d==1 & e==0) {
          plot_ly(data = tempSize1, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize2, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize4, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==1 & b==1 & c==0 & d==1 & e==1) {
          plot_ly(data = tempSize1, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize2, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize4, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize5, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==1 & b==1 & c==1 & d==0 & e==0) {
          plot_ly(data = tempSize1, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize2, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize3, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==1 & b==1 & c==1 & d==0 & e==1) {
          plot_ly(data = tempSize1, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize2, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize3, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize5, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==1 & b==1 & c==1 & d==1 & e==0) {
          plot_ly(data = tempSize1, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize2, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize3, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize4, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==1 & b==1 & c==1 & d==1 & e==1) {
          plot_ly(data = tempSize1, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize2, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize3, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize4, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize5, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==0 & b==1 & c==0 & d==0 & e==0) {
          plot_ly(data = tempSize2, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==0 & b==1 & c==0 & d==0 & e==1) {
          plot_ly(data = tempSize2, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize5, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==0 & b==1 & c==0 & d==1 & e==0) {
          plot_ly(data = tempSize2, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize4, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==0 & b==1 & c==0 & d==1 & e==1) {
          plot_ly(data = tempSize2, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize4, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize5, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==0 & b==1 & c==1 & d==0 & e==0) {
          plot_ly(data = tempSize2, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize3, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==0 & b==1 & c==1 & d==0 & e==1) {
          plot_ly(data = tempSize2, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize3, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize5, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==0 & b==1 & c==1 & d==1 & e==0) {
          plot_ly(data = tempSize2, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize3, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize4, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==0 & b==1 & c==1 & d==1 & e==1) {
          plot_ly(data = tempSize2, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize3, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize4, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize5, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==0 & b==0 & c==1 & d==0 & e==0) {
          plot_ly(data = tempSize3, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==0 & b==0 & c==1 & d==0 & e==1) {
          plot_ly(data = tempSize3, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize5, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==0 & b==0 & c==1 & d==1 & e==0) {
          plot_ly(data = tempSize3, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize4, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==0 & b==0 & c==1 & d==1 & e==1) {
          plot_ly(data = tempSize3, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize4, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize5, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==0 & b==0 & c==0 & d==1 & e==0) {
          plot_ly(data = tempSize4, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==0 & b==0 & c==0 & d==1 & e==1) {
          plot_ly(data = tempSize4, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6)) %>%
              add_trace(data = tempSize5, x = ~YearMonth, y = ~Avg, name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      } else
      if (a==0 & b==0 & c==0 & d==0 & e==1) {
          plot_ly(data = tempSize5, x = ~YearMonth, y = ~Avg, type = 'scatter', name = ~SizeDistrict, mode = 'markers', symbol = ~District, symbols = c('circle','square','star'), color = ~SizeDistrict, colors = pal, marker = list(size = 6))
      }
  })

  output$plot4 <- renderPlotly({

      temp = rent[c(3:5)]
      tempSales = salesIndice[9]
      temp = cbind(temp, tempSales)
      temp$YearMonth = as.Date(paste(temp$`Year-Month`, "01", sep="-"), "%Y-%m-%d")
      temp = temp[, c(5,2,3,4)]
      
      plot_ly(temp) %>%
          add_trace(x = ~YearMonth, y = ~`Rent Indice`, type = 'bar', name = 'Rent Indice',
                    marker = list(color = '#C9EFF9'),
                    hoverinfo = "text",
                    text = ~`Rent Indice`) %>%
          add_trace(x = ~YearMonth, y = ~`Price Rent Indice`, type = 'scatter', mode = 'lines', name = 'Rent price Indice', yaxis = 'y2',
                    line = list(color = "red"),
                    hoverinfo = "text",
                    text = ~`Price Rent Indice`) %>%
          add_trace(x = ~YearMonth, y = ~`Price Sales Indice`, type = 'scatter', mode = 'lines', name = 'Sales price Indice', yaxis = 'y2',
                    line = list(color = "green"),
                    hoverinfo = "text",
                    text = ~`Price Sales Indice`) %>%
          layout(title = 'Evolution of the property rent since 1993 (1999 = indice 100) in HK',
                 xaxis = list(title = ""),
                 yaxis = list(side = 'left', title = 'Rent Indice', showgrid = FALSE, zeroline = FALSE),
                 yaxis2 = list(side = 'right', overlaying = "y", title = 'Price Indice', showgrid = FALSE, zeroline = FALSE))

  })

  output$plotforecast1 <- renderPlot({
      mydata = stock[,c(1,5)]
      mydata$Stock = as.integer(mydata$Stock)
      mydata = aggregate(mydata, by=list(mydata$Year), sum, na.rm=TRUE)
      mydata = mydata[,c(1,3)]
      names(mydata) = c("Year", "Stock")

      # Forecast from mean
      mf = meanf(mydata[,2], h=12, level=c(90,95))
      plot(mf)
      })


  output$plotforecast2 <- renderPlot({
      mydata = stock[,c(1,5)]
      mydata$Stock = as.integer(mydata$Stock)
      mydata = aggregate(mydata, by=list(mydata$Year), sum, na.rm=TRUE)
      mydata = mydata[,c(1,3)]
      names(mydata) = c("Year", "Stock")

      # forecast from naive
      mn = naive(mydata[,2],h=12,level=c(90,95),fan=FALSE,lambda=NULL)
      plot(mn)
  })

  output$plotforecast3 <- renderPlot({
      mydata = stock[,c(1,5)]
      mydata$Stock = as.integer(mydata$Stock)
      mydata = aggregate(mydata, by=list(mydata$Year), sum, na.rm=TRUE)
      mydata = mydata[,c(1,3)]
      names(mydata) = c("Year", "Stock")

      # forecast from randow walk with drift
      md = rwf(mydata[,2],h=12,drift=T,level=c(90,95),fan=FALSE,lambda=NULL)
      plot(md)
  })

  output$plotforecast4 <- renderPlot({
      mydata = stock[,c(1,5)]
      mydata$Stock = as.integer(mydata$Stock)
      mydata = aggregate(mydata, by=list(mydata$Year), sum, na.rm=TRUE)
      mydata = mydata[,c(1,3)]
      names(mydata) = c("Year", "Stock")

      # forecast from auto arima on diff
      diff_data = diff(mydata[,2])
      m_aa = auto.arima(diff_data)
      mydata_arima = forecast(m_aa)
      plot(mydata_arima)
  })

  output$plotforecast5 <- renderPlot({
      mydata2 = as.data.frame(stock2)
      rownames(mydata2) <- mydata2[,1]
      mydata2 = mydata2[,c(2:13)]
      colnames(mydata2) <- month.abb
      mydata2 <- ts(as.vector(t(as.matrix(mydata2))), start=c(1999,1), end=c(2016,11), frequency=12)

      # forecast from ETS (exponential state smoothing)
      m_ets = ets(mydata2)
      f_ets = forecast(m_ets, h=12)
      plot(f_ets)
  })

  output$plotforecast6 <- renderPlot({
      mydata2 = as.data.frame(stock2)
      rownames(mydata2) <- mydata2[,1]
      mydata2 = mydata2[,c(2:13)]
      colnames(mydata2) <- month.abb
      mydata2 <- ts(as.vector(t(as.matrix(mydata2))), start=c(1999,1), end=c(2016,11), frequency=12)

      # forecast from auto arima with diff
      diff_data2 = diff(mydata2)
      m_aa2 = auto.arima(diff_data2)
      mydata_arima2 = forecast(m_aa2)
      plot(mydata_arima2)
  })

  output$plotforecast7 <- renderPlot({
      mydata2 = as.data.frame(stock2)
      rownames(mydata2) <- mydata2[,1]
      mydata2 = mydata2[,c(2:13)]
      colnames(mydata2) <- month.abb
      mydata2 <- ts(as.vector(t(as.matrix(mydata2))), start=c(1999,1), end=c(2016,11), frequency=12)

      # forecast from TBATS
      m_tbats = tbats(mydata2)
      f_tbats = forecast(m_tbats, h=12)
      plot(f_tbats)
  })

  output$plotforecastResult <- renderPlot({
      mydata2 = as.data.frame(stock2)
      rownames(mydata2) <- mydata2[,1]
      mydata2 = mydata2[,c(2:13)]
      colnames(mydata2) <- month.abb
      mydata2 <- ts(as.vector(t(as.matrix(mydata2))), start=c(1999,1), end=c(2016,11), frequency=12)

      # forecast from auto arima with diff
      diff_data2 = diff(mydata2)
      m_aa2 = auto.arima(diff_data2)

      # forecast from ETS (exponential state smoothing)
      m_ets = ets(mydata2)

      # forecast from TBATS
      m_tbats = tbats(mydata2)

      barplot(c(ETS=m_ets$aic, ARIMA=m_aa2$aic, TBATS=m_tbats$AIC), col="light blue", ylab="AIC")

  })



  output$plotforecastsales1 <- renderPlot({
      mydata2 = as.data.frame(salesIndiceFormat)
      rownames(mydata2) <- mydata2[,1]
      mydata2 = mydata2[,c(2:13)]
      colnames(mydata2) <- month.abb
      mydata2 <- ts(as.vector(t(as.matrix(mydata2))), start=c(1999,1), end=c(2016,11), frequency=12)

      # forecast from auto arima with diff
      diff_data2 = diff(mydata2)
      m_aa2 = auto.arima(diff_data2)
      mydata_arima2 = forecast(m_aa2)
      plot(mydata_arima2)
  })

  output$plotforecastsalesSize1 <- renderPlot({

      ####################### size 1
      sales1 = as.data.frame(salesIndiceFormatSize[1:24,])
      rownames(sales1) <- sales1[,1]
      sales1 = sales1[,c(2:13)]
      colnames(sales1) <- month.abb
      sales1 <- ts(as.vector(t(as.matrix(sales1))), start=c(1999,1), end=c(2016,11), frequency=12)

      # forecast from auto arima with diff
      diff_data1 = diff(sales1)
      m_aa1 = auto.arima(diff_data1)
      mydata_arima1 = forecast(m_aa1)
      for2ggplot(mydata_arima1)

  })

  output$plotforecastsalesSize2 <- renderPlot({


      ####################### size 2
      sales2 = as.data.frame(salesIndiceFormatSize[25:48,])
      rownames(sales2) <- sales2[,1]
      sales2 = sales2[,c(2:13)]
      colnames(sales2) <- month.abb
      sales2 <- ts(as.vector(t(as.matrix(sales2))), start=c(1999,1), end=c(2016,11), frequency=12)

      # forecast from auto arima with diff
      diff_data2 = diff(sales2)
      m_aa2 = auto.arima(diff_data2)
      mydata_arima2 = forecast(m_aa2)
      for2ggplot(mydata_arima2)


  })

  output$plotforecastsalesSize3 <- renderPlot({
      ####################### size 3
      sales3 = as.data.frame(salesIndiceFormatSize[49:72,])
      rownames(sales3) <- sales3[,1]
      sales3 = sales3[,c(2:13)]
      colnames(sales3) <- month.abb
      sales3 <- ts(as.vector(t(as.matrix(sales3))), start=c(1999,1), end=c(2016,11), frequency=12)

      # forecast from auto arima with diff
      diff_data3 = diff(sales3)
      m_aa3 = auto.arima(diff_data3)
      mydata_arima3 = forecast(m_aa3)
      for2ggplot(mydata_arima3)

  })

  output$plotforecastsalesSize4 <- renderPlot({

      ####################### size 4
      sales4 = as.data.frame(salesIndiceFormatSize[73:96,])
      rownames(sales4) <- sales4[,1]
      sales4 = sales4[,c(2:13)]
      colnames(sales4) <- month.abb
      sales4 <- ts(as.vector(t(as.matrix(sales4))), start=c(1999,1), end=c(2016,11), frequency=12)

      # forecast from auto arima with diff
      diff_data4 = diff(sales4)
      m_aa4 = auto.arima(diff_data4)
      mydata_arima4 = forecast(m_aa4)
      for2ggplot(mydata_arima4)

  })

  output$plotforecastsalesSize5 <- renderPlot({

      ####################### size 5
      sales5 = as.data.frame(salesIndiceFormatSize[97:120,])
      rownames(sales5) <- sales5[,1]
      sales5 = sales5[,c(2:13)]
      colnames(sales5) <- month.abb
      sales5 <- ts(as.vector(t(as.matrix(sales5))), start=c(1999,1), end=c(2016,11), frequency=12)

      # forecast from auto arima with diff
      diff_data5 = diff(sales5)
      m_aa5 = auto.arima(diff_data5)
      mydata_arima5 = forecast(m_aa5)
      for2ggplot(mydata_arima5)


  })
  
  output$plotforecastrent <- renderPlot({
      
      mydata = as.data.frame(rentIndiceFormat)
      rownames(mydata) <- mydata[,1]
      mydata = mydata[,c(2:13)]
      colnames(mydata) <- month.abb
      mydata <- ts(as.vector(t(as.matrix(mydata))), start=c(1993,1), end=c(2016,11), frequency=12)
      
      
      # # forecast from ETS (exponential state smoothing)
      # m_ets = ets(mydata)
      # f_ets = forecast(m_ets, h=12) 
      # plot(f_ets)
      
      # forecast from auto arima with diff
      diff_data2 = diff(mydata)
      m_aa2 = auto.arima(diff_data2)
      mydata_arima2 = forecast(m_aa2)
      # plot(mydata_arima2)
      
      # # forecast from TBATS
      # m_tbats = tbats(mydata)
      # f_tbats = forecast(m_tbats, h=12)
      # plot(f_tbats)
      # 
      # barplot(c(ETS=m_ets$aic, ARIMA=m_aa2$aic, TBATS=m_tbats$AIC), col="light blue", ylab="AIC")
      # # Arima is better
      
      for2ggplot(mydata_arima2)
  })

  # output$plot6 <- renderPlot({
  #     temp = rent[c(3:5)]
  #     temp$YearMonth = as.Date(paste(temp$`Year-Month`, "01", sep="-"), "%Y-%m-%d")
  #     temp = temp[, c(4,3)]
  #     temp$Dim = "Rent"
  #     names(temp) = c("YearMonth", "Price", "Dim")
  #     
  #     tempSales = salesIndice[c(3,9)]
  #     tempSales$YearMonth = as.Date(paste(tempSales$`Year-Month`, "01", sep="-"), "%Y-%m-%d")
  #     tempSales = tempSales[, c(3,2)]
  #     tempSales$Dim = "Sales"
  #     names(tempSales) = c("YearMonth", "Price", "Dim")
  #     temp2 = rbind(temp, tempSales)
  #     
  #     
  #     plot_ly(temp2, x = ~YearMonth, y = ~Price, type = 'scatter', mode = 'lines', linetype = ~Dim) %>%
  #         layout(title = 'Comparison between rent and sales indice',
  #                yaxis = list (title = 'Price Indice'))
  #     
  #     
  # })
  
  
  output$plot6old <- renderGvis({
      temp = rent[c(3:5)]
      temp$YearMonth = as.Date(paste(temp$`Year-Month`, "01", sep="-"), "%Y-%m-%d")
      temp = temp[, c(4,3)]
      temp$Dim = "Rent"
      names(temp) = c("YearMonth", "Price", "Dim")

      tempSales = salesIndice[c(3,9)]
      tempSales$YearMonth = as.Date(paste(tempSales$`Year-Month`, "01", sep="-"), "%Y-%m-%d")
      tempSales = tempSales[, c(3,2)]
      tempSales$Dim = "Sales"
      names(tempSales) = c("YearMonth", "Price", "Dim")
      temp2 = rbind(temp, tempSales)


      myState <- '
{"yZoomedIn":false,"yZoomedDataMin":0,"xAxisOption":"_TIME",


      "xZoomedDataMin":631152000000,"uniColorForNonSelected":false,
      "showTrails":false,"orderedByY":false,"nonSelectedAlpha":0,
      "orderedByX":false,"sizeOption":"_UNISIZE","xLambda":1,
      "colorOption":"_UNIQUE_COLOR",
      "iconKeySettings":[{"key":{"dim0":"Sales"}},
      {"key":{"dim0":"Rent"}}],
      "dimensions":{"iconDimensions":["dim0"]},
      "yZoomedDataMax":220,"xZoomedIn":false,"iconType":"LINE"}
      '

      #"xZoomedDataMax":1230768000000,"time":"2009","playDuration":15000,
      #"yAxisOption":"2","yLambda":0,"duration":{"timeUnit":"Y","multiplier":1},

      gvisMotionChart(temp2, idvar="Dim", timevar="YearMonth", options=list(width=800, height=600, state=myState))

      # myStateSettings <-'{"yAxisOption":"2","xZoomedDataMax":1477958400000,"showTrails":false,"playDuration":15000,
      # "orderedByX":false,"xAxisOption":"_TIME","duration":{"multiplier":1,"timeUnit":"D"},"xZoomedIn":false,
      # "iconKeySettings":[{"key":{"dim0":"Sales"}},{"key":{"dim0":"Rent"}}],"sizeOption":"_UNISIZE",
      # "yLambda":1,"yZoomedIn":false,"xLambda":1,"colorOption":"_UNIQUE_COLOR","xZoomedDataMin":725846400000,
      # "yZoomedDataMax":800,"yZoomedDataMin":0,"orderedByY":false,"uniColorForNonSelected":false,"nonSelectedAlpha":0.4,
      # "dimensions":{"iconDimensions":["dim0"]},"time":"2016-11-01","iconType":"LINE"}'

#
#
#       myStateSettings <-'
# {"xZoomedDataMin":1199145600000,"colorOption":"2",
#       "duration":{"timeUnit":"Y","multiplier":1},"yLambda":1,
#       "yAxisOption":"4","sizeOption":"_UNISIZE",
#       "iconKeySettings":[],"xLambda":1,"nonSelectedAlpha":0,
#       "xZoomedDataMax":1262304000000,"iconType":"LINE",
#       "dimensions":{"iconDimensions":["dim0"]},
#       "showTrails":false,"uniColorForNonSelected":false,
#       "xAxisOption":"_TIME","orderedByX":false,"playDuration":15000,
#       "xZoomedIn":false,"time":"2010","yZoomedDataMin":0,
#       "yZoomedIn":false,"orderedByY":false,"yZoomedDataMax":100}
#       '
#       M <- gvisMotionChart(Fruits, "Fruit", "Year", options=list(state=myStateSettings))
#       plot(M)

  })
 
  })


