library(shiny)
library(DT)
library(leaflet)
library(RColorBrewer)
library(ggplot2)

source("./main.R")
load('./data/air_quality_table_eng.RData')  # dd

shinyServer(function(input, output, session) {
  # air quality level definition
  output$table2 <- DT::renderDataTable(
    DT::datatable(dd, 
      # caption = htmltools::tags$caption(
      #   style = 'text-align: center;',  # caption-side: bottom; 
      #   '空气质量指数（AQI）及相关信息' #, htmltools::em('This is a simple caption for the table.') -> 斜体
      # ),
      colnames = c( "AQI", "Air Pollution Level", "Health Implications"),
      options = list(ordering = FALSE, dom = 't'),  # autoWidth = TRUE), 
      rownames = FALSE
    ) %>% 
      formatStyle(
        'Air Pollution Level', 
        target = 'row',
        color = styleEqual(dd$'Air Pollution Level', c(rep('black',3), rep('white',3))),
        backgroundColor = styleEqual(dd$'Air Pollution Level', cols)
      )
  )
  
  # Text: update time
  output$text1 <- renderText(
    update_time
  )
  
  # realtime table
  output$table1 <- DT::renderDataTable(
    DT::datatable(dat_AQI[c(2,1,seq(3,11))],
      options = list(columnDefs = list(list(className = 'dt-center', targets = c(1,3)))), #-1 since no rowname
      colnames = c('Rank', 'City','Air Quality Index（AQI）','AQI Level','PM2.5','PM10',
                   'CO','NO2','SO2','O3（Average per hour）','O3（Average per 8 hours）'),
      filter = 'bottom', 
      rownames = FALSE
    ) %>% 
      formatStyle(
        'Level', 
        backgroundColor = styleEqual(dat_AQI_levels, cols[names(cols) %in% dat_AQI_levels])
      ) 
  )
  
  # bar plot of realtime data's Level
  output$plot1 <- renderPlotly(
    p_bar
  )
  
  # pie chart of realtime data's Level
  output$plot2 <- renderPlotly(
    p_pie
  )
  
  # output$plot3 <- renderPlotly({
  #   g_hist <- ggplot(dat, aes_string(x = input$pollutant)) +
  #     geom_bar(fill = "#FF6666") + theme_set(theme_bw())
  #   ggplotly(g_hist)
  # })
  
  output$plot3 <- renderPlotly(
    p_pollutants
  )
  
  # Leaflet map
  output$realtimeMap <- renderLeaflet({
  leaflet(map) %>% amap() %>%
    #加入框边界及颜色
    addPolygons(stroke = TRUE,
                smoothFactor = 1,
                fillOpacity = 0.7,
                weight = 1,
                color = ~pal(value),
                popup = ~htmltools::htmlEscape(popup)
    ) %>%
    #加入右下角边框
    addLegend("bottomright", pal = pal, values = ~value,
              title = "AQI Level",
              labFormat = leaflet::labelFormat(prefix = ""),
              opacity = .9)  %>%
    addCircles(lng=dat_info$lon, lat=dat_info$lat, popup=paste0(dat_info$city, ' AQI: ', as.character(dat_info$AQI)),
               stroke = TRUE, fillOpacity = .5, radius = 10000)
  })
  
  # history data
  city_df = eventReactive(input$submit, {  # city_df is a expression
    cities = input$city_selected
    # return a list of dataframes
    dfs = lapply(cities, function(x){data.frame(city = x, get_AQI_history(x))})
    do.call("rbind", dfs)
  })
  
  output$text2 <- renderText(
    paste0('From ', city_df()[1,]$Date, ' to ', city_df()[dim(city_df())[1],]$Date)
  )
  
  output$table3 <- DT::renderDataTable(
    DT::datatable(city_df(),  # need to use city_df()
      options = list(columnDefs = list(list(className = 'dt-center', targets = c(1, 2, 4)))),
      colnames = c("City" = 'city'),
      # rownames = FALSE,
      filter = 'top'
    ) 
    %>%
      formatStyle(
        'AQI_Level',
        # backgroundColor = styleEqual(unique(na.omit(city_df()$'AQI_Level')), 
        #                              cols2[names(cols) %in% unique(na.omit(city_df()$'AQI_Level'))]) # na.omit() because there may be NAs in the data
        backgroundColor = styleEqual(levels(city_df()$AQI_Level), table_color(city_df()))
      )
  )

  observeEvent(input$submit, {
    updateCheckboxGroupInput(session, "city_motionchart",
      choices = input$city_selected,
      selected = input$city_selected,
      inline = TRUE
    )
  })
  
  observe({
    updateDateRangeInput(session, 'dates_motionchart',
      start = city_df()[1,]$Date,
      end = city_df()[dim(city_df())[1],]$Date,
      min = city_df()[1,]$Date, 
      max = city_df()[dim(city_df())[1],]$Date
    )
  })
  
  output$motionchart <- renderGvis({
    dat = city_df()[city_df()$city %in% input$city_motionchart,] 
    timeintvl = seq(input$dates_motionchart[1], input$dates_motionchart[2], by = 'day')
    dat = dat[dat$Date %in% timeintvl,]
    # dat = do.call("rbind", dfs[input$city_selected %in% input$city_motionchart])
    gvisMotionChart(dat, idvar = "city", timevar = "Date", options=list(width=1150, height=550))
  })
  
  observeEvent(input$submit, {
    updateSelectInput(session, "city_summary",
      choices = input$city_selected
    )
  })
  
  observe({
    updateDateRangeInput(session, 'dates_summary',
      start = city_df()[1,]$Date,
      end = city_df()[dim(city_df())[1],]$Date,
      min = city_df()[1,]$Date, 
      max = city_df()[dim(city_df())[1],]$Date
    )
  })
  
  output$summary <- renderPrint({
    dataset <- city_df()[city_df()$city == input$city_summary,-1]
    timeintvl = seq(input$dates_motionchart[1], input$dates_motionchart[2], by = 'day')
    dataset = dataset[dataset$Date %in% timeintvl,]
    dataset$AQI_Level = as.factor(dataset$AQI_Level)  # AQI级别 as.factor
    summary(dataset)  
  })
})
