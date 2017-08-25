library(shiny)
library(DT)
library(leaflet)
library(RColorBrewer)
library(ggplot2)

source("./main.R")
load('./data/air_quality_table.RData')

shinyServer(function(input, output, session) {
  # air quality level definition
  output$table2 <- DT::renderDataTable(
    DT::datatable(df_quality, 
      # caption = htmltools::tags$caption(
      #   style = 'text-align: center;',  # caption-side: bottom; 
      #   '空气质量指数（AQI）及相关信息' #, htmltools::em('This is a simple caption for the table.') -> 斜体
      # ),
      colnames = c( "AQI", "AQI级别", "对健康影响情况", "建议采取的措施"),
      options = list(ordering = FALSE, dom = 't'),  # autoWidth = TRUE), 
      rownames = FALSE
    ) %>% 
      formatStyle(
        '空气质量指数级别', 
        target = 'row',
        color = styleEqual(df_quality$空气质量指数级别, c(rep('black',3), rep('white',3))),
        backgroundColor = styleEqual(df_quality$空气质量指数级别, cols)
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
      colnames = c('排名', '城市','空气质量指数（AQI）','AQI级别','PM2.5（细颗粒物）','PM10（可吸入颗粒物）',
                   'CO（一氧化碳）','NO2（二氧化氮）','SO2（二氧化硫）','O3（臭氧1小时平均）','O3（臭氧8小时平均）'),
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
              title = "空气质量指数级别",
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
    paste0('数据时间范围: ', city_df()[1,]$日期, '到', city_df()[dim(city_df())[1],]$日期)
  )
  
  output$table3 <- DT::renderDataTable(
    DT::datatable(city_df(),  # need to use city_df()
      options = list(columnDefs = list(list(className = 'dt-center', targets = c(1, 2, 4)))),
      colnames = c("城市" = 'city'),
      # rownames = FALSE,
      filter = 'top'
    ) 
    %>%
      formatStyle(
        'AQI级别',
        backgroundColor = styleEqual(unique(na.omit(city_df()$AQI级别)), 
                                     cols[names(cols) %in% unique(na.omit(city_df()$AQI级别))]) # na.omit() because there may be NAs in the data
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
      start = city_df()[1,]$日期,
      end = city_df()[dim(city_df())[1],]$日期,
      min = city_df()[1,]$日期, 
      max = city_df()[dim(city_df())[1],]$日期
    )
  })
  
  output$motionchart <- renderGvis({
    dat = city_df()[city_df()$city %in% input$city_motionchart,] 
    timeintvl = seq(input$dates_motionchart[1], input$dates_motionchart[2], by = 'day')
    dat = dat[dat$日期 %in% timeintvl,]
    # dat = do.call("rbind", dfs[input$city_selected %in% input$city_motionchart])
    gvisMotionChart(dat, idvar = "city", timevar = "日期", options=list(width=1150, height=550))
  })
  
  observeEvent(input$submit, {
    updateSelectInput(session, "city_summary",
      choices = input$city_selected
    )
  })
  
  observe({
    updateDateRangeInput(session, 'dates_summary',
      start = city_df()[1,]$日期,
      end = city_df()[dim(city_df())[1],]$日期,
      min = city_df()[1,]$日期, 
      max = city_df()[dim(city_df())[1],]$日期
    )
  })
  
  output$summary <- renderPrint({
    dataset <- city_df()[city_df()$city == input$city_summary,-1]
    timeintvl = seq(input$dates_motionchart[1], input$dates_motionchart[2], by = 'day')
    dataset = dataset[dataset$日期 %in% timeintvl,]
    dataset$AQI级别 = as.factor(dataset$AQI级别)  # AQI级别 as.factor
    summary(dataset)  
  })
})
