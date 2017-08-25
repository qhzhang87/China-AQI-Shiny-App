library(shiny)
library(shinydashboard)

source("./main.R")

cityNames = c('上海', '北京', '广州', '深圳', '杭州', '天津', '南京', '成都', '武汉',
              '西安', '重庆', '长沙', '珠海', '厦门', '大连', '青岛', '宁波',
              '福州', '兰州', '南宁', '贵阳', 
              '海口', '郑州', '合肥', '沈阳', '济南', '太原', '昆明', '西宁', 
              '南昌', '长春', '银川', '拉萨',
              '呼和浩特', '乌鲁木齐', '石家庄', '哈尔滨')


db_sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("相关知识", tabName = 'info', icon = icon('info')),
    menuItem("实时数据", tabName = "realtime", icon = icon('database'),
      menuSubItem('表格', tabName = 'realtime_table', icon = icon("table")),
      menuSubItem('交互地图', tabName = 'realtime_map', icon = icon('map'))
    ),
    menuItem("历史数据", tabName = "history", icon = icon('database'),
      checkboxGroupInput("city_selected", label = h5("选择城市:"),
        # choiceNames = cityNames, choiceValues = cityNames,
        cityNames,
        inline = TRUE  # inline = TRUE for horizontal display
      ),
      actionButton("submit", label = "提交"),
      menuSubItem('表格', tabName = 'history_table', icon = icon('table')),
      menuSubItem('Motion Chart', tabName = 'motionchart', icon = icon('line-chart')) ,
      menuSubItem('Summary', tabName = 'summary', icon = icon('bar-chart'))
    )
  )
)

db_body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'info',
      fluidRow(
        column(width = 10, # offset = 1,
          h4('空气质量指数（AQI）及相关信息'),
          DT::dataTableOutput("table2") 
        )
      ),
      br(),
      fluidRow(
        box(title = '想了解更多？', solidHeader = TRUE, width = 10, # status="info", 
          actionLink('link1', label= h5("空气质量指数维基百科"),
                     onclick ="window.open('https://zh.wikipedia.org/wiki/空气质量指数')"), # icon = icon("th")
          # br(),
          actionLink('link2', label= h5("中国空气质量指数维基百科"),
                     onclick = "window.open('https://zh.wikipedia.org/wiki/中国空气质量指数')")
        )
      )
    ),
    
    tabItem(tabName = "realtime_table",
      fluidRow(
        textOutput("text1"),
        DT::dataTableOutput("table1")
      ),
      br(),
      fluidRow(
        tabBox(width = 5,
          title = '空气质量指数分布', 
          tabPanel("柱状图", plotlyOutput('plot1')),
          tabPanel("饼状图", plotlyOutput('plot2'))
        ),
        tabBox(width = 7, side = 'right',
          title = '污染物分布', 
          tabPanel('',
            plotlyOutput('plot3')
          )
        )
        # box(
        #   title = '污染物分布', background = 'maroon', status = 'primary', solidHeader = TRUE, width = 7,
        #     plotOutput('plot2')
        # )
      )  
    ),
    
    tabItem(tabName = 'realtime_map',
      leafletOutput("realtimeMap", width="100%", height="600px")
    ),
    
    tabItem(tabName = 'history_table',
      textOutput("text2"),      
      DT::dataTableOutput("table3")
    ),
    
    tabItem(tabName = "motionchart",
      # wellPanel(
      #   checkboxGroupInput("city_motionchart",
      #     label = "已选择城市:"
      #   )
      # ),
      fluidRow(
        column(width = 5,
          checkboxGroupInput("city_motionchart", label = "已选择城市:")
        ),
        column(width = 4,
          dateRangeInput("dates_motionchart", label = "时间范围")        
        )
      ),
      fluidRow(
        h4("Need Flash to show the Motion Chart"),
        htmlOutput("motionchart")
      )
    ),
    
    tabItem('summary',
      fluidRow(
        fluidRow(
          column(width = 5,
            selectInput('city_summary', '已选择城市:', '')
          ),
          column(width = 4,
            dateRangeInput("dates_summary", label = "时间范围")  
          )
        ),
          verbatimTextOutput("summary")
      )
    )
  )
)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "China Air Quality Index(AQI)"),
    db_sidebar, 
    db_body 
  )
) 

