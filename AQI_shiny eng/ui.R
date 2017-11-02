library(shiny)
library(shinydashboard)

source("./main.R")

# cityNames_eng = c('Shanghai', 'Beijing', 'Guangzhou', 'Shenzhen', 'Hangzhou', 'Tianjin', 'Nanjing', 'Chengdu', 'Wuhan',
#               "Xi'an", 'Chongqin', 'Changsha', 'Zhuhai', 'Xiamen', 'Dalian', 'Qingdao', 'Ningbo',
#               'Fuzhou', 'Lanzhou', 'Nanning', 'Guiyang', 
#               'Haikou', 'Zhengzhou', 'Hefei', '沈阳', '济南', '太原', '昆明', '西宁', 
#               '南昌', '长春', '银川', '拉萨',
#               '呼和浩特', '乌鲁木齐', '石家庄', '哈尔滨')

cityNames = c('上海', '北京', '广州', '深圳', '杭州', '天津', '南京', '成都', '武汉',
              '西安', '重庆', '长沙', '珠海', '厦门', '大连', '青岛', '宁波',
              '福州', '兰州', '南宁', '贵阳', 
              '海口', '郑州', '合肥', '沈阳', '济南', '太原', '昆明', '西宁', 
              '南昌', '长春', '银川', '拉萨',
              '呼和浩特', '乌鲁木齐', '石家庄', '哈尔滨')



db_sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Information about AQI", tabName = 'info', icon = icon('info')),
    menuItem("Realtime data", tabName = "realtime", icon = icon('database'),
      menuSubItem('Table', tabName = 'realtime_table', icon = icon("table")),
      menuSubItem('Map', tabName = 'realtime_map', icon = icon('map'))
    ),
    menuItem("History data", tabName = "history", icon = icon('database'),
      checkboxGroupInput("city_selected", label = h5("Choose cities:"),
        # choiceNames = cityNames, choiceValues = cityNames,
        cityNames,
        inline = TRUE  # inline = TRUE for horizontal display
      ),
      actionButton("submit", label = "Submit"),
      menuSubItem('Table', tabName = 'history_table', icon = icon('table')),
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
          h4('About Air Quality Index (AQI)'),
          DT::dataTableOutput("table2") 
        )
      ),
      br(),
      fluidRow(
        box(title = 'More about AQI:', solidHeader = TRUE, width = 10, # status="info", 
          actionLink('link1', label= h5("Air Quality Index (AQI) Basics"),
                     onclick ="window.open('https://airnow.gov/index.cfm?action=aqibasics.aqi')"), # icon = icon("th")
          # br(),
          actionLink('link2', label= h5("Wikipeida"),
                     onclick = "window.open('https://en.wikipedia.org/wiki/Air_quality_index')")
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
          title = 'Distribution of AQI', 
          tabPanel("Bar plot", plotlyOutput('plot1')),
          tabPanel("Pie chart", plotlyOutput('plot2'))
        ),
        tabBox(width = 7, side = 'right',
          title = 'Distribution of pollutants', 
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
          checkboxGroupInput("city_motionchart", label = "Selected cities:")
        ),
        column(width = 4,
          dateRangeInput("dates_motionchart", label = "Time range")        
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
            selectInput('city_summary', 'Selected cities:', '')
          ),
          column(width = 4,
            dateRangeInput("dates_summary", label = "Time range")  
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

