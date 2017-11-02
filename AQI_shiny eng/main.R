library(rvest)
library(leafletCN)
library(leaflet)
library(googleVis)
library(plotly)

# Realtime Data Scraping --------------------------------------------------

get_AQI_realtime = function(){
  doc = try(read_html("http://www.pm25s.com/cn/rank/"), silent = TRUE)
  while (class(doc)[1]=="try-error") {
    doc = try(read_html("http://www.pm25s.com/cn/rank/"), silent = TRUE)
  }
  
  time = doc %>% html_nodes('.center') %>% .[1] %>% html_text()
  cities = doc %>% html_nodes(".cityrank a") %>% html_text() 
  
  info = doc %>% html_nodes("span[class^='lv']") %>% html_text() %>% .[-1] %>% split(rep(1:3))
  rank = info %>% .[[1]] %>% as.numeric()
  AQI = info %>% .[[2]] %>% as.numeric()
  AQL = info %>% .[[3]] %>% factor(levels = c('优', '良', '轻度污染', '中度污染', '重度污染', '严重污染'))
  levels(AQL) = c('Excellent', 'Good', 'Lightly Polluted', 'Moderately Polluted', 'Heavily Polluted', 'Severely Polluted')
  
  
  others = doc %>% html_nodes('.aqis') %>% html_text() %>% .[-seq(1,7)] %>% split(rep(1:7))
  PM2.5 = others %>% .[[1]] %>% as.numeric()
  PM10 = others %>% .[[2]] %>% as.numeric()
  CO = others %>% .[[3]] %>% as.numeric()
  NO2 = others %>% .[[4]] %>% as.numeric()
  SO2 = others %>% .[[5]] %>% as.numeric()
  O3_1h = others %>% .[[6]] %>% as.numeric()
  O3_8h = others %>% .[[7]] %>% as.numeric()
  
  dat = data.frame(city = cities, rank = rank, AQI = AQI, Level = AQL, 
                   PM2.5 = PM2.5, PM10 = PM10, CO = CO, NO2 = NO2, SO2 = SO2, O3_1h = O3_1h, O3_8h = O3_8h,
                   stringsAsFactors = FALSE)
  list(time, dat)
}

realtimeAQI = get_AQI_realtime()
update_time = realtimeAQI[[1]]
dat_AQI = realtimeAQI[[2]]
dat_AQI_levels = unique(na.omit(dat_AQI$Level))


# Leaflet Map -------------------------------------------------------------

load('./data/city_coordinate.RData')  # coordiantes of cities
dat_info = merge(geoinfo, dat_AQI, by = 'city')

## use the official AQI Level color
# cols = brewer.pal(8,"Set1")[c(3, 6, 5, 1, 4, 7)]  
# need library(RColorBrewer) to run brewer.pal, so I use the result directly
cols = c("#4DAF4A", "#FFFF33", "#FF7F00", "#E41A1C", "#984EA3", "#A65628")
# names(cols) = c('优', '良', '轻度污染', '中度污染', '重度污染', '严重污染')
names(cols) = c('Excellent', 'Good', 'Lightly Polluted', 'Moderately Polluted', 'Heavily Polluted', 'Severely Polluted')

map = leafletGeo("city", 
                 dat_AQI[c('city', 'Level')], 
                 valuevar = ~Level)

# color 
pal <- colorFactor(
  palette = cols,
  # domain = c('优', '良', '轻度污染', '中度污染', '重度污染', '严重污染'),
  domain = c('Excellent', 'Good', 'Lightly Polluted', 'Moderately Polluted', 'Heavily Polluted', 'Severely Polluted'),
  ordered = TRUE
)


# Bar plot of realtime data -----------------------------------------------

# g_bar <- ggplot(dat_AQI, aes(Level, fill=Level))
# g_bar <- g_bar + geom_bar() + theme(text = element_text(family = "STHeiti")) + 
#   labs(x = "空气质量指数级别", y = '个数', color = '空气质量指数级') +
#   scale_fill_manual(values = cols,
#                   breaks = c('优', '良', '轻度污染', '中度污染', '重度污染', '严重污染')) +
#   scale_x_discrete(limits = c('优', '良', '轻度污染', '中度污染', '重度污染', '严重污染'))

dat_AQI_count = as.data.frame(table(dat_AQI$Level))
names(dat_AQI_count) = c('level', 'count')

p_bar <- plot_ly(dat_AQI_count, x = ~level, y = ~count, type = "bar",
                 marker = list(color = cols)  #line = list(color = 'rgb(8,48,107)', width = 1.5)))
) %>% layout(margin = list(b = 95, r = 50), xaxis = list(title = "", tickangle = 45), yaxis = list(title='City count'))
# annotations = list(x = ~Var1, y = ~Freq, text = ~Freq,
#                xanchor = 'center', yanchor = 'bottom',
#                showarrow = FALSE))


# Pie chart realtime data -------------------------------------------------

p_pie <- plot_ly(dat_AQI_count, labels = ~level, values = ~count, type = 'pie', 
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 # insidetextfont = list(size = 14), 
                 marker = list(colors = cols, line = list(color = '#FFFFFF', width = 1))) %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



# Pollutants --------------------------------------------------------------

updatemens = list(
  list(
    type = "buttons",# direction = "right", xanchor = 'center', yanchor = "top",
    buttons = list(
      list( 
        label = "AQI",
        method = "update",
        args = list(list(visible = c(TRUE, rep(FALSE, 7))),
                    list(xaxis = list(title = "AQI")))     
      ),
      list(
        label = 'PM2.5',
        method = 'update',
        args = list(list(visible = c(FALSE, TRUE, rep(FALSE, 6))),
                    list(xaxis = list(title = "PM2.5")))
      ),
      list(
        label = 'PM10',
        method = 'update',
        args = list(list(visible = c(FALSE, FALSE, TRUE, rep(FALSE, 5))),
                    list(xaxis = list(title = "PM10")))     
      ),
      list(
        label = 'CO',
        method = 'update',
        args = list(list(visible = c(rep(FALSE,3), TRUE, rep(FALSE, 4)),
                         xaxis = list(title = "CO")))
      ),
      list(
        label = 'NO2',
        method = 'update',
        args = list(list(visible = c(rep(FALSE,4), TRUE, rep(FALSE, 3))),
                    list(xaxis = list(title = "NO2")))
      ),
      list(
        label = 'SO2',
        method = 'update',
        args = list(list(visible = c(rep(FALSE,5), TRUE, rep(FALSE, 2))),
                    list(xaxis = list(title = "SO2")))
      ),
      list(
        label = 'O3_1h',
        method = 'update',
        args = list(list(visible = c(rep(FALSE,6), TRUE, FALSE)),
                    list(xaxis = list(title = "O3_1h")))
      ),
      list(
        label = 'O3_8h',
        method = 'update',
        args = list(list(visible = c(rep(FALSE,7), TRUE)),
                    list(xaxis = list(title = "O3_8h")))
      )
    )
  )
)
p_pollutants <- plot_ly(dat_AQI, marker = list(color = '#1a81bc')) %>% 
  add_histogram(x = ~AQI, name='AQI') %>% 
  add_histogram(x = ~PM2.5, name='PM2.5', visible = FALSE) %>% 
  add_histogram(x = ~PM10, name='PM10', visible = FALSE) %>% 
  add_histogram(x = ~CO, name='CO', visible = FALSE) %>% 
  add_histogram(x = ~NO2, name='NO2', visible = FALSE) %>% 
  add_histogram(x = ~SO2, visible = FALSE) %>% 
  add_histogram(x = ~O3_1h, visible = FALSE) %>% 
  add_histogram(x = ~O3_8h, visible = FALSE) %>% 
  layout(showlegend=FALSE, updatemenus = updatemens)


# Scraping history data ---------------------------------------------------

cityNames = c('上海', '北京', '广州', '深圳', '杭州', '天津', '南京', '成都', '武汉',
              '西安', '重庆', '长沙', '珠海', '厦门', '大连', '青岛', '宁波',
              '福州', '兰州', '南宁', '贵阳', 
              '海口', '郑州', '合肥', '沈阳', '济南', '太原', '昆明', '西宁', 
              '南昌', '长春', '银川', '拉萨',
              '呼和浩特', '乌鲁木齐', '石家庄', '哈尔滨') 

library(utils)

get_AQI_history = function(city){
    giturl = 'https://github.com/qhzhang87/China-AQI/raw/master/History%20Data'
    # need to encode Chinese name to URL's percent-encoding, using URLencode() from package utils
    tbl_url = paste(giturl, paste0(URLencode(city), '.csv'), sep = '/')
    
    data = read.csv(tbl_url, stringsAsFactors = FALSE, na.strings = c('NA', ''))
    ########
    data$日期 = as.Date(data$日期) 
    names(data)[1] = 'Date'
    names(data)[3] = "AQI_Level"
    # data$AQI_Level = factor(data$AQI_Level, levels = c('优', '良', '轻度污染', '中度污染', '重度污染', '严重污染'))
    # levels(data$AQI_Level) = c('Excellent', 'Good', 'Lightly Polluted', 'Moderately Polluted', 'Heavily Polluted', 'Severely Polluted')
    names(data)[10] = "Rank"
    # 排名 换到前面？？？
    ##
    data$AQI_Level = as.factor(data$AQI_Level)
    data
}

table_color = function(df){
  cols2 = c("#4DAF4A", "#FFFF33", "#FF7F00", "#E41A1C", "#984EA3", "#A65628", '#FFFFFF')
  names(cols2) = c('优', '良', '轻度污染', '中度污染', '重度污染', '严重污染', NA)
  cols2[levels(df$AQI_Level)]
}



# time series plot ---------------------------------------------------------

tsAQI <-
  function(aqidata, index, side = 2, coef, output = FALSE, plot = TRUE){ 
    if(side == 1 & any(coef > 1 | coef < 0) ) stop("a needs to be between 0 and 1")
    if(side == 2 & any(coef %% 1 != 0 | coef < 0) ) stop("q needs to be positive integer")
    
    City = unique(aqidata$City)
    CityTableList=lapply(City, function(x) subset(aqidata,City == x))
    result = vector("list", length(City))
    names(result) = City
    filters = vector("list", length(coef))
    names(filters) = paste0("q = ", coef)
    for(i in seq_len(length(City))){
      if(side == 2){  # two-sided
        filters = lapply(seq_len(length(coef)), 
                         function(q) filter(CityTableList[[i]][,index], sides = 2, rep(1,(q*2+1))/(q*2+1)))
      } else {  # one-sided
        for(q in seq_len(length(coef))){
          filters[[q]][1] = CityTableList[[1]][,index][1]
          for(j in 2:nrow(CityTableList[[i]])){
            filters[[q]][j] = filters[[q]][j-1]*(1-coef[q]) + CityTableList[[1]][,index][j]*coef[q]
          }
        }
      }
      
      result[[i]] = filters
      
      if(plot){
        colq = rep(coef, rep(nrow(CityTableList[[i]]), length(coef)))
        df = cbind(CityTableList[[i]][,c(index, "City", "Date")], coef = as.factor(colq), 
                   filters = unlist(filters), row.names = NULL)
        
        par(ask = TRUE)
        print(
          ggplot(df, aes_string(x = "Date", y = index)) + geom_line(color = "gray25") + labs(title= City[i]) + 
            geom_path(aes(y = filters, color = coef, linetype = coef), na.rm = TRUE)
        )
        par(ask = FALSE)
      }
    }
    if(output) print(result)
  }
