library("rwunderground")
library("devtools")
rwunderground::set_api_key("8808b676dd1bd44f")
require("rvest")
weather_data<-read_html("https://www.wunderground.com/history/airport/WMKM/2017/12/17/DailyHistory.html")
mean_daily_temp<-weather_data %>%
         html_node(".wx-value") %>%
         html_text() %>%
         as.numeric()






f <- file('wunder-data.txt','w')
for (m in 1:12) {
        for (d in 1:31) {
                if (m==2) {
                        if (d>28){
                                break
                        }
                }
                else if (m %in% c(4,5,9,11)){
                        if (d > 30){
                                break
                        }
                }
                url <- paste("https://www.wunderground.com/history/airport/WMKM/2017/12/17/DailyHistory.html",                           as.character(m),"/",as.character(d),"/DailyHistory.html",sep = '')
                weather_data<-read_html(url)
                mean_daily_temp<-weather_data %>% 
                        html_node(".wx-value") %>%
                        html_text() %>%
                        as.numeric()
                if (nchar(m)<2){
                        mStamp <- paste('0',as.character(m),sep = '')
                } else{
                        mStamp<-as.character(m)
                }
                
                if (nchar(d)<2){
                        dStamp <- paste('0',as.character(d),sep='')
                } else {
                        dStamp <- as.character(d)
                }
                timestamp <- paste('2017',mStamp,dStamp,sep = '')
                timestampPlusTemp <- paste(timestamp,',',as.character(mean_daily_temp),sep = '')
                writeLines(timestampPlusTemp,f)
        }
}
close(f)