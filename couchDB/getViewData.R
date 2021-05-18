library(httr)

myviewURL = "http://172.26.132.19:5984/twitter/_design/analysis/_view/city?reduce=true&group_level=3"
viewData = GET(url = myviewURL, 
               authenticate("admin","admin")
               ) %>% content()
viewData = viewData$rows

periods = c("2020 1-3", "2020 4-6", "2020 10-12", "2021 1-3")
data = data.frame(time = c(0),
                  city = c(0),
                  posCount = c(0),
                  negCount = c(0),
                  neuCount = c(0),
                  polaritySum = c(0),
                  totalCount = c(0),
                  posPercent = c(0),
                  negPercent = c(0),
                  neuPercent = c(0),
                  polarityAvg = c(0))

for (i in 1:length(viewData)){
  row = viewData[[i]]
  keys = row$key
  time = keys[1][[1]]
  city = keys[2][[1]]
  feature = keys[3][[1]]
  value = row$value
  if(!city %in% data$city){
    for(j in 1:4){
      n = nrow(data)
      data[n+1,1]= periods[j]
      data[n+1,2]= city
    }
  }
  if(feature == "Polarity"){
    data[which(data$time==time & data$city==city),6] = value
  }else if(feature == "positiveCount"){
    data[which(data$time==time & data$city==city),3] = value
  }else if(feature == "negativeCount"){
    data[which(data$time==time & data$city==city),4] = value
  }else if(feature == "nuetralCount"){
    data[which(data$time==time & data$city==city),5] = value
  }
}
data = data[-1,]
data[is.na(data)] = 0
data$totalCount = data$posCount + data$negCount + data$neuCount
data$posPercent = data$posCount/data$totalCount
data$negPercent = data$negCount/data$totalCount
data$neuPercent = data$neuCount/data$totalCount
data$polarityAvg = data$polaritySum/data$totalCount
