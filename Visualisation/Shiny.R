
library(dplyr)
library(leaflet)
library(shiny)
library(ggplot2)
library(httr)
library(data.table)


myviewURL = "http://172.26.132.19:5984/twitter/_design/analysis/_view/suburb?reduce=true&group_level=4"
viewData = GET(url = myviewURL,
               authenticate("admin","admin")
) %>% content()

data<-data.frame(matrix(ncol = 5, nrow = 0))
colnames(data) <- c("time","city","suburb","feature","value")

for (i in 1:2209){
  data[i,]$time<- viewData$rows[[i]]$key[[1]]
  data[i,]$city<- viewData$rows[[i]]$key[[2]]
  data[i,]$suburb<- viewData$rows[[i]]$key[[3]]
  data[i,]$feature<- viewData$rows[[i]]$key[[4]]
  data[i,]$value<- viewData$rows[[i]]$value
}


data$value<- as.numeric(data$value)
data<- as.data.table(data)
data<- dcast.data.table(data, time + city + suburb ~ feature, value.var = "value", fun.aggregate = sum)

data$totalCount = data$positiveCount + data$negativeCount + data$nuetralCount
data$posPercent = data$positiveCount/data$totalCount
data$negPercent = data$negativeCount/data$totalCount
data$neuPercent = data$nuetralCount/data$totalCount
data$polarityAvg = data$Polarity/data$totalCount

names(data)[4]<- "polaritySum"
names(data)[5]<- "negCount"
names(data)[6]<- "neuCount"
names(data)[7]<- "posCount"

data11<- data


# Define UI 
ui <- fluidPage(
  selectInput('City', 
              h4('Please select a city'),
              c('AllCity',"Melbourne","Sydney","Brisbane","Perth","Adelaide"),
              selected = "AllCity"
  ),
  
  selectInput('Variable', 
              h4('Please select a scenario'),
              c("NULL","Jobseeker","Income","Sentiment"),
              selected = "NULL"
  ),
  
  selectInput('Season', 
              h4('Please select a quarter'),
              c("2020 1-3","2020 4-6","2020 10-12","2021 1-3"),
              selected = "NULL"
  ),
  
  tabsetPanel(id='my_tabsetPanel',
              tabPanel("Plot", 
                       fluidRow(
                         plotOutput("chart1"),
                         plotOutput("chart2")
                         )
                ),
              tabPanel('Map',
                       leafletOutput('map1')   
              )
              )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$map1 <- renderLeaflet({
    
    
    if(input$City == "AllCity"& input$Variable == "NULL"){
      leaflet() %>% 
        setView(lng =133.2092 ,lat = -28,zoom = 5 ) %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data1,stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "green"
        ) %>% 
        addPolygons(data= data2,stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "green"
                    #,fillColor = ~Bpal(map@data$value)
                    # ,label = ~paste0(BoroName, ": ", count)
                    
        ) %>% 
        addPolygons(data= data3,stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "green"
                    #,fillColor = ~Bpal(map@data$value)
                    # ,label = ~paste0(BoroName, ": ", count)
                    
        ) %>% 
        addPolygons(data= data4,stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "green"
                    #,fillColor = ~Bpal(map@data$value)
                    # ,label = ~paste0(BoroName, ": ", count)
                    
        ) %>% 
        addPolygons(data= data5,stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "green"
                    #,fillColor = ~Bpal(map@data$value)
                    # ,label = ~paste0(BoroName, ": ", count)
                    
        )
      
    }
    else if(input$City == "Melbourne" & input$Variable == "NULL") {
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data1, smoothFactor = 0.3, fillOpacity = 0.8
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal1(0),popup = ~htmltools::htmlEscape(sa2_name)
        ) }
    
    else if(input$City == "Melbourne" &
            input$Variable == "Jobseeker" &
            input$Season != "2020 4-6"
    ) {
      data1 <- sp::merge(data1,data11[data11$time==input$Season,],by.x ="sa2_name",by.y = "suburb")
      data1$polarityAvg[is.na(data1$polarityAvg)] <- 0
      Bpal1 <- colorNumeric(palette = "Reds", data1@data$jobseeker0)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data1, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal1(data1@data$jobseeker0)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/>",
                                    sa2_name,
                                    ifelse(input$Season=="2020 1-3","First Quarter",
                                           ifelse(input$Season=="2020 10-12","Fourth Quarter","First Quarter 2021")
                                    ),
                                    paste("Jobseeker/population:",
                                          round(ifelse(is.na(jobseeker0/population),
                                                       0,jobseeker0/population*100)),"%")
                    )
                    
        ) }  
    
    else if(input$City == "Melbourne"  & 
            input$Variable == "Jobseeker" & 
            input$Season == "2020 4-6") {
      data1 <- sp::merge(data1,data11[data11$time=="2020 1-3",],by.x ="sa2_name",by.y = "suburb")
      
      data1 <- sp::merge(data1,data11[data11$time=="2020 4-6",],by.x ="sa2_name",by.y = "suburb")
      data1$polarityAvg.x[is.na(data1$polarityAvg.x)] <- 0
      Bpal1 <- colorNumeric(palette = "Reds", data1@data$jobseeker6)
      #data[is.na(data1@data)] <- 0
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data1, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal1(data1@data$jobseeker6)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/><br/>",
                                    paste(  sep = "<br/>",  
                                            sa2_name,
                                            "First Quarter" ,
                                            paste("Jobseeker/population:",round(ifelse(is.na(jobseeker0/population),
                                                                                       0,jobseeker0/population*100)),"%")),
                                    paste(  sep = "<br/>",  
                                            "Second Quarter" ,
                                            paste("Jobseeker/population:",round(ifelse(is.na(jobseeker6/population),
                                                                                       0,jobseeker6/population*100)),"%"))
                                    
                    )
                    
                    
        )
      
      
    }   
    
    else if(input$City == "Melbourne" &
            input$Variable == "Income" 
            
    ) {
      data1 <- sp::merge(data1,data11[data11$time==input$Season,],by.x ="sa2_name",by.y = "suburb")
      data1$polarityAvg[is.na(data1$polarityAvg)] <- 0
      data1$income[is.na(data1$income)] <- 0
      Bpal1 <- colorNumeric(palette = "Reds", data1@data$income)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data1, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal1(data1@data$income)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/>",
                                    sa2_name,
                                    paste("Median_income:",
                                          round(income)),
                                    paste("Avg_Sentiment_Score:",
                                          round(ifelse(is.na(polarityAvg),0,polarityAvg)*100),"%")
                    )
                    
        ) }  
    
    else if(input$City == "Melbourne" &
            input$Variable == "Sentiment" &
            input$Season == "2020 1-3"
    ) {
      data1 <- sp::merge(data1,data11[data11$time=="2020 1-3",],by.x ="sa2_name",by.y = "suburb")
      data1$polarityAvg[is.na(data1$polarityAvg)] <- 0
      data1$polarityAvg <-ifelse(data1$polarityAvg<0,0,data1$polarityAvg)
      data1$posPercent[is.na(data1$posPercent)] <- 0
      data1$negPercent[is.na(data1$negPercent)] <- 0
      data1$neuPercent[is.na(data1$neuPercent)] <- 0
      Bpal1 <- colorNumeric(palette = "Reds", data1@data$polarityAvg)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data1, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal1(data1@data$polarityAvg)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/>",
                                    sa2_name,
                                    "First Quarter:",
                                    paste("Positive:",
                                          round(posPercent*100),"%"),
                                    paste("Negative:",
                                          round(negPercent*100),"%"),
                                    paste("Neutral:",
                                          round(neuPercent*100),"%"),
                                    paste("Avg_Sentiment_Score:",
                                          round(polarityAvg*100),"%"),
                                    paste("Tweets Count:",
                                          totalCount)
                    )
                    
        ) }  
    
    
    else if(input$City == "Melbourne" &
            input$Variable == "Sentiment" &
            input$Season == "2020 4-6"
    ) {
      data1 <- sp::merge(data1,data11[data11$time=="2020 1-3",],by.x ="sa2_name",by.y = "suburb")
      
      data1 <- sp::merge(data1,data11[data11$time=="2020 4-6",],by.x ="sa2_name",by.y = "suburb")
      data1$polarityAvg.x[is.na(data1$polarityAvg.x)] <- 0
      data1$polarityAvg.x <-ifelse(data1$polarityAvg.x<0,0,data1$polarityAvg.x)
      data1$posPercent.x[is.na(data1$posPercent.x)] <- 0
      data1$negPercent.x[is.na(data1$negPercent.x)] <- 0
      data1$neuPercent.x[is.na(data1$neuPercent.x)] <- 0
      data1$polarityAvg.y[is.na(data1$polarityAvg.y)] <- 0
      data1$polarityAvg.y <-ifelse(data1$polarityAvg.y<0,0,data1$polarityAvg.y)
      data1$posPercent.y[is.na(data1$posPercent.y)] <- 0
      data1$negPercent.y[is.na(data1$negPercent.y)] <- 0
      data1$neuPercent.y[is.na(data1$neuPercent.y)] <- 0
      
      Bpal1 <- colorNumeric(palette = "Reds", data1@data$polarityAvg.y)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data1, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal1(data1@data$polarityAvg.y)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/><br/>",
                                    sa2_name,
                                    paste(sep = "<br/>",
                                          "First Quarter:",
                                          paste("Positive:",
                                                round(posPercent.x*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.x*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.x*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.x*100),"%"),
                                          paste("Tweets Count:",
                                                totalCount.x)
                                    ),
                                    paste(sep = "<br/>",
                                          "Second Quarter:",
                                          paste("Positive:",
                                                round(posPercent.y*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.y*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.y*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.y*100),"%"),
                                          paste("Tweets Count:",
                                                totalCount.y)
                                    )
                                    
                    )
                    
        ) }   
    
    else if(input$City == "Melbourne" &
            input$Variable == "Sentiment" &
            input$Season == "2020 10-12"
    ) {
      data1 <- sp::merge(data1,data11[data11$time=="2020 4-6",],by.x ="sa2_name",by.y = "suburb")
      
      data1 <- sp::merge(data1,data11[data11$time=="2020 10-12",],by.x ="sa2_name",by.y = "suburb")
      data1$polarityAvg.x[is.na(data1$polarityAvg.x)] <- 0
      data1$polarityAvg.x <-ifelse(data1$polarityAvg.x<0,0,data1$polarityAvg.x)
      data1$posPercent.x[is.na(data1$posPercent.x)] <- 0
      data1$negPercent.x[is.na(data1$negPercent.x)] <- 0
      data1$neuPercent.x[is.na(data1$neuPercent.x)] <- 0
      data1$polarityAvg.y[is.na(data1$polarityAvg.y)] <- 0
      data1$polarityAvg.y <-ifelse(data1$polarityAvg.y<0,0,data1$polarityAvg.y)
      data1$posPercent.y[is.na(data1$posPercent.y)] <- 0
      data1$negPercent.y[is.na(data1$negPercent.y)] <- 0
      data1$neuPercent.y[is.na(data1$neuPercent.y)] <- 0
      Bpal1 <- colorNumeric(palette = "Reds", data1@data$polarityAvg.y)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data1, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal1(data1@data$polarityAvg.y)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/><br/>",
                                    sa2_name,
                                    paste(sep = "<br/>",
                                          "Second Quarter:",
                                          paste("Positive:",
                                                round(posPercent.x*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.x*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.x*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.x*100),"%"),
                                          paste("Tweets Count:",
                                                totalCount.x)
                                    ),
                                    paste(sep = "<br/>",
                                          "Fourth Quarter:",
                                          paste("Positive:",
                                                round(posPercent.y*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.y*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.y*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.y*100),"%"),
                                          paste("Tweets Count:",
                                                totalCount.y)
                                    )
                                    
                    )
                    
        ) }   
    
    else if(input$City == "Melbourne" &
            input$Variable == "Sentiment" &
            input$Season == "2021 1-3"
    ) {
      data1 <- sp::merge(data1,data11[data11$time=="2020 10-12",],by.x ="sa2_name",by.y = "suburb")
      
      data1 <- sp::merge(data1,data11[data11$time=="2021 1-3",],by.x ="sa2_name",by.y = "suburb")
      data1$polarityAvg.x[is.na(data1$polarityAvg.x)] <- 0
      data1$polarityAvg.x <-ifelse(data1$polarityAvg.x<0,0,data1$polarityAvg.x)
      data1$posPercent.x[is.na(data1$posPercent.x)] <- 0
      data1$negPercent.x[is.na(data1$negPercent.x)] <- 0
      data1$neuPercent.x[is.na(data1$neuPercent.x)] <- 0
      data1$polarityAvg.y[is.na(data1$polarityAvg.y)] <- 0
      data1$polarityAvg.y <-ifelse(data1$polarityAvg.y<0,0,data1$polarityAvg.y)
      data1$posPercent.y[is.na(data1$posPercent.y)] <- 0
      data1$negPercent.y[is.na(data1$negPercent.y)] <- 0
      data1$neuPercent.y[is.na(data1$neuPercent.y)] <- 0
      Bpal1 <- colorNumeric(palette = "Reds", data1@data$polarityAvg.y)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data1, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal1(data1@data$polarityAvg.y)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/><br/>",
                                    sa2_name,
                                    paste(sep = "<br/>",
                                          "Fourth Quarter:",
                                          paste("Positive:",
                                                round(posPercent.x*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.x*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.x*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.x*100),"%"),
                                          paste("Tweets Count:",
                                                totalCount.x)
                                    ),
                                    paste(sep = "<br/>",
                                          "First Quarter of 2021:",
                                          paste("Positive:",
                                                round(posPercent.y*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.y*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.y*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.y*100),"%"),
                                          paste("Tweets Count:",
                                                totalCount.y)
                                    )
                                    
                    )
                    
        ) }   
    
    
    else if(input$City == "Sydney" & input$Variable == "NULL") {
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data2, smoothFactor = 0.3, fillOpacity = 0.8
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal2(0),popup = ~htmltools::htmlEscape(sa2_name)
        ) }
    
    else if(input$City == "Sydney" &
            input$Variable == "Jobseeker" &
            input$Season != "2020 4-6"
    ) {
      data2 <- sp::merge(data2,data11[data11$time==input$Season,],by.x ="sa2_name",by.y = "suburb")
      data2$polarityAvg[is.na(data2$polarityAvg)] <- 0
      Bpal2 <- colorNumeric(palette = "Reds", data2@data$jobseeker0)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data2, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal2(data2@data$jobseeker0)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/>",
                                    sa2_name,
                                    ifelse(input$Season=="2020 1-3","First Quarter",
                                           ifelse(input$Season=="2020 10-12","Fourth Quarter","First Quarter 2021")
                                    ),
                                    paste("Jobseeker/population:",
                                          round(ifelse(is.na(jobseeker0/population),
                                                       0,jobseeker0/population*100)),"%")
                    )
                    
        ) }  
    
    else if(input$City == "Sydney"  & 
            input$Variable == "Jobseeker" & 
            input$Season == "2020 4-6") {
      data2 <- sp::merge(data2,data11[data11$time=="2020 1-3",],by.x ="sa2_name",by.y = "suburb")
      
      data2 <- sp::merge(data2,data11[data11$time=="2020 4-6",],by.x ="sa2_name",by.y = "suburb")
      data2$polarityAvg.x[is.na(data2$polarityAvg.x)] <- 0
      Bpal2 <- colorNumeric(palette = "Reds", data2@data$jobseeker6)
      #data[is.na(data2@data)] <- 0
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data2, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal2(data2@data$jobseeker6)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/><br/>",
                                    paste(  sep = "<br/>",  
                                            sa2_name,
                                            "First Quarter" ,
                                            paste("Jobseeker/population:",round(ifelse(is.na(jobseeker0/population),
                                                                                       0,jobseeker0/population*100)),"%")
                                            ),
                                    paste(  sep = "<br/>",  
                                            "Second Quarter" ,
                                            paste("Jobseeker/population:",round(ifelse(is.na(jobseeker6/population),
                                                                                       0,jobseeker6/population*100)),"%"))
                                    
                    )
                    
                    
        )
      
      
    }   
    
    else if(input$City == "Sydney" &
            input$Variable == "Income" 
            
    ) {
      data2 <- sp::merge(data2,data11[data11$time==input$Season,],by.x ="sa2_name",by.y = "suburb")
      data2$polarityAvg[is.na(data2$polarityAvg)] <- 0
      data2$income[is.na(data2$income)] <- 0
      Bpal2 <- colorNumeric(palette = "Reds", data2@data$income)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data2, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal2(data2@data$income)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/>",
                                    sa2_name,
                                    paste("Median_income:",
                                          round(income)),
                                    paste("Avg_Sentiment_Score:",
                                          round(ifelse(is.na(polarityAvg),0,polarityAvg)*100),"%")
                    )
                    
        ) }  
    
    else if(input$City == "Sydney" &
            input$Variable == "Sentiment" &
            input$Season == "2020 1-3"
    ) {
      data2 <- sp::merge(data2,data11[data11$time=="2020 1-3",],by.x ="sa2_name",by.y = "suburb")
      data2$polarityAvg[is.na(data2$polarityAvg)] <- 0
      data2$polarityAvg <-ifelse(data2$polarityAvg<0,0,data2$polarityAvg)
      data2$posPercent[is.na(data2$posPercent)] <- 0
      data2$negPercent[is.na(data2$negPercent)] <- 0
      data2$neuPercent[is.na(data2$neuPercent)] <- 0
      Bpal2 <- colorNumeric(palette = "Reds", data2@data$polarityAvg)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data2, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal2(data2@data$polarityAvg)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/>",
                                    sa2_name,
                                    "First Quarter:",
                                    paste("Positive:",
                                          round(posPercent*100),"%"),
                                    paste("Negative:",
                                          round(negPercent*100),"%"),
                                    paste("Neutral:",
                                          round(neuPercent*100),"%"),
                                    
                                    paste("Avg_Sentiment_Score:",
                                          round(polarityAvg*100),"%")
                    )
                    
        ) }  
    
    
    else if(input$City == "Sydney" &
            input$Variable == "Sentiment" &
            input$Season == "2020 4-6"
    ) {
      data2 <- sp::merge(data2,data11[data11$time=="2020 1-3",],by.x ="sa2_name",by.y = "suburb")
      
      data2 <- sp::merge(data2,data11[data11$time=="2020 4-6",],by.x ="sa2_name",by.y = "suburb")
      data2$polarityAvg.x[is.na(data2$polarityAvg.x)] <- 0
      data2$polarityAvg.x <-ifelse(data2$polarityAvg.x<0,0,data2$polarityAvg.x)
      data2$posPercent.x[is.na(data2$posPercent.x)] <- 0
      data2$negPercent.x[is.na(data2$negPercent.x)] <- 0
      data2$neuPercent.x[is.na(data2$neuPercent.x)] <- 0
      data2$polarityAvg.y[is.na(data2$polarityAvg.y)] <- 0
      data2$polarityAvg.y <-ifelse(data2$polarityAvg.y<0,0,data2$polarityAvg.y)
      data2$posPercent.y[is.na(data2$posPercent.y)] <- 0
      data2$negPercent.y[is.na(data2$negPercent.y)] <- 0
      data2$neuPercent.y[is.na(data2$neuPercent.y)] <- 0
      
      Bpal2 <- colorNumeric(palette = "Reds", data2@data$polarityAvg.y)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data2, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal2(data2@data$polarityAvg.y)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/><br/>",
                                    sa2_name,
                                    paste(sep = "<br/>",
                                          "First Quarter:",
                                          paste("Positive:",
                                                round(posPercent.x*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.x*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.x*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.x*100),"%")
                                    ),
                                    paste(sep = "<br/>",
                                          "Second Quarter:",
                                          paste("Positive:",
                                                round(posPercent.y*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.y*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.y*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.y*100),"%")
                                    )
                                    
                    )
                    
        ) }   
    
    else if(input$City == "Sydney" &
            input$Variable == "Sentiment" &
            input$Season == "2020 10-12"
    ) {
      data2 <- sp::merge(data2,data11[data11$time=="2020 4-6",],by.x ="sa2_name",by.y = "suburb")
      
      data2 <- sp::merge(data2,data11[data11$time=="2020 10-12",],by.x ="sa2_name",by.y = "suburb")
      data2$polarityAvg.x[is.na(data2$polarityAvg.x)] <- 0
      data2$polarityAvg.x <-ifelse(data2$polarityAvg.x<0,0,data2$polarityAvg.x)
      data2$posPercent.x[is.na(data2$posPercent.x)] <- 0
      data2$negPercent.x[is.na(data2$negPercent.x)] <- 0
      data2$neuPercent.x[is.na(data2$neuPercent.x)] <- 0
      data2$polarityAvg.y[is.na(data2$polarityAvg.y)] <- 0
      data2$polarityAvg.y <-ifelse(data2$polarityAvg.y<0,0,data2$polarityAvg.y)
      data2$posPercent.y[is.na(data2$posPercent.y)] <- 0
      data2$negPercent.y[is.na(data2$negPercent.y)] <- 0
      data2$neuPercent.y[is.na(data2$neuPercent.y)] <- 0
      Bpal2 <- colorNumeric(palette = "Reds", data2@data$polarityAvg.y)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data2, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal2(data2@data$polarityAvg.y)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/><br/>",
                                    sa2_name,
                                    paste(sep = "<br/>",
                                          "Second Quarter:",
                                          paste("Positive:",
                                                round(posPercent.x*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.x*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.x*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.x*100),"%")
                                    ),
                                    paste(sep = "<br/>",
                                          "Fourth Quarter:",
                                          paste("Positive:",
                                                round(posPercent.y*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.y*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.y*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.y*100),"%")
                                    )
                                    
                    )
                    
        ) }   
    
    else if(input$City == "Sydney" &
            input$Variable == "Sentiment" &
            input$Season == "2021 1-3"
    ) {
      data2 <- sp::merge(data2,data11[data11$time=="2020 10-12",],by.x ="sa2_name",by.y = "suburb")
      
      data2 <- sp::merge(data2,data11[data11$time=="2021 1-3",],by.x ="sa2_name",by.y = "suburb")
      data2$polarityAvg.x[is.na(data2$polarityAvg.x)] <- 0
      data2$polarityAvg.x <-ifelse(data2$polarityAvg.x<0,0,data2$polarityAvg.x)
      data2$posPercent.x[is.na(data2$posPercent.x)] <- 0
      data2$negPercent.x[is.na(data2$negPercent.x)] <- 0
      data2$neuPercent.x[is.na(data2$neuPercent.x)] <- 0
      data2$polarityAvg.y[is.na(data2$polarityAvg.y)] <- 0
      data2$polarityAvg.y <-ifelse(data2$polarityAvg.y<0,0,data2$polarityAvg.y)
      data2$posPercent.y[is.na(data2$posPercent.y)] <- 0
      data2$negPercent.y[is.na(data2$negPercent.y)] <- 0
      data2$neuPercent.y[is.na(data2$neuPercent.y)] <- 0
      Bpal2 <- colorNumeric(palette = "Reds", data2@data$polarityAvg.y)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data2, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal2(data2@data$polarityAvg.y)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/><br/>",
                                    sa2_name,
                                    paste(sep = "<br/>",
                                          "Fourth Quarter:",
                                          paste("Positive:",
                                                round(posPercent.x*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.x*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.x*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.x*100),"%")
                                    ),
                                    paste(sep = "<br/>",
                                          "First Quarter of 2021:",
                                          paste("Positive:",
                                                round(posPercent.y*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.y*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.y*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.y*100),"%")
                                    )
                                    
                    )
                    
        ) }  
    
    
    
    else if(input$City == "Brisbane" & input$Variable == "NULL") {
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data3, smoothFactor = 0.3, fillOpacity = 0.8
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal3(0),popup = ~htmltools::htmlEscape(sa2_name)
        ) }
    
    else if(input$City == "Brisbane" &
            input$Variable == "Jobseeker" &
            input$Season != "2020 4-6"
    ) {
      data3 <- sp::merge(data3,data11[data11$time==input$Season,],by.x ="sa2_name",by.y = "suburb")
      data3$polarityAvg[is.na(data3$polarityAvg)] <- 0
      Bpal3 <- colorNumeric(palette = "Reds", data3@data$jobseeker0)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data3, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal3(data3@data$jobseeker0)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/>",
                                    sa2_name,
                                    ifelse(input$Season=="2020 1-3","First Quarter",
                                           ifelse(input$Season=="2020 10-12","Fourth Quarter","First Quarter 2021")
                                    ),
                                    paste("Jobseeker/population:",
                                          round(ifelse(is.na(jobseeker0/population),
                                                       0,jobseeker0/population*100)),"%"),
                                    paste("Avg_Sentiment_Score:",
                                          round(ifelse(is.na(polarityAvg),
                                                       0,polarityAvg)),"%")
                    )
                    
        ) }  
    
    else if(input$City == "Brisbane"  & 
            input$Variable == "Jobseeker" & 
            input$Season == "2020 4-6") {
      data3 <- sp::merge(data3,data11[data11$time=="2020 1-3",],by.x ="sa2_name",by.y = "suburb")
      
      data3 <- sp::merge(data3,data11[data11$time=="2020 4-6",],by.x ="sa2_name",by.y = "suburb")
      data3$polarityAvg.x[is.na(data3$polarityAvg.x)] <- 0
      Bpal3 <- colorNumeric(palette = "Reds", data3@data$jobseeker6)
      #data[is.na(data3@data)] <- 0
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data3, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal3(data3@data$jobseeker6)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/><br/>",
                                    paste(  sep = "<br/>",  
                                            sa2_name,
                                            "First Quarter" ,
                                            paste("Jobseeker/population:",round(ifelse(is.na(jobseeker0/population),
                                                                                       0,jobseeker0/population*100)),"%"),
                                            
                                            paste("Avg_Sentiment_Score:"
                                                  ,round(ifelse(is.na(polarityAvg.x),
                                                                0,polarityAvg.x)*100),"%")),
                                    paste(  sep = "<br/>",  
                                            "Second Quarter" ,
                                            paste("Jobseeker/population:",round(ifelse(is.na(jobseeker6/population),
                                                                                       0,jobseeker6/population*100)),"%"),
                                            
                                            paste("Avg_Sentiment_Score:"
                                                  ,round(ifelse(is.na(polarityAvg.y),
                                                                0,polarityAvg.y)*100),"%"))
                                    
                    )
                    
                    
        )
      
      
    }   
    
    else if(input$City == "Brisbane" &
            input$Variable == "Income" 
            
    ) {
      data3 <- sp::merge(data3,data11[data11$time==input$Season,],by.x ="sa2_name",by.y = "suburb")
      data3$polarityAvg[is.na(data3$polarityAvg)] <- 0
      data3$income[is.na(data3$income)] <- 0
      Bpal3 <- colorNumeric(palette = "Reds", data3@data$income)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data3, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal3(data3@data$income)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/>",
                                    sa2_name,
                                    paste("Median_income:",
                                          round(income)),
                                    paste("Avg_Sentiment_Score:",
                                          round(ifelse(is.na(polarityAvg),0,polarityAvg)*100),"%")
                    )
                    
        ) }  
    
    else if(input$City == "Brisbane" &
            input$Variable == "Sentiment" &
            input$Season == "2020 1-3"
    ) {
      data3 <- sp::merge(data3,data11[data11$time=="2020 1-3",],by.x ="sa2_name",by.y = "suburb")
      data3$polarityAvg[is.na(data3$polarityAvg)] <- 0
      data3$polarityAvg <-ifelse(data3$polarityAvg<0,0,data3$polarityAvg)
      data3$posPercent[is.na(data3$posPercent)] <- 0
      data3$negPercent[is.na(data3$negPercent)] <- 0
      data3$neuPercent[is.na(data3$neuPercent)] <- 0
      Bpal3 <- colorNumeric(palette = "Reds", data3@data$polarityAvg)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data3, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal3(data3@data$polarityAvg)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/>",
                                    sa2_name,
                                    "First Quarter:",
                                    paste("Positive:",
                                          round(posPercent*100),"%"),
                                    paste("Negative:",
                                          round(negPercent*100),"%"),
                                    paste("Neutral:",
                                          round(neuPercent*100),"%"),
                                    
                                    paste("Avg_Sentiment_Score:",
                                          round(polarityAvg*100),"%")
                    )
                    
        ) }  
    
    
    else if(input$City == "Brisbane" &
            input$Variable == "Sentiment" &
            input$Season == "2020 4-6"
    ) {
      data3 <- sp::merge(data3,data11[data11$time=="2020 1-3",],by.x ="sa2_name",by.y = "suburb")
      
      data3 <- sp::merge(data3,data11[data11$time=="2020 4-6",],by.x ="sa2_name",by.y = "suburb")
      data3$polarityAvg.x[is.na(data3$polarityAvg.x)] <- 0
      data3$polarityAvg.x <-ifelse(data3$polarityAvg.x<0,0,data3$polarityAvg.x)
      data3$posPercent.x[is.na(data3$posPercent.x)] <- 0
      data3$negPercent.x[is.na(data3$negPercent.x)] <- 0
      data3$neuPercent.x[is.na(data3$neuPercent.x)] <- 0
      data3$polarityAvg.y[is.na(data3$polarityAvg.y)] <- 0
      data3$polarityAvg.y <-ifelse(data3$polarityAvg.y<0,0,data3$polarityAvg.y)
      data3$posPercent.y[is.na(data3$posPercent.y)] <- 0
      data3$negPercent.y[is.na(data3$negPercent.y)] <- 0
      data3$neuPercent.y[is.na(data3$neuPercent.y)] <- 0
      
      Bpal3 <- colorNumeric(palette = "Reds", data3@data$polarityAvg.y)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data3, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal3(data3@data$polarityAvg.y)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/><br/>",
                                    sa2_name,
                                    paste(sep = "<br/>",
                                          "First Quarter:",
                                          paste("Positive:",
                                                round(posPercent.x*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.x*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.x*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.x*100),"%")
                                    ),
                                    paste(sep = "<br/>",
                                          "Second Quarter:",
                                          paste("Positive:",
                                                round(posPercent.y*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.y*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.y*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.y*100),"%")
                                    )
                                    
                    )
                    
        ) }   
    
    else if(input$City == "Brisbane" &
            input$Variable == "Sentiment" &
            input$Season == "2020 10-12"
    ) {
      data3 <- sp::merge(data3,data11[data11$time=="2020 4-6",],by.x ="sa2_name",by.y = "suburb")
      
      data3 <- sp::merge(data3,data11[data11$time=="2020 10-12",],by.x ="sa2_name",by.y = "suburb")
      data3$polarityAvg.x[is.na(data3$polarityAvg.x)] <- 0
      data3$polarityAvg.x <-ifelse(data3$polarityAvg.x<0,0,data3$polarityAvg.x)
      data3$posPercent.x[is.na(data3$posPercent.x)] <- 0
      data3$negPercent.x[is.na(data3$negPercent.x)] <- 0
      data3$neuPercent.x[is.na(data3$neuPercent.x)] <- 0
      data3$polarityAvg.y[is.na(data3$polarityAvg.y)] <- 0
      data3$polarityAvg.y <-ifelse(data3$polarityAvg.y<0,0,data3$polarityAvg.y)
      data3$posPercent.y[is.na(data3$posPercent.y)] <- 0
      data3$negPercent.y[is.na(data3$negPercent.y)] <- 0
      data3$neuPercent.y[is.na(data3$neuPercent.y)] <- 0
      Bpal3 <- colorNumeric(palette = "Reds", data3@data$polarityAvg.y)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data3, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal3(data3@data$polarityAvg.y)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/><br/>",
                                    sa2_name,
                                    paste(sep = "<br/>",
                                          "Second Quarter:",
                                          paste("Positive:",
                                                round(posPercent.x*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.x*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.x*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.x*100),"%")
                                    ),
                                    paste(sep = "<br/>",
                                          "Fourth Quarter:",
                                          paste("Positive:",
                                                round(posPercent.y*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.y*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.y*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.y*100),"%")
                                    )
                                    
                    )
                    
        ) }   
    
    else if(input$City == "Brisbane" &
            input$Variable == "Sentiment" &
            input$Season == "2021 1-3"
    ) {
      data3 <- sp::merge(data3,data11[data11$time=="2020 10-12",],by.x ="sa2_name",by.y = "suburb")
      
      data3 <- sp::merge(data3,data11[data11$time=="2021 1-3",],by.x ="sa2_name",by.y = "suburb")
      data3$polarityAvg.x[is.na(data3$polarityAvg.x)] <- 0
      data3$polarityAvg.x <-ifelse(data3$polarityAvg.x<0,0,data3$polarityAvg.x)
      data3$posPercent.x[is.na(data3$posPercent.x)] <- 0
      data3$negPercent.x[is.na(data3$negPercent.x)] <- 0
      data3$neuPercent.x[is.na(data3$neuPercent.x)] <- 0
      data3$polarityAvg.y[is.na(data3$polarityAvg.y)] <- 0
      data3$polarityAvg.y <-ifelse(data3$polarityAvg.y<0,0,data3$polarityAvg.y)
      data3$posPercent.y[is.na(data3$posPercent.y)] <- 0
      data3$negPercent.y[is.na(data3$negPercent.y)] <- 0
      data3$neuPercent.y[is.na(data3$neuPercent.y)] <- 0
      Bpal3 <- colorNumeric(palette = "Reds", data3@data$polarityAvg.y)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data3, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal3(data3@data$polarityAvg.y)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/><br/>",
                                    sa2_name,
                                    paste(sep = "<br/>",
                                          "Fourth Quarter:",
                                          paste("Positive:",
                                                round(posPercent.x*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.x*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.x*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.x*100),"%")
                                    ),
                                    paste(sep = "<br/>",
                                          "First Quarter of 2021:",
                                          paste("Positive:",
                                                round(posPercent.y*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.y*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.y*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.y*100),"%")
                                    )
                                    
                    )
                    
        ) }       
    
    
    
    
    
    else if(input$City == "Perth" & input$Variable == "NULL") {
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data4, smoothFactor = 0.3, fillOpacity = 0.8
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal4(0),popup = ~htmltools::htmlEscape(sa2_name)
        ) }
    
    else if(input$City == "Perth" &
            input$Variable == "Jobseeker" &
            input$Season != "2020 4-6"
    ) {
      data4 <- sp::merge(data4,data11[data11$time==input$Season,],by.x ="sa2_name",by.y = "suburb")
      data4$polarityAvg[is.na(data4$polarityAvg)] <- 0
      Bpal4 <- colorNumeric(palette = "Reds", data4@data$jobseeker0)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data4, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal4(data4@data$jobseeker0)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/>",
                                    sa2_name,
                                    ifelse(input$Season=="2020 1-3","First Quarter",
                                           ifelse(input$Season=="2020 10-12","Fourth Quarter","First Quarter 2021")
                                    ),
                                    paste("Jobseeker/population:",
                                          round(ifelse(is.na(jobseeker0/population),
                                                       0,jobseeker0/population*100)),"%"),
                                    paste("Avg_Sentiment_Score:",
                                          round(ifelse(is.na(polarityAvg),
                                                       0,polarityAvg)),"%")
                    )
                    
        ) }  
    
    else if(input$City == "Perth"  & 
            input$Variable == "Jobseeker" & 
            input$Season == "2020 4-6") {
      data4 <- sp::merge(data4,data11[data11$time=="2020 1-3",],by.x ="sa2_name",by.y = "suburb")
      
      data4 <- sp::merge(data4,data11[data11$time=="2020 4-6",],by.x ="sa2_name",by.y = "suburb")
      data4$polarityAvg.x[is.na(data4$polarityAvg.x)] <- 0
      Bpal4 <- colorNumeric(palette = "Reds", data4@data$jobseeker6)
      #data[is.na(data4@data)] <- 0
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data4, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal4(data4@data$jobseeker6)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/><br/>",
                                    paste(  sep = "<br/>",  
                                            sa2_name,
                                            "First Quarter" ,
                                            paste("Jobseeker/population:",round(ifelse(is.na(jobseeker0/population),
                                                                                       0,jobseeker0/population*100)),"%"),
                                            
                                            paste("Avg_Sentiment_Score:"
                                                  ,round(ifelse(is.na(polarityAvg.x),
                                                                0,polarityAvg.x)*100),"%")),
                                    paste(  sep = "<br/>",  
                                            "Second Quarter" ,
                                            paste("Jobseeker/population:",round(ifelse(is.na(jobseeker6/population),
                                                                                       0,jobseeker6/population*100)),"%"),
                                            
                                            paste("Avg_Sentiment_Score:"
                                                  ,round(ifelse(is.na(polarityAvg.y),
                                                                0,polarityAvg.y)*100),"%"))
                                    
                    )
                    
                    
        )
      
      
    }   
    
    else if(input$City == "Perth" &
            input$Variable == "Income" 
            
    ) {
      data4 <- sp::merge(data4,data11[data11$time==input$Season,],by.x ="sa2_name",by.y = "suburb")
      data4$polarityAvg[is.na(data4$polarityAvg)] <- 0
      data4$income[is.na(data4$income)] <- 0
      Bpal4 <- colorNumeric(palette = "Reds", data4@data$income)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data4, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal4(data4@data$income)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/>",
                                    sa2_name,
                                    paste("Median_income:",
                                          round(income)),
                                    paste("Avg_Sentiment_Score:",
                                          round(ifelse(is.na(polarityAvg),0,polarityAvg)*100),"%")
                    )
                    
        ) }  
    
    else if(input$City == "Perth" &
            input$Variable == "Sentiment" &
            input$Season == "2020 1-3"
    ) {
      data4 <- sp::merge(data4,data11[data11$time=="2020 1-3",],by.x ="sa2_name",by.y = "suburb")
      data4$polarityAvg[is.na(data4$polarityAvg)] <- 0
      data4$polarityAvg <-ifelse(data4$polarityAvg<0,0,data4$polarityAvg)
      data4$posPercent[is.na(data4$posPercent)] <- 0
      data4$negPercent[is.na(data4$negPercent)] <- 0
      data4$neuPercent[is.na(data4$neuPercent)] <- 0
      Bpal4 <- colorNumeric(palette = "Reds", data4@data$polarityAvg)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data4, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal4(data4@data$polarityAvg)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/>",
                                    sa2_name,
                                    "First Quarter:",
                                    paste("Positive:",
                                          round(posPercent*100),"%"),
                                    paste("Negative:",
                                          round(negPercent*100),"%"),
                                    paste("Neutral:",
                                          round(neuPercent*100),"%"),
                                    
                                    paste("Avg_Sentiment_Score:",
                                          round(polarityAvg*100),"%")
                    )
                    
        ) }  
    
    
    else if(input$City == "Perth" &
            input$Variable == "Sentiment" &
            input$Season == "2020 4-6"
    ) {
      data4 <- sp::merge(data4,data11[data11$time=="2020 1-3",],by.x ="sa2_name",by.y = "suburb")
      
      data4 <- sp::merge(data4,data11[data11$time=="2020 4-6",],by.x ="sa2_name",by.y = "suburb")
      data4$polarityAvg.x[is.na(data4$polarityAvg.x)] <- 0
      data4$polarityAvg.x <-ifelse(data4$polarityAvg.x<0,0,data4$polarityAvg.x)
      data4$posPercent.x[is.na(data4$posPercent.x)] <- 0
      data4$negPercent.x[is.na(data4$negPercent.x)] <- 0
      data4$neuPercent.x[is.na(data4$neuPercent.x)] <- 0
      data4$polarityAvg.y[is.na(data4$polarityAvg.y)] <- 0
      data4$polarityAvg.y <-ifelse(data4$polarityAvg.y<0,0,data4$polarityAvg.y)
      data4$posPercent.y[is.na(data4$posPercent.y)] <- 0
      data4$negPercent.y[is.na(data4$negPercent.y)] <- 0
      data4$neuPercent.y[is.na(data4$neuPercent.y)] <- 0
      
      Bpal4 <- colorNumeric(palette = "Reds", data4@data$polarityAvg.y)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data4, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal4(data4@data$polarityAvg.y)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/><br/>",
                                    sa2_name,
                                    paste(sep = "<br/>",
                                          "First Quarter:",
                                          paste("Positive:",
                                                round(posPercent.x*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.x*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.x*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.x*100),"%")
                                    ),
                                    paste(sep = "<br/>",
                                          "Second Quarter:",
                                          paste("Positive:",
                                                round(posPercent.y*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.y*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.y*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.y*100),"%")
                                    )
                                    
                    )
                    
        ) }   
    
    else if(input$City == "Perth" &
            input$Variable == "Sentiment" &
            input$Season == "2020 10-12"
    ) {
      data4 <- sp::merge(data4,data11[data11$time=="2020 4-6",],by.x ="sa2_name",by.y = "suburb")
      
      data4 <- sp::merge(data4,data11[data11$time=="2020 10-12",],by.x ="sa2_name",by.y = "suburb")
      data4$polarityAvg.x[is.na(data4$polarityAvg.x)] <- 0
      data4$polarityAvg.x <-ifelse(data4$polarityAvg.x<0,0,data4$polarityAvg.x)
      data4$posPercent.x[is.na(data4$posPercent.x)] <- 0
      data4$negPercent.x[is.na(data4$negPercent.x)] <- 0
      data4$neuPercent.x[is.na(data4$neuPercent.x)] <- 0
      data4$polarityAvg.y[is.na(data4$polarityAvg.y)] <- 0
      data4$polarityAvg.y <-ifelse(data4$polarityAvg.y<0,0,data4$polarityAvg.y)
      data4$posPercent.y[is.na(data4$posPercent.y)] <- 0
      data4$negPercent.y[is.na(data4$negPercent.y)] <- 0
      data4$neuPercent.y[is.na(data4$neuPercent.y)] <- 0
      Bpal4 <- colorNumeric(palette = "Reds", data4@data$polarityAvg.y)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data4, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal4(data4@data$polarityAvg.y)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/><br/>",
                                    sa2_name,
                                    paste(sep = "<br/>",
                                          "Second Quarter:",
                                          paste("Positive:",
                                                round(posPercent.x*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.x*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.x*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.x*100),"%")
                                    ),
                                    paste(sep = "<br/>",
                                          "Fourth Quarter:",
                                          paste("Positive:",
                                                round(posPercent.y*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.y*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.y*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.y*100),"%")
                                    )
                                    
                    )
                    
        ) }   
    
    else if(input$City == "Perth" &
            input$Variable == "Sentiment" &
            input$Season == "2021 1-3"
    ) {
      data4 <- sp::merge(data4,data11[data11$time=="2020 10-12",],by.x ="sa2_name",by.y = "suburb")
      
      data4 <- sp::merge(data4,data11[data11$time=="2021 1-3",],by.x ="sa2_name",by.y = "suburb")
      data4$polarityAvg.x[is.na(data4$polarityAvg.x)] <- 0
      data4$polarityAvg.x <-ifelse(data4$polarityAvg.x<0,0,data4$polarityAvg.x)
      data4$posPercent.x[is.na(data4$posPercent.x)] <- 0
      data4$negPercent.x[is.na(data4$negPercent.x)] <- 0
      data4$neuPercent.x[is.na(data4$neuPercent.x)] <- 0
      data4$polarityAvg.y[is.na(data4$polarityAvg.y)] <- 0
      data4$polarityAvg.y <-ifelse(data4$polarityAvg.y<0,0,data4$polarityAvg.y)
      data4$posPercent.y[is.na(data4$posPercent.y)] <- 0
      data4$negPercent.y[is.na(data4$negPercent.y)] <- 0
      data4$neuPercent.y[is.na(data4$neuPercent.y)] <- 0
      Bpal4 <- colorNumeric(palette = "Reds", data4@data$polarityAvg.y)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data4, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal4(data4@data$polarityAvg.y)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/><br/>",
                                    sa2_name,
                                    paste(sep = "<br/>",
                                          "Fourth Quarter:",
                                          paste("Positive:",
                                                round(posPercent.x*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.x*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.x*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.x*100),"%")
                                    ),
                                    paste(sep = "<br/>",
                                          "First Quarter of 2021:",
                                          paste("Positive:",
                                                round(posPercent.y*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.y*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.y*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.y*100),"%")
                                    )
                                    
                    )
                    
        ) }     
    
    
    
    else if(input$City == "Adelaide" & input$Variable == "NULL") {
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data5, smoothFactor = 0.3, fillOpacity = 0.8
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal5(0),popup = ~htmltools::htmlEscape(sa2_name)
        ) }
    
    else if(input$City == "Adelaide" &
            input$Variable == "Jobseeker" &
            input$Season != "2020 4-6"
    ) {
      data5 <- sp::merge(data5,data11[data11$time==input$Season,],by.x ="sa2_name",by.y = "suburb")
      data5$polarityAvg[is.na(data5$polarityAvg)] <- 0
      Bpal5 <- colorNumeric(palette = "Reds", data5@data$jobseeker0)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data5, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal5(data5@data$jobseeker0)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/>",
                                    sa2_name,
                                    ifelse(input$Season=="2020 1-3","First Quarter",
                                           ifelse(input$Season=="2020 10-12","Fourth Quarter","First Quarter 2021")
                                    ),
                                    paste("Jobseeker/population:",
                                          round(ifelse(is.na(jobseeker0/population),
                                                       0,jobseeker0/population*100)),"%"),
                                    paste("Avg_Sentiment_Score:",
                                          round(ifelse(is.na(polarityAvg),
                                                       0,polarityAvg)),"%")
                    )
                    
        ) }  
    
    else if(input$City == "Adelaide"  & 
            input$Variable == "Jobseeker" & 
            input$Season == "2020 4-6") {
      data5 <- sp::merge(data5,data11[data11$time=="2020 1-3",],by.x ="sa2_name",by.y = "suburb")
      
      data5 <- sp::merge(data5,data11[data11$time=="2020 4-6",],by.x ="sa2_name",by.y = "suburb")
      data5$polarityAvg.x[is.na(data5$polarityAvg.x)] <- 0
      Bpal5 <- colorNumeric(palette = "Reds", data5@data$jobseeker6)
      #data[is.na(data5@data)] <- 0
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data5, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal5(data5@data$jobseeker6)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/><br/>",
                                    paste(  sep = "<br/>",  
                                            sa2_name,
                                            "First Quarter" ,
                                            paste("Jobseeker/population:",round(ifelse(is.na(jobseeker0/population),
                                                                                       0,jobseeker0/population*100)),"%"),
                                            
                                            paste("Avg_Sentiment_Score:"
                                                  ,round(ifelse(is.na(polarityAvg.x),
                                                                0,polarityAvg.x)*100),"%")),
                                    paste(  sep = "<br/>",  
                                            "Second Quarter" ,
                                            paste("Jobseeker/population:",round(ifelse(is.na(jobseeker6/population),
                                                                                       0,jobseeker6/population*100)),"%"),
                                            
                                            paste("Avg_Sentiment_Score:"
                                                  ,round(ifelse(is.na(polarityAvg.y),
                                                                0,polarityAvg.y)*100),"%"))
                                    
                    )
                    
                    
        )
      
      
    }   
    
    else if(input$City == "Adelaide" &
            input$Variable == "Income" 
            
    ) {
      data5 <- sp::merge(data5,data11[data11$time==input$Season,],by.x ="sa2_name",by.y = "suburb")
      data5$polarityAvg[is.na(data5$polarityAvg)] <- 0
      data5$income[is.na(data5$income)] <- 0
      Bpal5 <- colorNumeric(palette = "Reds", data5@data$income)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data5, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal5(data5@data$income)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/>",
                                    sa2_name,
                                    paste("Median_income:",
                                          round(income)),
                                    paste("Avg_Sentiment_Score:",
                                          round(ifelse(is.na(polarityAvg),0,polarityAvg)*100),"%")
                    )
                    
        ) }  
    
    else if(input$City == "Adelaide" &
            input$Variable == "Sentiment" &
            input$Season == "2020 1-3"
    ) {
      data5 <- sp::merge(data5,data11[data11$time=="2020 1-3",],by.x ="sa2_name",by.y = "suburb")
      data5$polarityAvg[is.na(data5$polarityAvg)] <- 0
      data5$polarityAvg <-ifelse(data5$polarityAvg<0,0,data5$polarityAvg)
      data5$posPercent[is.na(data5$posPercent)] <- 0
      data5$negPercent[is.na(data5$negPercent)] <- 0
      data5$neuPercent[is.na(data5$neuPercent)] <- 0
      Bpal5 <- colorNumeric(palette = "Reds", data5@data$polarityAvg)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data5, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal5(data5@data$polarityAvg)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/>",
                                    sa2_name,
                                    "First Quarter:",
                                    paste("Positive:",
                                          round(posPercent*100),"%"),
                                    paste("Negative:",
                                          round(negPercent*100),"%"),
                                    paste("Neutral:",
                                          round(neuPercent*100),"%"),
                                    
                                    paste("Avg_Sentiment_Score:",
                                          round(polarityAvg*100),"%")
                    )
                    
        ) }  
    
    
    else if(input$City == "Adelaide" &
            input$Variable == "Sentiment" &
            input$Season == "2020 4-6"
    ) {
      data5 <- sp::merge(data5,data11[data11$time=="2020 1-3",],by.x ="sa2_name",by.y = "suburb")
      
      data5 <- sp::merge(data5,data11[data11$time=="2020 4-6",],by.x ="sa2_name",by.y = "suburb")
      data5$polarityAvg.x[is.na(data5$polarityAvg.x)] <- 0
      data5$polarityAvg.x <-ifelse(data5$polarityAvg.x<0,0,data5$polarityAvg.x)
      data5$posPercent.x[is.na(data5$posPercent.x)] <- 0
      data5$negPercent.x[is.na(data5$negPercent.x)] <- 0
      data5$neuPercent.x[is.na(data5$neuPercent.x)] <- 0
      data5$polarityAvg.y[is.na(data5$polarityAvg.y)] <- 0
      data5$polarityAvg.y <-ifelse(data5$polarityAvg.y<0,0,data5$polarityAvg.y)
      data5$posPercent.y[is.na(data5$posPercent.y)] <- 0
      data5$negPercent.y[is.na(data5$negPercent.y)] <- 0
      data5$neuPercent.y[is.na(data5$neuPercent.y)] <- 0
      
      Bpal5 <- colorNumeric(palette = "Reds", data5@data$polarityAvg.y)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data5, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal5(data5@data$polarityAvg.y)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/><br/>",
                                    sa2_name,
                                    paste(sep = "<br/>",
                                          "First Quarter:",
                                          paste("Positive:",
                                                round(posPercent.x*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.x*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.x*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.x*100),"%")
                                    ),
                                    paste(sep = "<br/>",
                                          "Second Quarter:",
                                          paste("Positive:",
                                                round(posPercent.y*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.y*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.y*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.y*100),"%")
                                    )
                                    
                    )
                    
        ) }   
    
    else if(input$City == "Adelaide" &
            input$Variable == "Sentiment" &
            input$Season == "2020 10-12"
    ) {
      data5 <- sp::merge(data5,data11[data11$time=="2020 4-6",],by.x ="sa2_name",by.y = "suburb")
      
      data5 <- sp::merge(data5,data11[data11$time=="2020 10-12",],by.x ="sa2_name",by.y = "suburb")
      data5$polarityAvg.x[is.na(data5$polarityAvg.x)] <- 0
      data5$polarityAvg.x <-ifelse(data5$polarityAvg.x<0,0,data5$polarityAvg.x)
      data5$posPercent.x[is.na(data5$posPercent.x)] <- 0
      data5$negPercent.x[is.na(data5$negPercent.x)] <- 0
      data5$neuPercent.x[is.na(data5$neuPercent.x)] <- 0
      data5$polarityAvg.y[is.na(data5$polarityAvg.y)] <- 0
      data5$polarityAvg.y <-ifelse(data5$polarityAvg.y<0,0,data5$polarityAvg.y)
      data5$posPercent.y[is.na(data5$posPercent.y)] <- 0
      data5$negPercent.y[is.na(data5$negPercent.y)] <- 0
      data5$neuPercent.y[is.na(data5$neuPercent.y)] <- 0
      Bpal5 <- colorNumeric(palette = "Reds", data5@data$polarityAvg.y)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data5, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal5(data5@data$polarityAvg.y)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/><br/>",
                                    sa2_name,
                                    paste(sep = "<br/>",
                                          "Second Quarter:",
                                          paste("Positive:",
                                                round(posPercent.x*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.x*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.x*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.x*100),"%")
                                    ),
                                    paste(sep = "<br/>",
                                          "Fourth Quarter:",
                                          paste("Positive:",
                                                round(posPercent.y*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.y*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.y*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.y*100),"%")
                                    )
                                    
                    )
                    
        ) }   
    
    else if(input$City == "Adelaide" &
            input$Variable == "Sentiment" &
            input$Season == "2021 1-3"
    ) {
      data5 <- sp::merge(data5,data11[data11$time=="2020 10-12",],by.x ="sa2_name",by.y = "suburb")
      
      data5 <- sp::merge(data5,data11[data11$time=="2021 1-3",],by.x ="sa2_name",by.y = "suburb")
      data5$polarityAvg.x[is.na(data5$polarityAvg.x)] <- 0
      data5$polarityAvg.x <-ifelse(data5$polarityAvg.x<0,0,data5$polarityAvg.x)
      data5$posPercent.x[is.na(data5$posPercent.x)] <- 0
      data5$negPercent.x[is.na(data5$negPercent.x)] <- 0
      data5$neuPercent.x[is.na(data5$neuPercent.x)] <- 0
      data5$polarityAvg.y[is.na(data5$polarityAvg.y)] <- 0
      data5$polarityAvg.y <-ifelse(data5$polarityAvg.y<0,0,data5$polarityAvg.y)
      data5$posPercent.y[is.na(data5$posPercent.y)] <- 0
      data5$negPercent.y[is.na(data5$negPercent.y)] <- 0
      data5$neuPercent.y[is.na(data5$neuPercent.y)] <- 0
      Bpal5 <- colorNumeric(palette = "Reds", data5@data$polarityAvg.y)
      
      leaflet() %>% 
        #addTiles() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% 
        addPolygons(data= data5, smoothFactor = 0.3, fillOpacity = 1
                    ,color = "black",weight=1,stroke = TRUE,
                    fillColor = ~Bpal5(data5@data$polarityAvg.y)
                    #,popup = ~htmltools::htmlEscape(sa2_name)
                    ,popup = ~paste(sep = "<br/><br/>",
                                    sa2_name,
                                    paste(sep = "<br/>",
                                          "Fourth Quarter:",
                                          paste("Positive:",
                                                round(posPercent.x*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.x*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.x*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.x*100),"%")
                                    ),
                                    paste(sep = "<br/>",
                                          "First Quarter of 2021:",
                                          paste("Positive:",
                                                round(posPercent.y*100),"%"),
                                          paste("Negative:",
                                                round(negPercent.y*100),"%"),
                                          paste("Neutral:",
                                                round(neuPercent.y*100),"%"),
                                          
                                          paste("Avg_Sentiment_Score:",
                                                round(polarityAvg.y*100),"%")
                                    )
                                    
                    )
                    
        ) }  
    
    
    
    
    
    
    
    
  })  
  
  chart1.data<- data %>% group_by(
    time, 
    city
  ) %>% summarise(
    totalCount = sum(totalCount),
    polaritySum = sum(polaritySum)
  ) %>% ungroup(
  ) %>% mutate(
    AvgPolarity = polaritySum/totalCount,
    city = ifelse(city == "Brisbane City", "Brisbane",city)
  ) %>% filter(
    city %in% c('Perth','Melbourne','Sydney','Adelaide','Brisbane')
  ) %>% select(
    time,
    city,
    totalCount
  ) %>% reactive()
    
  output$chart1 <-renderPlot({
    ggplot(data=chart1.data() %>% filter(
      time == input$Season
    ) , aes(x=city, y=totalCount))+ geom_bar(stat='identity', width = 0.5, 
                                                           aes(fill = totalCount)
                                             )+ 
      ggtitle("Total Number of Tweets")+
      theme(axis.text=element_text(size=20))
    })
  
  chart2.data<- data %>% group_by(
    time, 
    city
  ) %>% summarise(
    posCount = sum(posCount),
    negCount = sum(negCount),
    neuCount = sum(neuCount),
    totalCount = sum(totalCount)
  ) %>% ungroup(
  ) %>% mutate(
    Positive = posCount/totalCount,
    Negative = negCount/totalCount,
    Neutral = neuCount/totalCount,
    city = ifelse(city == "Brisbane City", "Brisbane",city)
  ) %>% filter(
    city %in% c('Perth','Melbourne','Sydney','Adelaide','Brisbane')
  ) %>% select(
    time,
    city,
    Positive,
    Negative,
    Neutral
  ) 
  
  chart2.data1<- melt(chart2.data %>% as.data.table(), id.vars = c("time", "city"),
       measure.vars = c("Positive","Neutral", "Negative")
  ) %>% arrange(
    time,
    city
  ) %>% group_by(
    time,
    city
  ) %>% mutate(
    cumvalue = cumsum(value) - (0.5 * value)
  ) %>% reactive()

  
  output$chart2 <-renderPlot({
    
    ggplot(chart2.data1()%>% filter(
      time == input$Season
    ), aes(fill=factor(variable, levels=c("Negative","Neutral", "Positive")), y=value, x=city)) + 
      geom_bar(position="fill", stat="identity") +
      ggtitle("Tweet Sentiment Distribution") +
      theme(axis.text=element_text(size=20)) +
      geom_text(aes(label = sprintf("%1.2f%%", 100*value), y = cumvalue), size = 6) +
      theme(legend.title=element_blank())
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)