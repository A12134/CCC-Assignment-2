install.packages('sf')
install.packages('rgdal')
install.packages('gdalUtils')
install.packages('jsonlite')
install.packages('httr')

setwd("/home/ubuntu/Aurin")

library(sf)
library(rgdal)
library(gdalUtils)
library(jsonlite)
library(httr)

temp_file = "output.json"

ogr2ogr("aurin_wfs_connection.xml",
        temp_file,
        "aurin:datasource-AU_Govt_DSS-UoM_AURIN_dss_quarterly_payment_recipients_sa2_mar_2020",
        f="GeoJSON",
        oo="INVERT_AXIS_ORDER_IF_LAT_LONG=NO")

data = read_json(temp_file,simplifyVector = FALSE)
data = data$features
n= length(data)
myurl = c("http://172.26.131.4:5984/quarterly_payment_recipients_sa2_mar_2020")
myheaders = c('content-type'='application/json')
for(i in 1:n){
  instance = toJSON(data[[i]]$properties)
  web = POST(url = myurl,
       add_headers(.headers = myheaders),
       authenticate("admin","admin"),
       body = instance
  )
}
