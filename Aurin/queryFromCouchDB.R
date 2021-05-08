library(sf)
library(rgdal)
library(gdalUtils)
library(jsonlite)
library(httr)

mangoQueryURL = c("http://172.26.131.4:5984/aurin_test/_find")
myheaders = c('content-type'='application/json')
sydney = POST(url = mangoQueryURL,
              add_headers(.headers = myheaders),
              authenticate("admin","admin"),
              body = '{
                       "selector": {
                          "$and": [
                             {"properties.sa2_5dig16": {"$gte": 11290}},
                             {"properties.sa2_5dig16": {"$lte": 11609}}
                          ]
                       },
                       "fields": [
                          "properties","geometry"
                       ],
                       "limit": 1000
                    }') %>% content()
## sydney$docs

## sydney
# 11290
# 11609
## melbourne
# 21105
# 21468
## Brisbane 
# 31001
# 31527
## Adelaide 
# 41001
# 41172
## Perth 
# 51030
# 51261
