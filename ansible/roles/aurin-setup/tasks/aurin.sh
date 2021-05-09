



echo "=============Aurin==================="

echo "##### 1. install R #####"
yes "" | sudo apt update -qq

yes "" | sudo apt install --no-install-recommends software-properties-common dirmngr

yes "" | sudo apt-key adv --keyserver keyserver.ubuntu.com --keyserver-options http-proxy=http://wwwproxy.unimelb.edu.au:8000/ --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9

yes "" | sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"

yes "" | sudo apt install --no-install-recommends r-base r-base-dev

yes "" | sudo add-apt-repository ppa:c2d4u.team/c2d4u4.0+

echo "##### 2. install GDAL #####"
yes "" | sudo apt update

yes "" | sudo apt install libpq-dev

yes "" | apt install gdal-bin libgdal-dev

echo "##### 3.  write XML file for Aurin API connection  #####"

mkdir /home/ubuntu/Aurin


cd /home/ubuntu/Aurin/

sudo echo "<OGRWFSDataSource>
 
<URL>http://openapi.aurin.org.au/wfs?version=1.0.0</URL>

<HttpAuth>BASIC</HttpAuth>

<UserPwd>student:dj78dfGF</UserPwd>

</OGRWFSDataSource>" > aurin_wfs_connection.xml

echo "##### 4. download Aurin data & upload to couchDB ##### "



sudo echo "           
     
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

data = read_json("output.json",simplifyVector = FALSE) 
data = data$features
n= length(data)
myurl = c("http://172.26.131.211:5984/quarterly_payment_recipients_sa2_mar_2020")
myheaders = c('content-type'='application/json')
for(i in 1:n){
  instance = toJSON(data[[i]]$properties)
  web = POST(url = myurl,
       add_headers(.headers = myheaders),
       authenticate("admin","admin"),
       body = instance
  )
} " > run.r
