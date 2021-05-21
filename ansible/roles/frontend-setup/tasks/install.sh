

sudo echo "           
     
install.packages("leaflet")
install.packages("rmarkdown")
install.packages("flexdashboard")
install.packages("shiny")

install.packages("rgdal")
install.packages("sp")
install.packages("rticles")
install.packages("httr")
install.packages("RCurl")" > install.r

wget http://security.ubuntu.com/ubuntu/pool/main/o/openssl1.0/libssl1.0.0_1.0.2n-1ubuntu5.6_amd64.deb
sudo apt install ./libssl1.0.0_1.0.2n-1ubuntu5.6_amd64.deb

sudo apt-get install gdebi-core -y
wget https://download2.rstudio.org/server/xenial/amd64/rstudio-server-1.4.1106-amd64.deb
sudo gdebi rstudio-server-1.4.1106-amd64.deb -y