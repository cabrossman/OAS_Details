library(stringr)
library(dplyr)

#functions
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

stringToRegex <- function (string,delim) {
  string <- tolower(trim(unlist(strsplit(string,delim))))
  string <- paste0('^',string,'$')
  string <- paste(string, collapse = "|")
  return (string)
}

factorPV <- function (regex_brand,regex_class,portalAbbv,data, sectionId = c('SR','DT')) {
  portNams <- c("YachtWorld","Boat Trader","boats.com")
  p2 <- c("YW","BT","BC")
  portals <- cbind.data.frame(portNams,p2)
  portals <- portals %>% filter(p2 %in% portalAbbv)
  data <- data %>% filter(portal %in% portals$portNams, section %in% sectionId)
  totPV <- sum(data$pv)
  
  data <- data %>% filter(grepl(regex_brand,tolower(data$make))|grepl(regex_class,tolower(data$class)))
  filterPV <- sum(data$pv)
  return(filterPV/totPV)
}

 factorPV_Type <- function (regex_brand,portalAbbv,data,regex_type, sectionId = c('SR','DT')) {
   portNams <- c("YachtWorld","Boat Trader","boats.com")
   p2 <- c("YW","BT","BC")
   portals <- cbind.data.frame(portNams,p2)
   portals <- portals %>% filter(p2 %in% portalAbbv)
   data <- data %>% filter(portal %in% portals$portNams, section %in% sectionId)
   totPV <- sum(data$pv)

   data <- data %>% filter(grepl(regex_brand,tolower(data$make))|grepl(regex_type,tolower(data$type)))
   filterPV <- sum(data$pv)
   return(filterPV/totPV)
 }

# factorPV_classType <- function (regex_brand,regex_class,portalAbbv,data,regex_type, sectionId = c('SR','DT')) {
#   portNams <- c("YachtWorld","Boat Trader","boats.com")
#   p2 <- c("YW","BT","BC")
#   portals <- cbind.data.frame(portNams,p2)
#   portals <- portals %>% filter(p2 %in% portalAbbv)
#   data <- data %>% filter(portal %in% portals$portNams, section %in% sectionId)
#   totPV <- sum(data$pv)
#
#   data2 <- data %>% filter(grepl(regex_brand,tolower(data$make)))
#   data3 <- data %>% filter(grepl(regex_type,tolower(data$type)),grepl(regex_class,tolower(data$class)))
#   data4 <- data %>% filter(grepl(regex_brand,tolower(data$make)),grepl(regex_type,tolower(data$type)),grepl(regex_class,tolower(data$class)))
#   filterPV <- sum(data2$pv) + sum(data3$pv) - sum(data4$pv)
#   return(filterPV/totPV)
# }






SR_productHeir <- read.csv("C:\\Users\\christopher.brossman\\Documents\\SR_productHeir.csv",stringsAsFactors = FALSE)
DT_productHeir <- read.csv("C:\\Users\\christopher.brossman\\Documents\\DT_productHeir.csv",stringsAsFactors = FALSE)

names(SR_productHeir) <- c('make','type','class','portal','pv'); SR_productHeir$section <- "SR"
names(DT_productHeir) <- c('make','type','class','portal','pv'); DT_productHeir$section <- "DT"

productHeir <- rbind(SR_productHeir,DT_productHeir); rm(SR_productHeir,DT_productHeir)


brand_string <- "Azimut, Ferretti, mangusta, Sealine, Sunseeker, Cranchi, Fairline, Princess, Sessa"
regex_brand <- stringToRegex(brand_string,",")
# type_string <- 'asdfsdfasdf'
# regex_type <- stringToRegex(type_string,",")
class_string <- "Cruisers, flybridge"
regex_class <- stringToRegex(class_string,",")

#brand or Class

factorPV(regex_brand,regex_class,"YW",productHeir,"SR")
factorPV(regex_brand,regex_class,"YW",productHeir,"DT")
factorPV(regex_brand,regex_class,"BT",productHeir,"SR")
factorPV(regex_brand,regex_class,"BC",productHeir)


# #brand or (Class & Type)
# factorPV_classType(regex_brand,regex_class,"YW",productHeir,regex_type)

# #brand or type
# factorPV_Type(regex_brand,"YW",productHeir,regex_type)






