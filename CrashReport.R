#Gatheringd
###alexandria read local:
###israel read local: 

##title:  “Crash Reporting - Drivers Data”
data = read.csv("https://raw.githubusercontent.com/IsraelAndrade22/MontgomeryCountyCrashData/master/Crash_Reporting_-_Drivers_Data.csv")
#Initial Exploration:

##Do some initial exploration of the data.  How many rows and columns?  What are the types of the columns (ints, strings, factors, …?).  Don’t forget the ‘summary’ and ‘str’ commands.


#Count the bad data
par(mar=c(4.1,8.1,2.1,2.1))
plot(data$Weather, horiz = TRUE, las = 2)
unknown_weather = data[data$Weather == 'UNKNOWN',]
Unavalible_Weather = data[data$Weather == 'N/A',]

countbadula = function(x){
  total = sum(is.na(x))
  total = total + sum(x == "N/A", na.rm = TRUE)
  total = total + sum(x == "", na.rm = TRUE)
  total/sum(lengths(x))
}
isblank = function(x) {
  mean(x=="", na.rm = TRUE)
}
countna = function(x) {
  mean(x=="", na.rm = TRUE)
}
#cleaning


#exploration and visualization
# 
  ####The heck is an autocycle
sort(table(data$Vehicle.Body.Type))# 1. the heck is an autocycle? 2. why so many emergency vehicles?
####Why is "Driverless moving vehicles" a movement type???
  ####pinpoint and identify dangerous stoplights

plot(data$Latitude, data$Longitude, xlim = c(38.7, 39.4), ylim = c(-78, -76.5))
#Find the intersectins.
#Also there's a person in our data
corner = data[data$Vehicle.Movement == c("MAKING LEFT TURN", "MAKING RIGHT TURN", "RIGHT TURN ON RED"),]
plot(corner$Latitude, corner$Longitude, xlim = c(38.9, 39.5), ylim = c(-77.6, -76.7))

#Agencies acording to how often the moterist is at fault
par(las = 2, mar = c(4,4,4,4))
plot(data$Agency.Name, data$Driver.At.Fault)
#collision types based on driver at fault
par(las = 2, mar = c(8,5,2,2), cex = 0.7)
plot(data$Collision.Type, data$Driver.At.Fault)

## NA, or corrupted data?  Identify and treat them appropriately.
  ####identify if Driver.substance.abuse goes in unknown or none
  ####
##    what are the most common kinds of crashes?
##are certain kinds of crashes more common at certain times and places?
##  are some kinds of crashes more common on certain kinds of roads?
  
sort(table(data$Vehicle.Body.Type))

isTruck = function(x){
  big = x$Vehicle.Body.Type == "TRUCK TRACTOR" | x$Vehicle.Body.Type == "MEDIUM/HEAVY TRUCKS 3 AXLES (OVER 10,000LBS (4,536KG))" | x$Vehicle.Body.Type == "CARGO VAN/LIGHT TRUCK 2 AXLES (OVER 10,000LBS (4,536 KG))" | x$Vehicle.Body.Type == "OTHER LIGHT TRUCKS (10,000LBS (4,536KG) OR LESS)" | x$Vehicle.Body.Type == "PICKUP TRUCK"
  big
}
isCar = function(x){
  big = x$Vehicle.Body.Type == "PASSENGER CAR" | x$Vehicle.Body.Type == "VAN" | x$Vehicle.Body.Type == "(SPORT) UTILITY VEHICLE"
  big
}
isEmergency = function(x){
  big = x$Vehicle.Body.Type == "POLICE VEHICLE/NON EMERGENCY" | x$Vehicle.Body.Type == "POLICE VEHICLE/EMERGENCY" | x$Vehicle.Body.Type == "FIRE VEHICLE/NON EMERGENCY" | x$Vehicle.Body.Type == "FIRE VEHICLE/EMERGENCY" | x$Vehicle.Body.Type == "AMBULANCE/NON EMERGENCY" | x$Vehicle.Body.Type == "AMBULANCE/EMERGENCY"
  big
}

#---------------------

plot(data$Collision.Type[data$Vehicle.Body.Type == "PASSENGER CAR"])
plot(data$Collision.Type[data$Vehicle.Body.Type == "PICKUP TRUCK"])
plot(data$Collision.Type[data$Vehicle.Body.Type == "(SPORT) UTILITY VEHICLE"])

percent_reared = function(x){
  values = data$Collision.Type[data$Vehicle.Body.Type == x
  mean(,] == "SAME DIR REAR END", na.rm = TRUE)
}
sapply(unique(data$Vehicle.Body.Type), percent_reared)
#conclusions

#Questions from exploration
#Why are there "N/A", "Other", "Unknown" and "" categories? That's quite a bit!
