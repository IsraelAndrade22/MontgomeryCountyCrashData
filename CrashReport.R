#Gatheringd
###alexandria read local:
###israel read local: 

##title:  “Crash Reporting - Drivers Data”
data = read.csv("https://raw.githubusercontent.com/IsraelAndrade22/MontgomeryCountyCrashData/master/Crash_Reporting_-_Drivers_Data.csv")
#Initial Exploration:

##Do some initial exploration of the data.  How many rows and columns?  What are the types of the columns (ints, strings, factors, …?).  Don’t forget the ‘summary’ and ‘str’ commands.

par(mar=c(4.1,8.1,2.1,2.1))
plot(data$Weather, horiz = TRUE, las = 2)
unknown = data[data$Weather == 'UNKNOWN',]
bad = data[data$Weather == 'N/A',]

countna = function(x){
  total = sum(is.na(x))
  total %+=% sum(x == "N/A")
  total %+=% sum(x == "")
  total
}


#cleaning


#exploration and visualization

## NA, or corrupted data?  Identify and treat them appropriately.

##    what are the most common kinds of crashes?
##are certain kinds of crashes more common at certain times and places?
##  are some kinds of crashes more common on certain kinds of roads?
  
  # 
#conclusions

#Questions from exploration
