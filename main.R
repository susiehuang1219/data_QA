options(echo=TRUE)

# ==============================================================
# Parse Parameters 
# ==============================================================
args <- commandArgs(trailingOnly = TRUE)

CUR_DATE <- args[1]
#CUR_DATE = 20140531

require(plyr)
require(reshape2)
require(chron)
require(forecast)


## Data Prep ------------------------------------

# Input Folder Location:
setwd("\\\\c\\user\\susie\\QA\\data\\UV_agg\\")
dataFolder = file.path("\\\\c\\user\\susie\\QA\\data\\UV_agg")

# Specify output folder:
outputFldr = file.path("\\\\c\\user\\susie\\QA\\data\\UV_agg\\output_r\\")

# Specify Cofindence Level for forecast Interval (lower->more aggresive->more flags):
ciLevel = .95

# Load table
data = read.table(paste0("categoryUV_last30_",as.character(CUR_DATE), ".txt"), sep="\t")
#data = read.table("categoryUV_last30_20140302.txt", sep="\t")
# Format Colnames:
colnames(data) = c("type", "Category.ID", "Count","Date")

# Cast category ID column to Factor:
data$Category.ID = as.factor(data$Category.ID)

# cast date:
data$Date = as.Date(as.character(data$Date), format = "%Y%m%d")

# Check Data Form:
#str(data)
#head(data)

# Set Max Date:
maxDate = as.Date(as.character(CUR_DATE), format = "%Y%m%d")

# Build Expo Smoothing Model and make next day forecast for each Category.ID
output = ddply(data, .(Category.ID), 
               function(x){
                 
                 # Print:
                 #cat("category Id:", as.character(unique(x$Category.ID)), as.character(maxDate), "Forecast Started", "\n")
                 
                 # Order Data by Date:
                 x = x[order(x$Date),]
                 #maxDate_real = max(x$Date)
                 
                 obs_count = length(unique(x$Date))
                 if (obs_count <= 10){
                   nextDayForecast = data.frame(cbind(
                   '','',''))
                   colnames(nextDayForecast) = c("Point.Forecast",
                                                 paste("Lo", ciLevel[1]*100, sep ="."),
                                                 paste("Hi", ciLevel[1]*100, sep ="."))
                 }else{
                   # Build Model on All but last day:
                   mdl = try(ets(x[x$Date != maxDate, "Count"]))
                   
                   # Make Forecast:
                   nextDayForecast = data.frame(forecast(mdl, h=1, level=ciLevel))
                   
                 }
                
            
                 
                # Make Forecast:
                # nextDayForecast = data.frame(forecast(mdl, h=1, level=ciLevel))
                 
                 
                # Add Current Actual:
                 nextDayForecast$Actual = x$Count[nrow(x)]
                 # Add Current Date:
                 # nextDayForecast$Date = maxDate
                 maxDate_real = max(x$Date)
                 
                 nextDayForecast$Last_Date = ifelse(maxDate_real == maxDate,as.character(maxDate),as.character(maxDate_real))
                 nextDayForecast$Current_Date = maxDate
                 
                 
                
                 
                 # Add Residual:
                 #nextDayForecast$Residual = nextDayForecast$Actual - nextDayForecast$Point.Forecast
                 
                 # Add Outlier Flags:
                 nextDayForecast$Flag = ifelse(nextDayForecast[,paste("Hi", ciLevel*100, sep =".")] > 50&(nextDayForecast$Actual < 0.75*nextDayForecast[,paste("Lo", ciLevel*100, sep =".")]|nextDayForecast$Actual > 1.5*nextDayForecast[,paste("Hi", ciLevel*100, sep =".")]), 1, 
                                               ifelse(maxDate_real!= maxDate&obs_count>=29, 2, 0))
                 nextDayForecast$Flag[is.na(nextDayForecast$Flag)] = 3
                 
    
                 #nextDayForecast$Flag2 = ifelse(nextDayForecast$Actual > nextDayForecast[, paste("Hi", ciLevel[2]*100, sep =".")] |
                                                  #nextDayForecast$Actual < nextDayForecast[,paste("Lo", ciLevel[2]*100, sep =".")],
                                                #1, 0)
                 
                 #colnames(nextDayForecast)[(ncol(nextDayForecast)-1):ncol(nextDayForecast)] = paste("Flag", ciLevel*100, sep=".")
                 # Return Output:
                 #outIndx = c("Date", "Actual", "Point.Forecast", 
                             #paste("Lo", ciLevel*100, sep ="."), paste("Hi", ciLevel*100, sep ="."), paste("Flag", ciLevel[1]*100, sep="."))
                 #nextDayForecast = nextDayForecast[, outIndx]
                 
                 # Print:
                 #cat("category Id:", as.character(unique(x$Category.ID)), as.character(maxDate), "Forecast Completed", "\n")
                 #cat("####################", "\n")
                 #cat("\n")
                 
                 nextDayForecast
               })

# Write output file to folder:
output$Category.ID = as.numeric(as.character(output$Category.ID))
output$Point.Forecast = as.integer(output$Point.Forecast)
output$Lo.95 = ceiling(output$Lo.95)
output$Hi.95 = ceiling(output$Hi.95)
output$Last_Date = as.Date(output$Last_Date)
#newoutput <- output[ which(output$Flag > 0),]
newoutput <- subset(output, Flag>=1&Flag<=2, select=c(Category.ID, Lo.95,Hi.95,Actual,Flag))
write.table(output, file = paste0(outputFldr, "categoryID_output_", as.character(CUR_DATE), ".txt"),sep="\t",col.names=FALSE,row.names=FALSE)
write.table(newoutput, file = paste0(outputFldr, "categoryID_output_email_", as.character(CUR_DATE), ".txt"),sep="\t",row.names=FALSE)
