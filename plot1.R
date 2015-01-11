
## read in the file and reset date format
filesdoc <- paste(getwd(), "household_power_consumption.txt", sep = "/")


plottingData <- read.csv(filesdoc, header = T, sep = ";")

## rewrite date format
plottingData$Date <- as.Date(plottingData$Date, format = "%d/%m/%Y")

## take desired days
plottingdata <- subset(plottingData, Date == "2007-02-01" | Date == "2007-02-02")

## change data into graphable form
traced <- c()

for (i in 1:nrow(plottingdata)){
    if(attr(factor(plottingdata[i,3]), "level") != "?") {
        traced <- c(traced, as.numeric(attr(factor(plottingdata[i,3]), "level")))
    }
}

## create histogram
hist(traced, main = "Global Active Power",
                        xlab = "Global Active Power (kilowatts)", ylab = "Frequency", 
                        col = "red")

## save graph in file and close the file
dev.copy(png, file = "plot1")
dev.off()