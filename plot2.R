
## read in the file and reset date format
filesdoc <- paste(getwd(), "household_power_consumption.txt", sep = "/")


plottingData <- read.csv(filesdoc, header = T, sep = ";")

## take desired days
plottingdata <- subset(plottingData, Date == "1/2/2007" | Date == "2/2/2007")

## rewrite date format
plottingdata$Date <- as.Date(plottingdata$Date, format = "%d/%m/%Y")

traced <- c()
datlist <- c()

for (i in 1:nrow(plottingdata)){
    if(as.numeric(attr(factor(plottingdata[i,3]), "level")) != "?") {
        traced <- c(traced, as.numeric(attr(factor(plottingdata[i,3]), "level")))
        
        datlist <- c(datlist, as.POSIXct(paste(plottingdata[i,1], 
                                            attr(factor(plottingdata[i,2]), "level"), 
                                            sep = " ")))
    }
}
## plot the data and format the graph
plot(datlist, traced, type = "n",xlab ="", ylab = "Global Active Power (kilowatts)", axes = F)
lines(datlist, traced, type = "l")
axis(side = 1, at = c(datlist[1]-1, datlist[length(datlist)/2], datlist[length(datlist)]),
     labels = c("Thu", "Fri", "Sat"))
box()


## save graph in file and close the file
dev.copy(png, file = "plot2")
dev.off()