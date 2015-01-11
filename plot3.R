## read in the file and reset date format
filesdoc <- paste(getwd(), "household_power_consumption.txt", sep = "/")


plottingData <- read.csv(filesdoc, header = T, sep = ";")

## take desired days
plottingdata <- subset(plottingData, Date == "1/2/2007" | Date == "2/2/2007")

## rewrite date format
plottingdata$Date <- as.Date(plottingdata$Date, format = "%d/%m/%Y")

datelist <- c()
straced <- c()
straced2 <- c()
straced3 <- c()


for (i in 1:nrow(plottingdata)){
    if(as.numeric(attr(factor(plottingdata[i,7]), "level")) != "?" &
           as.numeric(attr(factor(plottingdata[i,8]), "level")) != "?" & 
           as.numeric(attr(factor(plottingdata[i,9]), "level")) != "?") {
        
        straced <- c(straced, as.numeric(attr(factor(plottingdata[i,7]), "level")))
        straced2 <- c(straced2, as.numeric(attr(factor(plottingdata[i,8]), "level")))
        straced3 <- c(straced3, as.numeric(attr(factor(plottingdata[i,9]), "level")))
        
        datelist <- c(datelist, as.POSIXct(paste(plottingdata[i,1], 
                                               attr(factor(plottingdata[i,2]), "level"), 
                                               sep = " ")))
    }
}


## plot the data and format the graph
plot(datelist, straced, type = "n", xlab = "", ylab = "Energy sub metering",
     axes = F)
lines(datelist, straced, type = "l", col ="black")
points(c(datelist[1],0), c(0, 0), pch = ".:")
lines(datelist, straced2, type = "l", col = "red")
points(c(datelist[1],0), c(0, 0), pch = ".:")
lines(datelist, straced3, type = "l", col = "blue")
points(c(datelist[1],0), c(0, 0), pch = ".:")

axis(side = 1, at = c(datelist[1]-1, datelist[length(datelist)/2], datelist[length(datelist)]),
     labels = c("Thu", "Fri", "Sat"))

axis(side = 2, at = c(0, 10, 20, 30), labels = c(0, 10, 20, 30))
box()
legend("topright", legend = c("Sub_metering_1", "Sub_metering_2",
                              "Sub_metering_3"),
                                col = c("black", "red", "blue"),
                                xjust = 0, lty = 1, cex = .75)


## save the plot to a file then close it
dev.copy(file = "plot3", png)
dev.off()
