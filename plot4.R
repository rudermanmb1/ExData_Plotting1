## read in the file and reset date format
filesdoc <- paste(getwd(), "household_power_consumption.txt", sep = "/")


plottingData <- read.csv(filesdoc, header = T, sep = ";")

## take desired days
plottingdata <- subset(plottingData, Date == "1/2/2007" | Date == "2/2/2007")

## rewrite date format
plottingdata$Date <- as.Date(plottingdata$Date, format = "%d/%m/%Y")

## vectors for first graph
traced <- c()
datlist <- c()
## vectors for second graph
atraced <- c()
adatlist <-c()
## vectors for third graph
datelist <- c()
straced <- c()
straced2 <- c()
straced3 <- c()
## vectors for fourth graph
btraced <- c()
bdatlist <- c()

##for loops to initialize the vectors
for (i in 1:nrow(plottingdata)){
    if(as.numeric(attr(factor(plottingdata[i,3]), "level")) != "?") {
        traced <- c(traced, as.numeric(attr(factor(plottingdata[i,3]), "level")))
        
        datlist <- c(datlist, as.POSIXct(paste(plottingdata[i,1], 
                                               attr(factor(plottingdata[i,2]), "level"), 
                                               sep = " ")))
    }
}


for (i in 1:nrow(plottingdata)){
    if(as.numeric(attr(factor(plottingdata[i,5]), "level")) != "?") {
        atraced <- c(atraced, as.numeric(attr(factor(plottingdata[i,5]), "level")))
        
        adatlist <- c(adatlist, as.POSIXct(paste(plottingdata[i,1], 
                                               attr(factor(plottingdata[i,2]), "level"), 
                                               sep = " ")))
    }
}

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


for (i in 1:nrow(plottingdata)){
    if(as.numeric(attr(factor(plottingdata[i,4]), "level")) != "?") {
        btraced <- c(btraced, as.numeric(attr(factor(plottingdata[i,4]), "level")))
        
        bdatlist <- c(bdatlist, as.POSIXct(paste(plottingdata[i,1], 
                                                 attr(factor(plottingdata[i,2]), "level"), 
                                                 sep = " ")))
    }
}
##set the graphing environment for 4 graphs sorted by row
par(mfrow = c(2, 2))


plot(datlist, traced, type = "n",xlab ="", ylab = "Global Active Power", axes = F)
lines(datlist, traced, type = "l")
axis(side = 1, at = c(datlist[1]-1, datlist[length(datlist)/2], datlist[length(datlist)]),
     labels = c("Thu", "Fri", "Sat"))
axis(side = 2, at = c(0, 2, 4, 6), labels = c(0, 2, 4, 6))
box()


plot(adatlist, atraced, type = "n",xlab ="datetime", ylab = "Voltage", axes = F)
lines(adatlist, atraced, type = "l")
axis(side = 1, at = c(adatlist[1]-1, adatlist[length(adatlist)/2], adatlist[length(adatlist)]),
    labels = c("Thu", "Fri", "Sat"))
axis(side = 2, at = c(234, 236, 238, 240, 242, 244, 246), labels = c(234, NA , 238,
    NA, 242, NA, 246))

box()


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
       xjust = 0, lty = 1, cex = .6 ,bty = "n")


plot(bdatlist, btraced, type = "n",xlab ="datetime", ylab = "Global_reactive_power", axes = F)
lines(bdatlist, btraced, type = "l")
axis(side = 1, at = c(bdatlist[1]-1, bdatlist[length(bdatlist)/2], bdatlist[length(bdatlist)]),
     labels = c("Thu", "Fri", "Sat"))
axis(side = 2, at = c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels = c("0.0", "0.1", "0.2", "0.3", "0.4",
    "0.5"))
box()
par(cex.axis = .4)

dev.copy(png, file = "plot4")
dev.off()
