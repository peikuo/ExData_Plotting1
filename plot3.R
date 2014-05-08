plot3 <- function(data) {
  raw <- read.table(data, header=TRUE, sep=";",stringsAsFactors=FALSE)
  raw$Date <- as.Date(raw$Date, format="%d/%m/%Y")
  raw$Datetime <- strftime(paste(raw$Date, raw$Time), format="%d/%m/%Y %H:%M:%S")
  data <- raw[raw$Date>='2007-02-01'&raw$Date<="2007-02-02", ]
  
  data$Sub_metering_1 <- as.numeric(data$Sub_metering_1)
  data$Sub_metering_2 <- as.numeric(data$Sub_metering_2)
  data$Sub_metering_3 <- as.numeric(data$Sub_metering_3)
  orderData <- data[order(data$Datetime),]
  par(bg=NA) 
  plot(orderData$Sub_metering_1, type="l", xlab="", ylab="Energe sub metering" ,xaxt="n")
  lines(orderData$Sub_metering_2, col="red")
  lines(orderData$Sub_metering_3, col="blue")
  
  axis(1, at=c(1,length(orderData$Date)/2,length(orderData$Date)), labels=c("Thu","Fri","Sat"))
 
  legend("topright",cex=0.7, lty = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  dev.copy(png, file="plot3.png")
  dev.off()
}