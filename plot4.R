plot4 <- function(data) {
  raw <- read.table(data, header=TRUE, sep=";",stringsAsFactors=FALSE)
  raw$Date <- as.Date(raw$Date, format="%d/%m/%Y")
  raw$Datetime <- strftime(paste(raw$Date, raw$Time), format="%d/%m/%Y %H:%M:%S")
  data <- raw[raw$Date>='2007-02-01'&raw$Date<="2007-02-02", ]
  
  data$Sub_metering_1 <- as.numeric(data$Sub_metering_1)
  data$Sub_metering_2 <- as.numeric(data$Sub_metering_2)
  data$Sub_metering_3 <- as.numeric(data$Sub_metering_3)
  data$Global_active_power <- as.numeric(data$Global_active_power)
  data$Voltage <- as.numeric(data$Voltage)
  
  orderData <- data[order(data$Datetime),]
  
  par(mfrow=c(2,2), bg=NA)
  
  plot(orderData$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)" ,xaxt="n")
  axis(1, at=c(1,length(orderData$Date)/2,length(orderData$Date)), labels=c("Thu","Fri","Sat"))
  
  plot(orderData$Voltage, type="l", xlab="datetime", ylab="Voltage" ,xaxt="n")
  axis(1, at=c(1,length(orderData$Date)/2,length(orderData$Date)), labels=c("Thu","Fri","Sat"))
  
  plot(orderData$Sub_metering_1, type="l", xlab="", ylab="Energe sub metering" ,xaxt="n")
  lines(orderData$Sub_metering_2, col="red")
  lines(orderData$Sub_metering_3, col="blue")
  legend("topright",cex=0.4, lty = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  axis(1, at=c(1,length(orderData$Date)/2,length(orderData$Date)), labels=c("Thu","Fri","Sat"))
  
  plot(orderData$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power" ,xaxt="n")
  axis(1, at=c(1,length(orderData$Date)/2,length(orderData$Date)), labels=c("Thu","Fri","Sat"))
  
  axis(1, at=c(1,length(orderData$Date)/2,length(orderData$Date)), labels=c("Thu","Fri","Sat"))
  
  dev.copy(png, file="plot4.png")
  dev.off()
}