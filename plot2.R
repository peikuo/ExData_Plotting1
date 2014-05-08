plot2 <- function(data) {
  raw <- read.table(data, header=TRUE, sep=";",stringsAsFactors=FALSE)
  raw$Date <- as.Date(raw$Date, format="%d/%m/%Y")
  raw$Datetime <- strftime(paste(raw$Date, raw$Time), format="%d/%m/%Y %H:%M:%S")
  data <- raw[raw$Date>='2007-02-01'&raw$Date<="2007-02-02", ]
  data$Global_active_power <- as.numeric(data$Global_active_power)
  orderData <- data[order(data$Datetime),]
  par(bg=NA) 
  plot(orderData$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)" ,xaxt="n")
  axis(1, at=c(1,length(orderData$Global_active_power)/2,length(orderData$Global_active_power)), labels=c("Thu","Fri","Sat"))
  
  dev.copy(png, file="plot2.png")
  dev.off()
}