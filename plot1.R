plot1 <- function(data) {
  raw <- read.table(data, header=TRUE, sep=";",stringsAsFactors=FALSE)
  raw$Date <- as.Date(raw$Date, format="%d/%m/%Y")
  data <- raw[raw$Date>='2007-02-01'&raw$Date<="2007-02-02", ]
  data$Global_active_power <- as.numeric(data$Global_active_power)
  par(bg=NA) 
  hist(data$Global_active_power, main="Global Active Power", xlab="Global active power (kilowatts)",col="red")
  
  dev.copy(png, file="plot1.png")
  dev.off()
}
