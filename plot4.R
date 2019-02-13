plot4 <- function(){
  ## Read the data of the relevant two days
  
  data <- read.table(file="./household_power_consumption.txt", skip = grep("1/2/2007", readLines("./household_power_consumption.txt")), nrows = 2880)
  
  ## Loop the lines and convert the factors to character arrays
  mat <- matrix(, nrow = 0, ncol = 9)
  for (i in 1:length(data[[1]])) {
    char_arr <- strsplit(as.character(data[[1]][i]), ";")
    l <- char_arr[[1]]
    l[1] <- as.Date(l[1])
    l[2] <- strptime(l[2], format = "%H:%M")
    ## bind only if there is data
    if (!grepl("\\?", l[3])) {
      mat <- rbind(mat, l)
    }
  }
  par(mar = c(4,4,1,1), mfcol = c(2,2), oma = c(1,1,1,1))
  #draw plot2 at (1,1)
  plot(as.numeric(mat[,3]), type = "l", xlab="", xaxt = "n", ylab="Global Active Power (kilowatts)")
  axis(side=1, at=c(0,1440,2880), labels=c("Thu", "Fri", "Sat"))
  #draw plot3 at (2,1)
  plot(as.numeric(mat[,7]), type = "l", xlab="", xaxt = "n", ylab="Energy sub metering")
  axis(side=1, at=c(0,1440,2880), labels=c("Thu", "Fri", "Sat"))
  points(as.numeric(mat[,8]), type = "l", col = "red")
  points(as.numeric(mat[,9]), type = "l", col = "blue")
  legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black", "red", "blue"),
         text.col = "black", lty = c(1, 1, 1), pch = c(NA, NA, NA), cex = 0.6)
  #draw at (1,2)
  plot(as.numeric(mat[,5]), type = "l", xlab="datetime", xaxt = "n", ylab="Voltage")
  axis(side=1, at=c(0,1440,2880), labels=c("Thu", "Fri", "Sat"))
  #draw at (2,2)
  plot(as.numeric(mat[,4]), type = "l", xlab="datetime", xaxt = "n", ylab="Global_reactive_power")
  axis(side=1, at=c(0,1440,2880), labels=c("Thu", "Fri", "Sat"))
  
  dev.copy(png,'plot4.png', width = 480, height = 480)
  dev.off()
}

