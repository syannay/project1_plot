plot2 <- function(){
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
  par(mar = c(4,4,1,1))
  plot(as.numeric(mat[,3]), type = "l", xlab="", xaxt = "n", ylab="Global Active Power (kilowatts)")
  axis(side=1, at=c(0,1440,2880), labels=c("Thu", "Fri", "Sat"))
  
  
  dev.copy(png,'plot2.png', width = 480, height = 480)
  dev.off()
}

