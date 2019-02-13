plot1 <- function(){
  ## Read the data of the relevant two days
  
  data <- read.table(file="./household_power_consumption.txt", skip = grep("1/2/2007", readLines("./household_power_consumption.txt")), nrows = 2880)
  
  ## Loop the lines and convert the factors to character arrays
  mat <- matrix(, nrow = 0, ncol = 9)
  for (i in 1:length(data[[1]])) {
    char_arr <- strsplit(as.character(data[[1]][i]), ";")
    l <- char_arr[[1]]
    ## bind only if there is data
    if (!grepl("\\?", l[3])) {
      mat <- rbind(mat, l)
    }
  }
  par(mar = c(4,4,1,1))
  hist(as.numeric(mat[,3]), col = "red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
  dev.copy(png,'plot1.png', width = 480, height = 480)
  dev.off()
}

