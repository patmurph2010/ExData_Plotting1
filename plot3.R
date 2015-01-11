plot3 <- function() {
  epc <- read.table("household_power_consumption.txt", sep = ";", header = TRUE, colClasses = c("character", "character", rep("numeric", 7)), na.strings = "?")
  rightdate <- grepl("^1/2/2007", epc[, 1]) | grepl("^2/2/2007", epc[,1])
  epc_dates <- epc[rightdate,]
  epc_dates <- epc_dates[complete.cases(epc_dates),]
  dates <- as.Date(epc_dates[, "Date"], "%d/%m/%Y")
  days <- weekdays(dates, abbreviate = TRUE)
  point_vector <- NULL
  text_vector <- NULL
  current_day <- " "
  for(i in 1:length(dates)) {
    if(days[i] != current_day) {
      point_vector <- c(point_vector, i)
      text_vector <- c(text_vector, days[i])
      current_day <- days[i]
    }
  }
  point_vector <- c(point_vector, length(days))
  text_vector <- c(text_vector, weekdays(dates[length(dates)] + 1, abbreviate = TRUE))
  png(filename = "plot3.png")
  plot(epc_dates[, "Sub_metering_1"], type = "l", ylab = "Energy sub metering", xlab = " ", xaxt = "n")
  lines(epc_dates[, "Sub_metering_2"], col = "red")
  lines(epc_dates[, "Sub_metering_3"], col = "blue")
  legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = c(1, 1, 1), col = c("black", "red", "blue"))
  axis(1, point_vector, text_vector)
  stat <- dev.off()
}