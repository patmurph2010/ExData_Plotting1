plot2 <- function() {
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
  png(filename = "plot2.png")
  plot(epc_dates[, "Global_active_power"],type = "l", ylab = "Global Active Power (kilowatts)", xlab = " ", xaxt = "n")
  axis(1, point_vector, text_vector)
  stat <- dev.off()
}