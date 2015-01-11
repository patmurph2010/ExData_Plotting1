plot1 <- function() {
epc <- read.table("household_power_consumption.txt", sep = ";", header = TRUE, colClasses = c("character", "character", rep("numeric", 7)), na.strings = "?")
rightdate <- grepl("^1/2/2007", epc[, 1]) | grepl("^2/2/2007", epc[,1])
epc_dates <- epc[rightdate,]
epc_dates <- epc_dates[complete.cases(epc_dates),]
png(filename = "plot1.png")
hist(epc_dates[, "Global_active_power"], col = "red", xlab = "Global Active Power (kilowatts)", ylab = "Frequency", main = "Global Active Power")
stat <- dev.off()
}
