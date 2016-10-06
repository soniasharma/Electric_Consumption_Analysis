library(dplyr)
library(magrittr)
library(ggplot2)
library(cowplot)
library(reshape2)
library(lubridate)

if (!file.exists("household_power_consumption.txt")){
  URL<-"http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  dataFile<-download.file(URL, destfile= "./ElectricData.zip" , method="curl")
  unzip(zipfile= "./ElectricData.zip")}

# read the data
electricData<- read.table("household_power_consumption.txt", header=T, sep=";", na.strings=c("NA", "?"))


# Create a new variable DateTime in the Date/Time POSIX class format
electricData$DateTime <- paste(electricData$Date, electricData$Time) %>%
  strptime(format="%d/%m/%Y %H:%M:%S") %>%
  as.POSIXct()



# Create a new table with consumption measurements averaged over each day 
daily_consumption <- electricData %>%
  select(-c(DateTime)) %>%
  group_by(Date) %>%
  summarize(m.volt = mean(Voltage, na.rm = TRUE), m.gap = mean(Global_active_power, na.rm=TRUE), m.grp = mean(Global_reactive_power, na.rm=TRUE), m.gi= mean(Global_intensity, na.rm=TRUE), m.submeter_1 = mean(Sub_metering_1, na.rm = TRUE), m.submeter_2 = mean(Sub_metering_2, na.rm = TRUE), m.submeter_3 = mean(Sub_metering_3, na.rm = TRUE))

# convert Date variable to date/time class
daily_consumption$Date <- as.POSIXct(strptime(daily_consumption$Date, format = "%d/%m/%Y")) 
# arrange data table by date of voltage 
daily_consumption <- arrange(daily_consumption, Date) 



#View Sub metering readings 

meter1 <- ggplot(daily_consumption, aes(Date, m.submeter_1)) + geom_line(color= 'red') + ylab("") + xlab("")
meter2 <- ggplot(daily_consumption, aes(Date, m.submeter_2)) + geom_line(color= 'blue') + ylab("Consumption") + xlab("")
meter3 <- ggplot(daily_consumption, aes(Date, m.submeter_3)) + geom_line(color= 'green') + ylab("") + xlab("Years")
theme_set(theme_cowplot(font_size=12))
plot_grid(meter1, meter2, meter3, labels = c('Kitchen: Dishwasher, Oven', 'Laundry, Refridgrator', 'AC, Water Heater'), ncol = 1, align = 'v')


# Investigating the drop in second half of 2008

av.consumption_2008 <- filter(daily_consumption,  Date >= '2008-01-01'  & Date <= '2008-03-01')

meter1 <- ggplot(av.consumption_2008, aes(Date, m.submeter_1)) + geom_line(color= 'red') + ylab("") + xlab("")
meter2 <- ggplot(av.consumption_2008, aes(Date, m.submeter_2)) + geom_line(color= 'blue') + ylab("Consumption") + xlab("")
meter3 <- ggplot(av.consumption_2008, aes(Date, m.submeter_3)) + geom_line(color= 'green') + ylab("") + xlab("Years")
theme_set(theme_cowplot(font_size=12))
plot_grid(meter1, meter2, meter3, labels = c('Kitchen: Dishwasher, Oven', 'Laundry, Refridgrator', 'AC, Water Heater'), ncol = 1, align = 'v')



with(av.consumption_2008, plot(Date, m.gap, main="", type = 'l', xlab = "", ylim = c(0,7), ylab="Consumption"))
legend("topright", legend=c("Global_active_power", "Global_reactive_power", "Global_intensity"), bty = 'o', col=c( "black", "red", "blue"), lty = 'solid')
lines(av.consumption_2008$Date, av.consumption_2008$m.grp, col ="red")
lines(av.consumption_2008$Date, av.consumption_2008$m.gi, col ="blue")


# Look at data by months for a given year
daily_consumption$month <- as.factor(months(daily_consumption$Date, abbreviate = T))
# arrange the levels
levels(daily_consumption$month) = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# create days
daily_consumption$day <- mday(daily_consumption$Date)

# plot active power by month over the years 
ggplot(daily_consumption, aes(Date, m.gap)) + geom_line()+ facet_grid(month~.)

# yearly data by months
ggplot(filter(daily_consumption, year(Date)== '2008'), aes(day, m.gap, col = month)) + geom_line()+ facet_grid(month~., as.table = F) + ylab("Global Active Power (averaged by day)") + xlab("Day of the month") + ggtitle("Power consumption in 2008")


monthAvg <- summarize(group_by(filter(daily_consumption, year(Date) == '2008'), month), Power = mean(m.gap), Kitchen = mean(m.submeter_1), Laundry = mean(m.submeter_2), AC_Heater=mean(m.submeter_3))
gg <- melt(monthAvg, id="month")
ggplot(gg, aes(x=variable, y=value, fill=factor(month))) + 
  stat_summary(fun.y=mean, geom="bar",position=position_dodge(1)) + 
  scale_color_discrete("month") + xlab("Electric Quantities (2008)") + ylab('Value')
stat_summary(fun.ymin=min,fun.ymax=max,geom="errorbar",
             color="grey80",position=position_dodge(1), width=.2)

# comparing consumption of sub meter readings on different days

electricData$Date <- electricData$Date %>%
 strptime(format="%d/%m/%Y") %>%
 as.POSIXct()
electricData$Time <- electricData$Time %>%
  strptime(format="%H:%M:%S") %>%
  as.POSIXct()

graph_daily <- function(day){
  day_slice = electricData[electricData$Date == day, ]  
  meter1 <- ggplot(day_slice, aes(Time, Sub_metering_1)) + geom_line(color= 'red') + ylab("Kitchen: Dishwasher, Oven, Microwave") + xlab("")
  meter2 <- ggplot(day_slice, aes(Time, Sub_metering_2)) + geom_line(color= 'blue') + ylab("Laundry & Refridgerator") + xlab("")
  meter3 <- ggplot(day_slice, aes(Time, Sub_metering_3)) + geom_line(color= 'green') + ylab("AC & Water Heater") + xlab((day))
  theme_set(theme_cowplot(font_size=8))
  plot_grid(meter1, meter2, meter3, labels = c('', '', ''),ncol=1, align = 'v')
}

plot_grid(graph_daily("2008-05-01"), graph_daily("2009-05-01"), ncol = , align = 'h')
