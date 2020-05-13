
# NOTE: requires installing moveHMM if not already done so. 
# To do so run
# install.packages("moveHMM")


require(moveHMM)

library(readr)
Daily_step_and_turn_data = read_csv("../data files/Daily_step_and_turn_data.csv")

Vertical_movement_data = read_csv("../data files/Vertical_movement_data.csv")

# in case radian conversions are needed
rad2deg = function(rad) {(rad * 180) / (pi)}
deg2rad = function(deg) {(deg * pi) / (180)}

# calculate average vertical movement per day
dailyMeanLogVertical = rep(NA, nEntries)
for (iDate in 1:(41904/144))
{
  startInd = iDate-1 * 24*6 # 24 hours/day and every 10 minutes (6/hr)
  dailyMeanLogVertical[iDate] = mean(Vertical_movement_data$logverticalmovementinm[startInd:startInd+144])
  
}




# standardize mean vertical movement 
vertical = (dailyMeanLogVertical-mean(dailyMeanLogVertical))/sd(dailyMeanLogVertical)

nEntries = length(Daily_step_and_turn_data$latitude)
# lat and long are in degrees
codData = data.frame("ID"="Cod",
                     "Date" = Daily_step_and_turn_data$date, 
                     "Latitude" = Daily_step_and_turn_data$latitude,
                     "Longitude" = Daily_step_and_turn_data$longitude,
                     "vertical" = vertical)

plot(codData$Longitude,codData$Latitude, xlab = "Longitude (deg)", ylab = "Latitude (deg)", main = "Cod Location")

# prep data with moveHMM
formatted_CodData = prepData(codData,type="LL",coordNames=c("Latitude","Longitude"))



# visualize input data
plot(formatted_CodData, ask = F)



