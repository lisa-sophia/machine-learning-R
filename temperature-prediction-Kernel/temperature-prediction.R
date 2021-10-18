library(geosphere)
set.seed(123456)

stations <- read.csv("stations.csv") 
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")

# Predicting temperature for a given coordinate location given previous
# weather data from several weather stations using gaussian kernels
# -> one kernel for physical distance (location to predict to closest station)
# -> one kernel for data difference, and one for time difference

# smoothing factors for the gaussian kernels (value will be zero beyond these)
h_distance <- 20 # km
h_date <- 7 # days
h_time <- 2 # hours

# The point to predict: latitude/longitude for Stockholm
a <- 59.3420 # Latitude
b <- 18.0575 # Longitude
stockholm <- c(b,a)

# The date to predict
date <- "1997-07-15" 

# The times to predict
times <- c("04:00:00", "06:00:00", "08:00:00","10:00:00","12:00:00",
            "14:00:00","16:00:00","18:00:00","20:00:00","22:00:00","00:00:00")

temp <- vector(length=length(times))

distfunc <- function(t_longlat, s_longlat) {
  # physical distance function: haversine distance (dist on a globe)
  return (distHaversine(t_longlat, s_longlat, r=6378.137))
}

date_diff <- function(t_date,s_date) {
  # Given two dates, return the difference in days between them. 
  # This does not account for the year difference. 
  # 1997-01-01 and 2020-01-01 would give a distance of 0.
  days_in_month = c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)
  v_t <- unlist(strsplit(t_date, '-'))
  v_s <- unlist(strsplit(s_date, '-'))
  t_days <- days_in_month[as.numeric(v_t[2])] + as.numeric(v_t[3])
  s_days <- days_in_month[as.numeric(v_s[2])] + as.numeric(v_s[3])
  days <- abs(t_days - s_days)
  if (days > ceiling(365/2))
    days <- 365-days
  return (days)
}

time_diff <- function(t_time, s_time) {
  # distance between two times on 24 hour format
  # note that 22:00 and 02:00 have 4 hour difference! not 20 hour diff
  v_t <- as.numeric(unlist(strsplit(t_time, ':'))[1])
  v_s <- as.numeric(unlist(strsplit(s_time, ':'))[1])
  hours <- min(abs(v_t-v_s), min(v_t,v_s)+24-max(v_t,v_s))
  return (hours)
}

Gaussian_dist <- function(smooth_dist, t_longlat, s_longlat){
  # account for the physical distance from a station to the point of interest
  return (exp(-(distfunc(t_longlat, s_longlat)/smooth_dist)^2))
}

Gaussian_date <- function(smooth_date, t_date, s_date){
  # distance between the day a temperature measurement
  # was made and the day of interest
  days <- date_diff(t_date,s_date)
  return ((exp(-(days)/smooth_date)^2))
}

Gaussian_time <- function(smooth_time, t_time, s_time){
  # distance between the hour of the day a temperature 
  # measurement was made and the hour of interest
  hours <- time_diff(t_time, s_time)
  return ((exp(-(hours)/smooth_time)^2))
}

plot_time <- function() {
  # Plots Gaussian values of different times in times vector, 
  # where the predict time is 00:00:00
  gaus_time <- c()
  distance <- c()
  for(t in times) {
    distance <- c(distance, time_diff("04:00:00",t))
    gaus_time <- c(gaus_time, Gaussian_time(h_time,"04:00:00", t))
  }
  
  # Orders the data based on distance to for a more pleasing plot
  m.time <- cbind(distance,gaus_time)
  res <- m.time[order(m.time[,1]),]
  
  plot(m.time, type="o",
       main='Gaussian time kernel',  xlab='Distance (hours)',ylab='Gauss values',
       ylim=c(0,max(gaus_time)))
}

plot_date <- function() {
  # Plots Gaussian value of 30 dates, 
  # 15 days before and 15 days after the predicted date
  gaus_date <- c()
  distance <- c()
  dates_to_test <- c()
  dates_to_test2 <- c()
  dates_to_test3 <- c()
  # Build data to test
  for(i in 0:15) {
    dates_to_test <- c(dates_to_test, paste('1997','07',sprintf('%02d',i+15), sep ='-'))
    dates_to_test2 <- c(dates_to_test2, paste('1997','08',sprintf('%02d',i), sep ='-'))
    dates_to_test3 <- c(dates_to_test3, paste('1997','08',sprintf('%02d',i+15), sep ='-'))
  }
  dates_to_test <- c(dates_to_test,dates_to_test2,dates_to_test3)
  # Test the built data
  for(d in dates_to_test) {
    gaus_date <- c(gaus_date, Gaussian_date(h_date, date, d))
  }
  
  # Orders the data based on distance to for a more pleasing plot
  m.date <- cbind(distance,gaus_date)
  res <- m.date[order(m.date[,1]),]
  
  plot(m.date, type="o",
       main='Gaussian date kernel',  xlab='Distance (Days)',ylab='Gauss values',
       ylim=c(0,max(gaus_date)))
  
}

plot_dist <- function() {
  # Plots Gaussian values of 100 closest stations 
  gaus_dist <- c()
  m.longlat <- matrix(c( stations$longitude,stations$latitude), ncol=2)
  distances <- c()
  # Test the data
  for(row in 1:nrow(m.longlat)) {
    gaus_dist <- c(gaus_dist,Gaussian_dist(h_distance, stockholm,m.longlat[row,]))
    distances <- c(distances, distfunc(stockholm,m.longlat[row,])) 
  }
  m.longlat <- cbind(distances,gaus_dist)
  res <- m.longlat[order(m.longlat[,1]),]
  
  plot(res[1:100,], type='o',
       main='Gaussian Physical Distance kernel',  xlab='Distance (km)',ylab='Gauss values',
       ylim=c(0,max(gaus_dist)))

}

# Used for testing (tuning the smoothing parameters "h")
uddevalla <- c(11.9541,58.355)
Gaussian_dist(h_distance, stockholm, uddevalla)
Gaussian_date(h_date, date, "2020-11-07")
Gaussian_time(h_time, "15:00:00", "16:00:00")

plot_dist()
plot_time()
plot_date()

# The file temps50k.csv may contain temperature measurements that are posterior
# to the day and hour of your forecast (so "in the future") -> Filter such 
# measurements out! Meaning they are not used to compute the forecast.
filterfunc <- function(date, time, data) {
  predict_ <- paste(date, time, sep=' ')
  predict_ <- strptime(predict_, format='%F %T')
  data_    <- paste(data$date, ' ', data$time)
  data_    <- strptime(data_, format='%F %T')
  return (data[which(as.numeric(difftime(predict_, data_,)) > 0),])
}

temperature.sum <- c()
temperature.mult <- c()

for (t in times) {
  # only use data prior to the date to predict
  filter_data = filterfunc(date, t, st)
  temp.sum <- c()
  temp.mult <- c()
  # calculate the distances
  for (row in 1:nrow(filter_data)) {
    data_longlat <- c(filter_data[row,5],filter_data[row,4])
    data_time <- filter_data[row,10]
    data_date <- filter_data[row, 9]
    
    curr_dist <- Gaussian_dist(h_distance, stockholm, data_longlat)
    curr_date <- Gaussian_date(h_date,         date,    data_date)
    curr_time <- Gaussian_time(h_time,            t,    data_time)
    
    temp.sum  <- c( temp.sum, curr_dist+curr_date+curr_time)
    temp.mult <- c( temp.mult, curr_dist*curr_date*curr_time)
  }
  # try multiplication and summation kernel: use sum / product of the three 
  # distance kernels as a weight factor to predict the temperature
  temperature.sum <- c(temperature.sum, 
                       sum(temp.sum*filter_data$air_temperature)/sum(temp.sum))
  temperature.mult <- c(temperature.mult, 
                        sum(temp.mult*filter_data$air_temperature)/sum(temp.mult))
}

# plot predicted temperature for summation and multiplication kernel´
plot(temperature.sum, type="o",
     main='Kernel with Sum',  xlab='Time',ylab='Expected Temperature',
     ylim=c(0,max(temperature.sum)),xaxt='n')
axis(1,at=1:length(times), labels=times)

plot(temperature.mult, type="o",
     main='Kernel with Mult', xlab='Time',ylab='Expected Temperature',
     ylim=c(0,max(temperature.mult)),xaxt='n')
axis(1,at=1:length(times), labels=times)
# Conclusion: The product of kernels seems to give the correct temperature 
# predictions for our given place and date, probably due to the three Gaussian 
# kernels being dependable on each other when calculating the product of all 
# three (means that all three kernels have to "agree" on a result, i.e. give a 
# result as close to 1 as possible in order for the final result to be high).
# In the summation of kernels however, two kernels can give a really small value 
# while the third outputs a 1, and that would be enough for us to get a high 
# result. Example: If predicting on time and date values that are very near our  
# own date and time values (date kernel = 1 and time kernel = 1), but the
# distance value we predict on is 1000km away (distance kernel = 0), this value 
# should not matter that much since its too far away. The product of kernels 
# gives (1 ∗ 1 ∗ 0) / 3 = 0 -> the value does not matter to the prediction since  
# it's too far away. However, the sum of kernels will give the result 
# (1 + 1 + 0) / 3 = 2/3, which is quite high, so that value would matter a lot.

## Future improvements: Periodic or locally periodic kernel could have been 
# more suitable for this (periodic) data, as temperature over several years 
# goes periodically up and down: https://www.cs.toronto.edu/~duvenaud/cookbook/

