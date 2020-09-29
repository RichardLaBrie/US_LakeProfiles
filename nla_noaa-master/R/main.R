rm(list = ls()) 

options(noaakey = "QWlWJuQsHjCHRPUfYuOybgyeADcltdVJ")
library('rnoaa')
library("lubridate")


station_data <- ghcnd_stations()

#stations=read.csv("data/raw/transform_station_output.csv")
#temporal=read.csv("data/raw/transform_temporal_output.csv")

metadata = read.csv("./data/metadata.more40deep0712.csv")

sampling_date_location = data.frame(NLA_ID = paste(metadata$site_id, metadata$year, sep = "-"),
                         station = metadata$site_id,
                         date = paste(metadata$year,metadata$month, metadata$day, sep = "-"),
                         latitude = metadata$lat,
                         longitude = metadata$lon)

sampling_date_location$year = stringr::str_sub(sampling_date_location$date, start = 1, end = 4)
sampling_date_location$year = paste0(sampling_date_location$year,"-01-31")
#to init


output_temp <- data.frame(NLA_ID=sampling_date_location$NLA_ID,
                         date=sampling_date_location$year,
                         station=sampling_date_location$station,
                         temp_mean_min_1m=-99)
#save(output_temp,file="data/interim/output_temp")

i=which(output_temp$NLA_ID=="NLA06608-0001-2007")

#for(i in 1:nrow(sampling_date_location))
#limit = 3
limit = 20
for(i in 1:1034)
{
  #load("data/interim/output_temp")
  print(i)
  # if already in the data frame skip
  if(!is.na(output_temp$temp_mean_min_1m[i])) next
  
  lat_lon_df <- data.frame(id =as.character(sampling_date_location$NLA_ID[i]), 
                           latitude = sampling_date_location$latitude[i], 
                           longitude = sampling_date_location$longitude[i])
  
 
  d_sampling=as.Date(sampling_date_location$year[i])
  
  #1st loop radius = 20, 2nd loop limit = 20 3rd r = 50, 4th r = 100 5th r = 200, 6th r = 250
  met_station=rnoaa::meteo_nearby_stations( lat_lon_df,
                                            station_data = station_data,
                                            radius = 250,
                                            var = "TAVG",
                                            year_min=year(d_sampling),
                                            year_max=year(d_sampling),
                                            limit=limit)
  
  d_min=d_sampling

  # set minimum day to January 1st
  day(d_min) <- day(d_min)-30
  
  d_min_1m=d_min;month(d_min_1m) <- month(d_min) 

  
  

  t_1m=meteo_pull_monitors(met_station[[1]]$id,
                           date_min=d_min_1m,
                           date_max=d_sampling,
                           var="TAVG")

  output_temp[i,4] = mean(t_1m$tavg,na.rm = T)/10
}  
  safe = output_temp
  output_temp$temp_mean_min_1m[output_temp$temp_mean_min_1m < 4] = 4

  #save(output_temp,file="data/interim/output_temp")

  output=cbind(output_temp[,-2],sampling_date_location[,c(3:5)])

write.csv(output,"data//processed/temp_noaa_output.csv")
