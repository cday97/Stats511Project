
#build the crash dataset
crashprep <- function() {
  location <- read_csv("data/Crash_Data_14-20.csv")
  vehicle <- read_csv("data/Vehicle_Data_14-20.csv")
  rollups <- read_csv("data/Rollups_14-20.csv")
  
  location <- check_location(location)
  vehicle <- check_vehicle(vehicle)
  rollups <- check_rollups(rollups)
  
  crash <- left_join(location,rollups,by='crash_id')
  crash <- left_join(crash,vehicle,by='crash_id')
  rm("location","vehicle","rollups")
  
  crash
}