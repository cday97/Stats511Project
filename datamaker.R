
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

#build severity dataset
buildSeverity <- function(crashData) {
  crashData %>% 
    select(9,12,10,11,13,14) %>%
    mutate(collisionType = case_when(
      manner_collision_id == 1 ~ "1 Angle",
      manner_collision_id == 2 ~ "2 FrontToRear",
      manner_collision_id == 3 ~ "3 HeadOn",
      manner_collision_id == 4 ~ "4 SideSwipeSame",
      manner_collision_id == 5 ~ "5 SideSwipeOpp",
      manner_collision_id == 6 ~ "6 ParkedVeh",
      manner_collision_id == 7 ~ "7 RearToSide",
      manner_collision_id == 8 ~ "8 RearToRear",
      manner_collision_id == 96 ~ "96 SingleVeh",
      manner_collision_id == 97 ~ "97 Other",
      manner_collision_id %in% c(99,89) ~ "99 Unknown"
    )) %>%
    select(1,2,7)
}

fun_mean <- function(x){return(round(data.frame(y=mean(x),label=mean(x,na.rm=T)),digit=2))}

makeBoxPlot <- function(severity){
  ggplot(severity) +
    aes(x = as.factor(manner_collision_id), y = as.numeric(crash_severity_id)) +
    geom_boxplot(aes(fill = collisionType)) + 
    stat_summary(fun.y = mean, geom="crossbar",colour="blue", size=.5) +
    stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7) + 
    labs(x = "Collision ID", y = "Crash Severity") +
    theme_bw()
}