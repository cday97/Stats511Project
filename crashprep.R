# The following is the functions that prepare the crash data to be joined

# Keeps the following columns from the location file
check_location <- function(df){
  df %>%
    select(crash_id = crash_id, 
           crash_datetime = crash_datetime, 
           crash_severity_id = crash_severity_id, 
           light_condition_id = light_condition_id, 
           weather_condition_id = weather_condition_id, 
           manner_collision_id = manner_collision_id,
           roadway_surf_condition_id = roadway_surf_condition_id,
           roadway_junct_feature_id = roadway_junct_feature_id, 
           roadway_contrib_circum_id = roadway_contrib_circum_id)
}

# Keeps the following columns from the vehicle file
check_vehicle <- function(df){
  df %>%
    select(crash_id = crash_id, 
           vehicle_num = vehicle_num, 
           vehicle_year = vehicle_year,
           travel_direction_id = travel_direction_id, 
           event_1_id = event_sequence_1_id, 
           event_2_id = event_sequence_2_id, 
           event_3_id = event_sequence_3_id, 
           event_4_id = event_sequence_4_id,
           most_harmful_event_id = most_harmful_event_id, 
           vehicle_maneuver_id = vehicle_maneuver_id)
}

# Keeps the following columns from the rollups file
check_rollups <- function(df){
  df %>%
    select(crash_id = crash_id, 
           number_fatalities = number_fatalities, 
           number_four_injuries = number_four_injuries,
           number_three_injuries = number_three_injuries, 
           number_two_injuries = number_two_injuries,
           number_one_injuries = number_one_injuries,
           pedalcycle_involved = pedalcycle_involved, 
           pedalcycle_involved_level4_tot = pedalcycle_involved_level4_tot,	
           pedalcycle_involved_fatal_tot = pedalcycle_involved_fatal_tot,
           pedestrian_involved = pedestrian_involved,	
           pedestrian_involved_level4_tot = pedestrian_involved_level4_tot,	
           pedestrian_involved_fatal_tot = pedestrian_involved_fatal_tot,
           motorcycle_involved = motorcycle_involved,	
           motorcycle_involved_level4_tot = motorcycle_involved_level4_tot,	
           motorcycle_involved_fatal_tot = motorcycle_involved_fatal_tot,
           unrestrained = unrestrained, 
           dui = dui, 
           aggressive_driving = aggressive_driving,	
           distracted_driving = distracted_driving,	
           drowsy_driving = drowsy_driving,	
           speed_related = speed_related,
           intersection_related = intersection_related,	
           adverse_weather = adverse_weather,	
           adverse_roadway_surf_condition = adverse_roadway_surf_condition,	
           roadway_geometry_related = roadway_geometry_related,
           wild_animal_related = wild_animal_related,	
           domestic_animal_related = domestic_animal_related,	
           roadway_departure = roadway_departure,	
           overturn_rollover = overturn_rollover,	
           commercial_motor_veh_involved = commercial_motor_veh_involved,	
           interstate_highway = interstate_highway,	
           teen_driver_involved = teen_driver_involved,	
           older_driver_involved = older_driver_involved,	
           route_type = route_type,	
           night_dark_condition = night_dark_condition,	
           single_vehicle = single_vehicle,	
           train_involved = train_involved,	
           railroad_crossing = railroad_crossing,
           transit_vehicle_involved = transit_vehicle_involved,	
           collision_with_fixed_object = collision_with_fixed_object)
}