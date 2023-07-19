library(dplyr)
library(data.table)
library(readr)
# when reading the data one of the functions adds a 0 to some of the accidents references
remove_leading_zero <- function(accident_reference) {
  sub("^0+(?!$)", "", accident_reference, perl = TRUE)
}
# load the data
accidents_data <- readRDS("../filtered_accidents_data.rds")
# columns to keep
accidents_data_cols <- c("accident_index","accident_year","accident_reference","longitude","latitude","accident_severity","number_of_vehicles","number_of_casualties","date","day_of_week","time","first_road_class","road_type","speed_limit","junction_detail","junction_control","second_road_class","pedestrian_crossing_human_control","pedestrian_crossing_physical_facilities","light_conditions","weather_conditions","road_surface_conditions","special_conditions_at_site","carriageway_hazards")
# keep only the selected columns
accidents_data <- select(accidents_data, all_of(accidents_data_cols))
# make the lat/lon numeric for markers, reference for merging
accidents_data$accident_reference <- as.character(accidents_data$accident_reference)
accidents_data$longitude <- as.numeric(accidents_data$longitude)
accidents_data$latitude <- as.numeric(accidents_data$latitude)
# remove rows with missing values
accidents_data <- accidents_data[complete.cases(accidents_data), ]
# keep only the severe accidents
accidents_data <- accidents_data %>% filter(accident_severity == 1 | accident_severity == 2)
# visually verify that the columns is correct
all(accidents_data_cols %in% names(accidents_data))
# repeat
casualties_data <- readRDS("../filtered_casualties_data.rds")
casualties_data$accident_reference <- sapply(casualties_data$accident_reference, remove_leading_zero)
casualties_data_cols <- c("accident_index","accident_year","accident_reference","vehicle_reference","casualty_reference","casualty_class","sex_of_casualty","age_of_casualty","age_band_of_casualty","casualty_severity","pedestrian_location","pedestrian_movement","car_passenger","bus_or_coach_passenger","pedestrian_road_maintenance_worker","casualty_type")
casualties_data <- select(casualties_data, all_of(casualties_data_cols))
casualties_data <- casualties_data[complete.cases(casualties_data), ]
casualties_data <- casualties_data %>% filter(casualty_severity == 1 | casualty_severity == 2)
casualties_data$accident_reference <- as.character(casualties_data$accident_reference)
all(casualties_data_cols %in% names(casualties_data))
# merge the two datasets
merged <- merge.data.table(accidents_data, casualties_data, by = "accident_reference")
merged$longitude <- as.numeric(merged$longitude)
merged$latitude <- as.numeric(merged$latitude)
merged <- merged[complete.cases(merged), ]
# remove duplicated columns
merged <- merged %>% select(-"accident_index.y", -"accident_year.y")
vehicles_data <- readRDS("../filtered_vehicles_data.rds")
vehicles_data$accident_reference <- sapply(vehicles_data$accident_reference, remove_leading_zero)
vehicles_data$accident_reference <- as.character(vehicles_data$accident_reference)
vehicles_data_cols <- c("accident_index", "accident_year", "accident_reference", "vehicle_reference", "vehicle_type", "towing_and_articulation", "vehicle_manoeuvre", "vehicle_direction_from", "vehicle_direction_to", "vehicle_location_restricted_lane", "junction_location", "skidding_and_overturning", "hit_object_in_carriageway", "vehicle_leaving_carriageway", "hit_object_off_carriageway", "first_point_of_impact", "vehicle_left_hand_drive", "journey_purpose_of_driver", "sex_of_driver", "age_of_driver", "age_band_of_driver", "engine_capacity_cc", "propulsion_code", "age_of_vehicle", "generic_make_model", "driver_home_area_type")
vehicles_data <- select(vehicles_data, all_of(vehicles_data_cols))
vehicles_data <- vehicles_data[complete.cases(vehicles_data), ]
merged <- merge(merged, vehicles_data, by = "accident_reference", all.x = TRUE, all.y = FALSE)
merged2 <- merge.data.table(accidents_data, casualties_data, by = "accident_index")
merged2 <- merge.data.table(merged2, vehicles_data, by = "accident_index", all.x = TRUE, all.y = FALSE)
merge_data_table_both_index <- merge.data.table(accidents_data, casualties_data, by = "accident_index")
merge_data_table_both_index <- merge.data.table(merge_data_table_both_index, vehicles_data, by = "accident_index", all.x = TRUE, all.y = FALSE)
saveRDS(merge_data_table_both_index, "../../resources/merge_data_table_both_index.rds")
saveRDS(merge_data_table_both, "../../resources/merge_data_table_both.rds")
saveRDS(merged2, "../../resources/merged2.rds")
merged <- merged %>%
  group_by(accident_reference) %>%
  slice(which.max(rowSums(!is.na(.)))) %>%
  ungroup()

# write.csv(merged, "resources/merged_new.csv", row.names = FALSE)
# saveRDS(merged, "resources/merged_new.rds")
# aaa <- readRDS( "resources/merged_new.rds")

weekdays <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
data$day_of_week <- ifelse(data$day_of_week == 1, weekdays[1],
                           ifelse(data$day_of_week == 2, weekdays[2],
                                  ifelse(data$day_of_week == 3, weekdays[3],
                                         ifelse(data$day_of_week == 4, weekdays[4],
                                                ifelse(data$day_of_week == 5, weekdays[5],
                                                       ifelse(data$day_of_week == 6, weekdays[6], weekdays[7]))))))

data$weather_conditions <- ifelse(data$weather_conditions == 1, "Fine, no high wind",
                                  ifelse(data$weather_conditions == 2, "Raining, no high winds",
                                         ifelse(data$weather_conditions == 3, "Snowing, no high winds",
                                                ifelse(data$weather_conditions == 4, "Fine + high winds",
                                                       ifelse(data$weather_conditions == 5, "Raining + high winds",
                                                              ifelse(data$weather_conditions == 6, "Snowing + high winds",
                                                                     ifelse(data$weather_conditions == 7, "Fog or mist",
                                                                            ifelse(data$weather_conditions == 8, "Other",
                                                                                   ifelse(data$weather_conditions == 9, "Unknown", "Data missing or out of range")))))))))

data$light_conditions <- ifelse(data$light_conditions == 1, "Daylight",
                                ifelse(data$light_conditions == 4, "Darkness - lights lit",
                                       ifelse(data$light_conditions == 5, "Darkness - lights unlit",
                                              ifelse(data$light_conditions == 6, "Darkness - no lighting",
                                                     ifelse(data$light_conditions == 7, "Darkness - lighting unknown",
                                                            ifelse(data$light_conditions == -1, "Data missing or out of range", NA))))))

data$road_type <- ifelse(data$road_type == 1, "Roundabout",
                         ifelse(data$road_type == 2, "One way street",
                                ifelse(data$road_type == 3, "Dual carriageway",
                                       ifelse(data$road_type == 6, "Single carriageway",
                                              ifelse(data$road_type == 7, "Slip road",
                                                     ifelse(data$road_type == 9, "Unknown",
                                                            ifelse(data$road_type == 12, "One way street/Slip road",
                                                                   ifelse(data$road_type == -1, "Data missing or out of range", NA))))))))

data$speed_limit <- ifelse(data$speed_limit == -1, "Data missing or out of range",
                           ifelse(data$speed_limit == 99, "Unknown (self reported)", as.character(data$speed_limit)))

data$junction_detail <- ifelse(data$junction_detail == 0, "Not at junction or within 20 metres",
                               ifelse(data$junction_detail == 1, "Roundabout",
                                      ifelse(data$junction_detail == 2, "Mini-roundabout",
                                             ifelse(data$junction_detail == 3, "T or staggered junction",
                                                    ifelse(data$junction_detail == 5, "Slip road",
                                                           ifelse(data$junction_detail == 6, "Crossroads",
                                                                  ifelse(data$junction_detail == 7, "More than 4 arms (not roundabout)",
                                                                         ifelse(data$junction_detail == 8, "Private drive or entrance",
                                                                                ifelse(data$junction_detail == 9, "Other junction",
                                                                                       ifelse(data$junction_detail == 99, "Unknown (self reported)",
                                                                                              ifelse(data$junction_detail == -1, "Data missing or out of range", NA)))))))))))

data$pedestrian_crossing_physical_facilities <- ifelse(data$pedestrian_crossing_physical_facilities == 0, "No physical crossing facilities within 50 metres",
                                                       ifelse(data$pedestrian_crossing_physical_facilities == 1, "Zebra",
                                                              ifelse(data$pedestrian_crossing_physical_facilities == 4, "Pelican, puffin, toucan or similar non-junction pedestrian light crossing",
                                                                     ifelse(data$pedestrian_crossing_physical_facilities == 5, "Pedestrian phase at traffic signal junction",
                                                                            ifelse(data$pedestrian_crossing_physical_facilities == 7, "Footbridge or subway",
                                                                                   ifelse(data$pedestrian_crossing_physical_facilities == 8, "Central refuge",
                                                                                          ifelse(data$pedestrian_crossing_physical_facilities == -1, "Data missing or out of range",
                                                                                                 ifelse(data$pedestrian_crossing_physical_facilities == 9, "Unknown (self reported)", NA))))))))

data$road_surface_conditions <- ifelse(data$road_surface_conditions == 1, "Dry",
                                       ifelse(data$road_surface_conditions == 2, "Wet or damp",
                                              ifelse(data$road_surface_conditions == 3, "Snow",
                                                     ifelse(data$road_surface_conditions == 4, "Frost or ice",
                                                            ifelse(data$road_surface_conditions == 5, "Flood over 3cm. deep",
                                                                   ifelse(data$road_surface_conditions == 6, "Oil or diesel",
                                                                          ifelse(data$road_surface_conditions == 7, "Mud",
                                                                                 ifelse(data$road_surface_conditions == -1, "Data missing or out of range",
                                                                                        ifelse(data$road_surface_conditions == 9, "Unknown (self reported)", NA)))))))))

saveRDS(data, "resources/merged_2019_2021.rds")
data <- data %>% filter(data$casualty_class == 3 | data$vehicle_type == 1)