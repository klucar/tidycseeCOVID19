suppressPackageStartupMessages({
  library(tidyverse)
  library(compare)})


# update submodule with JHU data
system2("git", args = c("submodule", "update", "--remote", "--recursive"))


# get all the daily reports
data_path = './data-raw/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/'
csv_files = list.files(path = data_path,
                       pattern = "*.csv")


# There's two types of files, one with Lat/Lon the other without
# there's several date/time formats, so read as chr and deal with it later.
col_types1 = cols(
  Province = col_character(),
  Country = col_character(),
  Update = col_character(),
  Confirmed = col_double(),
  Deaths = col_double(),
  Recovered = col_double()
)
col_types2 = cols(
  Province = col_character(),
  Country = col_character(),
  Update = col_character(),
  Confirmed = col_double(),
  Deaths = col_double(),
  Recovered = col_double(),
  Latitude = col_double(),
  Longitude = col_double()
)

# Read in the files, they add Lat/Lon half way through.
tbl_raw <- tibble()
switch_date <- as.Date("02-29-2020", "%m-%d-%Y")
for (file in csv_files){
  file_date <- as.Date(str_extract(file , "..-..-...."), "%m-%d-%Y")
  if( file_date <= switch_date){
    col_types <- col_types1
  } else {
    col_types <- col_types2
  }

  part = read_csv(paste0(data_path, file, collapse = .Platform$file.sep),
                  col_names = names(col_types$cols),
                  col_types = col_types,
                  skip = 1)
  tbl_raw <- bind_rows(tbl_raw, part)
}

#################  END READ RAW CSV FILES ##################

tidy_data <- distinct(tbl_raw)


# Create columns for cruise ship data
# Ships with countries keep coutry info but get a new column that they were on a ship
ships <- tibble( ship_in = c("Grand Princess Cruise Ship",
                             "(From Diamond Princess)",
                             "Diamond Princess cruise ship",
                             "Diamond Princess",
                             "Grand Princess"),
                 ship = c("Grand Princess",
                          "Diamond Princess",
                          "Diamond Princess",
                          "Diamond Princess",
                          "Grand Princess"))

sdata <- tidy_data
sdata$ship <- NA
sdata$pp <- NA #tidy_data$Province
tt <- 1
# this is ugly, but easiest way for my brain to do it.
for(province in sdata$Province){
  ss <- 1
  for(ship_in in ships$ship_in){
    if(!is.na(province) & str_detect(province, ship_in)){
      sdata$ship[[tt]] <- ships$ship[[ss]]
      splt <- unlist(str_split(province, ship_in))[[1]] # hack!
      splt <- str_remove_all(splt, "[(]")
      splt <- trimws(splt)
      if( splt == ""){
        splt <- NA
      } else if(splt == "Unassigned Location"){
        splt <- NA
      }
      sdata$pp[[tt]] <- splt
      break
    }
    ss <- ss + 1
  }
  tt <- tt + 1
}
tidy_data$Ship <- sdata$ship
tidy_data$Province[ !is.na(sdata$ship) ] <- sdata$pp[ !is.na(sdata$ship) ]
tidy_data$Country[ tidy_data$Country == "Cruise Ship"] <- "Others"


# Normalize some country names
tidy_data$Country[ tidy_data$Country == "Iran (Islamic Republic of)"] <- "Iran"
tidy_data$Country[ tidy_data$Country == "Korea, South"] <- "South Korea"
tidy_data$Country[ tidy_data$Country == "Taiwan*"] <- "Taiwan"
tidy_data$Country[ tidy_data$Country == "UK"] <- "United Kingdom"
tidy_data$Country[ tidy_data$Country == "Viet Nam"] <- "Vietnam"
tidy_data$Country[ tidy_data$Country == "Ivory Coast"] <- "Cote d'Ivoire"
tidy_data$Country[ tidy_data$Country == "Taiwan"] <- "Taipei and environs"

tidy_data <- distinct(tidy_data)

# run this to look at country names to make sure no new variants have been added
tidy_data %>% distinct(Country) %>% View
# not sure if Guinea and Ecuatorial Guinea are the same
# Congo is handled later because it has cities


##### Fix City, State Entries #####

# Some of the US has city/county, state abbr. Make it just the state
us <- tidy_data %>% filter(Country == "US")
us_states <- us %>% filter(str_detect(Province, ","))
uss <- us_states %>% mutate(split = str_split(Province, ',') ) %>% unnest
us_states$abb <- trimws(uss$split[seq(2,length(uss$split),2)])
us_states$City <- uss$split[seq(1,length(uss$split),2)]
us_states <- tibble(abb = state.abb, name = state.name) %>%
  bind_rows(tibble(abb = "D.C.", name = "Washington, DC")) %>%
  right_join(us_states, by= c("abb" = "abb")) %>%
  mutate(Province2 = name) %>%
  select(-abb, -name)
tidy_states <- tidy_data %>% left_join(us_states)
tidy_states$Province[!is.na(tidy_states$Province2)] <- tidy_states$Province2[!is.na(tidy_states$Province2)]

tidy_data <- distinct(tidy_states %>% select(-Province2))

# Oh Canada!
# tidy_data$City[ tidy_data$Province == "Toronto, ON"] <- "Toronto"
tidy_data$City[ tidy_data$Province == "London, ON"] <- "London"
tidy_data$City[ tidy_data$Province == "Montreal, QC"] <- "Montreal"
tidy_data$City[ tidy_data$Province == "Calgary, Alberta"] <- "Calgary"
tidy_data$City[ tidy_data$Province == "Edmonton, Alberta"] <- "Edmonton"
# cities first, then provinces
tidy_data$Province[ tidy_data$Province == "Toronto, ON"] <- "Ontario"
tidy_data$Province[ tidy_data$Province == "London, ON"] <- "Ontario"
tidy_data$Province[ tidy_data$Province == "Montreal, QC"] <- "Quebec"
tidy_data$Province[ tidy_data$Province == "Calgary, Alberta"] <- "Alberta"
tidy_data$Province[ tidy_data$Province == "Edmonton, Alberta"] <- "Alberta"

# Chicago is just weird
tidy_data$City[ tidy_data$Province == "Chicago"] <- "Chicago"
tidy_data$Province[ tidy_data$Province == "Chicago"] <- "Illinois"

# Fix Congo and their cities
#Congo (Brazzaville)
#Congo (Kinshasa)
tidy_data$City[ tidy_data$Country == "Congo (Brazzaville)"] <- "Brazzaville"
tidy_data$City[ tidy_data$Country == "Congo (Kinshasa)"] <- "Kinshasa"
tidy_data$Country[ tidy_data$Country == "Congo (Brazzaville)"] <- "Congo"
tidy_data$Country[ tidy_data$Country == "Congo (Kinshasa)"] <- "Congo"

# Check Provinces
tidy_data %>% distinct(Province) %>% View


################### Lat / Lon Backfill ####################

# Earlier data doesn't have Lat/Lon, but newer does, so backfill it.
tidy_data <- tidy_data %>% filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  select(-Update, -Confirmed, -Deaths, -Recovered, -Ship, -City) %>%
  right_join(tidy_data %>% select(-Latitude, -Longitude),
             by = c("Province" = "Province", "Country" = "Country"))
tidy_data <- distinct(tidy_data)

# Change the None provinces to NA
tidy_data$Province[tidy_data$Province == "None"] <- NA

# There's still some gaps, filter the non-location stuff.
#tidy_data %>% filter(is.na(Latitude)) %>% View

# Fix UK as province
tidy_data$Province[tidy_data$Province == "UK"] <- NA
tidy_data$Province[tidy_data$Province == "United Kingdom"] <- NA


#############################

tidy_data <- tidy_data %>% filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  select(-Update, -Confirmed, -Deaths, -Recovered, -Ship, -City) %>%
  right_join(tidy_data %>% select(-Latitude, -Longitude),
             by = c("Province" = "Province", "Country" = "Country"))
tidy_data <- distinct(tidy_data)


# other cruise ships
tidy_data$Ship[tidy_data$Province == 'Cruise Ship'] <- "Others"
tidy_data$Province[tidy_data$Province == 'Cruise Ship'] <- NA

# Fix the rest

# Bavaria 48.137154  11.576124
# Israel 31.046051 34.851612
# Australia -25.274399 133.775131
# Ivory Coast -22.497511 17.015369

# TODO do this with bracket language

filler <- tibble("Country" = c("Germany", "Israel", "Australia", "Cote d'Ivoire"),
                 "Latitude" = c(48.137154, 31.046051, -25.274399, -22.497511),
                 "Longitude" = c(11.576124, 34.851612, 133.775131, 17.015369))

tidy_data <- tidy_data %>% filter(is.na(Latitude)) %>% select(-Latitude, -Longitude) %>%
  right_join(filler, by = c("Country" = "Country")) %>%
  bind_rows(tidy_data %>% filter(!is.na(Latitude))) %>%
  bind_rows(tidy_data %>% filter(Country == "Others"))

tidy_data <- distinct(tidy_data)

# Replace NA counts with 0
tidy_data$Confirmed[is.na(tidy_data$Confirmed)] <- 0
tidy_data$Recovered[is.na(tidy_data$Recovered)] <- 0
tidy_data$Deaths[is.na(tidy_data$Deaths)] <- 0

# At this point there are 3 rows with a NA for Update time,
# however the counts are zero, so remove them
# tidy_data %>% filter(is.na(Update)) %>% View
tidy_data <- tidy_data %>% filter(!is.na(Update))


# Let's make the Update field a real datetime
# earlier times are of two types. Some didn't learn from
# Y2K so some have two digit years.
# 1/30/20 16:00
#2/1/2020 7:38
update_times <- tidy_data %>% filter(str_detect(Update, "/"))
update_times$formatted <- gsub("/20 ", "/2020 ", update_times$Update)
update_times$dttm <- lubridate::as_datetime(update_times$formatted, format = "%m/%d/%Y %H:%M")
update_times <- update_times %>% select(-formatted)

other_times <- tidy_data %>%
  filter(!str_detect(Update, "/")) %>%
  mutate(dttm = lubridate::as_datetime(Update))

# as.POSIXct all the dttm so bind_rows works
update_times$dttm <- as.POSIXct(update_times$dttm)
other_times$dttm <- as.POSIXct(other_times$dttm)

tidy_data <- bind_rows(other_times, update_times)
tidy_data$Update <- tidy_data$dttm
tidy_data <- tidy_data %>% select(-dttm)

#################### Rename and Save #################

covid19_daily <- tidy_data
usethis::use_data(covid19_daily, overwrite = TRUE)

