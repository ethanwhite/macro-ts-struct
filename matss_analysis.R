library(rdataretriever)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)

get_data <- function(){
  if (!("bbs.sqlite" %in% list.files("data"))){
    rdataretriever::install('breed-bird-survey', 'sqlite', db_file = 'data/bbs.sqlite')
  }
  bbs_db <- src_sqlite("data/bbs.sqlite")
  query <- "SELECT
                  (counts.statenum*1000) + counts.Route AS site_id,
                  Latitude AS lat,
                  Longitude AS long,
                  Aou AS species_id,
                  counts.Year AS year,
                  speciestotal AS abundance
                FROM
                  breed_bird_survey_counts AS counts
                  JOIN breed_bird_survey_weather
                    ON counts.statenum=breed_bird_survey_weather.statenum
                    AND counts.route=breed_bird_survey_weather.route
                    AND counts.rpid=breed_bird_survey_weather.rpid
                    AND counts.year=breed_bird_survey_weather.year
                  JOIN breed_bird_survey_routes
                    ON counts.statenum=breed_bird_survey_routes.statenum
                    AND counts.route=breed_bird_survey_routes.route
                WHERE breed_bird_survey_weather.runtype=1 AND breed_bird_survey_weather.rpid=101"
  data <- tbl(bbs_db, dplyr::sql(query)) %>% 
    collect(n = Inf)
}

#' Filter poorly sampled BBS species
#' 
#' From https://github.com/weecology/bbs-forecasting
#'
#' Removes waterbirds, shorebirds, owls, kingfishers, knightjars,
#' dippers. These species are poorly sampled due to their aquatic or
#' noctural nature. Also removes taxa that were either partially unidentified
#' (e.g. "sp.") or were considered hybrids (e.g. "A x B") or were listed as more
#' than one species (e.g. "A / B")
#'
#' @param df dataframe containing an species_id column
#'
#' @return dataframe, filtered version of initial dataframe
#' @importFrom dplyr "%>%" inner_join do rowwise select filter group_by ungroup full_join n_distinct semi_join left_join
filter_species <- function(df){
  bbs_db <- src_sqlite("data/bbs.sqlite")
  species_table = tbl(bbs_db, "breed_bird_survey_species")
  
  is_unidentified = function(names) {
    #Befor filter account for this one hybrid of 2 subspecies so it's kept
    names[names=='auratus auratus x auratus cafer']='auratus auratus'
    grepl('sp\\.| x |\\/', names)
  }
  
  valid_taxa = species_table %>%
    filter(!is_unidentified(species)) %>%
    filter(aou > 2880) %>%
    filter(aou < 3650 | aou > 3810) %>%
    filter(aou < 3900 | aou > 3910) %>%
    filter(aou < 4160 | aou > 4210) %>%
    filter(aou != 7010)
  
  filter(df, species_id %in% valid_taxa$aou)
}

#' Combine subspecies into their common species
#' 
#' From https://github.com/weecology/bbs-forecasting
#'
#' @importFrom dplyr "%>%" filter slice group_by summarise ungroup
#' @importFrom magrittr extract2
#' @importFrom stringr word
combine_subspecies = function(df){
  bbs_db <- src_sqlite("data/bbs.sqlite")
  species_table = tbl(bbs_db, "breed_bird_survey_species")
  
  # Subspecies have two spaces separated by non-spaces
  subspecies_names = species_table %>%
    filter(species_table$aou %in% unique(df$species_id)) %>%
    magrittr::extract2("spanish_common_name") %>%
    grep(" [^ ]+ ", ., value = TRUE)
  
  subspecies_ids = species_table %>%
    filter(spanish_common_name %in% subspecies_names) %>%
    extract2("aou")
  
  # Drop all but the first two words to get the root species name,
  # then find the AOU code
  new_subspecies_ids = species_table %>%
    slice(match(word(subspecies_names, 1,2),
                species_table$spanish_common_name)) %>%
    extract2("aou")
  
  # replace the full subspecies names with species-level names
  for (i in seq_along(subspecies_ids)) {
    df$species_id[df$species_id == subspecies_ids[i]] = new_subspecies_ids[i]
  }
  
  df %>%
    group_by(site_id, year, species_id, lat, long) %>%
    summarise(abundance = sum(abundance)) %>%
    ungroup()
}

#' Get BBS population time-series data
#' 
#' Modified from https://github.com/weecology/bbs-forecasting
#'
#' Selects sites with data spanning 1982 through 2013 containing at least 25
#' samples during that period.
#'
#'
#' @param start_yr num first year of time-series
#' @param end_yr num last year of time-series
#' @param min_num_yrs num minimum number of years of data between start_yr & end_yr
#'
#' @return dataframe with site_id, lat, long, year, species_id, and abundance
get_pop_ts_data <- function(start_yr, end_yr, min_num_yrs){
  pop_ts__data = get_data() %>%
    filter_ts(start_yr, end_yr, min_num_yrs) %>%
    tidyr::complete(site_id, year) %>% 
    ungroup()
}

#' Filter BBS to specified time series period and number of samples
#'
#' @param bbs_data dataframe that contains BBS site_id and year columns
#' @param start_yr num first year of time-series
#' @param end_yr num last year of time-series
#' @param min_num_yrs num minimum number of years of data between start_yr & end_yr
#'
#' @return dataframe with original data and associated environmental data
filter_ts <- function(bbs_data, start_yr, end_yr, min_num_yrs){
  sites_to_keep = bbs_data %>%
    dplyr::filter(year >= start_yr, year <= end_yr) %>%
    dplyr::group_by(site_id) %>%
    dplyr::summarise(num_years=length(unique(year))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(num_years >= min_num_yrs)
  
  filterd_data <- bbs_data %>%
    dplyr::filter(year >= start_yr, year <= end_yr) %>%
    dplyr::filter(site_id %in% sites_to_keep$site_id)
}

pop_ts_data = get_pop_ts_data(1980, 2015, 35) #%>%
#   filter_species() %>%
#   group_by(site_id) %>%
#   combine_subspecies()

rand_pop_ts <- function(data){
  site <- sample(unique(data$site_id), 1)
  site_data <- filter(data, site_id == site)
  species <- sample(unique(site_data$species_id), 1)
  species_data <- filter(site_data, species_id == species)
  ggplot(species_data, aes(x = year, y = abundance)) +
    geom_point() +
    geom_line()
}

rand_pop_ts(pop_ts_data)
  