################################################################################
#                         Dublin Property Geocoding                            #
################################################################################

# libraries --------------------------------------------------------------------
library(here)
library(tidyverse)
library(utils)
library(janitor)
library(jsonlite)
library(config)
library(prettymapr)

# sources ----------------------------------------------------------------------
source(here::here("script/functions_project.R"))

# download PPR -----------------------------------------------------------------
utils::download.file(
  url = "https://www.propertypriceregister.ie/website/npsra/ppr/npsra-ppr.nsf/Downloads/PPR-2018-Dublin.csv/$FILE/PPR-2018-Dublin.csv",
  destfile = here::here("data/ppr_dublin_2018.csv")
)

# geocode addresses ------------------------------------------------------------
geocode_dublin_2018 <- here::here("data/ppr_dublin_2018.csv") %>% 
  readr::read_csv() %>% 
  dplyr::rename(
    date_of_sale = `Date of Sale (dd/mm/yyyy)`,
    price = `Price (<U+0080>)`
  ) %>%
  dplyr::mutate(price = readr::parse_number(price)) %>%
  dplyr::mutate(date_of_sale = as.Date(date_of_sale, format = "%d/%m/%Y")) %>%
  dplyr::mutate(full_address = paste(Address, County, "Ireland")) %>%
  tibble::rowid_to_column("sale_id") %>%
  dplyr::mutate(year = lubridate::year(date_of_sale)) %>% 
  janitor::clean_names() %>% 
  dplyr::filter(!grepl('apartment|Apartment|APART|APT|Apt', address)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(geocode_batch(address)) %>%
  dplyr::filter(between(lat, 50, 53.7) & between(lng, -7, -5.5)) %>% #gps artefact
  readr::write_rds(here::here("data/geocode_dublin_2018.rds"))