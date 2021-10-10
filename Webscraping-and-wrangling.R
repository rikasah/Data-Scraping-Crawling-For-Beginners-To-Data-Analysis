# Load packages -----------------------------------------------------------
install.packages('rvest')
install.packages('curl')
install.packages('archive')
install.packages('tidyverse')
install.packages('vroom')
install.packages('hrbrthemes')
install.packages('scales')
install.packages('sf')

library(rvest)
library(curl)
library(archive)
library(tidyverse)
library(vroom)
library(slider)
library(hrbrthemes)
library(scales)
library(sf)

# Scrape data from webpage ------------------------------------------------

#' We will scrape data from www.liburnasional.com. 
#' Let's start with simple procedure: get holidays date 
#' and corresponding details from year 2019.

libur2019_url <- "https://www.liburnasional.com/kalender-lengkap-2019/"
libur2019_html <- read_html(libur2019_url)
libur2019_html

#' In order to extract the data, first we need to find the CSS 
#' selector in which the information resides. 
#' It's essential for us to inspect HTML elements of our target webpage.

libur2019_list <-
  read_html(libur2019_url) %>%
  html_nodes(".libnas-calendar-full-detail") %>%
  html_table()

glimpse(libur2019_list)

# Preprocess data ---------------------------------------------------------

#' We have successfully scraped the data, but it doesn't mean the data 
#' is ready for analysis. Let's perform some data cleaning.

libur2019_semiclean <-
  libur2019_list %>%
  bind_rows() %>%
  transmute(
    date = paste(X1, "2019"),
    event = X3
  ) %>%
  separate_rows(
    date,
    sep = " - "
  ) %>%
  separate(
    date,
    into = c("date", "month", "year"),
    sep = "\\s",
    fill = "right"
  ) %>%
  fill(month, year, .direction = "up")

glimpse(libur2019_semiclean)

#' It would be even better if we combine date, month, and year 
#' columns into one. Followed by adjusting it's data type.

libur2019 <-
  libur2019_semiclean %>%
  mutate(
    month = recode(
      month,
      "Januari" = "January",
      "Februari" = "February",
      "Maret" = "March",
      "April" = "April",
      "Mei" = "May",
      "Juni" = "June",
      "Juli" = "July",
      "Agustus" = "August",
      "September" = "September",
      "Oktober" = "October",
      "November" = "November",
      "Desember" = "December"
    )
  ) %>%
  unite(
    col = "date",
    date,
    month,
    year,
    sep = "-"
  ) %>%
  mutate(
    date = as.Date(date, format = "%e-%B-%Y")
  ) %>%
  arrange(date)

glimpse(libur2019)

# Properly (function)-ing -------------------------------------------------

#' Great! But how if we wanted to perform similar procedure 
#' but for a different year? Of course we can re-write our previous code. 
#' But is there any better ways? Welcome: function creation in R.

foo <- function(name = "Putu") {
  res <- runif(1)
  message(name, ", your random number is ", res, "\n~~Current time: ", Sys.time())
  return(res)
}

foo()

#' In our case, we need a function to abstract our previous procedure. 
#' Let's call it `get_holidays()` that has single argument `year`.

get_holidays <- function(year) {

  if (length(year) != 1) {
    stop("please specify one year only", call. = FALSE)
    res <- NULL
  }

  if (!inherits(year, c("integer", "character"))) {
    stop("year must be a positive integer of character string", call. = FALSE)
    res <- NULL
  }

  if (nchar(year) != 4) {
    stop("year must be a four digits number", call. = FALSE)
    res <- NULL
  }

  if (as.numeric(year) < 2013 | as.numeric(year) > 2023) {
    stop("only data since 2013 up to 2023 are available", call. = FALSE)
    res <- NULL
  }

  res <-
    str_glue("https://www.liburnasional.com/kalender-lengkap-{year}/") %>%
    read_html() %>%
    html_nodes(".libnas-calendar-full-detail") %>%
    html_table() %>%
    bind_rows() %>%
    transmute(
      date = str_glue("{X1} {year}"),
      event = X3
    ) %>%
    separate_rows(
      date,
      sep = " - "
    ) %>%
    separate(
      date,
      into = c("date", "month", "year"),
      sep = "\\s",
      fill = "right"
    ) %>%
    fill(month, year, .direction = "up") %>%
    mutate(
      month = recode(
        month,
        "Januari" = "January",
        "Februari" = "February",
        "Maret" = "March",
        "April" = "April",
        "Mei" = "May",
        "Juni" = "June",
        "Juli" = "July",
        "Agustus" = "August",
        "September" = "September",
        "Oktober" = "October",
        "November" = "November",
        "Desember" = "December"
      )
    ) %>%
    unite(
      col = "date",
      date,
      month,
      year,
      sep = "-"
    ) %>%
    mutate(
      date = as.Date(date, format = "%e-%B-%Y")
    ) %>%
    arrange(date)

  return(res)
}

#' Try it out! Could you explain why we found some error in these lines?

get_holidays(TRUE)
get_holidays(1950)
get_holidays(1950L)
get_holidays(-2020)
get_holidays(2020.25)
get_holidays(123456L)
get_holidays(2014L)
get_holidays("2017")
get_holidays(c(1945L, 2045L))

# It begins: 2020 ---------------------------------------------------------

#' Now please scrape data for year 2020

libur_2020 <- get_holidays(year = 2020L)
libur_2021 <- get_holidays(year = '2021')
libur_2023 <- get_holidays(year = '2023')

#' How many unique holiday events are in 2020?

libur_2020 %>%
  distinct(event)

#' Let's see number of days per holiday events! We will sort it 
#' by the most largest number of days.

libur_2020 %>%
  count(event, sort = TRUE)

# Download Indonesia GADM data --------------------------------------------

#' Next, we are going to fetch another data 
#' from the internet about Indonesia spatial mapping

id_gadm_url <- "https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_IDN_2_sf.rds"
id_gadm_path <- file.path("data-raw", basename(id_gadm_url))

if (!file.exists(id_gadm_path)) {
  curl_download(
    url = id_gadm_url,
    destfile = id_gadm_path,
    quiet = FALSE
  )
}

# Download Facebook Movement Range data -----------------------------------

#' Another interesting data also exist: proportion of those 
#' who staying-at-home during pandemic time. 
#' Let's fetch the year 2020 data for now.

stayput_2020_url <-
  read_html("https://data.humdata.org/dataset/movement-range-maps") %>%
  html_node("li.resource-item:nth-child(3) > div:nth-child(4) > a:nth-child(1)") %>%
  html_attr("href") %>%
  paste0("https://data.humdata.org", .)

stayput_2020_path <-
  file.path("data-raw", basename(stayput_2020_url))

if (!file.exists(stayput_2020_path)) {
  curl_download(
    url = stayput_2020_url,
    destfile = stayput_2020_path,
    quiet = FALSE
  )
}

# Import data -------------------------------------------------------------

#' Next we want to import those data into our R session

id_gadm_sf <- read_rds(id_gadm_path)

stayput_2020_filename <-
  str_subset(archive(stayput_2020_path)[["path"]], "movement-range")

stayput_2020_big <-
  vroom(
    file = archive_read(stayput_2020_path, stayput_2020_filename),
    col_types = "cccccddcc"
  )

# Extract area metadata from GADM -----------------------------------------

class(id_gadm_sf)

id_gadm_metadata <-
  id_gadm_sf %>%
  as_tibble() %>%
  select(-VARNAME_2, -CC_2, -HASC_2, -geometry, -starts_with("NL"))

class(id_gadm_metadata)

# Extract movement data in Indonesia --------------------------------------

id_stayput_2020_district <-
  stayput_2020_big %>%
  filter(country == "IDN") %>%
  select(
    district_name = polygon_name,
    district_code_gadm = polygon_id,
    date = ds,
    stayput = all_day_ratio_single_tile_users,
    pct_movement_changes = all_day_bing_tiles_visited_relative_change
  ) %>%
  mutate(date = as.Date(date)) %>%
  arrange(district_name, date)

glimpse(id_stayput_2020_district)

# Subset the latest stayput proportion ------------------------------------

id_stayput_2020_district_latest <-
  id_stayput_2020_district %>%
  filter(date == max(date)) %>%
  left_join(id_gadm_metadata, by = c("district_code_gadm" = "GID_2"))

glimpse(id_stayput_2020_district_latest)

#' Challenge: please pick one holiday event from `libur_2020` data that you have scraped. Subset the `id_stayput_2020_district` by using that holiday event date!

# How is the share of people staying at home? -----------------------------

id_stayput_2020_district_latest %>%
  ggplot(aes(stayput)) +
  geom_histogram(fill = "cyan4") +
  scale_x_percent() +
  labs(
    x = "Proportion of stay-at-home",
    y = "Frequency",
    title = "Share of people staying at home during pandemic in Indonesia",
    caption = format(max(id_stayput_2020_district_latest$date), format = "Facebook Movement Range | %d %B %Y")
  ) +
  theme_ipsum(grid = "Y", ticks = TRUE)

#' Challenge: create the same plot for holiday event that you choose earlier!

# How is the comparison between provinces in Borneo Island? ---------------

plot_caption <- format(max(id_stayput_2020_district_latest$date), format = "Facebook Movement Range | %d %B %Y")

id_stayput_2020_district_latest %>%
  filter(str_detect(NAME_1, "Kalimantan")) %>%
  mutate(NAME_1 = fct_reorder(NAME_1, stayput, median)) %>%
  ggplot(aes(NAME_1, stayput)) +
  geom_boxplot(fill = "#00BD9D") +
  scale_y_percent() +
  labs(
    x = NULL,
    y = "Proportion of stay-at-home",
    title = "Stay-at-home behaviour between provinces in Borneo Island",
    caption = plot_caption
  ) +
  theme_ipsum(grid = "Y", ticks = TRUE)

#' Challenge: again, please similar plot for your holiday event that you choose. Also, you are free to choose another provinces within a certain island.


# In Banten, how is the trend comparison between City and Regency  --------

id_stayput_2020_district %>%
  left_join(id_gadm_metadata, by = c("district_code_gadm" = "GID_2")) %>%
  filter(NAME_1 == "Banten") %>%
  ggplot(aes(date, stayput)) +
  facet_wrap(~ENGTYPE_2) +
  geom_point(size = 1, shape = 21, fill = "#6D2E46", colour = "white", alpha = 0.5) +
  geom_smooth(colour = "#4062BB") +
  scale_x_date(
    breaks = "2 months",
    labels = date_format("%b\n%Y")
  ) +
  scale_y_percent() +
  labs(
    x = NULL,
    y = "Proportion of stay-at-home",
    title = "Trend of stay-at-home behaviour during pandemic in Banten",
    caption = plot_caption
  ) +
  theme_ipsum(grid = "Y", ticks = TRUE, axis_text_size = 9)

ggsave(
  "outfile/banten_stayput.png",
  width = 8,
  height = 5,
  dpi = 300,
  type = "cairo-png"
)

# Higher level ------------------------------------------------------------

#' Calculate for province level data using median summary.

id_stayput_2020_province <-
  id_stayput_2020_district %>%
  left_join(id_gadm_metadata, by = c("district_code_gadm" = "GID_2")) %>%
  group_by(
    province_name = NAME_1,
    province_code_gadm = GID_1,
    date
  ) %>%
  summarise(
    across(stayput:pct_movement_changes, median)
  ) %>%
  ungroup()

#' Calculate for country level data using median summary.

id_stayput_2020_country <-
  id_stayput_2020_district %>%
  group_by(date) %>%
  summarise(
    across(stayput:pct_movement_changes, median)
  )

id_stayput_2020_country %>%
  mutate(stayput_7ma = slide_dbl(stayput, mean, .before = 3, .after = 3)) %>%
  ggplot(aes(date, stayput)) +
  geom_line(colour = "gray80", size = 0.3) +
  geom_line(aes(y = stayput_7ma), colour = "#4062BB", size = 0.8) +
  scale_x_date(
    breaks = "1 months",
    labels = date_format("%b\n%Y"),
    expand = c(0.005, 0.005)
  ) +
  scale_y_percent(position = "right") +
  labs(
    x = NULL,
    y = NULL,
    title = "Stay-at-home behaviour during pandemic in Indonesia",
    subtitle = "Blue line shows 7 days moving average of stay-at-home proportion",
    caption = format(max(id_stayput_2020_country$date), format = "Facebook Movement Range | %d %B %Y")
  ) +
  theme_ipsum(grid = "Y", ticks = TRUE, axis_text_size = 9)

# Save the data -----------------------------------------------------------

write_csv(libur_2020, "data/libur_2020.csv")
write_rds(libur_2020, "data/libur_2020.rds")
write_csv(id_gadm_metadata, "data/id_gadm_metadata.csv")
write_rds(id_gadm_metadata, "data/id_gadm_metadata.rds")
write_csv(id_stayput_2020_district, "data/id_stayput_2020_district.csv")
write_rds(id_stayput_2020_district, "data/id_stayput_2020_district.rds")
write_csv(id_stayput_2020_province, "data/id_stayput_2020_province.csv")
write_rds(id_stayput_2020_province, "data/id_stayput_2020_province.rds")
write_csv(id_stayput_2020_country, "data/id_stayput_2020_country.csv")
write_rds(id_stayput_2020_country, "data/id_stayput_2020_country.rds")
 