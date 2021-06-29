#
#   Get CRAN download stats for packages released on or after 1.1.2021
#

setwd(here::here("r-trending"))

library(rvest)
library(cranlogs)
library(glue)
library(readr)
library(dplyr)
library(stringr)


if (!file.exists("data/packs-by-date.csv")) {
  url <- "https://cran.r-project.org/web/packages/available_packages_by_date.html"
  packs_by_date <- read_html(url) |> html_table()
  packs_by_date <- packs_by_date[[1]]
  packs_by_date$Date <- as.Date(packs_by_date$Date)
  colnames(packs_by_date) <- c("last_modified", "package", "title")
  write_csv(packs_by_date, "data/packs-by-date.csv")
} else {
  packs_by_date <- read_csv("data/packs-by-date.csv")
}


if (!file.exists("data/trending.csv")) {
  trending <- jsonlite::read_json("https://cranlogs.r-pkg.org/trending")
  trending <- bind_rows(lapply(trending, as_tibble))
  write_csv(trending, "data/trending.csv")
} else {
  trending <- read_csv("data/trending.csv")
}

# https://github.com/tomaztk/First_Release_Dates_R_Packages/blob/master/R_Package_First_Release.R


# Crawl archives for first release date -----------------------------------
#
#   How to ID packages released in 2020 or later? Check CRAN archives for
#   earliest version of each package.
#

root_url <- "https://cran.r-project.org/src/contrib/Archive/"

# Get the initial version of all packages listed in the archive. To get the
# first release date, I'm going to download each package archive page below;
# this takes a while and I've been running the loop intermittently, so do this
# chunk here only once to initialize the whole arch data frame, then read it
# from disk if needed.
if (!file.exists("data/arch.csv")) {
  arch <- html_table(read_html(root_url))[[1]]
  arch <- arch[, 2:3]
  arch <- tail(arch, -2)
  colnames(arch) <- c("package", "last_modified")
  arch$package <- gsub("/$", "", arch$package)
  arch$last_modified <- as.Date(arch$last_modified)
  arch <- arch[!arch$package=="", ]

  # take out REAMDE
  arch <- arch[!arch$package=="README", ]

  # initialize column we will fill in in the loop below
  arch$first_release <- as.Date(NA)
} else {
  arch <- read_csv("data/arch.csv")
}

# this loop is written so that i can be restarted without loosing progress
# i.e. filled in first_release dates will be preserved and skipped unless
# re-initializing above
arch <- arch[sample(1:nrow(arch)), ]
new <- 0  # separate counter for only when we download new first release
for (i in seq_len(nrow(arch))) {

  if (!is.na(arch$first_release[i])) next

  new <- new + 1
  if (new %% 20==0) cat(new, "\n")

  pack_url <- paste0(root_url, arch$package[i], "/")
  pack_url <- url(pack_url, "rb")
  page_i <- read_html(pack_url)
  close(pack_url)

  dates <- str_extract_all(as.character(page_i), "[0-9]{4}-[0-9]{2}-[0-9]{2}")[[1]]
  arch$first_release[i] <- min(as.Date(dates), na.rm = TRUE)
}
arch <- arch %>% arrange(package)
table(!is.na(arch$first_release))
write_csv(arch, "data/arch.csv")

# Merge the list of packages from the packages by last modified date page and
# what we get from the archives.
#
# Packages that have been removed will show up in archives, but not in the
# by date list, so use a left join to only keep the packages by date, i.e.
# packages still up on CRAN, list.
#
# Also, don't use "last_modified" as part of join, it doesn't always match up.
cran_packs <- left_join(packs_by_date, arch[, c("package", "first_release")], by = "package")

# Packages that only have one version on CRAN won't have an archive yet;
# we can set those dates to the last modified date
one_v_only <- !cran_packs$package %in% arch$package
cran_packs$first_release[one_v_only] <- cran_packs$last_modified[one_v_only]

write_csv(cran_packs, "data/cran-packs.csv")



# ID RStudio-related packages ---------------------------------------------

root_url <- "https://cran.r-project.org/package="

cran_packs$rstudio <- NA_integer_
for (i in 1:nrow(cran_packs)) {

  if (i %% 50==0) cat(i, "\n")

  if (!is.na(cran_packs$rstudio[[i]])) next
  # only do 2020+ packages
  if (cran_packs$first_release[[i]] < "2020-01-01") next

  cran_url <- paste0(root_url, cran_packs$package[[i]])
  cran_page <- read_html(cran_url)
  # search for RStudio, like "RStudio [cph, fnd]"
  cran_packs$rstudio[[i]] <- as.integer(str_detect(html_text(cran_page), "RStudio \\[cph"))

}



# Download stats for target packages --------------------------------------
#
#   Look at download stats only for packages released on or after 1 January 2020
#

sample <- cran_packs %>%
  filter(first_release >= "2020-01-01") %>%
  arrange(package)

for (i in seq_len(nrow(sample))) {
  pack_i <- sample$package[i]
  file_i <- glue("data/downloads/{pack_i}.csv")

  if (file.exists(file_i)) next

  cat(pack_i, "\n")
  dl <- cran_downloads(pack_i, from = "2020-01-01", to = "2021-06-09")
  write_csv(dl, file_i)
}



# Combine the download data

dl <- dir("data/downloads", full.names = TRUE) %>%
  lapply(read_csv, col_types = cols(
    date = col_date(format = ""),
    count = col_double(),
    package = col_character()
  )) %>%
  bind_rows()

dl <- dl %>%
  group_by(package) %>%
  mutate(count_ma21 = rollmean(count, k = 21, na.pad = TRUE, align = "center")) %>%
  ungroup()

# there are no downloads before a package was up on CRAN; join and filter based
# on first release date
dl <- left_join(dl, cran_packs[, c("package", "first_release", "rstudio")]) %>%
  filter(date >= first_release)

write_rds(dl, "data/downloads.rds", compress = "gz")

