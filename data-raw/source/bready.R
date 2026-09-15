## code to prepare `bready` dataset goes here
# date of access: 4/27/2026
library(openxlsx)
library(janitor)

# download zip file
url <- "https://www.worldbank.org/content/dam/sites/b-ready/documents/excel/B-READY_ALL_DATA_2025.zip"
temp_zip <- tempfile(fileext = ".zip")
download.file(url, destfile = temp_zip)
unzip(temp_zip, exdir = tempdir())

# read in excel file
xlsx_file <- list.files(tempdir(), pattern = "01_B-READY-2025-PILLAR-TOPIC-SCORES.xlsx", full.names = TRUE)

bready_input <- read.xlsx(
  xlsx_file,
  sheet = "00_B-READY_Pillar_Score",
  rows = 1:102
)

# clean data
bready <- bready_input |>
  clean_names() |>
  select(
    economy,
    country_code = economy_code,
    everything()
  )

topic_sheets <- c(
  "01_Business_Entry" = "Business Entry",
  "03_Utility_Services" = "Utility Services",
  "05_Financial_Services" = "Financial Services",
  "09_Market_Competition" = "Market competition"
)

bready_topic <- purrr::imap_dfr(topic_sheets, \(topic, sheet) {
  read.xlsx(
    xlsx_file,
    sheet = sheet,
    startRow = 4
  ) |>
    clean_names() |>
    select(
      country_code = economy_code,
      pillar_1_overall,
      pillar_2_overall,
      pillar_3_overall
    ) |>
    mutate(
      topic = topic
    )
})

usethis::use_data(bready, overwrite = TRUE)
usethis::use_data(bready_topic, overwrite = TRUE)
