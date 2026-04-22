library(pdftools)
library(glue)
library(tidyverse)

pdf_date <- ymd("20260301")
pdf_file <- glue("raw-data/month-{pdf_date}.pdf")

latest_url <- "https://www.scotrail.co.uk/media/85265/download?inline"
if (!file.exists(pdf_file)) {
  download.file(latest_url, pdf_file, mode = "wb")
}

# Raw data
txt <- pdf_text(pdf_file)
split_txt <- txt |>
  str_split_1("\n")
tbl_begins <- which(str_detect(split_txt, "Location"))
tbl_ends <- which(str_detect(split_txt, "On Time_T"))

clean_data <- tibble(
  raw_data = split_txt[(tbl_begins + 2):(tbl_ends - 1)]
) |>
  mutate(raw_data = str_trim(raw_data)) |>
  separate_wider_delim(raw_data,
                       delim = regex("\\s{2,}"),
                       names_sep = "_",
                       too_few = "align_start") |>
  drop_na(raw_data_1) |>
  separate_wider_delim(raw_data_9, delim = "% ", names_sep = "-")

df1 <- clean_data[,1:5]
colnames(df1) <- c("Location", "On_Time_T", "Booked_T", "On_Time_A", "STPM")
df2 <- clean_data[,6:10]
colnames(df2) <- c("Location", "On_Time_T", "Booked_T", "On_Time_A", "STPM")

final_data <- rbind(df1, df2) |>
  drop_na(Location) |>
  filter(Location != "") |>
  mutate(
    across(-Location, ~if_else(.x == "-", NA, .x)),
    across(-Location, ~str_remove(.x, "%")),
    across(-Location, ~as.numeric(.x))
  ) |>
  arrange(Location) |>
  mutate(date = pdf_date, .after = 0)

write_csv(final_data, "data/monthly_data_stations.csv")




# Big numbers
monthly_perf <- tibble(
  date = pdf_date,
  perf = str_extract(txt, "\\d+\\.?\\d*%")
) |>
  mutate(perf = str_remove(perf, "%"),
         perf = as.numeric(perf))

prev_date <- floor_date(pdf_date - 1, "month")
existing_data <- read_csv("data/monthly_data.csv")
prev_data <- existing_data |>
  filter(date == prev_date)
if (nrow(prev_data) == 0) {
  comp <- "Unavailable"
} else {
  comp_value <- monthly_perf - prev_data$perf
  if (comp_value < 0) {
    comp <- paste0(comp_value, " pp")
  } else if (comp_value > 0) {
    comp <- paste0(comp_value, " pp")
  } else {
    comp <- "Unchanged"
  }
}
monthly_perf$comparison <- comp

new_monthly_data <- rbind(existing_data, monthly_perf) |>
  distinct() |>
  arrange(desc(date))

write_csv(new_monthly_data, "data/monthly_data.csv")





