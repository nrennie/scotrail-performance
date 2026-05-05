library(pdftools)
library(tidyverse)
library(rvest)
library(polite)

page_url <- "https://www.scotrail.co.uk/performance-and-reliability/previous-daily-statistical-summaries"

session <- bow(page_url)

raw_url <- scrape(session)

daily_URLs <- raw_url |>
  html_elements(".cke-acc") |>
  as.character() |>
  as_tibble() |>
  separate_longer_delim(value, delim = "</p>") |>
  filter(row_number() != 1) |>
  mutate(
    date = str_extract(value, "(?<=summary).*?(?=</a>)"),
    date = str_trim(date),
    date = dmy(date)
  ) |>
  mutate(
    link = str_extract(value, "(?<=media).*?(?=download)"),
    link = paste0("https://www.scotrail.co.uk/media", link, "download?inline")
  ) |>
  select(date, link) |>
  filter(
    !is.na(date),
    !str_detect(link, "NA")
  ) |>
  distinct() |>
  arrange(desc(date))

# need to deal with NA values
write_csv(daily_URLs, "data/daily_URLs.csv")


get_daily_values <- function(pdf_date, data = daily_URLs) {
  # Look up URL
  pdf_url <- data |>
    filter(date == pdf_date) |>
    pull(link)
  # Download file if it doesn't already exist
  pdf_file <- paste0("raw-data/", pdf_date, ".pdf")
  if (!file.exists(pdf_file)) {
    pdf_session <- bow(pdf_url)
    download.file(pdf_session$url, pdf_file, mode = "wb")
  }
  # Process PDF data
  txt <- pdf_text(pdf_file)
  output <- txt |>
    str_split_1("\n\n") |>
    as_tibble() |>
    separate_wider_delim(value,
                         delim = regex("\\s{2,}"),
                         names_sep = "_",
                         too_few = "align_start"
    ) |>
    drop_na() |>
    mutate(
      across(
        everything(), ~ str_remove_all(.x, "\\b\\d{2}/\\d{2}/\\d{4}\\b")
      )
    ) |>
    mutate(
      value_2 = parse_number(value_2),
      value_1 = str_remove_all(value_1, "\n|:"),
      value_1 = str_trim(value_1),
      value_1 = case_when(
        row_number() == 3 ~ str_replace(
          value_1, "cancellations", "planned cancellations"
        ),
        row_number() == 4 ~ str_replace(
          value_1, "cancellations", "unplanned cancellations"
        ),
        TRUE ~ value_1
      )
    ) |>
    mutate(value_1 = if_else(
      str_detect(value_1, "PPM"), "PPM", value_1
    )) |>
    pivot_wider(
      names_from = value_1,
      values_from = value_2
    ) |>
    mutate(date = pdf_date, .before = 1)

  return(output)
}

# check against existing data
existing_data <- read_csv("data/daily_data.csv")
latest_existing_date <- max(existing_data$date)

new_dates <- daily_URLs |>
  filter(date > latest_existing_date)

if (nrow(new_dates) > 0) {
  new_daily_data <- purrr::map(
    .x = new_dates$date,
    .f = ~get_daily_values(.x)
  ) |>
    bind_rows() |>
    select(-any_of("value_3"))
}

daily_data_final <- rbind(existing_data, new_daily_data) |>
  arrange(desc(date))

write_csv(daily_data_final, "data/daily_data.csv")
