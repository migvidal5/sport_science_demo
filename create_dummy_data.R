library(tidyverse)
library(assertthat)

calc_ewma <- function(tl, n, seed = mean(tl, na.rm = TRUE), lag = 0) {
  tryCatch({
    assert_that(all(is.numeric(tl)))
    assert_that(length(tl) > 0)
    assert_that(is.numeric(n), n > 0)
    assert_that(is.numeric(lag), lag >= 0)
    assert_that(length(seed) == 1, is.numeric(seed), seed >= 0)
    
    l <- length(tl)
    ewma <- numeric(l)
    ewma[1] <- seed
    lambda <- 2 / (n + 1)
    
    for (i in 2:l) {
      ewma[i] <- tl[i] * lambda + (1 - lambda) * ewma[i - 1]
    }
    
    # Apply lag safely
    ewma <- dplyr::lag(ewma, n = lag)
    return(ewma)
  }, error = function(e) {
    message("Error in calc_ewma: ", e$message)
    return(rep(NA, length(tl)))
  })
}

set.seed(123)  # For reproducibility

# Players
players <- paste("player", letters[1:10])

# Dates
dates <- seq.Date(from = Sys.Date() - 29, by = "day", length.out = 30)

# Create data
gps_data <- expand.grid(player = players, date = dates) |>
  dplyr::mutate(
    total_distance = round(runif(n(), 4000, 10000), 1),
    hsr_distance = round(runif(n(), 1000, 3000), 1),
    sprint_distance = round(runif(n(), 200, 1000), 1),
    accels = sample(10:40, n(), replace = TRUE),
    decels = sample(10:40, n(), replace = TRUE)
  )

wellness_data <- expand.grid(player = players, date = dates) |>
  dplyr::mutate(
    stress = sample(1:7, n(), replace = TRUE),
    mood = sample(1:7, n(), replace = TRUE),
    soreness = sample(1:7, n(), replace = TRUE),
    energy = sample(1:7, n(), replace = TRUE)
  )

sleep_data <- expand.grid(player=players, date = dates) |>
  dplyr::mutate(
    sleep_duration = round(runif(n(), 3,10), 1),
    resting_hr = sample(40:100, n(), replace = TRUE),
    hrv = sample(40:200, n(), replace = TRUE)
  )

head(gps_data)

acwr_long <- gps_data %>%
  ungroup() %>%
  pivot_longer(
    cols = total_distance:decels,
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(player, variable) %>%
  mutate(
    atl = round(calc_ewma(tl = value, n = 7, lag = 0), 3),
    ctl = round(calc_ewma(tl = value, n = 28, lag = 3), 3),
    acwr = atl/ctl,
    acwr7 = zoo::rollmean(x = acwr, k = 7, fill = NA, align = "right")
  ) %>%
  ungroup()

acwr_wide <- acwr_long %>%
  pivot_wider(
    id_cols = c("player", "date"),
    names_from = "variable",
    values_from = c("value", "atl", "ctl", "acwr", "acwr7")
  ) %>%
  rename_with(~ sub("^value_", "", .), starts_with("value_"))

acwr7_only <- acwr_wide %>%
  select(player, date, starts_with("acwr7_"))


combined_data <- gps_data %>%
  left_join(wellness_data, by = c("player", "date")) %>%
  left_join(sleep_data, by = c("player", "date")) %>%
  left_join(acwr7_only, by = c("player", "date"))

save(acwr_wide, file = "./sport_science_demo/data/acwr_wide.rda")
save(wellness_data, file = "./sport_science_demo/data/wellness_data.rda")
save(sleep_data, file = "./sport_science_demo/data/sleep_data.rda")
save(combined_data, file = "./sport_science_demo/data/combined_data.rda")
save(acwr_long, file = "./sport_science_demo/data/acwr_long.rda")


