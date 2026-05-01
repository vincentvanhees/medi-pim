rm(list = ls())
library(tidyverse)
library(melidosData)
library(zoo)
library(gridExtra)
library(lubridate)

for (modality in c("light_wrist", "light_glasses")) {
  outputfile = paste0(modality, ".RData")
  if (!file.exists(outputfile)) {
    # Only download data when not downloaded before
    data <- melidosData::load_data(modality = modality)
    save(data, file = outputfile)
  } else {
    load(file = outputfile)
  }
  if (modality == "light_wrist") {
    data_light_wrist = data
  } else {
    data_light_glasses = data
  }
}

# combine into one data object with MEDI from glasses and PIM from wrist 
sites_to_remove = NULL
for (i in 1:length(data_light_glasses)) {
  j = which(names(data_light_wrist) == names(data_light_glasses)[i])
  if (length(j) == 0) {
    sites_to_remove = c(sites_to_remove, j)
    next
  }
  data_light_glasses[[i]] = merge(data_light_glasses[[i]],
                                  data_light_wrist[[j]],
                                  by = c("Datetime", "Id"),
                                  suffixes = c(".glass", ".wrist"))
  data_light_glasses[[i]] <- data_light_glasses[[i]] |>
    dplyr::select(Id, Datetime, MEDI.glass, PIM.wrist) |>
    rename(MEDI = MEDI.glass, PIM = PIM.wrist)
}
data = data_light_glasses
rm(data_light_glasses, data_light_wrist)


Nall = 0
for (i in 1:(length(data))) {
  N = length(unique(data[[i]]$Id))
  cat(paste0("N ", names(data)[[i]], ": ", N, "\n"))
  Nall = Nall + N
}
cat(paste0("N all ", Nall, "\n"))

D = purrr::map(data, bind_rows, .id = "name") |> 
  dplyr::bind_rows(.id = "site") |>
  dplyr::select(Id, Datetime, MEDI, PIM, site, name)
dplyr::glimpse(D)


D = D |> ungroup() |>
  dplyr::filter(if_all(c(Datetime, MEDI, PIM), ~!is.na(.x)))

dplyr::glimpse(D)


D = D |>
  mutate(delta_PIM = c(0, diff(PIM))) |>
  mutate(delta_MEDI = c(0, diff(MEDI)))

# ggplot(D, aes(x = MEDI, y = PIM)) +
#   geom_bin2d(binwidth = c(4000, 1000), aes(fill = after_stat(count)),
#              color = "black", alpha = 0.8) +
#   scale_fill_gradient(name = "Density", low = "lightgreen", high = "darkgreen") +
#   labs(title = "Density of MEDI vs PIM", x = "MEDI", y = "PIM") +
#   facet_wrap(~site) +
#   theme_minimal() +
#   scale_x_continuous(labels = NULL, breaks = NULL) +
#   scale_y_continuous(labels = NULL, breaks = NULL)
# 
# myplot = function(D) {  
#   # Define  custom plot function to show relation between variables
#   ggplot(D, aes(x = delta_MEDI, y = delta_PIM)) +
#     geom_bin2d(binwidth = c(4000, 1000), aes(fill = after_stat(count)),
#                color = "black", alpha = 0.8) +
#     scale_fill_gradient(name = "Density", low = "lightblue", high = "darkblue") +
#     labs(title = "Density of change in MEDI vs change in PIM",
#          x = "Change in in MEDI", y = "Change in PIM") +
#     facet_wrap(~site) +
#     theme_minimal() +
#     scale_x_continuous(labels = NULL, breaks = NULL) +
#     scale_y_continuous(labels = NULL, breaks = NULL)
# }
# myplot(D)

n_epoch_per_minute = 6
test = zoo::rollapply(D[, c("MEDI", "PIM")],
                      width = n_epoch_per_minute * 5, by = n_epoch_per_minute * 5, function(x) ifelse(sd(x[,1]) != 0 && sd(x[,2]) != 0, cor(x[, 1], x[, 2], use = "complete.obs"), 0), by.column = FALSE)
correlation = c(rep(test, each = n_epoch_per_minute * 5), rep(0, 20))
D$corr = correlation[1:nrow(D)]

# myplot(D |> filter(corr > 0.6))

D <- D |> mutate(hour = lubridate::hour(Datetime) + lubridate::minute(Datetime)/60)
# myplot(D |> filter(hour >= 9 & hour <= 20))


D <- D |> mutate(hour = lubridate::hour(Datetime) + lubridate::minute(Datetime)/60)
# myplot(D |> filter(hour <= 7 | hour >= 23))

D <- D |> mutate(month = lubridate::month(Datetime))
# myplot(D |> filter(month >= 6 & month <= 8))

D <- D |> mutate(month = lubridate::month(Datetime))
# myplot(D |> filter(month <= 2 | month >= 12))

D <- D |> dplyr::mutate(MEDI_LEVEL = cut(MEDI,
                                         breaks = c(0, 1, 10, 100, 1000, 10000, 100000, Inf)))

D <- D |> mutate(delta_MEDI_LEVEL = c(0, diff(MEDI_LEVEL)))

# ggplot(D, aes(x = delta_MEDI_LEVEL, y = delta_PIM)) +
#   geom_bin2d(binwidth = c(1, 1000), aes(fill = after_stat(count)),
#              color = "black", alpha = 0.8) +
#   scale_fill_gradient(name = "Density", low = "lightblue", high = "darkblue") +
#   labs(title = "Density of change in MEDI level vs change in PIM",
#        x = "Change in in MEDI level ", y = "Change in PIM") +
#   facet_wrap(~site) +
#   theme_minimal() +
#   scale_x_continuous(labels = NULL, breaks = NULL) +
#   scale_y_continuous(labels = NULL, breaks = NULL)

D <- D |> dplyr::mutate(PIM_LEVEL = cut(PIM,
                                        breaks = c(0, 10, 100, 1000, 10000, 100000, Inf)))

D <- D |> mutate(delta_PIM_LEVEL = c(0, diff(PIM_LEVEL)))

# ggplot(D, aes(x = delta_MEDI, y = delta_PIM_LEVEL)) +
#   geom_bin2d(binwidth = c(4000, 1), aes(fill = after_stat(count)),
#              color = "black", alpha = 0.8) +
#   scale_fill_gradient(name = "Density", low = "lightblue", high = "darkblue") +
#   labs(title = "Density of change in MEDI vs change in PIM level",
#        x = "Change in in MEDI", y = "Change in PIM level") +
#   facet_wrap(~site) +
#   theme_minimal() +
#   scale_x_continuous(labels = NULL, breaks = NULL) +
#   scale_y_continuous(labels = NULL, breaks = NULL)

D$rounded_time_minutes = round(as.numeric(D$Datetime) / 60) * 60

myagg = function(x) {
  if (inherits(x, "numeric")) {
    return(mean(x))
  } else {
    return(x[1])
  }
}
Dtmp <- as_tibble(aggregate(D, by = list(D$rounded_time_minutes, D$Id), FUN = myagg)) |>
  select(-c(Group.1, Group.2))

# ggplot(Dtmp, aes(x = delta_MEDI, y = delta_PIM)) +
#   geom_bin2d(binwidth = c(2000, 1000), aes(fill = after_stat(count)),
#              color = "black", alpha = 0.8) +
#   scale_fill_gradient(name = "Density", low = "lightblue", high = "darkblue") +
#   labs(title = "Density of change in MEDI vs change in PIM level",
#        x = "Change in in MEDI", y = "Change in PIM level") +
#   facet_wrap(~site) +
#   theme_minimal() +
#   scale_x_continuous(labels = NULL, breaks = NULL) +
#   scale_y_continuous(labels = NULL, breaks = NULL)

