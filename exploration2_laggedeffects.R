rm(list = ls())
graphics.off()
library(tidyverse)
library(melidosData)
library(zoo)
library(gridExtra)
library(lubridate)

modality = "light_wrist"
site = "TUM"
outputfile = paste0(modality, "_", site, ".RData")
if (!file.exists(outputfile)) {
  # Only download data when not downloaded before
  data <- melidosData::load_data(modality = modality, site = site)
  save(data, file = outputfile)
} else {
  load(file = outputfile)
}

# Select only relevant columns and keep complete cases
D = data |> dplyr::select(Id, Datetime, MEDI, PIM) |>
  ungroup() |>
  dplyr::filter(if_all(c(Datetime, MEDI, PIM), ~!is.na(.x))) |>
  arrange(Id, Datetime)

#==================================================================
#Find time points when activity transitions from inactive to active

# Find change points from inactive to active
PIM_change_detection = function(x){
  # x = sequence
  change = FALSE
  N = length(x)
  # ignore incomplete time series at start and end
  if (N < 60) return(change)
  midpoint = ceiling(N/2)
  mn_before = mean(x[1:midpoint])
  sd_before = sd(x[1:midpoint])
  max_before = max(x[1:midpoint])
  mn_after = mean(x[(midpoint + 1):N])
  # heuristic logic to identify changes from inactive to active
  if (mn_after > mn_before * 10 && 
      mn_before < 200 &&
      sd_before < 100 &&
      max_before < 100 &&
      mn_after > 500) { 
    # use t-test to test for actual difference (note: maybe redundant)
    tt = t.test(x[1:midpoint], x[(midpoint + 1):N])
    if (tt$p.value < 0.01) { # t-test significant
      change = TRUE
    }
  }
  return(change) # boolean: true = change, false = no change
}
epoch_size = 10
half_window_size = (60 / epoch_size) * 5 # 5 minutes
D$change_points = 0
for (id in unique(D$Id)) {
  id_segment = which(D$Id == id)
  D$change_points[id_segment] = slider::slide_dbl(.x = D$PIM[id_segment],
                                      .f = ~PIM_change_detection(.x),
                                      .before = half_window_size,
                                      .after = half_window_size,
                                      .complete = FALSE)
}

# Find middle of each sequence labelled as changepoint
D$change_points_final = 0

for (id in unique(D$Id)) {
  S = D[which(D$Id == id), ]
  K = rle(S$change_points)
  pos = 0
  change_point_positions = c()
  for (i in 1:length(K$values)) {
    if (K$values[i] == 0) {
      pos = pos + K$lengths[i]
    } else {
      pos = pos + ceiling(K$lengths[i] / 2) # add half to reach midpoint
      change_point_positions = c(change_point_positions, pos)
      pos = pos + floor(K$lengths[i] / 2) # add other half
    }
  }
  S$change_points_final[change_point_positions] = 1
  D$change_points_final[which(D$Id == id)] = S$change_points_final
}

# Create tibble with all time series centered around the change points
output = c()
for (id in unique(D$Id)) {
  S = D[which(D$Id == id),]
  N = nrow(S)
  change_points = which(S$change_points_final == 1)
  # initialise tibble to store all the sequences
  Nc =  length(change_points) * ((half_window_size * 2) + 1)
  ts = tibble(changepoint = numeric(Nc),
              MEDI = numeric(Nc),
              PIM = numeric(Nc),
              time = numeric(Nc),
              Id = character(Nc))
  ts$Id = id
  i = 1
  if (length(change_points) > 0) {
    for (j in change_points) {
      if (j > half_window_size + 1 && (j + half_window_size) <= N) {
        segment = i:(i + (half_window_size * 2))
        ts$PIM[segment] = S$PIM[(j - half_window_size):(j + half_window_size)]
        ts$MEDI[segment] = S$MEDI[(j - half_window_size):(j + half_window_size)]
        ts$time[segment] = -half_window_size:half_window_size / (60/epoch_size)
        ts$changepoint[segment] = j
        i = i + 1 + (half_window_size * 2)
      }
    }
    output = rbind(output, ts)
  }
}
output <- output |> group_by(changepoint)

# Plot
scaleFactor <- max(output$PIM) / max(output$MEDI)
ggplot(output, aes(x = time, group = changepoint)) +
  geom_line(aes(y = PIM), col = "blue") +
  geom_line(aes(y = MEDI * scaleFactor), col = "red") +
  scale_y_continuous(name = "MEDI", sec.axis = sec_axis(~./scaleFactor, name = "PIM")) +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.text.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = "red")
  ) +
  labs(x = "time (minutes)") + 
  facet_wrap(~Id)

