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

### Inspect sample sizes

Nall = 0
N = length(unique(data$Id))
cat(paste0("N ", site, ": ", N, "\n"))

### Select only relevant columns

D = data |> dplyr::select(Id, Datetime, MEDI, PIM)

### Keep only complete cases
D = D |> ungroup() |>
  dplyr::filter(if_all(c(Datetime, MEDI, PIM), ~!is.na(.x)))


#----------------------------------------------
## Average PIM and MEDI over 24 hours
# Derive average 24 cycle

D <- D |> mutate(clocktime = paste(lubridate::hour(Datetime),
                                   lubridate::minute(Datetime),
                                   lubridate::second(Datetime), sep = ":"),
                 hour_in_day = lubridate::hour(Datetime) +
                   (lubridate::minute(Datetime) / 60) +
                   (lubridate::second(Datetime) / 3600))

D = D |> arrange(Id, hour_in_day)

myagg = function(x) {
  if (inherits(x, "numeric")) {
    return(mean(x))
  } else {
    return(x[1])
  }
}
D24 <- as_tibble(aggregate(D, by = list(D$clocktime, D$Id), FUN = myagg))
D24 = D24 |> select(-c(Group.1, Group.2, Datetime, clocktime)) |> arrange(Id, hour_in_day)

# D24 = D24[which(D24$Id == D24$Id[1]), ]
# kkk
# library(cowplot)
# 
# p1 <- ggplot(D24, aes(hour_in_day)) +
#   geom_line(aes(y = PIM)) +
#   facet_wrap(~Id)
# 
# p2 <- ggplot(D24, aes(hour_in_day)) +
#   geom_line(aes(y = MEDI)) +
#   facet_wrap(~Id)
# cowplot::plot_grid(p1, p2, ncol = 1, labels = "AUTO")


scaleFactor <- max(D24$PIM) / max(D24$MEDI)
ggplot(D24, aes(x=hour_in_day)) +
  geom_line(aes(y=PIM), col="blue") +
  geom_line(aes(y=MEDI * scaleFactor), col="red") +
  scale_y_continuous(name="MEDI", sec.axis=sec_axis(~./scaleFactor, name="PIM")) +
  theme(
    axis.title.y.left=element_text(color="blue"),
    axis.text.y.left=element_text(color="blue"),
    axis.title.y.right=element_text(color="red"),
    axis.text.y.right=element_text(color="red")
  ) +
  facet_wrap(~Id)

scaleFactor <- max(D24$PIM) / max(D24$MEDI)
ggplot(D24, aes(x=hour_in_day)) +
  geom_smooth(aes(y=PIM), method="gam", col="blue") +
  geom_smooth(aes(y=MEDI * scaleFactor), method="gam", col="red") +
  scale_y_continuous(name="MEDI", sec.axis=sec_axis(~./scaleFactor, name="PIM")) +
  theme(
    axis.title.y.left=element_text(color="blue"),
    axis.text.y.left=element_text(color="blue"),
    axis.title.y.right=element_text(color="red"),
    axis.text.y.right=element_text(color="red")
  ) +
  facet_wrap(~Id)

