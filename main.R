graphics.off()
library(tidyverse)
library(melidosData)
library(zoo)
library(gridExtra)
library(lubridate)


project_root = "D:/Projects/Spitschan"

for (dataset in c("MeLiDos", "ADayInDaylight")) {
  
  project_path = paste0(project_root, "/", dataset)
  if (!dir.exists(project_path)) dir.create(project_path)
  #--------------------------
  # LOAD DATA
  if (dataset == "ADayInDaylight") {
    load("D:/Code/2025_ADayInDaylight/data/cleaned/dashboard.RData")
    # Add a site to each data.frame simplified as just the country
    for (i in 1:nrow(data_devices)) {
      data_devices$data[[i]]$site = data_devices$country[i] #paste0(, "-", data_devices$city[i])
    }
    data = dplyr::bind_rows(data_devices$data, .id = "name")
    cat("N ", length(unique(data$Id)))
    # Nall = 0
    # for (i in 1:(length(data))) {
    #   N = length(unique(data[[i]]$Id))
    #   N = length(unique(data[[i]]$Id))
    #   cat(paste0("N ", names(data)[[i]], ": ", N, "\n"))
    #   Nall = Nall + N
    # }
    # cat(paste0("N all ", Nall, "\n"))
    # No collapsing needed
    D = data
  } else if (dataset == "MeLiDos") {
    modality = "light_wrist"
    
    outputfile = paste0(modality, ".RData")
    if (!file.exists(outputfile)) {
      # Only download data when not downloaded before
      data <- melidosData::load_data(modality = modality)
      save(data, file = outputfile)
    } else {
      load(file = outputfile)
    }
    
    # Collapse data
    D = purrr::map(data, bind_rows, .id = "name") |> 
      dplyr::bind_rows(.id = "site") |>
      dplyr::select(Id, Datetime, MEDI, PIM, site, name)
    dplyr::glimpse(D)
  }
  # Keep only complete cases
  D = D |> ungroup() |>
    dplyr::filter(if_all(c(Datetime, MEDI, PIM), ~!is.na(.x)))
  dplyr::glimpse(D)
  
  #-------------------------------------------------------------
  # Add features
  #-------------------------------------------------------------
  
  # Add delta PIM and delta MEDI
  D = D |>
    mutate(delta_PIM = c(0, diff(PIM))) |>
    mutate(delta_MEDI = c(0, diff(MEDI)))
  
  # Add log transform
  D$PIM_log = log(D$PIM + 1)
  D$MEDI_log = log(D$MEDI + 1)
  
  # Add percentile (based on non-zero data, zero's are assigned zero)
  D$PIM_perc = 0
  nonzero = which(D$PIM != 0)
  # D$PIM_perc[nonzero] = fmsb::percentile(D$PIM[nonzero])
  myPerc = function(x) {
    # TO DO: Highlight that these are not realy percentiles but scaled data between 0 and 100
    x = x - min(x)
    x = (x / max(x)) * 100
    return(x)
  }
  D$PIM_perc[nonzero] = myPerc(D$PIM[nonzero])
  D$MEDI_perc = 0
  nonzero = which(D$MEDI != 0)
  # D$MEDI_perc[nonzero] = fmsb::percentile(D$MEDI[nonzero])
  D$MEDI_perc[nonzero] = myPerc(D$MEDI[nonzero])
  
  
  # Loop over three options: raw, log-transformed, within-person percentiles
  for (datamode in c("raw", "log-transformed", "percentiles")) {
    if (datamode == "raw") {
      PIMname = "PIM"
      MEDIname = "MEDI"
    } else if (datamode == "log-transformed") {
      PIMname = "PIM_log"
      MEDIname = "MEDI_log"
    } else if (datamode == "percentiles") {
      PIMname = "PIM_perc"
      MEDIname = "MEDI_perc"
    }
    #---------------------------------------------------------
    # Reproduce and quantify the “naive” epoch-level results
    #---------------------------------------------------------
    # For each of them
    
    #----------------------------------------------------------
    # Pooled correlations PIM and MEDI 
    corr_pooled = cor(D[, PIMname], D[, MEDIname])
    #----------------------------------------------------------
    # Within-person correlations PIM and MEDI 
    corr_tmp = by(D, D$Id, function(sub) cor(sub[,PIMname], sub[,MEDIname]))
    corr_per_id = data.frame(Id = names(corr_tmp), corr = as.numeric(corr_tmp))
    
    # >> TO DO: Store these values for each iteration
    #----------------------------------------------------------
    # Distribution of within-person correlations of PIM and MEDI
    p1 = ggplot(corr_per_id, aes(x = corr)) +
      geom_histogram(aes(y = after_stat(density)), colour = "black", fill = "white") +
      geom_density(alpha = .2, fill = "#FF6666") +
      labs(title = paste0("Distribution across individuals in ", dataset, ": ", datamode, " data")) +
      xlab("Pearson correlation coefficient") +
      xlim(0, 1)
    ggplot2::ggsave(filename = paste0("Distr_corr_MEDI_PIM_per_id_", datamode, ".png"),
                    path = project_path, plot = p1)
    #---------------------------------------------------------
    # Add within-person percentile co-distribution plots
    #---------------------------------------------------------
    # Only for percentile data
    if (datamode == "percentiles") {
      #----------------------------------------------------------
      # Per participant percentile-percentile plots
      p2 = ggplot(D[which(D$Id %in% sample(x = unique(D$Id), size = 9)),]) +
        scattermore::geom_scattermore(aes(x = PIM_perc, y = MEDI_perc),
                                      pointsize = 3,
                                      alpha = 0.1,
                                      pixels = c(1000, 1000),
                                      interpolate = TRUE) +
        facet_wrap(~Id) +
        labs(title = paste0("Co-distribution of percentiles across 9 random individuals in ", dataset)) +
        xlab("PIM percentile") +
        ylab("MEDI percentile")
      ggplot2::ggsave(filename = paste0("Distr_perc_MEDI_PIM_per_id_", datamode, ".png"),
                      path = project_path, plot = p2)
      #----------------------------------------------------------
      # Pooled plot using within person percentiles
      library(scattermore)
      p3 = ggplot(D) +
        scattermore::geom_scattermore(aes(x = PIM_perc, y = MEDI_perc),
                                      pointsize = 3,
                                      alpha = 0.1,
                                      pixels = c(1000, 1000),
                                      interpolate = TRUE)
      ggplot2::ggsave(filename = paste0("Distr_perc_MEDI_PIM_across_all_", datamode, ".png"),
                      path = project_path, plot = p3)
      
      #----------------------------------------------------------
      # A participant normalised average 2D histogram
      
      # First generated normalised average 2D density 
      # Doing this via dplyr pipe led to issues with some Ids,
      # so for the sake of time I have simplified my approach to a simple loop
      # where each failing Id is skipped, regardly of why it fails
      nbins = 101
      lims = rep(c(0, nbins - 1), 2)
      dgrid = matrix(0, nbins, nbins)
      cnt = 0
      for (idi in unique(D$Id)) {
        d = NULL
        select = which(D$Id == idi)
        try(expr = {d = MASS::kde2d(x = D$PIM_perc[select],
                                    y = D$PIM_perc[select],
                                    n = nbins,
                                    lims = lims)}, silent = TRUE)
        if (is.null(d)) {
          print(paste0(idi, " skipped"))
        } else {
          # add normalised density matrix
          dgrid = dgrid + (d$z / sum(d$z))
          cnt = cnt + 1
        }
      }
      dgrid = dgrid / cnt
      # dgrid is now the matrix to be visualised
      # reshape to make it ggplot friendly
      avg_density = data.frame(x = rep(1:101, 101),
                               y = rep(1:101, each = 101),
                               z = as.vector(dgrid))
      p1 <- ggplot(avg_density, aes(x = x, y = y, fill = z))+
        geom_tile()
      plot(p1)
      
      
      # >> TO DO: Simplify to only 1 id 
      # >> TO DO: Stratify by time of the day
      # TO DO: Test also with Day in the light
      
    }
    
    #---------------------------------------------------------
    # Compute rhythm metrics for light and activity
    #---------------------------------------------------------
    
    # Derive:
    # IV MEDI
    # IV PIM
    # IS MEDI
    # IS PIM
    
    # RA MEDI
    # RA PIM
    
    # # Cross metric correlation matrix
    # 
    # IV = LightLogR::intradaily_variability(
    #   D$MEDI,
    #   D$Datetime,
    #   use.samplevar = FALSE,
    #   na.rm = FALSE,
    #   as.df = FALSE
    # )
    # 
    # IS  = interdaily_stability(
    #   D$MEDI,
    #   D$Datetime,
    #   use.samplevar = FALSE,
    #   na.rm = FALSE,
    #   as.df = FALSE
    # )
    # 
    # 
    # 
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
    # 
    # 
    # n_epoch_per_minute = 6
    # test = zoo::rollapply(D[, c("MEDI", "PIM")],
    #                       width = n_epoch_per_minute * 5, by = n_epoch_per_minute * 5, function(x) ifelse(sd(x[,1]) != 0 && sd(x[,2]) != 0, cor(x[, 1], x[, 2], use = "complete.obs"), 0), by.column = FALSE)
    # D$corr = c(rep(test, each = n_epoch_per_minute * 5), rep(0, 15))
    # 
    # myplot(D |> filter(corr > 0.6))
  }
}