load("./data/maricunga_calcs.RData")
devtools::load_all()
library(dplyr)
library(ggplot2)

area_index <- unique(maricunga_melt$area.id)[!is.na(unique(maricunga_melt$area.id))]
lower_index <- tolower(gsub(" ", "_", area_index))

for (i in lower_index){
  dir.create(paste0("./output/", i))
  dir.create(paste0("./output/", i, "/slai"))
  dir.create(paste0("./output/", i, "/quant"))
  dir.create(paste0("./output/", i, "/low_streak"))
  dir.create(paste0("./output/", i, "/high_streak"))
  dir.create(paste0("./output/", i, "/decrease_streak"))
}

for (i in 1:length(area_index)){
  data <- dplyr::filter(maricunga_melt, area.id==area_index[i])
  auc_max <- max(data$slai)
  auc_min <- min(data$slai)
  for (j in unique(data$year)){
      p <- data %>% filter(year==j) %>%
      ggplot(aes(x=x, y=y)) +
      geom_tile(aes(fill=slai)) +
      ggtitle(paste0(area_index[i], " SLAI ", j)) +
      scale_fill_gradient2(high="#1a9641", low="#f7f7f7", space="Lab",
                           breaks=seq(0, auc_max, round(auc_max/4)), limits=c(0, auc_max)) +
      coord_fixed()
      makePNG(p, paste0("./output/", lower_index[i], "/slai/slai_", j, ".png"))
  }
}

for (i in 1:length(area_index)){
  data <- dplyr::filter(maricunga_melt, area.id==area_index[i])
  for (j in unique(data$year)){
    p <- data %>% filter(year==j) %>%
      ggplot(aes(x=x, y=y)) +
      geom_tile(aes(fill=quant.score)) +
      ggtitle(paste0(area_index[i], " SLAI Quantile Scores ", j)) +
      scale_fill_gradient2(low="#d7191c", mid="#f7f7f7", high="#1a9641", midpoint=0.5, space="Lab",
                           breaks=c(0.25, 0.5, 0.75), limits=c(0, 1)) +
      coord_fixed()
    makePNG(p, paste0("./output/", lower_index[i], "/quant/quant_", j, ".png"))
  }
}

for (i in 1:length(area_index)){
  data <- dplyr::filter(maricunga_melt, area.id==area_index[i])
  data$low.streak.score <- factor(data$low.streak.score)
  for (j in unique(data$year)){
    p <- data %>% filter(year==j) %>%
      ggplot(aes(x=x, y=y)) +
      geom_tile(aes(fill=low.streak.score)) +
      ggtitle(paste0(area_index[i], " < 0.1 Quantile Streak ", j)) +
      scale_fill_brewer(breaks=c(0:5), type="seq", palette="Reds",
                        name="Years", labels=c("0", "1", "2", "3", "4", "5+"),
                        limits=c(0:5)) +
      coord_fixed()
    makePNG(p, paste0("./output/", lower_index[i], "/low_streak/low_streak_", j, ".png"))
  }
}

for (i in 1:length(area_index)){
  data <- dplyr::filter(maricunga_melt, area.id==area_index[i])
  data$high.streak.score <- factor(data$high.streak.score)
  for (j in unique(data$year)){
    p <- data %>% filter(year==j) %>%
      ggplot(aes(x=x, y=y)) +
      geom_tile(aes(fill=high.streak.score)) +
      ggtitle(paste0(area_index[i], " > 0.9 Quantile Streak ", j)) +
      scale_fill_brewer(breaks=c(0:5), type="seq", palette="Greens",
                        name="Years", labels=c("0", "1", "2", "3", "4", "5+"),
                        limits=c(0:5)) +
      coord_fixed()
    makePNG(p, paste0("./output/", lower_index[i], "/high_streak/high_streak_", j, ".png"))
  }
}

for (i in 1:length(area_index)){
  data <- dplyr::filter(maricunga_melt, area.id==area_index[i])
  data$decrease.score <- factor(data$decrease.score)
  for (j in unique(data$year)){
    p <- data %>% filter(year==j) %>%
      ggplot(aes(x=x, y=y)) +
      geom_tile(aes(fill=decrease.score)) +
      ggtitle(paste0(area_index[i], " SLAI Decrease Streak ", j)) +
      scale_fill_brewer(breaks=c(0:5), type="seq", palette="Blues",
                        name="Years", labels=c("0", "1", "2", "3", "4", "5+"),
                        limits=c(0:5)) +
      coord_fixed()
    makePNG(p, paste0("./output/", lower_index[i], "/decrease_streak/decrease_streak_", j, ".png"))
  }
}


