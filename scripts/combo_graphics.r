load("./data/maricunga_calcs.RData")
devtools::load_all()
library(dplyr)
library(ggplot2)
library(gridExtra)

area_index <- unique(maricunga_melt$area.id)[!is.na(unique(maricunga_melt$area.id))]
lower_index <- tolower(gsub(" ", "_", area_index))

for (i in lower_index){
  dir.create(paste0("./output/", i))
  dir.create(paste0("./output/", i, "/combo"))
}

for (i in 1:length(area_index)){
  data <- dplyr::filter(maricunga_melt, area.id==area_index[i])
  auc_max <- max(data$slai)
  auc_min <- min(data$slai)
  data$low.streak.score <- factor(data$low.streak.score)
  data$high.streak.score <- factor(data$high.streak.score)
  data$decrease.score <- factor(data$decrease.score)
  for (j in unique(data$year)){
    p1 <- data %>% filter(year==j) %>%
      ggplot(aes(x=x, y=y)) +
      geom_tile(aes(fill=slai)) +
      ggtitle(paste0(area_index[i], " SLAI ", j)) +
      scale_fill_gradient2(high="#1a9641", low="#f7f7f7", space="Lab",
                           breaks=seq(0, auc_max, round(auc_max/4)), limits=c(0, auc_max)) +
      coord_fixed()
    p2 <- data %>% filter(year==j) %>%
      ggplot(aes(x=x, y=y)) +
      geom_tile(aes(fill=quant.score)) +
      ggtitle(paste0(area_index[i], " SLAI Quantile Scores ", j)) +
      scale_fill_gradient2(low="#d7191c", mid="#f7f7f7", high="#1a9641", midpoint=0.5, space="Lab",
                           breaks=c(0.25, 0.5, 0.75), limits=c(0, 1)) +
      coord_fixed()
    p3 <- data %>% filter(year==j) %>%
      ggplot(aes(x=x, y=y)) +
      geom_tile(aes(fill=low.streak.score)) +
      ggtitle(paste0(area_index[i], " < 0.1 Quantile Streak ", j)) +
      scale_fill_brewer(breaks=c(0:5), type="seq", palette="Reds",
                        name="Years", labels=c("0", "1", "2", "3", "4", "5+"),
                        limits=c(0:5)) +
      coord_fixed()
    p4 <- data %>% filter(year==j) %>%
      ggplot(aes(x=x, y=y)) +
      geom_tile(aes(fill=high.streak.score)) +
      ggtitle(paste0(area_index[i], " > 0.9 Quantile Streak ", j)) +
      scale_fill_brewer(breaks=c(0:5), type="seq", palette="Greens",
                        name="Years", labels=c("0", "1", "2", "3", "4", "5+"),
                        limits=c(0:5)) +
      coord_fixed()
    p5 <- data %>% filter(year==j) %>%
      ggplot(aes(x=x, y=y)) +
      geom_tile(aes(fill=decrease.score)) +
      ggtitle(paste0(area_index[i], " SLAI Decrease Streak ", j)) +
      scale_fill_brewer(breaks=c(0:5), type="seq", palette="Blues",
                        name="Years", labels=c("0", "1", "2", "3", "4", "5+"),
                        limits=c(0:5)) +
      coord_fixed()
    p6 <- arrangeGrob(p1, p2, p3, p4, p5, ncol=5)
    png(filename=paste0("./output/", lower_index[i], "/combo/combo_", j),
        height=6, width=23.34, units="in", res=600)
    grid.arrange(p1, p2, p3, p4, p5, ncol=5)
    dev.off()
  }
}
