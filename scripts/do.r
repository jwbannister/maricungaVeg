load("./data/maricunga_veg.RData")
devtools::load_all()
library(dplyr)

maricunga_melt <- score_quantile(maricunga_melt, cdf_years=c(1988:2000))
maricunga_melt <- score_low_streak(maricunga_melt, threshold=0.1)
maricunga_melt <- score_high_streak(maricunga_melt, threshold=0.9)

save(maricunga_melt, vega_df, file="./data/maricunga_calcs.RData")

auc_max <- max(data$slai)
auc_min <- min(data$slai)

yr <- 2012
data %>% filter(year==yr) %>%
ggplot(aes(x=x, y=y)) +
  geom_tile(aes(fill=slai)) +
  ggtitle(paste0("AUC - ", yr)) +
  scale_fill_gradient2(high="#1a9641", low="#f7f7f7", space="Lab",
                       breaks=seq(0, auc_max, round(auc_max/4)), limits=c(0, auc_max)) +
  coord_fixed()

yr <- 2012
data %>% filter(year==yr) %>%
  ggplot(aes(x=x, y=y)) +
  geom_tile(aes(fill=quant.score)) +
  ggtitle(paste0("AUC Quantile Scores - ", yr)) +
  scale_fill_gradient2(low="#d7191c", mid="#f7f7f7", high="#1a9641", midpoint=0.5, space="Lab",
                       breaks=c(0.25, 0.5, 0.75), limits=c(0, 1))+
  coord_fixed()

yr <- 2015
data$low.streak.score <- factor(data$low.streak.score)
data %>% filter(year==yr) %>%
  ggplot(aes(x=x, y=y)) +
  geom_tile(aes(fill=low.streak.score)) +
  ggtitle(paste0("< 0.1 Quantile Streak - ", yr)) +
  scale_fill_brewer(breaks=c(0:5), type="seq", palette="Reds",
                    name="Years", labels=c("0", "1", "2", "3", "4", "5+"),
                    limits=c(0:5)) +
  coord_fixed()

yr <- 1995
data$high.streak.score <- factor(data$high.streak.score)
data %>% filter(year==yr) %>%
  ggplot(aes(x=x, y=y)) +
  geom_tile(aes(fill=high.streak.score)) +
  ggtitle(paste0("> 0.9 Quantile Streak - ", yr)) +
  scale_fill_brewer(breaks=c(0:5), type="seq", palette="Greens",
                    name="Years", labels=c("0", "1", "2", "3", "4", "5+"),
                    limits=c(0:5)) +
  coord_fixed()
