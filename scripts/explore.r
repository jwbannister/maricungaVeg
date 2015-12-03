load("./data/maricunga_veg.RData")
devtools::load_all()
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(RColorBrewer)
library(magrittr)

palit <- brewer.pal(6, "Set1")

slai_sums <- sum_lai(data=maricunga_melt)

p1 <- ggplot(slai_sums, aes(x=year, y=sum.slai)) +
  geom_bar(stat="identity", fill=palit[1], color="#000000") +
  facet_grid(area.id ~ ., scales="free_y")
makePNG(p1, "./output/slai_sums.png", wt=10, ht=16)

# based on observations of sum of pixel seasonal LAIs across areas, severe
# decline in Pantanillo, Pantanillo Ancho 1 and Pantanillo Ancho 2 began in
# 2000. So, estimated CDF for each area on data from 1988 - 2000.

# yearly SLAI for a pixel
pix <- 2064
maricunga_melt %>% filter(feid==pix) %>%
  ggplot(aes(x=year, y=slai)) +
  geom_bar(stat="identity", fill=palit[1], color="#000000") +
  ggtitle(paste0("Pixel FEID ", as.character(pix)))

# by-pixel example plot of how kernel ECDF compares to historical data
pix <- 2064
dat <- filter(maricunga_melt, feid==pix, year %in% c(1988:2000))
ggplot() +
  geom_path(data=build_ecdf_df(dat, kern="normal"), mapping=aes(x=x, y=Fhat), color=palit[1]) +
  geom_path(data=build_ecdf_df(dat, kern="epanechnikov"), mapping=aes(x=x, y=Fhat), color=palit[2]) +
  stat_ecdf(data=dat, mapping=aes(x=slai), color="#000000") +
  xlab("slai") + ylab("cumulative probability") +
  ggtitle(paste0("Pixel FEID ", as.character(pix)))

# Epanechnikov kernel eswtimate provides more accurate estimate of low values
# (low SLAI values will have appropriately low estimated quantile scores)

pix <- 2064
filter(maricunga_melt, feid==pix) %>% score_quantile(cdf_years=c(1988:2000)) %>%
  select(year, slai, quant.score) %>% melt(id.vars="year") %>%
  ggplot(aes(x=year, y=value)) +
  geom_bar(stat="identity", fill=palit[1], color="#000000") +
  facet_grid(variable ~ ., scale="free_y") +
  ggtitle(paste0("Pixel FEID ", as.character(pix)))








