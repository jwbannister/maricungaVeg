load("./data/maricunga_slai.RData")
devtools::load_all()
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(RColorBrewer)
palit <- brewer.pal(6, "Set1")

maricunga_melt <- melt(maricunga_df, id.vars=c("x", "y", "feid", "area.id"))
colnames(maricunga_melt)[5:6] <- c("year", "slai")
maricunga_melt$year <- as.numeric(substring(maricunga_melt$year, 2))

slai_sums <- expand.grid(unique(maricunga_melt$area.id), unique(maricunga_melt$year),
                         stringsAsFactors=FALSE )
slai_sums <- slai_sums[complete.cases(slai_sums), ]
colnames(slai_sums) <- c("area.id", "year")

pb <- txtProgressBar(min=0, max=nrow(slai_sums), style=3, width=80)
for (i in 1:nrow(slai_sums)){
  slai_sums$sum.slai[i] <- sum(filter(maricunga_melt, year==slai_sums$year[i],
                                      area.id==slai_sums$area.id[i])$slai)
  setTxtProgressBar(pb, i)
}
close(pb)

area.index <- 2
slai_sums %>% filter(area.id==unique(slai_sums$area.id)[area.index]) %>%
ggplot(aes(x=year, y=sum.slai)) +
  geom_bar(stat="identity", fill=palit[1], color="#000000") +
  ggtitle(unique(slai_sums$area.id)[area.index])

p1 <- ggplot(slai_sums, aes(x=year, y=sum.slai)) +
  geom_bar(stat="identity", fill=palit[1], color="#000000") +
  facet_grid(area.id ~ ., scales="free_y")
makePNG(p1, "./output/slai_sums.png", wt=10, ht=16)

# based on observations of sum of pixel seasonal LAIs across areas, severe
# decline in Pantanillo, Pantanillo Ancho 1 and Pantanillo Ancho 2 began in
# 2000. So, build estimated CDF for each area on data from 1988 - 2000.

# yearly SLAI for a pixel
pix <- 2064
maricunga_melt %>% filter(feid==pix) %>%
  ggplot(aes(x=year, y=slai)) +
  geom_bar(stat="identity", fill=palit[1], color="#000000") +
  ggtitle(paste0("Pixel FEID ", as.character(pix)))

# by-pixel example plot of how kernel ECDF compares to non-par historical data
pix <- 2064
ggplot() +
  geom_path(data=build_ecdf(maricunga_melt, pixel=pix, startyear=1988, endyear=2000, kern="normal"),
            mapping=aes(x=x, y=Fhat), color=palit[1]) +
  geom_path(data=build_ecdf(maricunga_melt, pixel=pix, startyear=1988, endyear=2000, kern="epanechnikov"),
            mapping=aes(x=x, y=Fhat), color=palit[2]) +
  stat_ecdf(data=filter(maricunga_melt, feid==pix, year %in% c(1988:2000)),
            mapping=aes(x=slai), color="#000000")













melt(maricunga_df[!is.na(maricunga_df$area.id), 5:32]) %>%
ggplot(aes(x=value)) +
  geom_density(aes(color=variable)) +
  xlim(0, 100)

area.index <- 3
area.name <- unique(maricunga_df$area.id)[area.index]
max.slai <- max(filter(maricunga_df, area.id==area.name)[5:32])
min.slai <- min(filter(maricunga_df, area.id==area.name)[5:32])

year.index <- 12
year.name <- colnames(maricunga_df)[year.index]

p1 <- maricunga_df %>% filter(area.id==area.name) %>%
  ggplot(aes(x=x, y=y)) +
  geom_tile(aes_string(fill=year.name)) +
  scale_fill_gradient(low="#f7fcb9", high="#31a354",
                      limits=c(min.slai, max.slai), name="SLAI") +
  geom_path(data=filter(vega_df, area.id==area.name),
            mapping=aes(group=area.id), color="red") +
  coord_fixed() +
  ggtitle(paste0(area.name, " - ", year.name))

p2 <- maricunga_quantiles %>% filter(area.id==area.name) %>%
  ggplot(aes(x=x, y=y)) +
  geom_tile(aes_string(fill=year.name)) +
  scale_fill_gradient2(low="#ca0020", mid="#f7f7f7", high="#0571b0",
                      midpoint=0.5, limits=c(0, 1), name="Quantile Score") +
  geom_path(data=filter(vega_df, area.id==area.name),
            mapping=aes(group=area.id), color="red") +
  coord_fixed() +
  ggtitle(paste0(area.name, " - ", year.name))

grid.arrange(p1, p2, ncol=2)
