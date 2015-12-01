load("../data/maricunga_slai.RData")
load("../data/maricunga_quantiles.RData")
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)

# write.csv(maricunga_quantiles, file="../output/maricunga_quantiles.csv")

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
