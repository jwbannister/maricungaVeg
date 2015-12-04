devtools::load_all()
load("./data/maricunga_calcs.RData")
library(dplyr)
library(reshape2)

maricunga_melt$year <- paste0("y", maricunga_melt$year)

quant_df <- maricunga_melt %>% select(feid, year, quant.score) %>%
  dcast(feid ~ year)
write.csv(quant_df, file="./output/maricunga_quantile_scores.csv", row.names=FALSE)

low_streak_df <- maricunga_melt %>% select(feid, year, low.streak.score) %>%
  dcast(feid ~ year)
write.csv(low_streak_df, file="./output/maricunga_low_streak.csv", row.names=FALSE)

high_streak_df <- maricunga_melt %>% select(feid, year, high.streak.score) %>%
  dcast(feid ~ year)
write.csv(high_streak_df, file="./output/maricunga_high_streak.csv", row.names=FALSE)

decrease_streak_df <- maricunga_melt %>% select(feid, year, decrease.score) %>%
  dcast(feid ~ year)
write.csv(decrease_streak_df, file="./output/maricunga_decrease_streak.csv", row.names=FALSE)
