load("./data/maricunga_veg.RData")
devtools::load_all()
library(dplyr)

maricunga_melt <- score_quantile(maricunga_melt, cdf_years=c(1988:2000))
maricunga_melt <- score_low_streak(maricunga_melt, threshold=0.1)
maricunga_melt <- score_high_streak(maricunga_melt, threshold=0.9)
maricunga_melt <- score_consec_decrease(maricunga_melt)

save(maricunga_melt, vega_df, file="./data/maricunga_calcs.RData")

