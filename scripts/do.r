load("./data/maricunga_slai.RData")

maricunga_quantiles <- score_quantile(data=maricunga_df, column_index=5:32)

save(maricunga_quantiles, file="../data/maricunga_quantiles.RData")

maricunga_streak <- maricunga_quantiles
pb <- txtProgressBar(min=0, max=nrow(maricunga_streak), style=3, width=80)
for (i in 1:nrow(maricunga_streak)){
  maricunga_streak[i, 5] <- NA
  for (j in 6:ncol(maricunga_streak)){
    n <- 0
    for (k in j:5){
      if (is.na(maricunga_quantiles[i, k])){
        break
      }
      if (maricunga_quantiles[i, k] < 0.1){
        n <- n + 1
      } else {
        break
      }
    }
    maricunga_streak[i, j] <- n
  }
  setTxtProgressBar(pb, i)
}
close(pb)

zone_index <- unique(maricunga_quantiles$area.id)[!is.na(unique(maricunga_quantiles$area.id))]

for (i in zone_index){
  dir.create(paste0("../output/", i))
}

for (i in zone_index){
  temp <- dplyr::filter(maricunga_quantiles, area.id==i)
  dir.create(paste0("../output/", i, "/quant/"))
  for (j in 5:ncol(temp)){
    yr <- colnames(temp)[j]
    p1 <- dplyr::select(temp, 1:4, j) %>%
      ggplot(aes(x=x, y=y)) +
      geom_tile(aes_string(fill=yr)) +
      ggtitle(paste0("AUC Quantile Scores - ", i, ", ", yr)) +
      scale_fill_gradient2(low="#d7191c", mid="#f7f7f7", high="#1a9641", midpoint=0.5, space="Lab",
                           breaks=c(0.25, 0.5, 0.75), limits=c(0, 1))+
      coord_fixed()
    makePNG(p1, paste0("../output/", i, "/quant/quant_", yr, ".png"))
  }
}

for (i in zone_index){
  temp <- dplyr::filter(maricunga_df, area.id==i)
  dir.create(paste0("../output/", i, "/auc"))
  auc_max <- max(temp[ , 5:ncol(temp)])
  auc_min <- min(temp[ , 5:ncol(temp)])
  for (j in 5:ncol(temp)){
    yr <- colnames(temp)[j]
    p1 <- dplyr::select(temp, 1:4, j) %>%
      ggplot(aes(x=x, y=y)) +
      geom_tile(aes_string(fill=yr)) +
      ggtitle(paste0("AUC - ", i, ", ", yr)) +
      scale_fill_gradient2(high="#1a9641", low="#f7f7f7", space="Lab",
                           breaks=seq(0, auc_max, round(auc_max/4)), limits=c(0, auc_max)) +
      coord_fixed()
    makePNG(p1, paste0("../output/", i, "/auc/auc_", yr, ".png"))
  }
}

for (i in zone_index){
  temp <- dplyr::filter(maricunga_streak, area.id==i)
  dir.create(paste0("../output/", i, "/streak"))
  for (j in 6:ncol(temp)){
    yr <- colnames(temp)[j]
    temp2 <- dplyr::select(temp, 1:4, j)
    temp2[ , 5] <- factor(temp2[ , 5])
    p1 <- ggplot(temp2, aes(x=x, y=y)) +
      geom_tile(aes_string(fill=yr)) +
      ggtitle(paste0("< 0.1 Quantile Streak - ", i, ", ", yr)) +
      scale_fill_brewer(breaks=c(0, 1, 2, 3, 4, 5), type="seq", palette="Oranges",
                        limits=c(0, 1, 2, 3, 4, 5), name="Years") +
      coord_fixed()
    makePNG(p1, paste0("../output/", i, "/streak/streak_", yr, ".png"))
  }
}

ggplot(maricunga_df, aes(x=x, y=y)) +
  geom_tile(aes(fill=area.id)) +
  coord_fixed()
