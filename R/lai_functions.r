sum_lai <- function(data){
  slai_sums <- expand.grid(unique(data$area.id), unique(data$year),
                         stringsAsFactors=FALSE )
  slai_sums <- slai_sums[complete.cases(slai_sums), ]
  colnames(slai_sums) <- c("area.id", "year")
  pb <- txtProgressBar(min=0, max=nrow(slai_sums), style=3, width=80)
  for (i in 1:nrow(slai_sums)){
    slai_sums$sum.slai[i] <- sum(filter(data, year==slai_sums$year[i],
                                        area.id==slai_sums$area.id[i])$slai)
    setTxtProgressBar(pb, i)
  }
  close(pb)
  slai_sums
}

build_ecdf_df <- function(data, kern="epanechnikov"){
  kernel_ecdf <- sROC::kCDF(data$slai, kernel=kern)
  ecdf_df <- data.frame(x=kernel_ecdf$x, Fhat=kernel_ecdf$Fhat)
  ecdf_df
}

score_quantile <- function(data, cdf_years=NULL){
  pb <- txtProgressBar(min=0, max=length(unique(data$feid)), style=3, width=80)
  tick <- 1
  df <- dplyr::filter(data, feid==0)
  for (i in unique(data$feid)){
    temp <- dplyr::filter(data, feid==i)
    kernel_ecdf <- build_ecdf_df(data=dplyr::filter(temp, year %in% cdf_years))
    for (j in 1:nrow(temp)){
      temp$quant.score[j] <- kernel_ecdf$Fhat[which.min(abs(kernel_ecdf$x - temp$slai[j]))]
    }
    df <- rbind(df, temp)  
    setTxtProgressBar(pb, tick)
    tick <- tick + 1
  }
  close(pb)
  df
}

score_low_streak <- function(data, threshold){
  pb <- txtProgressBar(min=0, max=length(unique(data$feid)), style=3, width=80)
  tick <- 1
  df <- dplyr::filter(data, feid==0)
  for (i in unique(data$feid)){
    temp <- dplyr::filter(data, feid==i)
    temp <- temp[sort.int(temp$year, index.return=TRUE)$ix, ]
    for (j in 1:nrow(temp)){
      n <- 0
      for (k in j:1){
        if (temp$quant.score[k] < threshold){
          n <- n + 1
        } else {
          break
        }
      }
      temp$low.streak.score[j] <- min(n, 5)
    }
    df <- rbind(df, temp)
    setTxtProgressBar(pb, tick)
    tick <- tick + 1
  }
  close(pb)
  df
}

score_high_streak <- function(data, threshold){
  pb <- txtProgressBar(min=0, max=length(unique(data$feid)), style=3, width=80)
  tick <- 1
  df <- dplyr::filter(data, feid==0)
  for (i in unique(data$feid)){
    temp <- dplyr::filter(data, feid==i)
    temp <- temp[sort.int(temp$year, index.return=TRUE)$ix, ]
    for (j in 1:nrow(temp)){
      n <- 0
      for (k in j:1){
        if (temp$quant.score[k] > threshold){
          n <- n + 1
        } else {
          break
        }
      }
      temp$high.streak.score[j] <- min(n, 5)
    }
    df <- rbind(df, temp)
    setTxtProgressBar(pb, tick)
    tick <- tick + 1
  }
  close(pb)
  df
}