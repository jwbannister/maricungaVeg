

build_ecdf <- function(data=NULL, pixel=NULL, startyear=NULL, endyear=NULL,
                       kern=NULL){
  temp <- filter(data, feid==pixel, year %in% c(startyear:endyear))
  kernel_ecdf <- sROC::kCDF(temp$slai, kernel=kern)
  ecdf_df <- data.frame(x=kernel_ecdf$x, Fhat=kernel_ecdf$Fhat)
  temp <- data.frame(slai=sort(kernel_ecdf$data))
  for (i in 1:nrow(temp)){
    temp$percentile[i] <- i/nrow(temp)
  }
  for (i in 1:nrow(ecdf_df)){
    if (ecdf_df$x[i] <= min(temp$slai)){
      ecdf_df$nonpar[i] <- 0
    } else if (ecdf_df$x[i] >= max(temp$slai)){
      ecdf_df$nonpar[i] <- 1
    } else{
        ecdf_df$nonpar[i] <- max(temp$percentile[temp$slai < ecdf_df$x[i]])
    }
  }
  ecdf_df
}

nonpar_cdf <- function(data=NULL, pixel=NULL, startyear=NULL, endyear=NULL){
  temp <- filter(data, feid==pixel, year %in% c(startyear:endyear))
  temp <- temp[sort.int(temp$slai, index.return=TRUE)$ix, ]
  for (i in 1:nrow(temp)){
    temp$percentile[i] <- i/nrow(temp)
  }
  temp
}

score_quantile <- function(data=NULL){
  for (i in 1:nrow(data)){
      kernel_ecdf <- build_ecdf
      val <- history[j]
      quantiles[j] <- kernel_ecdf$Fhat[which.min(abs(kernel_ecdf$x - val))]
    }
    quant_df[i, column_index] <- quantiles
    setTxtProgressBar(pb, i)
  }
  close(pb)
  quant_df
}
