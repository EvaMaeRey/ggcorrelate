######################################
# Layer 0 (actually don't use in the statistical exploration, but it's a very simple case, so here for educational purposes)

compute_group_xymean <- function(data, scales){
  
  data |> mutate(x = mean(x), y = mean(y))
  
  }

#' @export
geom_xy_means <- function(...){
  
  geom_point(stat = qstat(compute_group_xymean), ...)
  
  }

######################################
# Layer 1, geom_xmean
compute_group_xmean <- function(data, scales){
  data |> dplyr::summarize(xintercept = mean(x))
  }

#' @export
geom_xmean <- function(...){
  
  QStat <- qstat(compute_group_xmean, dropped_aes = c("x", "y"))
  
  qlayer(geom = GeomVline, stat = QStat, ...)
  
  
  }

######################################
# Layer 2, geom_ymean

compute_group_ymean <- function(data, scales){data |> summarize(yintercept = mean(y))}

#' @export
geom_ymean <- function(...){
  
  QStat <- qstat(compute_group_ymean, dropped_aes = c("x", "y"))
  
  qlayer(geom = GeomHline, stat = QStat, ...)
  
  }

######################################
# Layer 3, geom_xmeandiff
compute_group_xmeandiff <- function(data, scales){
  
  sign_levels <- c("Neg", "Pos")
  
  data |> mutate(xend = mean(x), 
                 yend = y, 
                 xdiff = x - mean(x), 
                 sign = ifelse(xdiff < 0, "Neg", "Pos"),
                 sign = factor(sign, sign_levels)
  )
  
  }

#' @export
geom_xmeandiff <- function(...){
  
  
  QStat <- qstat(compute_group_xmeandiff, 
                 default_aes = aes(color = after_stat(sign)))
  
  geom_segment(stat = QStat, ...)
  
  }

######################################
# Layer, geom_ymeandiff
compute_group_ymeandiff <- function(data, scales){
  
  sign_levels <- c("Neg", "Pos")

  data |> mutate(yend = mean(y), 
                 xend = x, 
                 ydiff = y - mean(y), 
                 sign = ifelse(ydiff < 0, "Neg", "Pos"),
                 sign = factor(sign, sign_levels))
  }

#' @export
geom_ymeandiff <- function(...){
  
  QStat <- qstat(compute_group_ymeandiff, 
              default_aes = aes(color = after_stat(sign)))
  
  geom_segment(stat = QStat, ...)
  
  }

######################################
# Layer, geom_xydiffs
compute_group_xymeandiffs <- function(data, scales){
  
  sign_levels <- c("Neg", "Pos")

  data |> dplyr::mutate(xmin = mean(x), ymin = mean(y), 
                 xmax = x, ymax = y, 
                 area = (xmax-xmin)*(ymax-ymin), 
                 sign = ifelse(area < 0, "Neg", "Pos"),
                 sign = factor(sign, sign_levels))
  }

#' @export
geom_xydiffs <- function(alpha = .2, ...){
  
  QS <- qstat(compute_group_xymeandiffs, 
               default_aes = aes(fill = after_stat(sign)))
  QG <- qproto_update(GeomRect, aes(alpha = .2))
  
  qlayer(stat = QS, geom = QG, ...)
  
  }


######################################
# Layer 6 & 7 geom_covariance & geom_covariance_label
compute_covariance <- function(data, scales){
   
  sign_levels <- c("Neg", "Pos")
  
  xmean = mean(data$x)
  ymean = mean(data$y)
  xsd = sd(data$x)
  ysd = sd(data$y)
  
  data |> 
    dplyr::mutate(xdiff = x - mean(x),                                
           ydiff = y - mean(y),   
           area = xdiff * ydiff) |>
    dplyr::summarize(mean_area = sum(area)/(n()-1)) |>
    dplyr::pull(mean_area) ->
  mean_area
  
  sign_levels <- c("Neg", "Pos")
  
  data.frame(xmin = xmean, ymin = ymean,
             xmax = xmean + xsd, ymax = ymean + mean_area/xsd,
             covariance = mean_area,
             correlation = mean_area/(xsd*ysd)) %>% 
    mutate(sign = ifelse(mean_area < 0, "Neg", "Pos"),
           sign = factor(sign, sign_levels)) |>
    mutate(x = (xmin + xmax)/2,
           y = (ymin + ymax)/2)
  
}
  
#' @export
geom_covariance <- function(...){
  
  QS <- qstat(compute_covariance,
                 default_aes = aes(fill = after_stat(sign)))
  
  QG <- qproto_update(GeomRect, aes(color = "black"))
  
  qlayer(stat = QS, geom = QG, ...)
  
}  


#' @export
geom_mean_xdiffXydiff <- geom_covariance

#' @export
geom_covariance_label <- function(...){
  
  QS <- qstat(compute_covariance,
                 default_aes = 
                   aes(label = round(after_stat(covariance), 2))
                 )
                                   
  geom_label(stat = QS,
             show.legend = F,
                         ...)
  
}

#' @export
geom_correlation_label <- function(...){
  
  QS <- qstat(compute_covariance,
                 default_aes = 
                   aes(label = round(after_stat(correlation), 3))
                 )
                                   
  geom_label(stat = QS,
             show.legend = F,
                         ...)
  
}
  

######################################
# Layer geom_x_sd
compute_sdx <- function(data, scales){
  
  ymean <- mean(data$y)
  xmean <- mean(data$x)
  xsd <- sd(data$x)
  
  data.frame(xintercept = xmean + xsd*c(-2,-1,1,2)) |>
    dplyr::mutate(xstart = xmean,
            ystart = ymean,
            xend = xstart + xsd,
            yend = ystart)

    }

#' @export
geom_x_sd <- function(...){
  
  QS <- qstat(compute_sdx, dropped_aes = c("x", "y"))
  QG <- qproto_update(GeomVline, aes(linetype = "dotted"))
  
  qlayer(geom = QG, stat = QS, ...)
  
}

######################################
# Layer geom_y_sd
compute_sdy <- function(data, scales){
  
  ymean <- mean(data$y)
  ysd <- sd(data$y)
  
  data.frame(yintercept = ymean + ysd*c(-2,-1,1,2))

    }

#' @export
geom_y_sd <- function(...){
  
  QS <- qstat(compute_sdy, 
              dropped_aes = c("x", "y"))
  
  # GeomHline$default_aes
  QG <- qproto_update(GeomHline, aes(linetype = "dotted"))

  qlayer(geom = QG, stat = QS, ...)
  
}
