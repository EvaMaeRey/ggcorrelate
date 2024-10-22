
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggcorrelate

<!-- badges: start -->
<!-- badges: end -->

The goal of ggcorrelate is to …

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
devtools::create(".")
```

# some express extension stuff

``` r
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r


# express Stat setup.
#' @export
qstat <- function(compute_group, ...){
  ggproto("StatTemp", Stat, compute_group = compute_group, ...)
  }

#' @export
qgeom_mod_default_aes <- function(`_inherit` = GeomPoint, default_aes = GeomPoint$default_aes, ...){
  
  ggproto("GeomTemp", `_inherit` = `_inherit`, default_aes = default_aes, ...)

}

#' @export
qlayer <- function (mapping = NULL, data = NULL, geom = "point", stat = "identity", position = "identity", 
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = geom, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = rlang::list2(na.rm = na.rm, ...))
}

#' @export
proto_update <- function(`_class`, `_inherit`, default_aes_update = NULL, ...){
  
  if(!is.null(default_aes_update)){
  
    default_aes <- aes(!!!modifyList(`_inherit`$default_aes, default_aes_update))
    
    }
  
  ggplot2::ggproto(`_class` = `_class`, 
          `_inherit` = `_inherit`, 
          default_aes = default_aes, ...)
  
}

#' @export
qproto_update <- function(`_inherit`, default_aes_update = NULL, ...){
  
  proto_update("protoTemp", 
               `_inherit`, 
               default_aes_update = default_aes_update,
               ...)
}
```

## write some layers

``` r
# Assume qlayer() and qstat() exists:

# Layer 0 (actually don't use in var/sd/cov/corr/R^2 but demos qstat simplest usage)
compute_group_xymean <- function(data, scales){
  data |> mutate(x = mean(x), y = mean(y))
  }

geom_xy_means <- function(...){
  geom_point(stat = qstat(compute_group_xymean), ...)
  }


# Layer 1
compute_group_xmean <- function(data, scales){
  data |> summarize(xintercept = mean(x))
  }

geom_xmean <- function(...){
  
  QStat <- qstat(compute_group_xmean, dropped_aes = c("x", "y"))
  
  qlayer(geom = GeomVline, stat = QStat, ...)
  
  
  }

# Layer 2
compute_group_ymean <- function(data, scales){data |> summarize(yintercept = mean(y))}

geom_ymean <- function(...){
  
  QStat <- qstat(compute_group_ymean, dropped_aes = c("x", "y"))
  
  qlayer(geom = GeomHline, stat = QStat, ...)
  
  }

# Layer 3
compute_group_xmeandiff <- function(data, scales){
  
  data |> mutate(xend = mean(x), 
                 yend = y, 
                 xdiff = x - mean(x), 
                 sign = factor(sign(xdiff)))
  
  }

geom_xmeandiff <- function(...){
  
  QStat <- qstat(compute_group_xmeandiff, 
              default_aes = aes(color = after_stat(sign)))
  
  geom_segment(stat = QStat, ...)
  
  }

# Layer 4
compute_group_ymeandiff <- function(data, scales){
  
  data |> mutate(yend = mean(y), 
                 xend = x, 
                 ydiff = y - mean(y), 
                 sign = factor(sign(ydiff)))
  }

geom_ymeandiff <- function(...){
  
  QStat <- qstat(compute_group_ymeandiff, 
              default_aes = aes(color = after_stat(sign)))
  
  geom_segment(stat = QStat, ...)
  
  }


# Layer 5
compute_group_xymeandiffs <- function(data, scales){
  
  data |> mutate(xmin = mean(x), ymin = mean(y), 
                 xmax = x, ymax = y, 
                 area = (xmax-xmin)*(ymax-ymin), 
                 sign = factor(sign(area)))
  }

geom_xydiffs <- function(alpha = .2, ...){
  
  QS <- qstat(compute_group_xymeandiffs, 
               default_aes = aes(fill = after_stat(sign)))
  QG <- qproto_update(GeomRect, aes(alpha = .2))
  
  qlayer(stat = QS, geom = QG, ...)
  
  }

# Layer 6 & 7
compute_covariance <- function(data, scales){
   
  xmean = mean(data$x)
  ymean = mean(data$y)
  xsd = sd(data$x)
  
  data |> 
    mutate(xdiff = x - mean(x),                                      
           ydiff = y - mean(y),   
           area = xdiff * ydiff) |>
    summarize(mean_area = sum(area)/(n()-1)) |>
    pull(mean_area) ->
  mean_area
  
  data.frame(xmin = xmean, ymin = ymean,
             xmax = xmean + xsd, ymax = ymean + mean_area/xsd,
             covariance = mean_area,
             sign = factor(sign(mean_area))) |>
    mutate(x = (xmin + xmax)/2,
          y = (ymin + ymax)/2)
  
}
  
  
geom_covariance <- function(...){
  
  QS <- qstat(compute_covariance,
                 default_aes = aes(fill = after_stat(sign)))
  
  QG <- qproto_update(GeomRect, aes(color = "black"))
  
  qlayer(stat = QS, geom = QG, ...)
  
}  



geom_mean_xdiffXydiff <- geom_covariance

geom_covariance_label <- function(...){
  
  QS <- qstat(compute_covariance,
                 default_aes = 
                   aes(label = round(after_stat(covariance), 2))
                 )
                                   
  geom_label(stat = QS,
             show.legend = F,
                         ...)
  
}
  

# SDs
compute_sdx <- function(data, scales){
  
  ymean <- mean(data$y)
  xmean <- mean(data$x)
  xsd <- sd(data$x)
  
  data.frame(xintercept = xmean + xsd*c(-2,-1,1,2)) |>
    mutate(xstart = xmean,
            ystart = ymean,
            xend = xstart + xsd,
            yend = ystart)

    }

geom_x_sd <- function(...){
  
  QS <- qstat(compute_sdx, dropped_aes = c("x", "y"))
  QG <- qproto_update(GeomVline, aes(linetype = "dotted"))
  
  qlayer(geom = QG, stat = QS, ...)
  
}

# SDs
compute_sdy <- function(data, scales){
  
  ymean <- mean(data$y)
  ysd <- sd(data$y)
  
  data.frame(yintercept = ymean + ysd*c(-2,-1,1,2))

    }


geom_y_sd <- function(...){
  
  QS <- qstat(compute_sdy, 
              dropped_aes = c("x", "y"))
  
  # GeomHline$default_aes
  QG <- qproto_update(GeomHline, aes(linetype = "dotted"))

  qlayer(geom = QG, stat = QS, ...)
  
}
```

## Use the layers…

``` r
cars |>
  ggplot() + 
  aes(x = speed, y = dist) +
  geom_point() + # x, y  0
  geom_xmean() + # x-bar 1
  geom_ymean() + # y-bar 2
  geom_xmeandiff() + # 3
  geom_ymeandiff() + # 4
  geom_xydiffs() + # 5
  geom_covariance() + #6
  geom_covariance_label() + #7
  geom_x_sd() + 
  geom_y_sd()
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r

last_plot() + 
  aes(x = speed/sd(speed), 
      y = dist/sd(dist))
```

![](README_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
layers_covariance <- function(){
  
  list(
  geom_point(),
  geom_xmean(),
  geom_ymean(),
  geom_xmeandiff(),
  geom_ymeandiff(),
  geom_xydiffs(),
  geom_covariance(),
  geom_covariance_label(),
  geom_x_sd(),
  geom_y_sd())
  
}
```

``` r
cars |>
  ggplot() + 
  aes(x = speed, y = dist) + 
  layers_covariance()
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
library(datasauRus)

datasaurus_dozen |> 
  filter(dataset == "dino") |> 
  ggplot() + 
  aes(x = x, y = y) + 
  geom_point() + # x, y  0
  geom_xmean() + # x-bar 1
  geom_ymean() + # y-bar 2
  geom_xmeandiff() + # 3
  geom_ymeandiff() + # 4
  geom_xydiffs() + # 5
  geom_covariance() + #6
  geom_covariance_label() + #7
  geom_x_sd() + 
  geom_y_sd() 
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
head(datasaurus_dozen)
#> # A tibble: 6 × 3
#>   dataset     x     y
#>   <chr>   <dbl> <dbl>
#> 1 dino     55.4  97.2
#> 2 dino     51.5  96.0
#> 3 dino     46.2  94.5
#> 4 dino     42.8  91.4
#> 5 dino     40.8  88.3
#> 6 dino     38.7  84.9
```

``` r

datasaurus_dozen |>
  ggplot() + 
  aes(x, y) + 
  geom_point() + 
  facet_wrap(facet = vars(dataset)) + 
  layers_covariance()
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r

last_plot() + 
  aes(x = x/sd(x), 
      y = y/sd(y))
```

![](README_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r

anscombe |> 
  pivot_longer(everything(), 
    names_to = c(".value", "dataset"), 
    names_pattern = "(.)(.)"
  ) %>% 
  arrange(dataset) ->
anscombe_long

head(anscombe_long)
#> # A tibble: 6 × 3
#>   dataset     x     y
#>   <chr>   <dbl> <dbl>
#> 1 1          10  8.04
#> 2 1           8  6.95
#> 3 1          13  7.58
#> 4 1           9  8.81
#> 5 1          11  8.33
#> 6 1          14  9.96
```

``` r

last_plot() %+%
  anscombe_long
```

![](README_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub.
