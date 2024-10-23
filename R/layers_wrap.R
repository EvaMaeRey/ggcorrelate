#' @export
layers_covariance <- function(){
  
  list(
  geom_point(),
  geom_xmean(),
  geom_ymean(),
  geom_xmeandiff(),
  geom_ymeandiff(),
  geom_xydiffs(),
  geom_covariance(),
  geom_covariance_label())
  
}

#' @export
layers_correlation <- function(){
  
  list(
  geom_point(),
  geom_xmean(),
  geom_ymean(),
  geom_xmeandiff(),
  geom_ymeandiff(),
  geom_xydiffs(),
  geom_x_sd(),
  geom_y_sd(),
  geom_covariance(),
  geom_correlation_label())
  
}
