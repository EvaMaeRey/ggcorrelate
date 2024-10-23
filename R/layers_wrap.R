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
  geom_covariance_label(),
  geom_x_sd(),
  geom_y_sd())
  
}
