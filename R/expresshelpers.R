# Some functionality that is being tried out in {statexpress}


###############################################
# qstat - an express method for creating Stats locally
######################################
qstat <- function(compute_group, ...){
  ggproto("StatTemp", Stat, compute_group = compute_group, ...)
  }

################################
# qlayer - a version of layer with more defaults
###########################
qlayer <- function (mapping = NULL, data = NULL, geom = "point", stat = "identity", position = "identity", 
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = geom, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = rlang::list2(na.rm = na.rm, ...))
}

################################
# proto_update - update default_aes quickly, based on existing defaults
###########################
proto_update <- function(`_class`, `_inherit`, default_aes_update = NULL, ...){
  
  if(!is.null(default_aes_update)){
  
    default_aes <- aes(!!!modifyList(`_inherit`$default_aes, default_aes_update))
    
    }
  
  ggplot2::ggproto(`_class` = `_class`, 
          `_inherit` = `_inherit`, 
          default_aes = default_aes, ...)
  
}

################################
# qproto_update - update, defaults for ggplot2 object, and use locally
###########################
qproto_update <- function(`_inherit`, default_aes_update = NULL, ...){
  
  proto_update("protoTemp", 
               `_inherit`, 
               default_aes_update = default_aes_update,
               ...)
}
