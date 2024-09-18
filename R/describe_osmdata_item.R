describe_osmdata_item=function(one_path_results,key,value){
  get_shp_summary=function(path, type){
    if(file.exists(path)){
      shape=sf::st_read(path,quiet=TRUE)
      if(type=="osm_points"){size=nrow(shape)}
      if(type %in% c("osm_lines","osm_multilines")){size=shape  %>% sf::st_make_valid() %>% sf::st_length() %>%  as.numeric() %>% sum()}
      if(type %in% c("osm_polygons","osm_multipolygons")){size=shape %>% sf::st_make_valid() %>%   sf::st_area() %>% as.numeric() %>% sum()}
      result=tibble::tibble(n_items=nrow(shape),
                            size=size)
    }else{result=tibble::tibble(n_items=0,size=0)}
    return(result)
  }
  result=tibble::tibble(
    type=c("osm_points",
           "osm_lines",
           "osm_polygons",
           "osm_multilines",
           "osm_multipolygons")) %>%
    dplyr::mutate(shp_path=glue::glue("{one_path_results}/{key}-{value}-{type}.shp")) %>%
    dplyr::mutate(result=purrr::map2(shp_path,type,get_shp_summary)) %>%
    tidyr::unnest(cols=c("result"))
  return(result)
}
