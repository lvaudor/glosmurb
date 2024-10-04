#' Describe the content of an osmdata directory for a particular key-value association
#' @param directory the directory
#' @param key the osm key to consider
#' @param value the osm value to consider
#' @return a table with the number, geometry type, and other characteristics of osm elements
#' @export
describe_osmdata_item=function(directory,key,value){
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
    dplyr::mutate(shp_path=glue::glue("{directory}/{key}-{value}-{type}.shp")) %>%
    dplyr::mutate(result=purrr::map2(shp_path,type,get_shp_summary)) %>%
    tidyr::unnest(cols=c("result"))
  return(result)
}
