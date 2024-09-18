#' intersect the OSM elements shapefile with a sf object defining zones.
#' @param osm_shape_path the path to a saved OSM elements shapefile (saved by save_osmdata() function)
#' @param zones_shape the shapes of zones in which to dispatch the osm elements (each OSM element will be associated to one or several zones)
#' @param from the name of the directory containing the original osm data shapefiles
#' @param to the name of the directory which will contain the resulting shapefiles
#' @return
#' @export
#'
#' @examples


dispatch_osmdata_in_zones=function(osm_shape_path,zones_shape,from="data/osmdata",to="data/osmdata_trimmed"){
  osm_shape=sf::st_read(osm_shape_path,quiet=TRUE) %>%
    sf::st_make_valid()
  new_path=stringr::str_replace(osm_shape_path,
                                glue::glue("/{from}/"),
                                glue::glue("/{to}/"))
  if(file.exists(new_path)){return("Done")}
  sf::sf_use_s2(FALSE)
  zones_shape
  intersect_shape=function(shape){
    intersects=sf::st_intersects(osm_shape,shape,sparse=FALSE)
    result=osm_shape %>%
      dplyr::mutate(intersects=intersects) %>%
      dplyr::filter(intersects==TRUE) %>%
      dplyr::select(-intersects) %>%
      dplyr::mutate(data=purrr::map(osm_id,function(x){shape %>% st_drop_geometry()})) %>%
      tidyr::unnest(cols=c("data"))
    return(result)
  }
  zones_shapes=zones_shape %>%
    mutate(npol=1:n()) %>%
    group_by(npol) %>%
    tidyr::nest() %>%
    dplyr::mutate(data=purrr::map(data,intersect_shape)) %>%
    sf::st_drop_geometry()
  result=do.call(rbind,zones_shapes$data)
  directory=stringr::str_replace(new_path,"[^\\/]*\\.shp","")
  if(!dir.exists(directory)){dir.create(directory,recursive=TRUE)}
  sf::st_write(result,dsn=new_path,quiet=TRUE)
  return("Done")
}

