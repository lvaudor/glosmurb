#' collect OSM data corresponding to a key-value inside a bounding shape. If a file already exists corresponding to "{dir_name}/{shape_name}_{key}_{value}, the data is not retrieved through a new query but through reading this file.
#' @param key OSM key
#' @param value OSM value
#' @param shape_name shape (sf object) or name of a shape (inside quotes), or path to a shapefile (inside quotes and corresponding to an existing path) corresponding to the geographical area inside which to look for key-value occurrences
#' @param dir_name the directory under which returned data is saved. Defaults to "data/osmdata".
#' @return a list with elements key, value, shape_name and result which is an osmdata_sf object
#' @export
#'
#' @examples
#' data(confluence)
#' bridges_confluence=save_osmdata("man_made","bridge","confluence")
#' bridges_confluence=save_osmdata("man_made","bridge","confluence")
#' drinking_water_confluence=save_osmdata("amenity","drinking_water","confluence")
#' landuse_vineyard_condrieu=save_osmdata("landuse","vineyard","condrieu")
#' landuse_port_lyon=save_osmdata("landuse","port","lyon")
save_osmdata=function(key, value, shape_name, dir_name="data/osmdata", pause=0){
  # shape_name can either be a path to a shapefile
  # or a sf object
  # the extraction is carried out for the bounding box of the shape
  if(is.character(shape_name)){
    if(file.exists(shape_name)){
      shape=sf::st_read(shape_name,quiet=TRUE)}else{
          shape=get(shape_name)}
  }else{
    shape=shape_name
    shape_name=as.character(substitute(shape_name))
  }
  # keep just file name as stripped_shape_name (to name the results directory)
  stripped_shape_name=stringr::str_extract(shape_name,
                                           "[^\\/]*(?=\\.shp)")
  if(!dir.exists(glue::glue("{dir_name}/{stripped_shape_name}"))){
    dir.create(glue::glue("{dir_name}/{stripped_shape_name}"),
               recursive=TRUE)
  }
  # Check if key-value for this shape has already been extracted
  file_done_already=glue::glue("{dir_name}/{stripped_shape_name}/done.csv")
  if(file.exists(file_done_already)){
    done_already=read.csv(file_done_already)
    its_done_already=done_already %>%
      dplyr::filter(keys==key,values==value) %>%
      nrow()
    if(its_done_already>0){return()}
  }
  # If not, then consider bounding box and divide it into a grid such that
  # each cell is less than 15 squared kilometers on average
  bbox=sf::st_bbox(shape)
  bbox_sf <- sf::st_as_sfc(bbox)
  ngrid=round((sqrt(sf::st_area(shape) %>% as.numeric())/1000)/15)
  mygrid <- sf::st_make_grid(bbox_sf, n = c(ngrid, ngrid)) %>%
    purrr::map(sf::st_bbox)
  # The extraction of one cell is carried out by function get_result_grid()
  get_result_grid=function(bbox){
    raw_result=osmdata::opq(bbox, timeout=120)%>%
      osmdata::add_osm_feature(key = key,
                               value = value) %>%
      osmdata::osmdata_sf()
    # write the results in various shapefiles according to geometry
    for(type in c("osm_points",
                  "osm_lines",
                  "osm_polygons",
                  "osm_multilines",
                  "osm_multipolygons")){
    if(!is.null(raw_result[[type]])){
      result=raw_result[[type]]
        if(nrow(result)>0){
              if(!("osm_id" %in% colnames(result))){result=result %>% mutate(osm_id=rownames(result))}
          # Some rare items come with a Wikidata identifier. Save them in wikidata.csv (one per shape)
          if("wikidata" %in% colnames(result)){
                tib_wikidata=result %>%
                  sf::st_drop_geometry() %>%
                  dplyr::filter(wikidata!="") %>%
                  dplyr::select(osm_id,wikidata) %>%
                  dplyr::mutate(key=key,
                                value=value,
                                type=type)
                print(tib_wikidata)
                readr::write_csv(tib_wikidata,
                                 glue::glue("{dir_name}/{stripped_shape_name}/wikidata.csv"),append=TRUE)
              }
              result=result %>%
                select(osm_id)
              osmdata_file=glue::glue("{dir_name}/{stripped_shape_name}/{key}-{value}-{type}.shp")
              sf::st_write(result, dsn=osmdata_file, append=TRUE)
      }
    }
    }
  }
  data=purrr::map(mygrid,get_result_grid)
  # Once the data has been saved for all cells of the grid, keep in mind that the extraction has been done
  # for this shape, and this combination key-value
  readr::write_csv(tibble::tibble(keys=key,values=value),
                   glue::glue("{dir_name}/{stripped_shape_name}/done.csv"),
                   append=TRUE,col_names=TRUE)

}
