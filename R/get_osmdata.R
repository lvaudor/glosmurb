#' collect OSM data corresponding to a key-value inside a bounding shape. If a file already exists corresponding to "{dir_name}/{shape_name}_{key}_{value}, the data is not retrieved through a new query but through reading this file.
#' @param keyvalue OSM keyvalue
#' @param type the type of OSM geometry (defaults to "osm_polygons")
#' @param citycode the citycode
#' @param conn a connection to database
#' @return a list with elements key, value, shape_name and result which is an osmdata_sf object
#' @export
#'
#' @examples
#' citycode=1235681_21899
#' get_osmdata(keyvalue="landuse-farmland",type="osm_polygons",citycode="1235681_21899",conn=sandbox_conn())
get_osmdata=function(keyvalue=NA, type="osm_polygons", citycode, conn){
  if(is.na(keyvalue)){
    query = glue::glue("SELECT * FROM {type} WHERE citycode = '{citycode}';")
  }
  if(is.na(citycode)){
    query=glue::glue("SELECT * FROM {type} WHERE osm_keyvalue='{keyvalue}';")
  }
  if(!is.na(keyvalue) & !is.na(citycode)){
      query=glue::glue("SELECT * FROM {type} WHERE citycode = '{citycode}' AND osm_keyvalue='{keyvalue}';")
  }
  result= DBI::dbGetQuery(conn, query)
  return(result)
}
