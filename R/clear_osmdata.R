# WARNING - Generated by {fusen} from dev/flat_1.Rmd: do not edit by hand

#' Clears the local {dir_name}/{shape_name}/{key}_{value}.RDS file if it exists
#' @param key OSM key
#' @param value OSM value
#' @param shape_name shape (sf object) or name of a shape (inside quotes), corresponding to the geographical area inside which to look for key-value occurrences
#' @param dir_name the directory under which returned data is saved. Defaults to "data/osmdata".
#' @export
#' @examples
#' bridges_confluence=get_osmdata("man_made","bridge","confluence", save=TRUE)
#' file.exists("data/osmdata/confluence/man_made-bridge.RDS")
#' clear_osmdata("man_made","bridge","confluence")
#' file.exists("data/osmdata/confluence/man_made-bridge.RDS")
clear_osmdata <- function(key,value,shape_name,dir_name="data/osmdata"){
  osmdata_file=glue::glue("{dir_name}/{shape_name}/{key}-{value}.RDS")
  result="does not exist"
  if(file.exists(osmdata_file)){
    done=file.remove(osmdata_file)
    result="removed"
  }
  return(result)
}
