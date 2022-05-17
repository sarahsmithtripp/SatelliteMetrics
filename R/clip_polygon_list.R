#' Uses a polygon list to clip a given raster
#'
#'
#' @param sample_polygons should be a polygon list in SF language
#' @param raster is a raster that will be clipped for each polygon
#' @param return_rasters would yuo like to return the raster list?
#'
#' #' @examples
#'
#' @export
#' @importFrom dplyr %>%


clip_polygon_list <- function(sample_polygons, raster, return_rasters) {
  #Make a some lists to store information from the function
  polygon_info <- list()
  summary_polygons <- list()
  rast_clips <- list()
  if (class(sample_polygons) == 'SpatVector') {
    polygon_vect = sample_polygons
  }
  else {
    print('Converting to a Spat Vector - this function might break here')
    polygon_vect <- terra::vect(sample_polygons)
  }
  if (grepl('-', names(raster)) == T) {
    for (i in 1:length(polygon_vect$ID)) {
      polygon <- polygon_vect[i]
      r.clip <-
        raster %>% terra::project(terra::crs(polygon)) %>% terra::crop(polygon)
      r.dt = terra::xyFromCell(r.clip, 1:terra::ncell(r.clip)) %>%
        cbind(terra::values(r.clip)) %>%
        terra::as.data.frame() %>% dplyr::mutate(CellID = rownames(.)) %>% tidyr::pivot_longer(
          cols = c(contains('-')),
          names_to = 'year',
          values_to = 'metric'
        ) %>%
        dplyr::mutate(year_n = as.numeric(substr(as.character(.$year), 1, 4)))
      summary_polygons[[i]] <-
        r.dt %>% tidyr::group_by(year_n) %>% dplyr::summarize(
          mean_nbr = mean(nbr, na.rm = T),
          sd_nbr = sd(nbr, na.rm = T),
          upp_bound = mean_nbr + sd_nbr,
          lwr_bound = mean_nbr - sd_nbr,
          ID = polygon$ID
        ) %>% dplyr::left_join(polygon, copy = T)
      polygon_info[[i]] <- r.dt
      rast_clips[[i]] <- r.clip
    }
  }
  else {
    for (i in 1:length(polygon_vect$ID)) {
      polygon <- polygon_vect[i]
      r.clip <-
        raster %>% terra::project(terra::crs(polygon)) %>% terra::crop(polygon)
      r.dt = terra::xyFromCell(r.clip, 1:terra::ncell(r.clip)) %>%
        cbind(terra::values(r.clip)) %>%
        terra::as.data.frame() %>% dplyr::mutate(CellID = rownames(.)) %>% tidyr::pivot_longer(cols = starts_with('y_'),
                                                                                               names_to = 'year',
                                                                                               values_to = 'metric') %>%
        dplyr::mutate(year_n = as.numeric(substr(as.character(.$year), 1, 4)))
      summary_polygons[[i]] <-
        r.dt %>% tidyr::group_by(year_n) %>% dplyr::summarize(
          mean_nbr = mean(nbr, na.rm = T),
          sd_nbr = sd(nbr, na.rm = T),
          upp_bound = mean_nbr + sd_nbr,
          lwr_bound = mean_nbr - sd_nbr,
          ID = polygon$ID
        ) %>% dplyr::left_join(polygon, copy = T)
      polygon_info[[i]] <- r.dt
      rast_clips[[i]] <- r.clip
    }
  }
  if (return_rasters == T) {
    list_outs <- list(as.data.frame(bind_rows(summary_polygons)),
                      rast_clips)
    return(list_outs)
  }
  if (missing(return_rasters))
  {
    return(as.data.frame(bind_rows(summary_polygons)))
  }
}
