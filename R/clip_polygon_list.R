#' Uses a polygon list to clip a given raster
#'
#'
#' @param sample_polygons should be a polygon list in SF language
#' @param sds_choose is a spat raster dataset that will be clipped for each polygon
#' @param file_save the location to store the clipped polygons
#' @param disturbed Is this location disturbed?
#' @examples
#' \dontrun{
#'
#' file_save <- c('.')
#' sample_polygons <- read_sf('./shapefile.shp')
#' sds_to_run <- terra::sds(list.files('./stacked_metrics', pattern =- '00.tif$', full.names = T))
#'
#' clip_polygon_list(sample_polygons, sds_choose = sds_to_run, file_save = file_save)
#'}
#'
#' @export
#' @importFrom dplyr %>%
#' @rawNamespace import(ggplot2)
#' @rawNamespace import(dplyr)



clip_polygon_list <-
  function(sample_polygons, sds_choose, file_save,
           disturbed) {
    print(file_save)
    for (i in 1:length(sample_polygons)) {
      poly <- sample_polygons[[i]]
      print(i)
      query_site_nums <- which(grepl('ID$', names(poly))) # Get the ID column to store in the file_names
      site_id <- paste0(poly[1, query_site_nums])[1]
      if (missing(disturbed) == F) {
        query_disturbance <-
          which(grepl('SR_10S_$', names(poly)))
        disturbance_year <-
          paste0(poly[1, query_disturbance])[1]
        site_name <- paste0(site_id, '_', disturbance_year)
        print(paste(
          'working on site id',
          site_id,
          'which was disturbed in',
          disturbance_year
        ))
        poly_vect <- terra::vect(poly) %>% terra::project(sds_choose)
        crop_sds <- sds_choose %>% terra::crop(poly_vect)
        direct_save <- paste0(file_save,'/', site_id)
        if (dir.exists(direct_save) == T) {
          print(paste0('Overwriting Site Numbers and storing to',
                       direct_save))
          for (n in 1:length(crop_sds)) {
            file_tosave <-
              paste0(direct_save,
                     '/site_' ,
                     site_name,
                     "_",
                     substr(names(crop_sds)[n], 1, 4),
                     ".tif")
            terra::writeRaster(x = crop_sds[[n]],
                               filename = file_tosave,
                               overwrite = T)
          }
        }
        else if (dir.exists(direct_save) == F) {
          print(paste0('Creating a new directory to:', direct_save))
          dir.create(direct_save)
          for (n in 1:length(crop_sds)) {
            file_tosave <-
              paste0(direct_save,
                     '/site_',
                     site_name,
                     "_",
                     substr(names(crop_sds)[n], 1, 4),
                     ".tif")
            rast_tosave <- crop_sds[[n]]
            terra::writeRaster(rast_tosave,
                               filename = file_tosave,
                               overwrite = F)
          }
        }
      }
      else if (missing(disturbed) == T) {
        site_name <- paste0(site_id, '_')
        print(paste(
          'working on site id',
          site_id,
          'which has no NTEMS disturbance information'
        ))
        poly_vect <- terra::vect(poly) %>% terra::project(sds_choose)
        crop_sds <- sds_choose %>% terra::crop(poly_vect)
        direct_save <- paste0(file_save, site_id)
        if (dir.exists(direct_save) == T) {
          print('Overwriting Site Numbers')
          for (n in 1:length(crop_sds)) {
            file_tosave <-
              paste0(direct_save,
                     '/site_' ,
                     site_name,
                     "_",
                     substr(names(crop_sds)[n], 1, 4),
                     ".tif")
            terra::writeRaster(x = crop_sds[[n]],
                               filename = file_tosave,
                               overwrite = T)
          }
        }
        else if (dir.exists(direct_save) == F) {
          dir.create(direct_save)
          for (n in 1:length(crop_sds)) {
            file_tosave <-
              paste0(direct_save,
                     '/site_',
                     site_name,
                     "_",
                     substr(names(crop_sds)[n], 1, 4),
                     ".tif")
            rast_tosave <- crop_sds[[n]]
            terra::writeRaster(rast_tosave,
                               filename = file_tosave,
                               overwrite = F)
          }
        }
      }
      print(paste(
        'SpatRasterDataset cropped files for each polygon stored in',
        file_save
      ))
    }
  }
