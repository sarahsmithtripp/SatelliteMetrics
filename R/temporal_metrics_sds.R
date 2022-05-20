#' Calculates a temporal values for a user defined time period and user defined function. Example function included in the documentation.
#'
#'
#' @param sds_choose this is a spatrasterdataset where each spat raster is stacked annually
#' @param time_length what is the time length to calculate over ?
#' @param last_year last year of the dataset to calculate last metric to go by
#' @param eval_funct functions to evaluate for temporal metrics
#' @param polygon region to clip spat raster dataset by
#' @param disturbance_year optional argument to state what you would like to calculate by
#' @param parent_folder a folder that designates where to store the files
#' @return metrics
#' @examples
#' \dontrun{
#' A an examples of the function summary input by the user
#' funSummary <- function(x){
#'
#'   c(
#'     median = median(x,na.rm=TRUE),
#'     IQR = IQR(x,na.rm=TRUE),
#'    slope = foster::theilSen(x)
#'   )
#' }
#' }
#'@export
#'@importFrom dplyr %>%
#'@rawNamespace import(foster)

temporal_metrics_sds <- function(sds_choose,
                         time_length,
                         last_year,
                         eval_funct,
                         polygon,
                         disturbance_year,
                         parent_folder) {
  ## polygon is the data that you are working with
  sds_choose <- sds_choose
  ##sds is a spd datset
  tik <- length(sds_choose)
  names <- names(sds_choose)
  year_n <- dim(sds_choose[[1]])[3]
  metrics <- list()
  if (missing(polygon) == F) {
    if (class(polygon) != 'SpatVector') {
      print('input must be a spat vector with the same CRS as the spat raster dataset')
    }
    else {
      sds_choose <- sds_choose %>% terra::crop(polygon)
    }
  }
  else if (missing(polygon) == T) {
    print('If area of the raster is very large function may take a long time to run')
    sds_choose <- sds_choose
  }
  if (missing(disturbance_year) == T) {
    for (n in 1:length(sds_choose)) {
      metric_name <- names[n]
      print(paste('Working On', metric_name))
      r <- sds_choose[[n]]
      l_names <- names(r)
      vect_years <- floor(terra::nlyr(r) / time_length)
      years_to_return <-
        seq(
          from = last_year,
          to = c(last_year - time_length * vect_years),
          by = -time_length
        )
      if(missing(year_format) == F){
        year_day <- paste0(years_to_return)
      pairs <-
        data.frame(v1 = year_day[-length(year_day)], v2 = year_day[-1])
      }
      else if (missing(year_format) == T){
        year_day <- paste0(years_to_return, '-08-01')
        pairs <-
          data.frame(v1 = year_day[-length(year_day)], v2 = year_day[-1])
      }
      for (l in 1:length(pairs$v1)) {
        subset_years <- pairs[l, ]
        print(paste('Working On', subset_years[2], '-', subset_years[1]))
        file_name <-
          paste0(
            parent_folder,'/',
            metric_name,
            '_',
            substr(subset_years[2], 1, 4),
            '_',
            substr(subset_years[1], 1, 4),
            '.tif'
          )
        match <-
          unique(grep(paste(subset_years, collapse =  "|"), l_names))
        r_sub <-
          r %>% terra::subset(match[1]:match[2]) ## Subset raster to just years of interest
        metrics[[l]] <- terra::app(r_sub,
                                   fun = eval_funct, filename = file_name,
                                   overwrite = T)
      }
    }
  }
  else if
    (missing(disturbance_year) == F) {
      for (n in 1:length(sds_choose)) {
        metric_name <- names[n]
        print(paste('Working On', metric_name))
        r <- sds_choose[[n]]
        l_names <- names(r)
        vect_years <- floor(terra::nlyr(r) / time_length)
        time_points <- last_year - disturbance_year
        vect_years <- floor(time_points / time_length)
        years_to_return <-
          seq(
            from = disturbance_year,
            to = disturbance_year + (time_length * vect_years),
            by = time_length
          )
        year_day <- paste0(years_to_return, '-08-01')
        pairs <-
          data.frame(v1 = year_day[-length(year_day)], v2 = year_day[-1])
        for (l in 1:length(pairs$v1)) {
          subset_years <- pairs[l, ]
          print(paste('Working On', subset_years[2], '-', subset_years[1]))
          file_name <-
            paste0(
              parent_folder,'/',
              metric_name,
              '_',
              substr(subset_years[2], 1, 4),
              '_',
              substr(subset_years[1], 1, 4),
              '.tif'
            )
          match <-
            unique(grep(paste(subset_years, collapse =  "|"), l_names))
          r_sub <-
            r %>% terra::subset(match[1]:match[2]) ## Subset raster to just years of interest
          metrics[[l]] <- terra::app(r_sub,
                                     fun = eval_funct, filename = file_name,
                                     overwrite = F)
        }
      }
    }
}
