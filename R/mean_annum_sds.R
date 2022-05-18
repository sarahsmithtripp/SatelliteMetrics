#' Calculates the mean and standard deviation of annual metrics from BAP SDS
#'
#' @param sds_choose this is a spatrasterdataset where each spat raster is stacked annually
#' @param polygon a polygon of some vector that is a sf object. The function will convert to a vector object to crop
#' @export
#' @importFrom dplyr %>%


mean_annum_sds <- function(sds_choose, polygon) {
  ## polygon is the data that you are working with
  if (missing(polygon) == F)
  {
    change_year <- polygon$SR_10S_
    query_site_nums <-
      names(polygon)[which(grepl('ID$', names(polygon)))] # Get the ID column to store in the file_names
    ID_col <- paste0(polygon[1, query_site_nums])[1]
    polygon <-
      polygon %>% terra::vect() %>% terra::project(sds_choose)
    sds_choose <- sds_choose %>% terra::crop(polygon)
  }
  else if (missing(polygon) == T) {
    print('If area of the raster is very large function may take a long time to run')
    names <- substr(names(sds_choose), 22, 25)
    ID_col <- substr(names(sds_choose), 6, 7)
    change_year <- substr(names(sds_choose), 17, 20)[1]
  }
  ##sds is a sds datset
  r_list_length <- length(sds_choose)
  names <- substr(names(sds_choose), 1, 3)
  data_out <- list()
  for (i in 1:r_list_length) {
    r <- sds_choose[[i]]
    name_metric <- names(sds_choose)[i]
    print(ID_col)
    r.dt <-
      terra::values(r) %>% terra::as.data.frame() %>% dplyr::mutate(CellID = rownames(.)) %>% tidyr::pivot_longer(
        cols = c(dplyr::contains('-08-01')),
        names_to = 'year',
        values_to = 'metric'
      ) %>%
      dplyr::mutate(year_n = as.numeric(gsub("-08-01", "", as.character(.$year))))
    data <-
      r.dt %>% dplyr::group_by(year_n) %>% dplyr::summarize(
        mean = mean(metric, na.rm = T),
        sd = sd(metric, na.rm = T),
        upp_bound = mean + sd,
        lwr_bound = mean - sd,
        ID_ty = as.numeric(paste(ID_col)),
        change_year = paste(change_year))
    name_call <- paste(name_metric, names(data)[2:6])
    names(data) <- c('year', paste0(name_call), 'change_year')
    data_out[[i]] <- data
  }
  df.out <- do.call(cbind, data_out)
  df <- df.out[!duplicated(as.list(df.out))] %>% tidyr::as_tibble()
  names_pivot <- names(df)[2:length(names(df))]
  type <- c('lwr_bound', 'mean', 'sd',
            'upp_bound', 'ID_ty')
  df_rt <- df %>% tidyr::pivot_longer(
    cols = c(dplyr::contains(type)),
    names_to = 'type',
    values_to = 'value'
  ) %>% tidyr::separate(type, into = c('Metric', 'type'), sep = " ")
}
