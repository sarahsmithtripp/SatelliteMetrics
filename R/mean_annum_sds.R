#' Calculates the mean and standard deviation of annual metrics from BAP SDS
#'
#'
#' @param sds_choose this is a spatrasterdataset where each spat raster is stacked annually
#' @param polygon a polygon of some vector that is the same projection of the spat raster used to crop the sds
#' @export
#' @importFrom dplyr %>%

mean_annum_sds <- function(sds_choose, polygon) {
  ## polygon is the data that you are working with
  sds_choose <- sds_choose
  if (missing(polygon) == F) {
    if (class(polygon) != 'SpatVector') {
      print('input must be a spat vector with the same CRS as the spat raster dataset')
    }
    else {
      sds_choose <- sds_choose %>% crop(polygon)
      change_year <- polygon$SR_10S_
      ID <- polygon$ID
    }
  }
  else if (missing(polygon) == T) {
    print('If area of the raster is very large function may take a long time to run')
    sds_choose <- sds_choose
    names <- substr(names(sds_choose), 22, 25)
    ID <- substr(names(sds_choose), 6,7)
    change_year <- substr(names(sds_choose), 17,20)[1]
  }

  ##sds is a spd datset
  tik <- length(sds_choose)
  names <- substr(names(sds_choose), 1, 3)
  data_out <- list()
  for (tik in 1:length(sds_choose)) {
    r <- sds_choose[[tik]]
    name_metric <- names[[tik]]
    r.dt <-
      terra::values(r) %>% terra::as.data.frame() %>% dplyr::mutate(CellID = rownames(.)) %>% tidyr::pivot_longer(
        cols = c(dplyr::contains('-08-01')),
        names_to = 'year',
        values_to = 'metric'
      ) %>%
      mutate(year_n = as.numeric(gsub("-08-01", "", as.character(.$year))))
    data <-
      r.dt %>% dplyr::group_by(year_n) %>% dplyr::summarize(
        mean = mean(metric, na.rm = T),
        sd = sd(metric, na.rm = T),
        upp_bound = mean + sd,
        lwr_bound = mean - sd,
        ID = as.numeric(paste(ID)),
        change_year = paste(change_year)
      )
    name_call <- paste(name_metric, names(data)[2:6])
    names(data) <- c('year', paste0(name_call), 'change_year')
    data_out[[tik]] <- data
  }

  df.out <- do.call(cbind, data_out)
  df <- df.out[!duplicated(as.list(df.out))] %>% as_tibble()
  names_pivot <- names(df)[2:length(names(df))]
  type <- c('lwr_bound', 'mean', 'sd',
            'upp_bound', 'ID')
  df_rt <- df %>% tidyr::pivot_longer(
    cols = c(dplyr::contains(type)),
    names_to = 'type',
    values_to = 'value'
  ) %>% tidyr::separate(type, into = c('Metric', 'type'), sep = " ") #%>% mutate(value = value/1000)
}
