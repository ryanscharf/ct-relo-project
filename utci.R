library(tidyverse)
library(sf)
library(stars)
library(tmap)

st <- tigris::states(cb=T)

categories <- data.frame(
  stringsAsFactors = FALSE,
          category = c("Extreme Heat Stress",
                       "Very Strong Heat Stress","Strong Heat Stress",
                       "Moderate Heat Stress","No Thermal Stress","Slight Cold Stress",
                       "Moderate Cold Stress","Strong Cold Stress",
                       "Very Strong Cold Stress","Extreme Cold Stress"),
               low = c(46L, 38L, 32L, 26L, 9L, 0L, -13L, -26L, -39L, -274L),
              high = c(999L, 45L, 37L, 31L, 25L, 8L, -1L, -14L, -27L, -40L)
)


nc <- read_ncdf('ECMWF_utci_20191001_v1.1_int.nc')
nc_c <- (nc - 273.15)

nc_c <- st_crop(nc_c, 
        st_bbox(
          c(
            xmin = -124.848974, 
            xmax = -66.885444, 
            ymax = 49.384358, 
            ymin = 24.396308),
          crs = st_crs(4326)
          )
        )

nc_c %>% 
  #slice('time',1) %>% 
  tm_shape() + 
    tm_raster(
              breaks = c(-273, -40, -27, -13, 0, 9, 26, 32, 38, 46, 100),
              labels = categories$category,
              midpoint = 0
              ) + 
  tm_shape(st) + 
    tm_borders()

maxx <- nc_c %>% aggregate(by = '1 day', max) 
minn <- nc_c %>% aggregate(by = '1 day', min) 

good_index <- function(x){
  #  sum(
      ifelse(
        between(x[[1]],0,9),1L,0L
        )
     # )
}

# gi <- nc_c %>% aggregate(by = '1 hour', FUN = good_index)
# gi2 <- gi %>% aggregate(by = '1 day', sum)
# 
# gi2 %>% aggregate(by = '1 day', sum) %>% tm_shape() + tm_raster() + tm_shape(st) + tm_borders()

path <- 'C:/Users/ryans/Downloads/dataset-derived-utci-historical.zip'
files <- unzip(path, list = T)

#drop first and last files. they're incomplete
files <- files[c(-1, -nrow(files)),]

agg_gi <- function(fl, path = 'C:/Users/ryans/Downloads/dataset-derived-utci-historical.zip',
         output_file = 'utci_daily_good_index.rds'){
  
  dir <- tempdir()
  
  file <- unzip(path, file = fl, exdir = dir)
  
  nc <- read_ncdf(paste(dir, fl, sep = '/'))

  #convert K to C
  nc_c <- (nc - 273.15)
  
  
  # crop to continental US
  nc_c <- st_crop(nc_c,
                  st_bbox(
                    c(
                      xmin = -124.848974,
                      xmax = -66.885444,
                      ymax = 49.384358,
                      ymin = 24.396308
                    ),
                    crs = st_crs(4326)
                    )
                  )
  
  gi <- nc_c %>% aggregate(by = '1 hour', FUN = good_index)
  gi2 <- gi %>% aggregate(by = '1 day', sum)
  
  gi2 <- aperm(gi2, c(2,3,1))
  gi2 <- st_set_dimensions(gi2, 3, 
                           values = 
                             lubridate::ymd(st_get_dimension_values(gi2, 'time')))
  st_crs(gi2) <- st_crs(4326)
  
  if(file.exists(output_file)){
    of <- readRDS(output_file)
    c(of,gi2, along = 3) %>%
      saveRDS(output_file)
  } else {
    saveRDS(gi2, output_file)
  }
}

purrr::walk(files$Name, agg_gi)

readRDS('utci_daily_good_index.rds')
