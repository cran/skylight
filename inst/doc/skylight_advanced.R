## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dpi = 300
)

## ----eval = FALSE-------------------------------------------------------------
#  library(ecmwfr)
#  
#  request <- list(
#    product_type = "reanalysis",
#    format = "netcdf",
#    variable = "total_cloud_cover",
#    year = "2021",
#    month = "02",
#    day = "01",
#    time = c("00:00", "01:00", "02:00", "03:00", "04:00",
#             "05:00", "06:00", "07:00", "08:00", "09:00",
#             "10:00", "11:00", "12:00", "13:00", "14:00",
#             "15:00", "16:00", "17:00", "18:00", "19:00",
#             "20:00", "21:00", "22:00", "23:00"),
#    area = c(70, -20, 33, 25),
#    dataset_short_name = "reanalysis-era5-single-levels",
#    target = "era5.nc"
#  )
#  
#  wf_request(
#    user = "xxxx",
#    request = request,
#    transfer = TRUE
#  )

## ----echo = FALSE-------------------------------------------------------------
library(terra)

# read in data
era5 <- try(terra::rast(system.file(package = "skylight", "extdata/era5.nc")))

if(inherits(era5, "try-error")){
  process <- FALSE
} else {
  process <- TRUE
}

# if the file can be read but the version
# is too old do not process
v <- (as.numeric(version$major) + as.numeric(version$minor)/10) > 4.2
process <- all(v, process)

## ----eval = FALSE-------------------------------------------------------------
#  library(terra)
#  
#  # read in data
#  era5 <- terra::rast(system.file(package = "skylight", "extdata/era5.nc"))
#  
#  # use this command when downloading the data yourself
#  # era5 <- terra:rast(file.path(tempdir(),"era5.nc"))

## ----eval = process-----------------------------------------------------------
library(dplyr)

# create a data frame with values
df <- era5 |> 
  as.data.frame(xy = TRUE) |>
  rename(
    longitude = "x",
    latitude = "y"
  )

# add the original time stamp
df$date <- time(era5)

print(head(df))

## ----eval = process-----------------------------------------------------------
library(skylight)

# calculate sky illuminance values for
# a single date/time and location
df <- df |>
  mutate(
    # values of cloud cover lower than
    # 30% are considered clear conditions
    # for the skylight model - adjust tcc values
    tcc = ifelse(tcc <= 0.3, 1, tcc),
    # rescale total cloud cover between 1 - 10
    # the acceptable range for skylight's
    # sky_condition parameter
    sky_condition = scales::rescale(df$tcc, to = c(1,10))
  )

# pipe into skylight
df <- df |> skylight()

print(head(df))

## ----eval = process-----------------------------------------------------------
library(ggplot2)
library(rnaturalearth)

# country outlines
outlines <- rnaturalearth::ne_countries(returnclass = "sf")

ggplot(df) +
  geom_tile(
    aes(
      longitude,
      latitude,
      fill = log(total_illuminance)
    )
  ) +
  scale_fill_viridis_c(
    option = "B",
    name = "Total illuminance (log(lux))"
  ) +
  geom_sf(
    data = outlines,
    colour = "white",
    fill = NA
    ) +
  coord_sf(
    xlim = c(-20, 25),
    ylim = c(33, 70)
  ) +
  labs(
    title = "Total illuminance with spatial variability\n due to cloud cover and location",
    y = "",
    x = ""
  ) +
  theme(
    legend.position = "bottom"
  )

