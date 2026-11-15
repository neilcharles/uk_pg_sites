# Sample terrain heights on rings around each takeoff from the Copernicus GLO-30 DEM
# (public COG tiles on AWS, read remotely; only the needed pixels are downloaded).
#
# Usage: Rscript wind/terrain.R sites.csv <out terrain.csv>

suppressPackageStartupMessages({library(terra); library(dplyr)})
args <- commandArgs(TRUE)
sites <- read.csv(args[1])
out <- args[2]

BEARINGS <- seq(0, 337.5, by = 22.5)
RADII <- c(60, 120, 200, 300, 500, 800)

dest <- function(lat, lon, brg, d) {
  R <- 6371000; p1 <- lat*pi/180; l1 <- lon*pi/180; b <- brg*pi/180
  p2 <- asin(sin(p1)*cos(d/R) + cos(p1)*sin(d/R)*cos(b))
  l2 <- l1 + atan2(sin(b)*sin(d/R)*cos(p1), cos(d/R) - sin(p1)*sin(p2))
  cbind(lat = p2*180/pi, lon = l2*180/pi)
}

grid <- expand.grid(bearing = BEARINGS, radius = RADII)
pts <- bind_rows(lapply(seq_len(nrow(sites)), function(i) {
  ll <- dest(sites$takeoff_lat[i], sites$takeoff_lon[i], grid$bearing, grid$radius)
  rbind(data.frame(site_idx = i, bearing = 0, radius = 0,
                   lat = sites$takeoff_lat[i], lon = sites$takeoff_lon[i]),
        data.frame(site_idx = i, grid, lat = ll[, "lat"], lon = ll[, "lon"]))
}))

tile_name <- function(lat, lon) {
  la <- floor(lat); lo <- floor(lon)
  sprintf("Copernicus_DSM_COG_10_%s%02d_00_%s%03d_00_DEM",
          ifelse(la >= 0, "N", "S"), abs(la), ifelse(lo >= 0, "E", "W"), abs(lo))
}
pts$tile <- tile_name(pts$lat, pts$lon)
pts$elev <- NA_real_
for (t in unique(pts$tile)) {
  url <- sprintf("/vsicurl/https://copernicus-dem-30m.s3.amazonaws.com/%s/%s.tif", t, t)
  r <- tryCatch(rast(url), error = function(e) NULL)
  idx <- which(pts$tile == t)
  # No tile = all-sea tile
  pts$elev[idx] <- if (is.null(r)) 0 else extract(r, cbind(pts$lon[idx], pts$lat[idx]))[, 1]
  cat(t, length(idx), "\n")
}
pts$elev[is.na(pts$elev)] <- 0
write.csv(pts[, c("site_idx", "bearing", "radius", "elev")], out, row.names = FALSE)
