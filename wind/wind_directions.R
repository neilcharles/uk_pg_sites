# Populate the wind_directions / wind_source columns of sites.csv.
#
# Evidence, in order of preference:
#  1. flights  - XC flights from the site, each tagged with the actual historical wind
#                (ERA5 100m wind via Open-Meteo, at the flight's start hour). A sector is
#                flyable if it was flown repeatedly AND more often than a fraction of what
#                the local wind climatology would predict (so rare easterlies still count,
#                but sectors only reached via ERA5 noise don't).
#  2. terrain  - for sites with too few flights: directions in which the ground falls away
#                steeply from the takeoff (Copernicus 30m DEM, see wind/terrain.R).
#  3. manual   - wind/wind_overrides.csv, applied last; always wins.
#
# Usage (from repo root):
#   Rscript wind/terrain.R sites.csv wind/cache/terrain.csv          # once, ~1 min
#   Rscript wind/wind_directions.R [flights.csv] [review.csv]
# flights.csv defaults to wind/xc_flights_to_2025.csv (the wingman-scraper summary archive; kept
# out of git because it contains pilot names) and review.csv to wind/review.csv.
# Wind data is cached in wind/cache/wind (resumable; ~8,000 Open-Meteo calls on first run,
# which is within the free tier's 10,000/day if nothing else is using it).

suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(jsonlite)})

args <- commandArgs(TRUE)
flights_path <- if (length(args) > 0) args[1] else "wind/xc_flights_to_2025.csv"
review_path  <- if (length(args) > 1) args[2] else "wind/review.csv"
sites_path   <- "sites.csv"
cache_dir    <- "wind/cache/wind"
terrain_path <- "wind/cache/terrain.csv"
override_path <- "wind/wind_overrides.csv"

MAX_MATCH_KM   <- 1      # flight takeoff must be within this distance of the site
MIN_FLIGHTS    <- 10     # sites with fewer wind-tagged flights fall back to terrain
MIN_SECTOR_N   <- 3      # flights needed in a sector
MIN_SECTOR_SHARE <- 0.03 # ...and share of the site's flights
MIN_RATIO      <- 0.5    # ...and flights / climatology-expected flights
STRONG_RATIO   <- 1.2    # a sector flown less than this (vs climatology) must also either...
SPILL_FRAC     <- 0.3    # ...have >= this x its two neighbours' flights (not just ERA5 spillover)
                         #    or be a terrain face
TERRAIN_GRAD   <- 0.10   # ground must fall >= 10% to count as a face...
TERRAIN_REL    <- 0.7    # ...and be >= 70% as steep as the steepest face at the site

pts <- c("N","NE","E","SE","S","SW","W","NW")
to_sector <- function(deg) pts[(floor((deg + 22.5) / 45) %% 8) + 1]
hav <- function(lat1, lon1, lat2, lon2) {
  r <- pi/180; dlat <- (lat2-lat1)*r; dlon <- (lon2-lon1)*r
  a <- sin(dlat/2)^2 + cos(lat1*r)*cos(lat2*r)*sin(dlon/2)^2
  2*6371*asin(pmin(1, sqrt(a)))
}

# ---- sites (raw lines, so the rest of the file is written back untouched) ----
lines <- readLines(sites_path)
gen_cols <- c("wind_directions", "wind_source")
csv_field <- '("[^"]*"|[^,"]*)'
while (any(endsWith(lines[1], paste0(",", gen_cols)))) {
  lines <- sub(paste0(",", csv_field, "$"), "", lines)
}
sites <- read_csv(I(paste(lines, collapse = "\n")), show_col_types = FALSE,
                  col_types = cols(.default = col_character()))
sites$takeoff_lat <- as.numeric(sites$takeoff_lat)
sites$takeoff_lon <- as.numeric(sites$takeoff_lon)
n_sites <- nrow(sites)

# ---- flights -> nearest site ----
flights <- read_csv(flights_path, show_col_types = FALSE) %>%
  filter(!is.na(takeoff_lat), !is.na(takeoff_lon), !is.na(start_time))
nn <- vapply(seq_len(nrow(flights)), function(i) {
  d <- hav(flights$takeoff_lat[i], flights$takeoff_lon[i], sites$takeoff_lat, sites$takeoff_lon)
  j <- which.min(d); c(j, d[j])
}, numeric(2))
flights <- flights %>%
  mutate(site_idx = nn[1, ], site_dist = nn[2, ]) %>%
  filter(site_dist <= MAX_MATCH_KM) %>%
  mutate(date = as.Date(sub("^(\\d+)(st|nd|rd|th) ", "\\1 ", flight_date), "%d %B %Y"),
         hour = (as.numeric(start_time) %/% 3600) %% 24,
         clat = round(sites$takeoff_lat[site_idx] * 2) / 2,
         clon = round(sites$takeoff_lon[site_idx] * 2) / 2,
         time = sprintf("%sT%02d:00", format(date), hour),
         blk = as.integer(date) %/% 14) %>%
  filter(!is.na(date))

# ---- historical wind (ERA5 on a 0.5 deg grid, fetched in fortnight blocks) ----
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
reqs <- distinct(flights, clat, clon, blk) %>%
  mutate(start = as.Date(blk * 14), end = pmin(start + 13, Sys.Date() - 7),
         file = file.path(cache_dir, sprintf("%s_%s_%s.json", clat, clon, start)))
todo <- reqs[!file.exists(reqs$file), ]
if (nrow(todo)) message(nrow(todo), " wind blocks to fetch")
for (i in seq_len(nrow(todo))) {
  url <- sprintf(paste0("https://archive-api.open-meteo.com/v1/archive?latitude=%s&longitude=%s",
                        "&start_date=%s&end_date=%s&hourly=wind_direction_100m,wind_speed_100m",
                        "&models=era5&timezone=Europe%%2FLondon"),
                 todo$clat[i], todo$clon[i], todo$start[i], todo$end[i])
  for (attempt in 1:8) {
    h <- tryCatch(fromJSON(url)$hourly, error = function(e) { message(conditionMessage(e)); NULL })
    if (!is.null(h)) break
    Sys.sleep(if (attempt < 3) 20 else 120)
  }
  if (is.null(h)) stop("giving up on ", url)
  write_json(list(time = h$time, dir = h$wind_direction_100m, spd = h$wind_speed_100m), todo$file[i])
  Sys.sleep(0.8)  # stay under 5,000 calls/hour
}
wind <- bind_rows(lapply(seq_len(nrow(reqs)), function(i) {
  w <- fromJSON(reqs$file[i])
  tibble(clat = reqs$clat[i], clon = reqs$clon[i], time = unlist(w$time),
         dir = as.numeric(unlist(w$dir)), spd = as.numeric(unlist(w$spd)))
})) %>% filter(!is.na(dir)) %>% distinct(clat, clon, time, .keep_all = TRUE)

flights <- flights %>% inner_join(wind, by = c("clat", "clon", "time")) %>%
  mutate(sector = to_sector(dir))

# Climatology: afternoon winds in the same cells/fortnights, at speeds pilots actually fly in
spd_lim <- quantile(flights$spd, c(0.02, 0.98))
clim <- wind %>%
  filter(as.integer(substr(time, 12, 13)) %in% 11:16, spd >= spd_lim[1], spd <= spd_lim[2]) %>%
  mutate(sector = to_sector(dir)) %>%
  count(clat, clon, sector) %>% group_by(clat, clon) %>% mutate(p = n / sum(n)) %>%
  ungroup() %>% select(-n)

fl <- flights %>% count(site_idx, clat, clon, sector) %>%
  complete(nesting(site_idx, clat, clon), sector = pts, fill = list(n = 0)) %>%
  left_join(clim, by = c("clat", "clon", "sector")) %>%
  group_by(site_idx) %>%
  mutate(N = sum(n), share = n / N, ratio = n / (N * coalesce(p, 0.01)),
         k = match(sector, pts), nb = n[match((k %% 8) + 1, k)] + n[match(((k - 2) %% 8) + 1, k)]) %>%
  ungroup()

# ---- terrain faces ----
ter <- read_csv(terrain_path, show_col_types = FALSE)
e0 <- ter %>% filter(radius == 0) %>% select(site_idx, e0 = elev)
tg <- ter %>% filter(radius %in% c(120, 300)) %>% left_join(e0, by = "site_idx") %>%
  mutate(g = (e0 - elev) / radius) %>%
  group_by(site_idx, bearing) %>% summarise(g = max(g), .groups = "drop") %>%
  # each 8-point sector = its centre bearing and the two 22.5 deg neighbours
  mutate(sector = to_sector(bearing), centre = bearing %% 45 == 0)
tsec <- bind_rows(lapply(seq_along(pts), function(k) {
  b <- (c(-22.5, 0, 22.5) + (k - 1) * 45) %% 360
  tg %>% filter(bearing %in% b) %>% group_by(site_idx) %>%
    summarise(g = max(g[bearing == b[2]], mean(g)), .groups = "drop") %>% mutate(sector = pts[k])
})) %>% group_by(site_idx) %>%
  # a face = at least TERRAIN_GRAD steep and >= TERRAIN_REL of the site's steepest face
  # (tuned against well-flown sites: ~80% of terrain faces match flights, ~73% of flown sectors found)
  mutate(face = g >= TERRAIN_GRAD & g >= TERRAIN_REL * max(g)) %>% ungroup()

# A flown sector counts if it's flown repeatedly and not implausibly rarely for its wind; a
# weakly-flown one (ratio < STRONG_RATIO) must also not be explained by ERA5 direction error
# spilling over from a busy neighbouring sector, unless the takeoff has a face that way.
# (Calibrated so e.g. Wether Fell SW - common wind, flown but only as spillover from W - drops.)
fl <- fl %>% left_join(select(tsec, site_idx, sector, face), by = c("site_idx", "sector")) %>%
  mutate(ok = n >= MIN_SECTOR_N & share >= MIN_SECTOR_SHARE & ratio >= MIN_RATIO &
           (ratio >= STRONG_RATIO | n >= SPILL_FRAC * nb | coalesce(face, FALSE)))

# ---- combine ----
fmt <- function(s) paste(pts[pts %in% s], collapse = ",")
N_by_site <- fl %>% distinct(site_idx, N)
res <- tibble(site_idx = seq_len(n_sites)) %>% left_join(N_by_site, by = "site_idx") %>%
  mutate(N = coalesce(N, 0L))
res$flights_dirs <- sapply(res$site_idx, function(i) fmt(fl$sector[fl$site_idx == i & fl$ok]))
res$terrain_dirs <- sapply(res$site_idx, function(i) fmt(tsec$sector[tsec$site_idx == i & tsec$face]))
# For sites with only a handful of flights: every sector flown at least once, if not implausibly rare
res$few_dirs <- sapply(res$site_idx, function(i) fmt(fl$sector[fl$site_idx == i & fl$n >= 1 & fl$ratio >= MIN_RATIO]))
res <- res %>% mutate(
  wind_source = case_when(N >= MIN_FLIGHTS & flights_dirs != "" ~ "flights",
                          terrain_dirs != "" ~ "terrain",
                          few_dirs != "" ~ "few_flights",
                          TRUE ~ ""),
  wind_directions = case_when(wind_source == "flights" ~ flights_dirs,
                              wind_source == "terrain" ~ terrain_dirs,
                              wind_source == "few_flights" ~ few_dirs,
                              TRUE ~ ""))

# Flag flight-derived sectors where the takeoff terrain rises into wind (possible lee / wrong site)
lee <- fl %>% filter(ok) %>% select(-face) %>% inner_join(tsec, by = c("site_idx", "sector")) %>%
  group_by(site_idx) %>% filter(max(g) >= TERRAIN_GRAD, g < 0) %>%  # skip flat (tow) sites
  summarise(lee_sectors = fmt(sector))
res <- res %>% left_join(lee, by = "site_idx")

if (file.exists(override_path)) {
  ov <- read_csv(override_path, show_col_types = FALSE, col_types = cols(.default = col_character()))
  for (k in seq_len(nrow(ov))) {
    i <- which(sites$takeoff_name == ov$takeoff_name[k])
    if (!length(i)) { warning("override for unknown site: ", ov$takeoff_name[k]); next }
    res$wind_directions[i] <- ov$wind_directions[k]
    res$wind_source[i] <- "manual"
  }
}

q <- function(x) ifelse(grepl(",", x), paste0('"', x, '"'), x)
writeLines(c(paste0(lines[1], ",wind_directions,wind_source"),
             paste0(lines[-1], ",", q(res$wind_directions), ",", res$wind_source)), sites_path)
print(table(res$wind_source, useNA = "ifany"))

if (!is.na(review_path)) {
  counts <- fl %>% select(site_idx, sector, n) %>%
    pivot_wider(names_from = sector, values_from = n, names_prefix = "n_")
  ratios <- fl %>% select(site_idx, sector, ratio) %>% mutate(ratio = round(ratio, 2)) %>%
    pivot_wider(names_from = sector, values_from = ratio, names_prefix = "r_")
  grads <- tsec %>% select(site_idx, sector, g) %>% mutate(g = round(g, 2)) %>%
    pivot_wider(names_from = sector, values_from = g, names_prefix = "g_")
  review <- res %>% left_join(e0, by = "site_idx") %>%
    left_join(counts, by = "site_idx") %>% left_join(ratios, by = "site_idx") %>%
    left_join(grads, by = "site_idx") %>%
    mutate(takeoff_name = sites$takeoff_name[site_idx], lat = sites$takeoff_lat[site_idx],
           lon = sites$takeoff_lon[site_idx], exclude = sites$exclude[site_idx],
           site_guide = sites$site_guide[site_idx]) %>%
    relocate(takeoff_name, lat, lon, wind_directions, wind_source, N, flights_dirs, terrain_dirs, lee_sectors)
  write_csv(review, review_path, na = "")
}
