# UK Paragliding Sites
UK paragliding and hang gliding sites database, indended to help power weather and other helpful apps.

Includes manually maintained site guide links and exclusions for school training sites etc.

Originally produced as part of The PG & HG Alerts project http://alerts.hilltop-analytics.com/

## Wind directions

`wind_directions` lists the wind directions (8 compass points, e.g. `"SW,W,NW"`) a site is flyable in. `wind_source` says where that came from:

- `flights` - 10+ XC flights from the site, each tagged with the actual ERA5 wind at its start time; a direction counts if it's flown repeatedly and not far less often than that wind occurs locally; a direction flown less often than its wind occurs must also either clearly stand out from its neighbouring directions (rather than being ERA5 wind error spilling over from them) or have a terrain face.
- `terrain` - too few flights, so directions where the ground falls away steeply from the takeoff (Copernicus 30m DEM). Less reliable; worth checking.
- `few_flights` - no clear terrain, so the directions of the handful of flights there are.
- `manual` - from `wind/wind_overrides.csv`. Put corrections there, not in sites.csv, so they survive re-runs.

Regenerate with `Rscript wind/terrain.R sites.csv wind/cache/terrain.csv` then `Rscript wind/wind_directions.R <summary_archive.csv> wind/review.csv` (run from the repo root). `wind/review.csv` holds the evidence per site (flight counts, flown-vs-expected ratios, terrain gradients, and `lee_sectors` flags where flown directions face uphill - often a sign of nearby takeoffs being mixed up).
