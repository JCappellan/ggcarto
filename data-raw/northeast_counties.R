# Builds data/northeast_counties.rda
#
# Sources
# * Geometry: US Census Bureau 2010 cartographic boundary file, counties,
#   1:5,000,000 (gz_2010_us_050_00_5m), as GeoJSON from
#   https://github.com/kjhealy/us-county (data/geojson/gz_2010_us_050_00_5m.json)
# * Attributes: `county_complete` from the usdata package (OpenIntro,
#   https://github.com/OpenIntroStat/usdata, GPL-3), which compiles
#   American Community Survey 2015-2019 5-year estimates (pop_2019,
#   median_household_income_2019, unemployment_rate_2019, bachelors_2019,
#   hs_grad_2019, poverty_2019, median_age_2019, hispanic_2019, black_2019,
#   white_not_hispanic_2019) and 2010 land area (area_2010) from the Census
#   Bureau's QuickFacts.
#
# Run from the package root with the two files in data-raw/.
library(sf)

geo <- read_sf("data-raw/gz_2010_us_050_00_5m.json")
ne_fips <- c(CT = "09", ME = "23", MA = "25", NH = "33", RI = "44", VT = "50",
             NJ = "34", NY = "36", PA = "42")
geo <- geo[geo$STATE %in% ne_fips, ]
geo$fips <- paste0(geo$STATE, geo$COUNTY)

load("data-raw/county_complete.rda")
d <- county_complete
d$fips <- sprintf("%05d", d$fips)
d <- d[d$fips %in% geo$fips, ]

attrs <- data.frame(
  fips          = d$fips,
  county        = sub(" County$", "", d$name),
  state         = names(ne_fips)[match(substr(d$fips, 1, 2), ne_fips)],
  state_name    = d$state,
  pop           = as.integer(d$pop_2019),
  area          = d$area_2010,                       # land area, square miles
  density       = d$pop_2019 / d$area_2010,          # people per square mile
  median_income = as.integer(d$median_household_income_2019),
  unemployment  = d$unemployment_rate_2019,
  pct_bachelors = d$bachelors_2019,
  pct_hs_grad   = d$hs_grad_2019,
  pct_poverty   = d$poverty_2019,
  median_age    = d$median_age_2019,
  pct_hispanic  = d$hispanic_2019,
  pct_black     = d$black_2019,
  pct_white_nh  = d$white_not_hispanic_2019,
  stringsAsFactors = FALSE
)

# An equal-area conic centred on the region, so geom_sf() needs no coord_sf().
crs_ne <- "+proj=aea +lat_1=40 +lat_2=44 +lat_0=42 +lon_0=-73 +datum=NAD83 +units=m +no_defs"
g <- st_transform(geo[, c("fips", "geometry")], crs_ne)
g <- st_simplify(g, dTolerance = 300, preserveTopology = TRUE)
stopifnot(all(st_is_valid(g)))

northeast_counties <- merge(g, attrs, by = "fips")
northeast_counties <- northeast_counties[order(northeast_counties$state, northeast_counties$county), ]
northeast_counties <- northeast_counties[, c(names(attrs), "geometry")]
rownames(northeast_counties) <- NULL
northeast_counties <- st_as_sf(northeast_counties)
st_geometry(northeast_counties) <- "geometry"

stopifnot(nrow(northeast_counties) == 217L)
save(northeast_counties, file = "data/northeast_counties.rda", compress = "xz")
