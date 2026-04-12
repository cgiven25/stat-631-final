# https://www.birds.cornell.edu/home/us-state-level-conservation-data-summaries
# Website is very detailed and includes good info for the background

# percent_x: percent of global population occurring in given state
#   breeding: during breeding season
#   rounded data, values of zero indicate there were still sightings but so few that it rounds to zero
# max_week_season: season where bird population reaches max in state (if resident/year-round resident, look at previous cols for population portion)
# max_week: week where that maximum occurs (week midpoints)
# max_week_percent_pop: maximum global population proportion corresponding to the max week
# state_rank_breeding: state's rank relative to other states which have a nonzero breeding season for that species
# breeding_habitat: from Avian Conservation Assessment Database (https://pif.birdconservancy.org/avian-conservation-assessment-database/)
birds <- read.csv("birds_ma/MA_regional_status_2023.csv")

# trend_period: year range for trend being modeled
# season_code: season for which trend is modeled (breeding, nonbreeding, year_round)
# state_trend: percent change in species population over the trend period  (includes 80% confidence intervals, median estimate)
# rangewide_trend: median trend across the entire range of a species over the trend period, with 80% CIs
birds_t <- read.csv("birds_ma/MA_regional_trends_2023.csv")

# I didn't include the abundance/shared stewardships maps.
# It seems like we need some familiarity with GIS software to use them (I have none) but it looks interesting
# Might not be useful for our project though

# -----------------------------------------

# https://data.nasa.gov/dataset/fireball-and-bolide-reports
# "Exceptionally bright meteors that are spectacular enough to be seen over a very wide area"
# Meteoroid: asteroid or comet fragment that orbits the Sun
# Meteor: "shooting star", visible paths of meteoroids that have entered the atmosphere at high velocities
# Fireball: unusually bright meteor
# Bolide: fireball that explodes in the atmosphere

# radiated energy: amount of energy burned in the atmosphere
# impact energy: calculated energy of impact, always larger than radiated
#    but some of these don't impact the ground so idk
fireball <- read.csv("fireball_bolide/reports.csv")

# -----------------------------------------

# https://data.nasa.gov/dataset/meteorite-landings
# Data on all known meteorite landings

# nametype: Valid for most, Relict if they have been highly altered by weathering on Earth
# recclass: meteorite classification based on composition (see https://en.wikipedia.org/wiki/Meteorite_classification)
# fall: Fell or Found, assuming that this is whether or not we just know that they fell and have not been able to find or if we found it
# reclat, reclong, geolocation are just the lat/long/coordinates of the recovery location
meteorites <- read.csv("meteorites/meteors.csv")

# -----------------------------------------

# https://data.nasa.gov/dataset/wise-nea-comet-discovery-statistics
# Discovery statistics for WISE/NEOWISE missions

# H: absolute magnitude
# MOID: minimum orbit intersection distance (closest points of osculating orbit b/t )
# q: perihelion
# Q: aphelion
# period: how long it takes to orbit sun
# inclination: angle of orbit relative to the ecliptic (the plane of Earth's orbit relative to the sun)
# PHA: potentially hazardous asteroid
# orbit class:
#   Amor: near-earth asteroid orbits similar to 1221 Amor
#   Apollo: near-earth asteroid orbits which cross the Earth's orbit similar to 1862 Apollo
#   Aten: near-earth asteroid orbits similar to that of 2062 Aten
#   Comet: comet orbit not matching any defined orbit class
#   Parabolic comet: Comets on parabolic orbits (eccentricity = 1)
#   see https://pdssbn.astro.umd.edu/data_other/objclass.shtml for other classifications
comets <- read.csv("comets/comets.csv")
