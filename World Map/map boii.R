library(sf)          # classes and functions for vector data
library(raster)      # classes and functions for raster data

library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data

vignette(package = "sf") # see which vignettes are available
vignette("sf1")          # an introduction to the package

names(world)

plot(world)

summary(world["lifeExp"])

world_mini = world[1:2, 1:3]
world_mini

data(world)

# View(world)

library(sp)
world_sp = as(world, Class = "Spatial")

world_sf = st_as_sf(world_sp)


plot(world[3:6])
plot(world["pop"])

world_asia = world[world$continent == "Asia", ]
asia = st_union(world_asia)

plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")


plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop) / 10000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)
