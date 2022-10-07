

library(sp)
#This is the Final dataset 
load("data.combined_withPredPollution.RData")
load("pop_grid.RData")

# Layout of the map
northarrow <- list("SpatialPolygonsRescale", layout.north.arrow(), 
                   offset = c(70000,580000), scale = 30000)
scalebar <- list("SpatialPolygonsRescale", layout.scale.bar(), 
                 offset = c(70000, 570000), scale = 50000, fill = c("transparent", "black"))
text1 <- list("sp.text", c(70000, 550000), "0", cex=2)
text2 <- list("sp.text", c(120000, 550000), "50 km", cex=2) 


# an example for the pm10 map
spplot(data.combined, "predpm102018mean", sp.layout = list(northarrow, scalebar, text1, text2),
       at = seq(0, 15, length.out = 8), col="transparent", 
       col.regions=gray.colors(10, start = 0.9, end = 0.1, gamma = 2.2, alpha = NULL),
       colorkey = list(labels = list( cex = 3)))

# population map
spplot(pop_grid, sp.layout = list(northarrow, scalebar, text1, text2),
       at = seq(min(pop_grid$population,na.rm = T), max(pop_grid$population,na.rm = T), length.out = 8), col="transparent", 
       col.regions=gray.colors(10, start = 0.9, end = 0.1, gamma = 2.2, alpha = NULL),
       colorkey = list(labels = list( cex = 3)))




