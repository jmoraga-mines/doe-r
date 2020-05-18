if (!require("pacman")){
  install.packages("pacman")
  require("pacman")
} 
pacman::p_load(kohonen, RColorBrewer)

# reverse color ramp
colors <- function(n, alpha = 1) {
  rev(heat.colors(n, alpha))
}

SOM.measures1 <- c("Minerals", "Temperature", "Faults")
gwr_sb_df <- read.csv("doe-imagestacks/gwr_sb.csv")
gwr_sb_df <- as.data.frame(gwr_sb_df)
temp_2019<-gwr_sb_df["Temperature"]
idx_tmp_na <- is.na(temp_2019)
temp_2019[idx_tmp_na]<-0
gwr_sb_df["Temperature"] <- temp_2019

SOM.measures1 <- c("Minerals", "Temperature", "Faults")
SOM.SOM1 <- som(scale(gwr_sb_df[SOM.measures1]), grid=somgrid(6,4,"rectangular"))

SOM.measures2 <- c("Chalcedony", "Kaolinite", "Gypsum", "Temperature", "Faults")
SOM.measures3 <- c("Minerals", "Temperature", "Faults", "Deformation")

SOM.SOM2 <- som(scale(gwr_sb_df[SOM.measures2]), grid=somgrid(6,4,"rectangular"))

SOM.SOM3 <- som(scale(gwr_sb_df[SOM.measures3]), grid=somgrid(6,4,"rectangular"))
plot(SOM.SOM1, main="SOM v1")
plot(SOM.SOM1, type = "dist.neighbours", palette.name = terrain.colors, main="Neighbor distance SOM v1")
plot(SOM.SOM2, main="SOM v2")
plot(SOM.SOM2, type = "dist.neighbours", palette.name = terrain.colors, main="Neighbor distance SOM v2")
plot(SOM.SOM3, main="SOM v3")
plot(SOM.SOM3, type = "dist.neighbours", palette.name = terrain.colors, main="Neighbor distance SOM v3")


# plot(SOM.SOM1, type = "counts", palette.name = colors, heatkey = TRUE, main ="Color count SOM v1")
# plot(SOM.SOM2, type = "counts", palette.name = colors, heatkey = TRUE, main ="Color count SOM v2")
# plot(SOM.SOM3, type = "counts", palette.name = colors, heatkey = TRUE, main ="Color count SOM v3")

### https://clarkdatalabs.github.io/soms/SOM_NBA#supervised_soms

