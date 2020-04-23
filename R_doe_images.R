# Load images
if (!require("pacman")){
  install.packages("pacman")
  require("pacman")
} 
pacman::p_load(raster)
all_layers <- stack ("doe-images/all_layers_stack.envi")
names(all_layers) <- c("Chalcedony", "Kaolinite", "Gypsum", 
                       "Hematite", "Opal", "lst_06222325", 
                       "big_faults", "Geothermal")