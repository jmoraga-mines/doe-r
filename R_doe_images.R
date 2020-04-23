# Scripts to load images

if (!require("pacman")){
  install.packages("pacman")
  require("pacman")
} 
pacman::p_load(raster, tools)


# all_layers <- stack ("doe-images/all_layers_stack.envi")
# names(all_layers) <- c("Chalcedony", "Kaolinite", "Gypsum", 
#                        "Hematite", "Opal", "lst_06222325", 
#                        "big_faults", "Geothermal")

buildup_mask <- stack("doe-images/buildup_mask.grd")
idx_build<-buildup_mask==0

doe_writeRaster <- function(x, filename, format="raster", overwrite=TRUE, bandorder="BSQ"){
  if(tools::file_ext("filename.grd") != "grd") {
    filename <- tools::file_path_sans_ext(filename)
    filename <- paste(filename, ".grd", sep="")
  }
  f1<-writeRaster(x=x, filename=filename, bandorder=bandorder, 
                  format=format, overwrite=overwrite)
  hdr(f1, "ENVI")
  return(f1)
}

normalize_raster <- function(r, num_points=NULL, cut_off = NULL, clean_na = FALSE, na_val = 0){
  for (i in seq(nlayers(r))){
    l<-r[[i]]
    if(clean_na) {
      idx_na<-is.na(l)
      l[idx_na]<-na_val
    }
    
    if(!is.null(num_points)){
      num_points <- max(num_points,1)
      l_df <- as.data.frame(l)
      s1<-order(l_df, decreasing = TRUE)
      cut_off1 <- l_df[s1[num_points],1]
      l[l<=cut_off1]<-0
    }
    if (!is.null(cut_off)) {
      l[l<=cut_off]<-0
    }
    
    max_v <- maxValue(l)
    l<-l/max_v
    r[[i]]<-l
  }
  return(r)
}

apply_threshold <- function(my_layer, a_threshold) { 
  return(sum(as.matrix(my_layer)>a_threshold)) 
}

fill_out <- function(r, radius=1){
  w<-focalWeight(south_brady_def, d=radius, type = "circle")
  diameter = ncol(w)
  w<-w/max(w)
  focus_point = ceiling( (diameter^2)/2 )
  fill.na <- function(x, i=focus_point) {
    if( is.na(x)[i] ) {
      return( round(mean(x, na.rm=TRUE),0) )
    } else {
      return( round(x[i],0) )
    }
  }
}

prep_minerals <- function() {
  # mineral_stack<-stack("d:/CEM/HyMap_CEM")
  buildup_mask <- stack("doe-images/buildup_mask.grd")
  mineral_stack<-stack("d:/CEM/HyMap_CEM")
  mineral_stack<-stack(projectRaster(mineral_stack, buildup_mask, method="ngb"))
  names(mineral_stack)<-c("KrattOpal", "Chalcedony", "Kaolinite",  "Hematite",   "Gypsum") 
  n<-normalize_raster(mineral_stack, num_points = 10000, cut_off = 0.02, clean_na = TRUE)
  idx_build<-buildup_mask==0
  n[idx_build]<-NA
  return(n)
}

prep_deformation <- function(){
  buildup_mask <- stack("doe-images/buildup_mask.grd")
  def_2019 <- stack("doe-images/Def_UTM_2019_IDW_2.tif")
  def_2019 <- stack(projectRaster(def_2019, buildup_mask, method="ngb"))
  idx_build<-buildup_mask==0
  def_2019[idx_build]<-NA
  return(def_2019)
  
}

prep_faults <- function() {
  fault_300m <- stack("doe-images/fault_300m.tif")
  buildup_mask <- stack("doe-images/buildup_mask.grd")
  idx_build<-buildup_mask==0
  fault_300m[idx_build]<-NA
  f <- normalize_raster(stack(fault_300m))
  return(f)
}

# (Do not run)
# mineral_stack <- stack(prep_minerals())
# def_2019 <- stack(prep_deformation())
# faults_2019 <- stack(prep_faults())
# geothermal_2019 <- stack("doe-images/Geothermal_19.tif")
# 
