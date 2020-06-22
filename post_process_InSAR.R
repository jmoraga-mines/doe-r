# Scripts to load images

if (!require("pacman")){
  install.packages("pacman")
  require("pacman")
} 
pacman::p_load(raster, rasterKernelEstimates, spdep)


# Inputs are
# r: a stack  -- Mahmut: I converting to Raster in order to feed the function ???
# radius: radius (in meters)
# base_raster: a raster to use a base to calculate radius in pixels
# r and base_raster should have the same resolution

fill_out <- function(r, radius=1, base_raster){ 
  w<-focalWeight(base_raster, d=radius, type = "circle")
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
  r2 <- focal(r, w = w, fun = fill.na, 
              pad = TRUE, na.rm = FALSE )
  return (r2)
}


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


#### kmeans for one layer, no distance
#### input data are:
#### r_pts: is a data frame with x, y coordinates
#### img_name: name of the layer (data frame column), to analyze

kmeans_tmp <- function(r_pts, img_name, n_clusters=5, plot_image = FALSE) {
  pt_tmp_dst <- r_pts[,c(img_name)]
  k_tmp_dst_pt<- r_pts[,c("x","y")]
  center = mean(pt_tmp_dst)
  scale = sd(pt_tmp_dst)
  pt_tmp_dst<-(pt_tmp_dst-mean(pt_tmp_dst))/(sd(pt_tmp_dst))
  set.seed(42)
  k_tmp_dst <- kmeans(pt_tmp_dst,n_clusters, iter.max = 2000)
  cluster_lst <- as.data.frame(k_tmp_dst$centers)
  cluster_lst$cluster <- seq(1,length(k_tmp_dst$centers))
  colnames(cluster_lst)<-c("temp", "cluster")
  sorted_clusters <- cluster_lst[order(cluster_lst[,1]),]
  # print("sorted clusters:")
  # print(sorted_clusters)
  k_tmp_dst$cluster <- match(unlist(k_tmp_dst$cluster), sorted_clusters[,2])
  k_tmp_dst$centers <- unlist(sorted_clusters$temp)
  k_tmp_dst$size <- k_tmp_dst$size[sorted_clusters$cluster]
  k_tmp_dst$withinss <- k_tmp_dst$withinss[sorted_clusters$cluster]
  k_tmp_dst$center <- center
  k_tmp_dst$scale <- scale
  k_tmp_dst$centroids <- k_tmp_dst$centers*k_tmp_dst$scale+k_tmp_dst$center
  if (plot_image){
    k_tmp_dst_pt[,3] <- k_tmp_dst$cluster
    coordinates(k_tmp_dst_pt) <- ~ x+ y
    proj4string(k_tmp_dst_pt) <- crs(" +proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
    gridded(k_tmp_dst_pt) <- TRUE
    plot(k_tmp_dst_pt, col=rev(rainbow(n_clusters)), main=img_name)
    # k_tmp_dst_pt<-stack(k_tmp_dst_pt)
    # k_tmp_dst_pt[[1]]$centers <- k_tmp_dst$centers
    # k_tmp_dst_pt[[1]]$size <- k_tmp_dst$size
    # k_tmp_dst_pt[[1]]$withinss <- k_tmp_dst$withinss
    # k_tmp_dst_pt[[1]]$center <- k_tmp_dst$center
    # k_tmp_dst_pt[[1]]$scale <- k_tmp_dst$scale
    # k_tmp_dst_pt[[1]]$centroids <- k_tmp_dst$centroids
    return(k_tmp_dst_pt)
  }
  #summary(pt_tmp_dst)
  return (k_tmp_dst)
}


#### temperature + distance
kmeans_dst <- function(r_pts, img_name, n_clusters=5, plot_image = FALSE) {
  pt_tmp_dst <- r_pts[,c("x","y",img_name)]
  k_tmp_dst_pt<-pt_tmp_dst
  pt_tmp_dst["x"]<-(pt_tmp_dst["x"]-mean(pt_tmp_dst[,"x"]))/(sd(pt_tmp_dst[,"x"]))
  pt_tmp_dst["y"]<-(pt_tmp_dst["y"]-mean(pt_tmp_dst[,"y"]))/(sd(pt_tmp_dst[,"y"]))
  pt_tmp_dst[3]<-(pt_tmp_dst[3]-mean(pt_tmp_dst[,3]))/(sd(pt_tmp_dst[,3]))
  #summary(pt_tmp_dst)
  k_tmp_dst <- kmeans(pt_tmp_dst,n_clusters, iter.max = 2000)
  #cluster_lst <- as.data.frame(k_tmp_dst$centers)
  #cluster_lst$cluser <- seq(1,length(k_tmp_dst$centers))
  #colnames(cluster_lst)<-c("temp", "cluster")
  #sorted_clusters <- cluster_lst[order(cluster_lst[,1]),]
  #k_tmp_dst$cluster <- match(unlist(k_tmp_dst$cluster), sorted_clusters[,2])
  #k_tmp_dst$centers <- unlist(sorted_clusters$temp)
  if (plot_image){
    k_tmp_dst_pt[,3] <- k_tmp_dst$cluster
    coordinates(k_tmp_dst_pt) <- ~ x+ y
    proj4string(k_tmp_dst_pt) <- crs(" +proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
    gridded(k_tmp_dst_pt) <- TRUE
    plot(k_tmp_dst_pt, col=rev(rainbow(n_clusters)))
    return(stack(k_tmp_dst_pt))
  }
  # View(pt_tmp_dst)
  # summary(pt_tmp_dst)
  return (k_tmp_dst)
}
# setwd("D:\\Layers05042020")
# def <- stack("Def_Bardy_Merge.tif")
# idx_na <- def==0
# def[idx_na] <- NA
# fault <- stack("FaultDen300m.tif")
# crs(fault)
# f<- fill_out(def2, radius = 90, fault)
# def_pts <- as.data.frame(rasterToPoints(f))

