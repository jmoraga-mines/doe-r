def_new <- function(filename, coherence = 0.8, sd_multiplier = 1){
  d1 <- read.csv(filename)
  t_cols <- ncol(d1)
  # data to analyze starts in column 17
  s_col <- 17
  
  id_coherence <- d1["COHER"]>=coherence
  d_samples <- sum(id_coherence)
  print(paste("Samples: ", d_samples))  
  d1_coord <- d1[c("LAT","LON")]
  coord.dec <- SpatialPoints(cbind(d1_coord$LON,d1_coord$LAT), proj4string = CRS("+proj=longlat"))
  coord.UTM <- spTransform(coord.dec, CRS("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  d1_coord <- as.data.frame(coord.UTM)
  names(d1_coord) <- c("x","y")
  d1 <- cbind(d1_coord, d1[,3:t_cols])
  d1 <- d1[id_coherence, ]
  m1 <- as.matrix(d1[, s_col:t_cols])
  d_min <- min(m1)
  print(paste("Minimum: ", d_min))
  d_max <- max(m1)
  print(paste("Maximum: ", d_max))
  d_mean <- mean(m1)
  print(paste("Average: ", d_mean))
  d_sd <- sd(m1)
  
  print(paste("Standard Deviation: ", d_sd))
  
  uplift <- rowSums(d1[,s_col:t_cols]>(d_mean+sd_multiplier*d_sd))
  subsidence <- rowSums(d1[,s_col:t_cols]<(d_mean-sd_multiplier*d_sd))
  d1$uplift <- uplift
  d1$subsidence <- subsidence
  
  return (d1)
}

anomaly_raster <- function(df, r, items=c("uplift", "subsidence")){
  df <- as.data.frame(df)
  l <- length(items)
  v <- vector(mode="list", length = l)
  print("Creating Rasters:")
  for (i in seq(l)) {
    print(items[i])
    # v[[i]] <- rasterize( df[, c("x","y")], r, df[, items[i]], fun=mean )
    v[[i]] <- rasterize( df[, c("x","y")], r, df[, items[i]])
  }
  print("Creating Stack")
  s<-stack(v)
  names(s) <- items
  return(s)
}

normalize_raster <- function(r, num_points=NULL, cut_off = NULL, clean_na = FALSE, na_val = 0){
  r<-stack(r) # Make sure we are working with a stack object
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

anomaly_sp <- function(x, new_extent){
  x <- as.data.frame(x)
  d8 <- SpatialPoints(x[c("x", "y")], proj4string = CRS("+init=epsg:32611"))
  d8$uplift <- x$uplift
  d8$subsidence <- x$subsidence
  d8s <- crop(d8, extent(new_extent))
  d8s$anomaly <- d8s$uplift+d8s$subsidence
  return(d8s)
}


my_data <- def_new("d:/Deformation/PSI_17_18_19_M_1_65_59img_deltas.csv", coherence = 0.7)
d_mean <- mean(as.matrix(my_data[,17:74]))
d_sd <- sd(as.matrix(my_data[,17:74]))

# d1_coord <- my_data[c("LAT","LON")]
# coord.dec <- SpatialPoints(cbind(d1_coord$LON,d1_coord$LAT), proj4string = CRS("+proj=longlat"))
# coord.UTM <- spTransform(coord.dec, CRS("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

d1_coord <- my_data[c("x","y")]
# d1 <- d1_coord
# d1 <- coord.UTM
uplift <- rowSums(my_data[,17:74]>(d_mean+2*d_sd))
subsidence <- rowSums(my_data[,17:74]<(d_mean-2*d_sd))
d1$uplift <- uplift
d1$subsidence <- subsidence

# coordinates(d1) <- ~ LON+LAT
# proj4string(d1) <- crs("+proj=longlat +datum=WGS84")
# gridded(d1)<-TRUE

extent_brady <- raster(xmn=327499.1, xmx=329062.1, ymn = 4405906, ymx=4409320, res=c(3,3), crs="+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
# Both CRS codes are equivalent for UTM 11N WGS84 projection
# CRS("+init=epsg:32611")
# CRS("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# d2 <- as.data.frame(d1)[,c("coords.x1","coords.x2", "subsidence", "uplift")]
# d2 <- as.data.frame(d1)[,c("x","y", "subsidence", "uplift")]
# names(d2) <- c("x", "y", "subsidence", "uplift")

# r1 <- rasterize(d2[,c("x","y")], extent_brady, d2[,c("subsidence")], fun=sum)
# r2 <- rasterize(d2[,c("x","y")], extent_brady, d2[,c("uplift")], fun=sum)

# grd <- as.data.frame(spsample(d1, "regular", n=50000))
# names(grd)       <- c("x", "y")
# coordinates(grd) <- ~ x + y
# gridded(grd)     <- TRUE  # Create SpatialPixel object
# fullgrid(grd)    <- TRUE  # Create SpatialGrid object
# proj4string(grd) <- proj4string(d1)
# d_idw <- idw( subsidence ~ 1, d1, newdata=grd, idp=1)

# d2 <- crop(d1, extent(extent_brady))
# d2 <- as.data.frame(d2)
# r1 <- rasterize(d2[,c("x","y")], extent_brady, d2[,c("subsidence")], fun=sum)
# r2 <- rasterize(d2[,c("x","y")], extent_brady, d2[,c("uplift")], fun=sum)

d6s <- anomaly_sp(
  def_new("d:/Deformation/PSI_17_18_19_M_1_65_59img_deltas.csv", 
          coherence = 0.8), 
  extent_brady)
i8 <- anomaly_raster(d8s, extent_brady)
i8fu <- fill_out(i8[["uplift"]], radius = 120, i8)
i8fs <- fill_out(i8[["subsidence"]], radius = 120, i8)
i8fa <- fill_out(i8[["anomaly"]], radius = 120, i8)
i8fu <- fill_out(i8fu, radius = 120, i8)
i8fs <- fill_out(i8fs, radius = 120, i8)
i8fa <- fill_out(i8fa, radius = 120, i8)
spplot(stack(i8fu, i8fs, i8fa), main="Fill out (c>=0.8)")

process_def_csv <- function(filename, 
                            new_extent = NA, 
                            coherence = 0.8, 
                            radius = 120,
                            influence_radius = 30) {
  d8 <- def_new(filename, 
                coherence = coherence) 
  if (is.na(new_extent)){
    d0 <- SpatialPoints(d8[c("x", "y")], proj4string = CRS("+init=epsg:32611"))
    new_extent <- raster(extent(d0), crs =  CRS("+init=epsg:32611"), res=c(3,3))
  }
  d8s <- anomaly_sp(d8, new_extent)
  i8 <- anomaly_raster(d8s, 
                       new_extent, 
                       c("uplift", "subsidence", "anomaly"))
  print("Filling out missing data... pass 1")
  print("... uplift")
  i8fu <- fill_out(i8[["uplift"]], radius = radius, i8)
  print("... subsidence")
  i8fs <- fill_out(i8[["subsidence"]], radius = radius, i8)
  print("... anomaly")
  i8fa <- fill_out(i8[["anomaly"]], radius = radius, i8)
  print("Filling out missing data... pass 2")
  print("... uplift")
  i8fu <- fill_out(i8fu, radius = radius, i8)
  print("... subsidence")
  i8fs <- fill_out(i8fs, radius = radius, i8)
  print("... anomaly")
  i8fa <- fill_out(i8fa, radius = radius, i8)
  print("Calculating influence area")
  ii8fa <- influence_area(i8fa, radius = influence_radius)
  print("... uplift")
  ii8fs <- influence_area(i8fs, radius = influence_radius)
  print("... subsidence")
  ii8fu <- influence_area(i8fu, radius = influence_radius)
  print("... anomaly")
  print("Stacking data and finishing")
  s1 <- stack(i8fu, i8fs, i8fa)
  names(s1) <- c("u", "s", "a")
  s1 <- normalize_raster(s1)
  s2 <- stack(ii8fu, ii8fs, ii8fa)
  names(s2) <- c("uplift", "subsidence", "anomaly")
  # spplot(s2, main=paste("Fill out + influence (c>=", coherence,")"))
  return(s2)
}

# https://rspatial.org/raster/analysis/4-interpolation.html
# require(gstat)
# gs <- gstat(formula=subsidence~1, locations=d0, nmax=12, set=list(idp = 0))
# r <- raster(extent(d0), res=c(3,3), crs=crs(d0))
# r <- raster((d0), res=c(3,3))
# nn <- interpolate(r, gs)

