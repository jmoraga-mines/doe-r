if (!require("pacman")){
  install.packages("pacman")
  require("pacman")
} 
pacman::p_load(kohonen, RColorBrewer)

# reverse color ramp
colors <- function(n, alpha = 1) {
  rev(heat.colors(n, alpha))
}



### plot(SOM.SOM1, type = "counts", palette.name = colors, heatkey = TRUE, main ="Color count SOM v1")
### plot(SOM.SOM2, type = "counts", palette.name = colors, heatkey = TRUE, main ="Color count SOM v2")
### plot(SOM.SOM3, type = "counts", palette.name = colors, heatkey = TRUE, main ="Color count SOM v3")

### https://clarkdatalabs.github.io/soms/SOM_NBA#supervised_soms

### SOM.measures4 <- c("Minerals", "Temperature", "Faults")
### SOM.SOM4 <- som(scale(gwr_sb_df[SOM.measures4]), grid=somgrid(3,3,"rectangular"), rlen=1000)
### plot(SOM.SOM4, main="SOM v4")
### SOM.SOM4 <- som(scale(gwr_sb_df[SOM.measures4]), grid=somgrid(3,3,"rectangular"), rlen=10000)
### plot(SOM.SOM4, main="SOM v4 (10,000 iterations)")
### SOM.SOM5 <- som(scale(gwr_sb_df[SOM.measures3]), grid=somgrid(4,4,"rectangular"), rlen=1000)
### plot(SOM.SOM5, main="SOM v3 (1,000 iterations)")
### summary(SOM.SOM5)

### View(gwr_sb_df)
### View(SOM.SOM5)
### SOM.SOM5[[1]]
### head(SOM.SOM4$unit.classif)

### sb_df <- gwr_sb_df[c("x", "y")]
### sb_df$class <- SOM.SOM4$unit.classif
### coordinates(sb_df ) <- ~x+y
### proj4string(sb_df) <- crs(gwr_sb)
### gridded(sb_df) <- TRUE
### spplot(sb_df)
### plot(gwr_sb)
### plot(gwr_stack2)
### sb_df1 <- sb_df
### idx_1 <- sb_df1[[1]]==1
### sb_df1[[1]][!idx_1] <- NA
### plot(sb_df1)
### plot(sb_df)
### plot(SOM.SOM5$codes[[1]])
### View(SOM.SOM5$codes[[1]])
### summary(gwr_sb_df[idx_1,])

simple_som <- function(measures, data, grid=somgrid(3,3,"rectangular"), rlen=100, seed = 42, main_title="SOM"){
  s <- scale(data[measures])
  s_center <- attr(s,"scaled:center")
  s_scale <- attr(s,"scaled:scale")
  s_fun <- function(x){
    y = x * s_scale + s_center
  }
  s_revfun <- function(x){
    y = ((x - s_centers) / s_scale)
  }
  # 
  # Force the same number generation for SOM
  set.seed( seed )
  s_model <- som(s, grid=grid, rlen=rlen)
  s_model$s_center <- s_center
  s_model$s_scale <- s_scale
  s_model$s_fun <- s_fun
  s_model$s_revfun <- s_revfun
  s_model$clusters<-t(apply(s_model$codes[[1]], 1, s_fun))
  return(s_model)
}




# Random number generation in R
# R has nine pseudo-random generators they are as follows.
#
# Uniform Distribution – runif(number, minimum, maximum)
# Normal Distribution – rnorm(number, mean, standard deviation)
# Binomial Distribution – rbinom(number, size, probability)
# The log-normal Distribution – rlnorm(number, mean log, standard deviation log)
# Weibull Distribution – rweibull(number, shape, scale)
# Exponential Distribution – rexp(number, rate)
# Poisson Distribution – rpois(number, lambda)
# Gamma Distribution – rgamma(number, shape, rate)
# Chisquare Distribution – rchisq(number, degrees of freedom, non-centrality parameter)

### s3_10000 <- simple_som(SOM.measures3, gwr_df, grid=somgrid(3,3,"rectangular"), rlen=10000, main_title = "SOM (3,3) rlen=10,000")
### s4_10000 <- simple_som(SOM.measures3, gwr_df, grid=somgrid(4,4,"rectangular"), rlen=10000, main_title = "SOM (4,4) rlen=10,000")


### test <- function(){
###   SOM.measures1 <- c("Minerals", "Temperature", "Faults")
### gwr_sb_df <- read.csv("doe-imagestacks/gwr_sb.csv")
###   gwr_sb_df <- as.data.frame(gwr_sb_df)
### temp_2019<-gwr_sb_df["Temperature"]
###   idx_tmp_na <- is.na(temp_2019)
###   temp_2019[idx_tmp_na]<-0
###   gwr_sb_df["Temperature"] <- temp_2019
###   
###   SOM.measures1 <- c("Minerals", "Temperature", "Faults")
### SOM.SOM1 <- som(scale(gwr_sb_df[SOM.measures1]), grid=somgrid(6,4,"rectangular"))
###   
###   SOM.measures2 <- c("Chalcedony", "Kaolinite", "Gypsum", "Temperature", "Faults")
###   SOM.measures3 <- c("Minerals", "Temperature", "Faults", "Deformation")
### 
###   SOM.SOM2 <- som(scale(gwr_sb_df[SOM.measures2]), grid=somgrid(6,4,"rectangular"))
###   
###   SOM.SOM3 <- som(scale(gwr_sb_df[SOM.measures3]), grid=somgrid(6,4,"rectangular"))
###   plot(SOM.SOM1, main="SOM v1")
###   plot(SOM.SOM1, type = "dist.neighbours", palette.name = terrain.colors, main="Neighbor distance SOM v1")
###   plot(SOM.SOM2, main="SOM v2")
###   plot(SOM.SOM2, type = "dist.neighbours", palette.name = terrain.colors, main="Neighbor distance SOM v2")
###   plot(SOM.SOM3, main="SOM v3")
###   plot(SOM.SOM3, type = "dist.neighbours", palette.name = terrain.colors, main="Neighbor distance SOM v3")
### }


# s3_100 <- simple_som(SOM.measures3, gwr_df, grid=somgrid(3,3,"rectangular"), 
#                      rlen=100, main_title = "SOM (3,3) rlen=100")
# s3_1000 <- simple_som(SOM.measures3, gwr_df, grid=somgrid(3,3,"rectangular"), 
#                       rlen=1000, main_title = "SOM (3,3) rlen=1,000")
# s3_2000 <- simple_som(SOM.measures3, gwr_df, grid=somgrid(3,3,"rectangular"), 
#                       rlen=2000, main_title = "SOM (3,3) rlen=2,000")

# s4_100 <- simple_som(SOM.measures3, gwr_df, grid=somgrid(4,4,"rectangular"), 
#                      rlen=100, main_title = "SOM (4,4) rlen=100")
# s4_1000 <- simple_som(SOM.measures3, gwr_df, grid=somgrid(4,4,"rectangular"), 
#                       rlen=1000, main_title = "SOM (4,4) rlen=1,000")
# s4_2000 <- simple_som(SOM.measures3, gwr_df, grid=somgrid(4,4,"rectangular"), 
#                       rlen=2000, main_title = "SOM (4,4) rlen=2,000")

# r<-gwr_df[c("x","y")]
# r3 <- r
# r4 <- r
# r3$s1 <- s3_100$unit.classif
# r3$s2 <- s3_1000$unit.classif
# r3$s3 <- s3_2000$unit.classif
# names(r3)  <- c("x", "y", "s3_100", "s3_1k", "s3_2k")
### r3$s3 <- s3_10000$unit.classif
### names(r3)  <- c("x", "y", "s3_100", "s3_1k", "s3_10k")

# r4$s1 <- s4_100$unit.classif
# r4$s2 <- s4_1000$unit.classif
# r4$s3 <- s4_2000$unit.classif
# names(r4)  <- c("x", "y", "s4_100", "s4_1k", "s4_2k")
### r4$s3 <- s4_10000$unit.classif
### names(r4)  <- c("x", "y", "s4_100", "s4_1k", "s4_10k")

# coordinates(r3) <- ~ x+y
# proj4string(r3)<- crs(gwr_stack[[1]])
# gridded(r3)<-TRUE

# coordinates(r4) <- ~ x+y
# proj4string(r4)<- crs(gwr_stack[[1]])
# gridded(r4)<-TRUE



# som_stack <- stack("doe-imagestacks/som_stack")
# spplot(som_stack)
# idx <- is.na(som_stack[["Faults"]])
# som_stack[idx]<-NA
# som_measures <- names(som_stack)
# som_df <- rasterToPoints(som_stack)
# som_df <- as.data.frame(som_df)
# s3_100 <- simple_som(som_measures, som_df, grid=somgrid(3,3,"rectangular"), 
#                       rlen=100, main_title = "SOM (3,3) rlen=100")
# s3_500 <- simple_som(som_measures, som_df, grid=somgrid(3,3,"rectangular"), 
#                       rlen=500, main_title = "SOM (3,3) rlen=500")
# s3_1000 <- simple_som(som_measures, som_df, grid=somgrid(3,3,"rectangular"), 
#                        rlen=1000, main_title = "SOM (3,3) rlen=1000")
# r<- coordinates(som_stack)
# r3<-r
# r3$s1 <- s3_100$unit.classif
# r3$s2 <- s3_500$unit.classif
# r3$s3 <- s3_1000$unit.classif
# names(r3)  <- c("x", "y", "s3_100", "s3_500", "s3_1k")
# coordinates(r3) <- ~ x+y
# proj4string(r3)<- crs(som_stack2[[1]])
# gridded(r3)<-TRUE
# spplot(r3)
# plot(s3_1000, main="SOM v3 (1,000 iterations)")
