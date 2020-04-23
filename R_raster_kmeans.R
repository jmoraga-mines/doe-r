#### temperature
kmeans_tmp <- function(pt_lst_16, img_name, n_clusters=5, plot_image = FALSE) {
  pt_tmp_dst <- pt_lst_16[,c(img_name)]
  k_tmp_dst_pt<-pt_lst_16[,c("x","y")]
  pt_tmp_dst<-(pt_tmp_dst-mean(pt_tmp_dst))/(sd(pt_tmp_dst))
  k_tmp_dst <- kmeans(pt_tmp_dst,n_clusters, iter.max = 2000)
  cluster_lst <- as.data.frame(k_tmp_dst$centers)
  cluster_lst$cluser <- seq(1,length(k_tmp_dst$centers))
  colnames(cluster_lst)<-c("temp", "cluster")
  sorted_clusters <- cluster_lst[order(cluster_lst[,1]),]
  k_tmp_dst$cluster <- match(unlist(k_tmp_dst$cluster), sorted_clusters[,2])
  k_tmp_dst$centers <- unlist(sorted_clusters$temp)
  if (plot_image){
    k_tmp_dst_pt[,3] <- k_tmp_dst$cluster
    coordinates(k_tmp_dst_pt) <- ~ x+ y
    proj4string(k_tmp_dst_pt) <- crs(" +proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
    gridded(k_tmp_dst_pt) <- TRUE
    plot(k_tmp_dst_pt, col=rev(rainbow(n_clusters)))
  }
  #summary(pt_tmp_dst)
  return (k_tmp_dst)
}


#### temperature + distance
kmeans_dst <- function(pt_lst_16, img_name, n_clusters=5, plot_image = FALSE) {
  pt_tmp_dst <- pt_lst_16[,c("x","y",img_name)]
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
  }
  # View(pt_tmp_dst)
  # summary(pt_tmp_dst)
  return (k_tmp_dst)
}