############Partition Clustering: Kmeans, CLARA #######
#####kmeans###############
install.packages("factoextra")
library(factoextra)

"Los problemas con kmeans son: en z particiona cualquier cosa para med
con fill 0.35 y casi todas las v, y cuando uso z y vz quedan outliers 
que no respetan el criterio de corte de simulación"

######CLARA###############

install.packages("cluster")
library(cluster)

cl.res <- clara(select(myfiles, z, vz), 2, metric = "euclidean", stand = FALSE)
clust <- as.data.frame(as.factor(cl.res$clustering))
names(clust) <- paste("clust")
last_output <- bind_cols(myfiles, clust)

"según tengo entendido, clara pone enfasis en que los clusters
sean lo más disímiles posibles, pero también da problemas parecidos
a kmeans"

###############################################################
"y de acá: https://scikit-learn.org/stable/modules/clustering.html
saqué los que vienen ahora"
###gaussian mixture
"hay dos implementaciones. una no termino de entender, pero que devuelve 
los centroides (gaussian_comps)"
install.packages("ClusterR")
library(ClusterR)
gm.res <- GMM(select(myfiles, z, vz), gaussian_comps = 2, dist_mode = "eucl_dist",
              seed_mode = "random_subset", km_iter = 10, em_iter = 5,
              verbose = FALSE, var_floor = 1e-10, seed = 1)

"Esta otra implementación es parecida a kmeans, pero de todas maneras
no da bien"
install.packages("mclust")
library(mclust)
mclust.res <- Mclust(select(myfiles, vz), G=2)
clust <- as.data.frame(as.factor(mclust.res$classification))
names(clust) <- paste("clust")
last_output <- bind_cols(myfiles, clust)

#########spectral clustering
install.packages("anocva")
library(anocva)
spectralClustering(select(myfiles, z, vz), 2)
"esta no le da a mi compu"

###########dbscan
install.packages("fpc")
library(fpc)
db <-  fpc::dbscan(testing$vz, eps = 0.018, MinPts = 4)
plot(db, testing$vz, main = "DBSCAN", frame = FALSE)
db
"no le da a mi compu tampoco"

####EXTRA
"también probé sacar los clusters usando gmm y dándoselos a 
kmeans directamente como puntos de inicio, pero tampoco dio bien"