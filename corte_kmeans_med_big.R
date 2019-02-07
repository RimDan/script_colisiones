############################LIBRERÍAS#####################################################
library(dplyr)
library(ggplot2)
library(gtools)
library(purrr)
library(caret)
library(tictoc)
library(stats)
library(factoextra)
library(plyr)
#library(readr)
#library(e1071)
#library(corrplot)


####DIRECTORIO DE LAS SIMULACIONES######################################################
setwd("/media/daniela/SAMSUNG/granular_sims_set100/")

####donde guardar los plots##########
path <- as.character("/home/daniela/simulaciones_daniela/plots_kmeans/")


root_dir <- getwd()
num_files_train <- 1
num_dumps_to_read <- 100

toteng_cut <- 0

dirs <- list.files(path="./", full.names = TRUE, pattern = "granular_*")


last_step_med <- data.frame( size= character(),
                               fill= numeric(),
                               velocity=numeric(),
                               last_step = numeric(),
                               final_t = numeric(),
                               stringsAsFactors=FALSE)

k <- list()
#radio de corte
R_c <- 0.76e-6 * 6

for (f in 1:length(dirs)) {
  #for (f in 1:1) {
  tic("input")
  print("DIRECTORY:")
  print(dirs[f])
  
  size <- as.character(unlist(strsplit(dirs[f],"[_]"))[2])
  fill <- as.numeric(unlist(strsplit(dirs[f],"[_]"))[3])
  vel  <- as.numeric(unlist(strsplit(dirs[f],"[_]"))[4])
  
  #if (size == "big" & vel == -0.25 & fill == 0.15){
  if (size != "small" & vel >= -1.0 & vel == -0.1){
    
    #cd to simulation directory
    directory <- paste(getwd(),"/granular_",size,"_",fill,"_",vel,sep ="")
    setwd(directory)
    
    total_grains <- as.numeric(unlist(strsplit(grep("Loop time", readLines("log.lammps"),value = TRUE), " "))[12])
    print(paste("Total_grains:",total_grains))
    
    final_time <- as.numeric(unlist(strsplit(grep("Loop time", readLines("log.lammps"),value = TRUE), " "))[4])
    
    total_steps <- as.numeric(unlist(strsplit(grep("Loop time", readLines("log.lammps"),value = TRUE), " "))[9])
    print(paste("Total Steps:",total_steps))
    
    #predict_between_n_steps <- total_steps / num_dumps_to_read
    #print(paste("Cantidad de pasos entre dumps: ", predict_between_n_steps))
    
    temp = list.files(pattern="output.*.gz")
    vals <- unlist(lapply(temp, FUN=function(x){strsplit(x[1], ".g")[[1]][1]}))
    vals2 <- unlist(lapply(vals, FUN=function(x){strsplit(x[1], "output.")[[1]][2]}))
    #vkeys_lastfile <- tail(mixedorder(vals2),1)
    
    for (i in 1:length(temp)) {
      vkeys_lastfile <- mixedorder(vals2, decreasing = TRUE)[[i]]
      myfiles = as.data.frame(read.table(temp[vkeys_lastfile], header= FALSE, sep=' ', strip.white = TRUE, skip = 9, skipNul = TRUE, stringsAsFactors=FALSE,
                                         col.names=c("id", "type", 
                                                     "x", "y", "z",
                                                     "vx", "vy", "vz",
                                                     "omegax", "omegay", "omegaz", "NA")))
      
      ###############BORRAR NA'S############################################
      myfiles <- myfiles[-12]
      ###KMEANS######
      set.seed(123)
      km.res <- kmeans(select(myfiles, z, vz), 2, nstart = 25)
      clust <- as.data.frame(as.factor(km.res$cluster))
      names(clust) <- paste("clust")
      last_output <- bind_cols(myfiles, clust)
      
      if(abs(mean(filter(last_output, clust == 1)[["vz"]])) < abs(mean(filter(last_output, clust == 2)[["vz"]]))){
        clust <- as.data.frame(as.factor(km.res$cluster)) 
        names(clust) <- paste("clust")
      }else{
        vec <- vector("double", nrow(last_output))
        
        for (i in 1:nrow(last_output)) {
          if(as.numeric(last_output[[12]][i])==1){
            vec[i] <- 2
          }else{
            vec[i] <- 1
          }
        }
        
        clust <- as.data.frame(as.factor(vec))
        names(clust) <- paste("clust")
        
      }
      myfiles <- bind_cols(myfiles, clust)
      
      
      min_c1 <- min(filter(myfiles, clust == 1)[[5]]) 
      max_c2 <- max(filter(myfiles, clust == 2)[[5]])
      dis <- abs(min_c1 - max_c2)
      if(dis >= R_c & min_c1>=max_c2){  
        v <- print(temp[vkeys_lastfile])
      }else if(dis >= R_c & min_c1<=max_c2){
        v <- print(temp[vkeys_lastfile-1])
        break
      }else{
        v <- print(temp[vkeys_lastfile])
        break
      }
    }
        
    esteplot <- ggplot(myfiles, aes(id, z, color=clust)) + geom_point()
    plot <- as.character(paste("pkmeans", size,fill, vel, sep = "_"))
    plot <- as.character(paste(plot, "pdf", sep="."))
    ggsave(plot, plot = esteplot, path = path)
    
    s <- as.data.frame(rep(size, nrow(myfiles)))
    x <- as.data.frame(rep(vel,nrow(myfiles)))
    filf <- as.data.frame(rep(fill, nrow(myfiles)))
    y <- as.data.frame(bind_cols(x,filf))
    y <- as.data.frame(bind_cols(s,y))
    names(y) <- paste(c("size","velocity", "fill.factor"))
    myfiles <- bind_cols(y,myfiles)
    
    v1 <- strsplit(v[1], ".g")[[1]][1]
    last_step <- as.numeric(strsplit(v1[1], "output.")[[1]][2])
    
    tf <- as.numeric((last_step * final_time) / total_steps)
    

    
    new_data <- data.frame(size = size,  fill=fill, velocity = vel, last_step = last_step, 
                           final_t = tf)
    last_step_med <- rbind(last_step_med, new_data) 
    
    k[[f]] <- myfiles
    setwd(root_dir)
  }
}


super_last_output_med <- ldply(k, rbind)

#save(super_last_output,file="all_lo.Rda")
save(last_step_med,file="cut_time_med.Rda")

#load("/home/daniela/THESIS/all_lo.Rda")

