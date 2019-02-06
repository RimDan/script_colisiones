library(e1071)


df <- super_last_output
df[[14]] <- as.numeric(df[[14]])
df <- filter(df,  clust == 1)  #choose only one of the classes

x <- subset(df, select = vz) #make x variables
y <- df$clust #make y variable(dependent)
model <- svm(x, y,type='one-classification', nu=0.10, 
             kernel="radial", data = df) #train an one-classification model 


print(model)
summary(model) #print summary




get_time <- function(type,time_list){
  t <- mean(as.numeric(unlist(lapply(time_list, 
                                     FUN=function(x){ 
                                       v <- strsplit(x[1],":"); 
                                       if (v[[1]][1] == type) strsplit(v[[1]][2]," ")[[1]][2]; 
                                     }))))
}

####DIRECTORIO DE TODOS LOS OUTPUTS######################################################
#setwd("/home/daniela/simulaciones_daniela/")
setwd("/media/daniela/SAMSUNG/granular_sims_set100/")

root_dir <- getwd()
num_dumps_to_read <- 100

dirs <- list.files(path="./", full.names = TRUE, pattern = "granular_med_*")


output_predict   <- data.frame( size= character(),
                                fill= numeric(),
                                velocity=numeric(),
                                step=numeric(),
                                accuracy=numeric(),
                                specificity=numeric(),
                                sensitivity=numeric(),
                                kappa=numeric(),
                                precision=numeric(),
                                recall=numeric(),
                                stringsAsFactors=FALSE)

output_benchmark <- data.frame(
  size= character(),
  fill= numeric(),
  velocity=numeric(),
  predict_total = numeric(),
  time_train_svm = numeric(),
  time_predict_svm = numeric(),
  time_input = numeric(),
  time_save_total = numeric())

options(scipen = 999)

tic("total")
for (f in 1:length(dirs)) {
  #for (f in 1:1) {
  tic("input")
  print("DIRECTORY:")
  print(dirs[f])
  
  #extract year,coef and landa from filename
  size <- as.character(unlist(strsplit(dirs[f],"[_]"))[2])
  fill <- as.numeric(unlist(strsplit(dirs[f],"[_]"))[3])
  velocity  <- as.numeric(unlist(strsplit(dirs[f],"[_]"))[4])
  
  if (size == "med" & velocity >= -1.0 & velocity <= -0.1){
    
    #cd to simulation directory
    directory <- paste(getwd(),"/granular_",size,"_",fill,"_",velocity,sep ="")
    setwd(directory)
    l <- fill
    v <- velocity
    min_step <- filter(last_step_med_big, size =="med" & fill == l & velocity == v)$last_step
    #total_grains <- as.numeric(unlist(strsplit(grep("Loop time", readLines("log.lammps"),value = TRUE), " "))[12])
    #print(paste("Total_grains:",total_grains))
    
    #total_steps <- as.numeric(unlist(strsplit(grep("Loop time", readLines("log.lammps"),value = TRUE), " "))[9])
    #print(paste("Total Steps:",total_steps))
    
    #predict_between_n_steps <- total_steps / num_dumps_to_read
    #print(paste("Cantidad de pasos entre dumps: ", predict_between_n_steps))

    
    ####kmeans###
   # temp = list.files(pattern="output.*.gz")
    out <- as.character(paste("output.", min_step, ".gz", sep = ""))
    #vals <- unlist(lapply(temp, FUN=function(x){strsplit(x[1], ".g")[[1]][1]}))
    #vals2 <- unlist(lapply(vals, FUN=function(x){strsplit(x[1], "output.")[[1]][2]}))
    #vkeys_lastfile <- tail(mixedorder(vals2),1)
    

    myfiles = as.data.frame(read.table(out, header= FALSE, sep=' ', strip.white = TRUE, skip = 9, skipNul = TRUE, stringsAsFactors=FALSE,
                                         col.names=c("id", "type", 
                                                     "x", "y", "z",
                                                     "vx", "vy", "vz",
                                                     "omegax", "omegay", "omegaz", "NA")))
    
    myfiles <- select(myfiles,-NA.)
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
    
    
    #min_c1 <- min(filter(myfiles, clust == 1)[[5]]) 
    #max_c2 <- max(filter(myfiles, clust == 2)[[5]])
    #dis <- abs(min_c1 - max_c2)
    #if(dis >= R_c){  
    #  v <- print(temp[vkeys_lastfile])
    #}else{
    #  v <- print(temp[vkeys_lastfile])
    #  break
    #}
  

    myfiles <- arrange(myfiles, id)
    pred_clust <- myfiles[c(1,12)]
    #names(pred_clust)[[14]] <- paste("clust")
    #pred_clust <- select(pred_clust, c(id,clust))
    
    archivos <- list.files(path=".", full.names = TRUE, pattern = "output.*.gz")
    
    #SUPPORT VECTOR MACHINE PARA TODOS LOS OUTPUTS
    for (i in 1:length(archivos)) {
      
      step <- as.double(unlist(strsplit(archivos[i], "[.]"))[3])
      #para probar algunos y no todos
      if (step < min_step) {
        print(archivos[i])
        testing <- as.data.frame(
          read.table(archivos[i], header = FALSE, sep = ' ', strip.white = TRUE, skip = 9, skipNul = TRUE, stringsAsFactors=FALSE,
                     col.names = c( "id", "type", "x", "y", "z", 
                                    "vx", "vy", "vz", 
                                    "omegax", "omegay", "omegaz", "NA")))
        v1 <- as.data.frame(rep(v,nrow(testing)))
        f1 <- as.data.frame(rep(l,nrow(testing)))
        names(v1) <- paste("velocity")
        names(f1) <- paste("fill.factor")
        ddd <- bind_cols(v1,f1)
        testing <-bind_cols(ddd,testing)
        testing <- select(testing,-NA.)
        testing <- left_join(testing, pred_clust, by = "id")
        colnames(testing)[14] <- "clust_test"
        testing[[14]] <- as.numeric(testing[[14]])
        
        tic("predict SVM")
        #SVM
        predictions <- predict(model, testing[10])
        predictions <- as.factor(predictions)
        c <- confusionMatrix(predictions, as.factor(testing$clust_test ==1))
        accuracy_svm <- as.numeric(c$overall[1][[1]])
        kappa_svm <- as.numeric(c$overall[2][[1]])
        recall_svm <- as.numeric(c$byClass[6][[1]])
        precision_svm <- as.numeric(c$byClass[5][[1]])
        sensitivity_svm <- as.numeric(c$byClass[1][[1]])
        specificity_svm <- as.numeric(c$byClass[2][[1]])
        toc(log = TRUE, quiet = TRUE)
        
        tic("save SVM")
        new_data_svm <- data.frame(size = size,  fill=fill, velocity = velocity, step = step, 
                                   accuracy = accuracy_svm, specificity = specificity_svm, sensitivity = sensitivity_svm,
                                   kappa=kappa_svm, precision=precision_svm, recall=recall_svm)
        output_predict <- rbind(output_predict, new_data_svm) 
        toc(log = TRUE, quiet = TRUE)
        
      }
    }
    toc(log = TRUE, quiet = TRUE)
    
    time_list <- unlist(tic.log())
    
    save_svm <- get_time("save SVM",time_list)
    predict_svm <- get_time("predict SVM",time_list)
    # save_rf <- get_time("save RF",time_list)
    #  predict_rf <- get_time("predict RF",time_list)
    train_svm <- get_time("train SVM",time_list)
    #  train_rf <- get_time("train RF",time_list)
    input <- get_time("input",time_list)
    predict <- get_time("predict",time_list)
    
    
    time_df <- data.frame(size= size, fill= fill, velocity=velocity, predict_total = predict,
                          time_train = train_svm, time_predict = predict_svm, time_input = input, time_save_total = save_svm)
    output_benchmark <- rbind(output_benchmark, time_df)
    
    
    setwd(root_dir)
  }
}
toc()

#save(output_predict,file="S_001_013.Rda")
#save(output_benchmark,file="S_bench_001_013.Rda")

#setwd("/home/daniela/")
#save(super_last_output, file =  "slo.Rda")

#time_list <- toc(log = TRUE, quiet = TRUE)

# save_svm <- get_time("save SVM",time_list)
# predict_svm <- get_time("predict SVM",time_list)
# save_rf <- get_time("save RF",time_list)
# predict_rf <- get_time("predict RF",time_list)
# train_svm <- get_time("train SVM",time_list)
# train_rf <- get_time("train RF",time_list)

#save_svm <- mean(as.numeric(unlist(lapply(time_list, FUN=function(x){ v <- strsplit(x[1],":"); if (v[[1]][1] == "save SVM") strsplit(v[[1]][2]," ")[[1]][2]; }))))
#predict_svm <- mean(as.numeric(unlist(lapply(time_list, FUN=function(x){ v <- strsplit(x[1],":"); if (v[[1]][1] == "predict SVM") strsplit(v[[1]][2]," ")[[1]][2]; }))))
#save_rf <- mean(as.numeric(unlist(lapply(time_list, FUN=function(x){ v <- strsplit(x[1],":"); if (v[[1]][1] == "save RF") strsplit(v[[1]][2]," ")[[1]][2]; }))))
#predict_rf <- mean(as.numeric(unlist(lapply(time_list, FUN=function(x){ v <- strsplit(x[1],":"); if (v[[1]][1] == "predict RF") strsplit(v[[1]][2]," ")[[1]][2]; }))))
#train_svm <- mean(as.numeric(unlist(lapply(time_list, FUN=function(x){ v <- strsplit(x[1],":"); if (v[[1]][1] == "train SVM") strsplit(v[[1]][2]," ")[[1]][2]; }))))
#train_rf <- mean(as.numeric(unlist(lapply(time_list, FUN=function(x){ v <- strsplit(x[1],":"); if (v[[1]][1] == "train RF") strsplit(v[[1]][2]," ")[[1]][2]; }))))

#PLOTEAR RANDOM FOREST Y SVM EN EL MISMO GRÁFICO.
p1 <- output_predict %>%
  ggplot() + 
  # ggtitle("v=1.0") + 
  geom_point(aes(step, accuracy)) + 
  xlab("Steps") + 
  ylab("accuracy") + 
  theme_bw() + 
  labs(colour="Method") #+ geom_vline(xintercept = 3e+06)
p1 <- p1 + facet_grid(fill~velocity)
p1
#}
