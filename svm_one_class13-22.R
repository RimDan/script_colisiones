############################LIBRERÍAS#####################################################
library(dplyr)
library(ggplot2)
library(gtools)
library(purrr)
library(caret)
library(tictoc)

library(readr)
library(e1071)
#library(corrplot)



get_time <- function(type,time_list){
  t <- mean(as.numeric(unlist(lapply(time_list, 
                                     FUN=function(x){ 
                                       v <- strsplit(x[1],":"); 
                                       if (v[[1]][1] == type) strsplit(v[[1]][2]," ")[[1]][2]; 
                                     }))))
}
####training con super lo####

lo_10 <- filter(super_last_output, velocity == c(-1,-0.1) )

##SVM#######################

ctrl <- trainControl(method = "cv", number = 5)
set.seed(12)
#entrenar modelo
print("train SVM")
tic("train SVM")
fit.rad <- train(clust~vz, data = lo_10, trControl=ctrl, preProcess= c("center", "scale") ,method = "svmRadial", metric ="Accuracy")
toc(log = TRUE, quiet = TRUE)
####RF######################
set.seed(2134)
tic("train RF")
fit.rf <- train(clust~vz, data = lo_10, trControl=ctrl, preProcess= c("center", "scale") ,method = "rf", metric = "Accuracy", tuneLength = 10)
toc(log = TRUE, quiet = TRUE)
########svm1class#############
lo_10[[14]] <- as.numeric(lo_10[[14]])
df <- filter(lo_10,  clust == 1)  #choose only one of the classes

x <- subset(df, select = vz) #make x variables
y <- df$clust #make y variable(dependent)
tic("train SVM1class")
fit.svm <- svm(x, y,type='one-classification', nu=0.05, 
             kernel="radial", data = df) #train an one-classification model 
toc(log = TRUE, quiet = TRUE)

####DIRECTORIO DE TODOS LOS OUTPUTS######################################################
setwd("/home/daniela/simulaciones_daniela/")

root_dir <- getwd()
num_dumps_to_read <- 100

dirs <- list.files(path="./", full.names = TRUE, pattern = "granular_*")


output_predict   <- data.frame( model= character(),
                                size= character(),
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

output_benchmark <- data.frame(model = character(),
                               size= character(),
                               fill= numeric(),
                               velocity=numeric(),
                               predict_total = numeric(),
                               time_train_svm = numeric(),
                               time_train_rf = numeric(),
                               time_predict_svm = numeric(),
                               time_predict_rf = numeric(),
                               time_input = numeric(),
                               time_save_total = numeric())


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
  
  if (size == "small" & velocity >= -1.0){
    
    #cd to simulation directory
    directory <- paste(getwd(),"/granular_",size,"_",fill,"_",velocity,sep ="")
    setwd(directory)
    l <- fill
    v <- velocity
    min_step <- filter(cut_analyst_time, fill == l & velocity == v)$last_step
    total_grains <- as.numeric(unlist(strsplit(grep("Loop time", readLines("log.lammps"),value = TRUE), " "))[12])
    print(paste("Total_grains:",total_grains))
    
    total_steps <- as.numeric(unlist(strsplit(grep("Loop time", readLines("log.lammps"),value = TRUE), " "))[9])
    print(paste("Total Steps:",total_steps))
    
    predict_between_n_steps <- total_steps / num_dumps_to_read
    print(paste("Cantidad de pasos entre dumps: ", predict_between_n_steps))
    
    archivos <- list.files(path=".", full.names = TRUE, pattern = "output.*.gz")
    
    pred_clust <- filter(super_last_output, fill.factor == l & velocity == v)
    pred_clust <- arrange(pred_clust, id)
    names(pred_clust)[[14]] <- paste("clust")
    pred_clust <- select(pred_clust, c(id,clust))
    #SUPPORT VECTOR MACHINE PARA TODOS LOS OUTPUTS
    for (i in 1:length(archivos)) {
      
      step <- as.double(unlist(strsplit(archivos[i], "[.]"))[3])
      #para probar algunos y no todos
      if (step %% predict_between_n_steps == 0 & step < min_step) {
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
        testsvm <-bind_cols(ddd,testing)
        testsvm<- select(testsvm,-NA.)
        testing <- select(testing,-NA.)
        testsvm <- left_join(testsvm, pred_clust, by = "id")
        colnames(testsvm)[14] <- "clust_test"
        testing <- left_join(testing, pred_clust, by = "id")
        colnames(testing)[12] <- "clust_test"
        
        tic("predict SVM")
        #SVM
        predictions <- predict(fit.rad, testing[-12])
        c <- confusionMatrix(predictions, testing$clust_test)
        accuracy_svm <- as.numeric(c$overall[1][[1]])
        kappa_svm <- as.numeric(c$overall[2][[1]])
        recall_svm <- as.numeric(c$byClass[6][[1]])
        precision_svm <- as.numeric(c$byClass[5][[1]])
        sensitivity_svm <- as.numeric(c$byClass[1][[1]])
        specificity_svm <- as.numeric(c$byClass[2][[1]])
        toc(log = TRUE, quiet = TRUE)
        
        tic("save SVM")
        new_data_svm <- data.frame(model="svm", size = size,  fill=fill, velocity = velocity, step = step, 
                                   accuracy = accuracy_svm, specificity = specificity_svm, sensitivity = sensitivity_svm,
                                   kappa=kappa_svm, precision=precision_svm, recall=recall_svm)
        output_predict <- rbind(output_predict, new_data_svm) 
        toc(log = TRUE, quiet = TRUE)
        
        tic("predict RF")
        #RANDOM FOREST
        predictions <- predict(fit.rf, testing[-12])
        c <- confusionMatrix(predictions, testing$clust_test)
        accuracy_rf <- as.numeric(c$overall[1][[1]])
        kappa_rf <- as.numeric(c$overall[2][[1]])
        recall_rf <- as.numeric(c$byClass[6][[1]])
        precision_rf <- as.numeric(c$byClass[5][[1]])
        sensitivity_rf <- as.numeric(c$byClass[1][[1]])
        specificity_rf <- as.numeric(c$byClass[2][[1]])
        toc(log = TRUE, quiet = TRUE)
        
        tic("save RF")
        new_data_rf <- data.frame(model="rf", size = size,  fill=fill, velocity = velocity, step = step, 
                                  accuracy = accuracy_rf, specificity = specificity_rf, sensitivity = sensitivity_rf,
                                  kappa=kappa_rf, precision=precision_rf, recall=recall_rf)
        output_predict <- rbind(output_predict, new_data_rf) 
        toc(log = TRUE, quiet = TRUE)
        

        
        tic("predict SVM1class")
        #SVM
        predictions <- predict(fit.svm, testsvm[10])
        predictions <- as.factor(predictions)
        c <- confusionMatrix(predictions, as.factor(testsvm$clust_test ==1))
        accuracy_svm1 <- as.numeric(c$overall[1][[1]])
        kappa_svm1 <- as.numeric(c$overall[2][[1]])
        recall_svm1 <- as.numeric(c$byClass[6][[1]])
        precision_svm1 <- as.numeric(c$byClass[5][[1]])
        sensitivity_svm1 <- as.numeric(c$byClass[1][[1]])
        specificity_svm1 <- as.numeric(c$byClass[2][[1]])
        toc(log = TRUE, quiet = TRUE)
        
        tic("save SVM1class")
        new_data_svm <- data.frame(model = "svm1class", size = size,  fill=fill, velocity = velocity, step = step, 
                                   accuracy = accuracy_svm1, specificity = specificity_svm1, sensitivity = sensitivity_svm1,
                                   kappa=kappa_svm1, precision=precision_svm1, recall=recall_svm1)
        output_predict <- rbind(output_predict, new_data_svm) 
        toc(log = TRUE, quiet = TRUE)
      }
    }
    toc(log = TRUE, quiet = TRUE)
    
    time_list <- unlist(tic.log())
    
    save_svm <- get_time("save SVM",time_list)
    predict_svm <- get_time("predict SVM",time_list)
    save_rf <- get_time("save RF",time_list)
    predict_rf <- get_time("predict RF",time_list)
    save_svm1 <- get_time("save SVM1class",time_list)
    predict_svm1 <- get_time("predict SVM1class",time_list)
    train_svm <- get_time("train SVM",time_list)
    train_rf <- get_time("train RF",time_list)
    train_svm1 <- get_time("train SVM1class",time_list)
    input <- get_time("input",time_list)
    predict <- get_time("predict",time_list)
    
    
    time_df <- data.frame(model = "svm", size= size, fill= fill, velocity=velocity, predict_total = predict,
                          time_train = train_svm, time_predict = predict_svm, time_input = input, time_save_total = save_svm)
    output_benchmark <- rbind(output_benchmark, time_df)
    
    time_df <- data.frame(model = "rf", size= size, fill= fill, velocity=velocity, predict_total = predict,
                          time_train = train_rf, time_predict = predict_rf, time_input = input, time_save_total = save_rf)
    output_benchmark <- rbind(output_benchmark, time_df)
    
    time_df <- data.frame(model = "svm1class", size= size, fill= fill, velocity=velocity, predict_total = predict,
                          time_train = train_svm1, time_predict = predict_svm1, time_input = input, time_save_total = save_svm1)
    output_benchmark <- rbind(output_benchmark, time_df)
    setwd(root_dir)
  }
}
toc()



setwd("/home/daniela/simulaciones_daniela/S_001_vz_all3/")
save(output_predict,file="S_001_023.Rda")
save(output_benchmark,file="S_bench_001_023.Rda")

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
  geom_point(aes(step, kappa, color= model)) + 
  xlab("Steps") + 
  ylab("kappa") + 
  theme_bw() + 
  labs(colour="Method") #+ geom_vline(xintercept = 3e+06)
p1 <- p1 + facet_grid(fill~velocity)
p1
#}

p1 <- ggplot() + 
  geom_point(data=filter(output_predict, model == "svm" & fill == 0.15), aes(velocity, kappa, color = "0.15")) + 
  geom_point(data=filter(output_predict, model == "svm" & fill == 0.25), aes(velocity, kappa, color = "0.25")) + 
  geom_point(data=filter(output_predict, model == "svm" & fill == 0.35), aes(velocity, kappa, color = "0.35")) + 
  xlab("Steps") + 
  ylab("kappa") + 
  theme_bw() + 
  labs(colour="Method") #+ geom_vline(xintercept = 3e+06)
p1 <- p1 + facet_grid(~fill)
p1



p1 <- ggplot() + 
  geom_point(data=filter(output_predict, fill == 0.15), aes(velocity, kappa, color = step)) + 
  geom_point(data=filter(output_predict, fill == 0.25), aes(velocity, kappa, color = step)) + 
  geom_point(data=filter(output_predict, fill == 0.35), aes(velocity, kappa, color = step)) + 
  xlab("velocity") + 
  ylab("kappa") + 
  theme_bw() + 
  xlim(-1,0.05)+
  labs(colour="Fill factor") #+ geom_vline(xintercept = 3e+06)
p1 <- p1 + facet_grid(model~fill)
p1
