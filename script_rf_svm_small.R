############################LIBRERÍAS#####################################################
library(dplyr)
library(ggplot2)
library(gtools)
library(purrr)
library(caret)
library(tictoc)
library(stats)
library(factoextra)
#library(readr)
#library(e1071)
#library(corrplot)


df <- read.csv("~/THESIS/all_log_lamps.csv")

get_time <- function(type,time_list){
    t <- mean(as.numeric(unlist(lapply(time_list, 
                           FUN=function(x){ 
                               v <- strsplit(x[1],":"); 
                               if (v[[1]][1] == type) strsplit(v[[1]][2]," ")[[1]][2]; 
                               }))))
    return(t)
}

####DIRECTORIO DE LOS ÚLTIMOS 10 STEPS######################################################
#setwd("/home/daniela/THESIS/GRANULAR/simulaciones_granular_Daniela_SMALL/granular_small_0.15_-1")
#setwd("/root/01-DOCTORADO/00-CODE/granular_daniela/simulaciones_granular_Daniela_small/granular_small_0.15_-1")
#setwd("/root/01-DOCTORADO/00-CODE/granular_daniela/simulaciones_granular_Daniela_small")
#setwd("/mnt/hd/emillan/granular_daniela/TODAS")
setwd("/home/daniela/simulaciones_daniela/")

###Matriz con con vel, ff, y step de corte#######
M <- matrix(NA, 21,2)
for (i in 1:length(dirs)) {
  M[i,1] <- as.numeric(unlist(strsplit(dirs[i],"[_]"))[3])
  M[i,2]  <- as.numeric(unlist(strsplit(dirs[i],"[_]"))[4])
  print(M)
}
M <- as.data.frame(M)
names(M) <- paste(c("ff", "vel"))
M <- filter(M, vel != -2.5 & vel != -5)
guimme_end <- function(vel, ff){
  df <- as.data.frame(filter(df, velocity == vel & fill.factor == ff))
  c <- as.numeric((abs(max(df[["TotEng"]])) + abs(min(df[["TotEng"]])))/2)
  df1 <- filter(df, TotEng <= c)
  ###ojo, no siempre los dumps van a ser cada 10000 steps####
  df1 <- as.data.frame(filter(df1, Step %% 10000 == 0 ))
  x <- as.vector(df1[["TotEng"]])
  y <- as.vector(lag(df1[["TotEng"]]))
  z <- as.data.frame(x-y)
  names(z) <- paste("stap")
  df1 <- bind_cols(df1, z)
  z1 <- filter(df1, abs(z) == 0) 
  min_step <- as.numeric(min(z1[4]))
  print(min_step)
}
v <- vector("double", nrow(M))
for (i in 1:nrow(M)) {
  df <- read.csv("~/THESIS/all_log_lamps.csv")
  v[i]<-guimme_end(vel = as.numeric(M[i,2]), ff=as.numeric(M[i,1]))
  print(v)
}
v <- as.data.frame(v)
names(v) <- paste("corte")
M <- bind_cols(M, v)


root_dir <- getwd()
num_files_train <- 1
num_dumps_to_read <- 100

toteng_cut <- 0

dirs <- list.files(path="./", full.names = TRUE, pattern = "granular_*")


output_predict   <- data.frame( model= character(),
                           size= character(),
                           fill= numeric(),
                           velocity=numeric(),
                           step=numeric(),
                           accuracy=numeric(),
                           specificity=numeric(),
                           sensitivity=numeric(), stringsAsFactors=FALSE)

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
    vel  <- as.numeric(unlist(strsplit(dirs[f],"[_]"))[4])
  
    if (size == "small" & vel >= -1.0){
    
    #cd to simulation directory
    directory <- paste(getwd(),"/granular_",size,"_",fill,"_",vel,sep ="")
    setwd(directory)
    
    total_grains <- as.numeric(unlist(strsplit(grep("Loop time", readLines("log.lammps"),value = TRUE), " "))[12])
    print(paste("Total_grains:",total_grains))
    
    total_steps <- as.numeric(unlist(strsplit(grep("Loop time", readLines("log.lammps"),value = TRUE), " "))[9])
    print(paste("Total Steps:",total_steps))
    
    predict_between_n_steps <- total_steps / num_dumps_to_read
    print(paste("Cantidad de pasos entre dumps: ", predict_between_n_steps))
    
    #############criterio de corte###################
    min_step <- filter(M, vel == vel, ff == fill)[[3]]

    ######### CARGAR LAS 10 TABLAS EN UNA LISTA COMO DATAFRAMES ###############################
    ### Cargo el nombre de todos los archivos output en el directorio     #####################
    ### luego ordeno por numero de dump y guardo el indice en vkeys de los ultimos 10 dumps ###
    ### finalmente solo abro en myfiles los ultimos 10 archivos ###############################
    ###########################################################################################
    temp = list.files(pattern="output.*.gz")
    vals <- unlist(lapply(temp, FUN=function(x){strsplit(x[1], ".g")[[1]][1]}))
    vals2 <- unlist(lapply(vals, FUN=function(x){strsplit(x[1], "output.")[[1]][2]}))
    holi <- as.data.frame(as.numeric(vals2))
    names(holi) <- paste("output")
    holi <- filter(holi, output <= min_step)
    vals2 <- as.character(holi[[1]])
    vkeys <- tail(mixedorder(vals2),num_files_train)
    vkeys_lastfile <- tail(mixedorder(vals2),1)
    
    
    myfiles = lapply(temp[vkeys], 
                     function(x){
                         as.data.frame(read.table(x, header= FALSE, sep=' ', strip.white = TRUE, skip = 9, skipNul = TRUE, stringsAsFactors=FALSE,
                                                  col.names=c("id", "type", 
                                                              "x", "y", "z",
                                                              "vx", "vy", "vz",
                                                              "omegax", "omegay", "omegaz", "NA")))
                     })
    
    ###############BORRAR NA'S############################################
    for (i in 1:num_files_train) {
        myfiles[[i]] <- myfiles[[i]][-12]
    }
    
    #####AGREGA ÚLTIMA COL DE CLUST PARA EVALUAR DESPUÉS##################
    last_output <- as.data.frame(read.table(temp[vkeys_lastfile], header= FALSE, sep=' ', strip.white = TRUE, skip = 9, skipNul = TRUE, stringsAsFactors=FALSE,
                                            col.names=c("id", "type", 
                                                        "x", "y", "z",
                                                        "vx", "vy", "vz",
                                                        "omegax", "omegay", "omegaz", "NA")))
    last_output <- select(last_output,-NA.)
    
    set.seed(123)
    
    km.res <- kmeans(select(last_output, vz), 2, nstart = 25)
    clust <- as.data.frame(as.factor(km.res$cluster)) 
    names(clust) <- paste("clust")
    last_output <- bind_cols(last_output, clust)
    
    #CANTIDAD QUE QUEDA EN EL FRAGMENTO DE ARRIBA#
    map(last_output[12], function(x){sum(x=="c1")})
    
    for (i in 1:num_files_train) {
        myfiles[[i]] <- bind_cols(myfiles[[i]],clust)
    }
    
    #un solo df con los últimos outputs
    lo_10 <- do.call(rbind.data.frame, myfiles)

    #incluir clust para poder hacer la matriz de confusión después###
    last_output <- arrange(last_output, id)
    pred_clust <- select(last_output, c(id,clust))
    toc(log = TRUE, quiet = TRUE)

    tic("train SVM")
    ###Aplicar SVM########################################
    ctrl <- trainControl(method = "cv", number = 5)
    set.seed(12)
    #entrenar modelo
    print("train SVM")
    fit.rad <- train(clust~vz, data = lo_10, trControl=ctrl, preProcess= c("center", "scale") ,method = "svmRadial", metric ="Accuracy")
    toc(log = TRUE, quiet = TRUE)
    

    tic("train RF")
    ####APLICAR RF##############################################
    #entrenar modelo
    print("train RF")
    fit.rf <- train(clust~vz, data = lo_10, trControl=ctrl, preProcess= c("center", "scale") ,method = "rf")
    toc(log = TRUE, quiet = TRUE)
    
    tic("predict")
    #####SI TODO OK, ENTONCES#########################################
    archivos <- list.files(path=".", full.names = TRUE, pattern = "output.*.gz")
    
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
            testing <- select(testing,-NA.)
            testing <- left_join(testing, pred_clust, by = "id")
            colnames(testing)[12] <- "clust_test"
            
            tic("predict SVM")
            #SVM
            predictions <- predict(fit.rad, testing[-12])
            c <- confusionMatrix(predictions, testing$clust_test)
            accuracy_svm <- as.numeric(c$overall[1][[1]])
            sensitivity_svm <- as.numeric(c$byClass[1][[1]])
            specificity_svm <- as.numeric(c$byClass[2][[1]])
            toc(log = TRUE, quiet = TRUE)

            tic("save SVM")
            new_data_svm <- data.frame(model="svm", size = size,  fill=fill, velocity = vel, step = step, 
                                   accuracy = accuracy_svm, specificity = specificity_svm, sensitivity = sensitivity_svm)
            output_predict <- rbind(output_predict, new_data_svm) 
            toc(log = TRUE, quiet = TRUE)
            
            tic("predict RF")
            #RANDOM FOREST
            predictions <- predict(fit.rf, testing[-12])
            c <- confusionMatrix(predictions, testing$clust_test)
            accuracy_rf <- as.numeric(c$overall[1][[1]])
            sensitivity_rf <- as.numeric(c$byClass[1][[1]])
            specificity_rf <- as.numeric(c$byClass[2][[1]])
            toc(log = TRUE, quiet = TRUE)
            
            tic("save RF")
            new_data_rf <- data.frame(model="rf", size = size,  fill=fill, velocity = vel, step = step, 
                                   accuracy = accuracy_rf, specificity = specificity_rf, sensitivity = sensitivity_rf)
            output_predict <- rbind(output_predict, new_data_rf) 
            toc(log = TRUE, quiet = TRUE)
        }
    }
    toc(log = TRUE, quiet = TRUE)
    
    time_list <- unlist(tic.log())
    
    save_svm <- get_time("save SVM",time_list)
    predict_svm <- get_time("predict SVM",time_list)
    save_rf <- get_time("save RF",time_list)
    predict_rf <- get_time("predict RF",time_list)
    train_svm <- get_time("train SVM",time_list)
    train_rf <- get_time("train RF",time_list)
    input <- get_time("input",time_list)
    predict <- get_time("predict",time_list)
    
    
    time_df <- data.frame(model = "svm", size= size, fill= fill, velocity=vel, predict_total = predict,
                          time_train = train_svm, time_predict = predict_svm, time_input = input, time_save_total = save_svm)
    output_benchmark <- rbind(output_benchmark, time_df)
    
    time_df <- data.frame(model = "rf", size= size, fill= fill, velocity=vel, predict_total = predict,
                          time_train = train_rf, time_predict = predict_rf, time_input = input, time_save_total = save_rf)
    output_benchmark <- rbind(output_benchmark, time_df)
    
    
    setwd(root_dir)
    }
}
toc()

save(output_predict,file="predict_granular_Daniela_2018-10-03_SMALL_100points_2trains.Rda")
save(output_benchmark,file="benchmark_granular_Daniela_2018-10-03_SMALL_100points_2trains.Rda")


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
p1 <-   output_predict %>% #filter(fill==0.15 & velocity == -0.5) %>%
        ggplot() + 
        geom_line(aes(step, accuracy, color=model)) + 
        xlab("Steps") + 
        ylab("Accuracy") + 
        theme_bw() + 
        labs(colour="Method")
p1 <- p1 + facet_grid(velocity~fill)
p1
#}
  

