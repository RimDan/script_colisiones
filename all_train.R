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
rm(lo_10)
super_last_output$vz <- -(super_last_output$vz/velocity)

###pruebas por phi###
lo_10 <- filter(super_last_output, fill.factor == 0.15)
#lo_10norm <- filter(super_last_output, fill.factor == 0.15)
#lo_10 <- filter(super_last_output, fill.factor == 0.25)
#lo_10 <- filter(super_last_output, fill.factor == 0.35)

###pruebas por velocidad #####
#lo_10 <- filter(super_last_output, velocity == -0.1)
#lo_10 <- filter(super_last_output, velocity == -0.25)
#lo_10 <- filter(super_last_output, velocity == -0.5)
#lo_10 <- filter(super_last_output, velocity == -0.75)
#lo_10 <- filter(super_last_output, velocity == -1)

###pruebas combinadas #####
#lo_10 <- filter(super_last_output, (velocity == -0.1|fill.factor == 0.35)) #L
#lo_10 <- filter(super_last_output, velocity == -0.1 & fill.factor == 0.35)

##all
#lo_10 <- sample_n(super_last_output, size = 100000)

##SVM#######################
ctrl <- trainControl(method = "cv", number = 5)
#lo_10 <- sample_n(lo_10, size = 60000)
set.seed(12)
#entrenar modelo
print("train SVM")
tic("train SVM")
fit.rad <- train(clust~vz, data = lo_10, trControl=ctrl, preProcess= c("center", "scale") ,method = "svmLinear", metric ="Accuracy")
#fit.rad <- train(clust~vz, data = lo_10, trControl=ctrl, preProcess= c("center", "scale") ,method = "svmRadial", metric ="Accuracy")
toc(log = TRUE, quiet = TRUE)
####RF######################
set.seed(2134)
tic("train RF")
#lo_10 <- sample_n(lo_10, size = 60000)
fit.rf <- train(clust~vz, data = lo_10, trControl=ctrl, preProcess= c("center", "scale") ,method = "rf", metric = "Accuracy", tuneLength = 10)
toc(log = TRUE, quiet = TRUE)
########svm1class#############

#lo_10 <- super_last_output

lo_10[[14]] <- as.numeric(lo_10[[14]])
df <- filter(lo_10,  clust == 1)  #choose only one of the classes

x <- subset(df, select = vz) #make x variables
y <- as.factor(df$clust) #make y variable(dependent)
tic("train SVM1class")
tuned <- tune.svm(x, y, data = df, 
                #  nu =  0.001:0.5,
                nu =  0.001:0.05,
                  gamma = 10^(-2:0), 
                  type='one-classification')
y <- as.numeric(df$clust)
fit.svm <- svm(x, y,type='one-classification', 
             kernel="radial", data = df,  nu = tuned$best.parameters$nu, gamma = tuned$best.parameters$gamma) 

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
                                TP=numeric(),
                                TN=numeric(),
                                FP=numeric(),
                                FN=numeric(),
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
                               time_train_svm1class= numeric(),
                               time_predict_svm = numeric(),
                               time_predict_rf = numeric(),
                               time_predict_svm1class = numeric(),
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
  
  if (size == "small" & velocity >= -1.0 ){
    
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
        
        arch <- strsplit(archivos[i], split = "[.]")[[1]][3]
        v1 <- as.data.frame(rep(v,nrow(testing)))
        f1 <- as.data.frame(rep(l,nrow(testing)))
        names(v1) <- paste("velocity")
        names(f1) <- paste("fill.factor")
        ddd <- bind_cols(v1,f1)
        testsvm <-bind_cols(ddd,testing)
        testsvm<- select(testsvm,-NA.)
        testing <- select(testing,-NA.)
        testsvm <- left_join(testsvm, pred_clust, by = "id")
        testsvm$vz <- -(testsvm$vz/velocity)
        colnames(testsvm)[14] <- "clust_test"
        testing <- left_join(testing, pred_clust, by = "id")
        colnames(testing)[12] <- "clust_test"
        testing$vz <- -(testing$vz/velocity)

        tic("predict SVM")
        #SVM
        predictions <- predict(fit.rad, testing[-12])
        c <- confusionMatrix(predictions, testing$clust_test)
        TP_s <- as.numeric(c$table[1])/total_grains
        TN_s <- as.numeric(c$table[4])/total_grains
        FP_s <- as.numeric(c$table[3])/total_grains
        FN_s <- as.numeric(c$table[2])/total_grains
        accuracy_svm <- as.numeric(c$overall[1][[1]])
        kappa_svm <- as.numeric(c$overall[2][[1]])
        recall_svm <- as.numeric(c$byClass[6][[1]])
        precision_svm <- as.numeric(c$byClass[5][[1]])
        sensitivity_svm <- as.numeric(c$byClass[1][[1]])
        specificity_svm <- as.numeric(c$byClass[2][[1]])
        toc(log = TRUE, quiet = TRUE)
        tic("save SVM")
        new_data_svm <- data.frame(model="SVM", size = size,  fill=fill, velocity = velocity, step = step, 
                                   accuracy = accuracy_svm, specificity = specificity_svm, sensitivity = sensitivity_svm,
                                   kappa=kappa_svm, TP=TP_s, TN=TN_s, FP=FP_s, FN=FN_s, precision=precision_svm, recall=recall_svm)
        output_predict <- rbind(output_predict, new_data_svm) 
        toc(log = TRUE, quiet = TRUE)
        
        tic("predict RF")
        #RANDOM FOREST
        predictions <- predict(fit.rf, testing[-12])
        c <- confusionMatrix(predictions, testing$clust_test)
        TP_r <- as.numeric(c$table[1])/total_grains
        TN_r <- as.numeric(c$table[4])/total_grains
        FP_r <- as.numeric(c$table[3])/total_grains
        FN_r <- as.numeric(c$table[2])/total_grains
        accuracy_rf <- as.numeric(c$overall[1][[1]])
        kappa_rf <- as.numeric(c$overall[2][[1]])
        recall_rf <- as.numeric(c$byClass[6][[1]])
        precision_rf <- as.numeric(c$byClass[5][[1]])
        sensitivity_rf <- as.numeric(c$byClass[1][[1]])
        specificity_rf <- as.numeric(c$byClass[2][[1]])
        toc(log = TRUE, quiet = TRUE)

        tic("save RF")
        new_data_rf <- data.frame(model="RF", size = size,  fill=fill, velocity = velocity, step = step, 
                                  accuracy = accuracy_rf, specificity = specificity_rf, sensitivity = sensitivity_rf,
                                  kappa=kappa_rf, TP=TP_r, TN=TN_r, FP=FP_r, FN=FN_r, precision=precision_rf, recall=recall_rf)
        output_predict <- rbind(output_predict, new_data_rf) 
        toc(log = TRUE, quiet = TRUE)
        
        tic("predict SVM1class")
        #SVM
        #predictions <- predict(fit.svm, select(testsvm, z, vz))        
        predictions <- predict(fit.svm, testsvm[10])
        predictions <- as.factor(predictions)
        topo <- as.data.frame(select(testsvm, id,x, y, z, vz,  clust_test))
        levels(topo$clust_test) <- c("TRUE", "FALSE")
        topo2 <- as.data.frame(predictions)
        names(topo2) <- paste("pred")
        top <-  cbind(topo, topo2)
        p<- ggplot(top, aes(x, z, color = pred)) + geom_point()
        pname <- as.character(paste(l, v, arch, sep = "_" ))
       ggsave(paste(pname, ".png", sep = ""), plot = p, path = "/home/daniela/simulaciones_daniela/plots_norm/")
        c <- confusionMatrix(predictions, as.factor(testsvm$clust_test ==1), positive = "TRUE")
        TP_1 <- as.numeric(c$table[4])/total_grains
        TN_1 <- as.numeric(c$table[1])/total_grains
        FP_1 <- as.numeric(c$table[2])/total_grains
        FN_1 <- as.numeric(c$table[3])/total_grains
        accuracy_svm1 <- as.numeric(c$overall[1][[1]])
        kappa_svm1 <- as.numeric(c$overall[2][[1]])
        recall_svm1 <- as.numeric(c$byClass[6][[1]])
        precision_svm1 <- as.numeric(c$byClass[5][[1]])
        sensitivity_svm1 <- as.numeric(c$byClass[1][[1]])
        specificity_svm1 <- as.numeric(c$byClass[2][[1]])
        toc(log = TRUE, quiet = TRUE)
        tic("save SVM1class")
        new_data_svm <- data.frame(model = "OCSVM", size = size,  fill=fill, velocity = velocity, step = step, 
                                   accuracy = accuracy_svm1, specificity = specificity_svm1, sensitivity = sensitivity_svm1,
                                   kappa=kappa_svm1, TP=TP_1, TN=TN_1, FP=FP_1, FN=FN_1, precision=precision_svm1, recall=recall_svm1)
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



#setwd("/home/daniela/simulaciones_daniela/S_all/")
#save(output_predict,file="S_001_norm_001.Rda")
#save(output_benchmark,file="S_bad_bench_001_025.Rda")

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
p1 <-filter(output_predict) %>%
  ggplot() + 
  # ggtitle("v=1.0") + 
  geom_point(aes(step, accuracy, color= model)) + 
  scale_x_log10(breaks=c(0,1e+07), labels=c(0,expression("1x"*10^7)))+
  xlab("Steps") + 
  ylab("accuracy") + 
  theme_bw() + 
  labs(colour="Method") #+ geom_vline(xintercept = 3e+06)
p1 <- p1 + facet_grid(fill~velocity, scales = "free")
p1
#}

#           ggsave(plot, plot = esteplot, path = path,width = 4.78, height =  7.02)
#           ggsave(plot, plot = esteplot, path = path,width = 4.67, height =  5.02)
#p1 <-filter(output_predict, model=="RF" & (velocity==-0.1|velocity==-0.25))

p1 <-filter(output_predict, model=="RF" & velocity==-0.1) %>%
  ggplot() + 
  # ggtitle("v=1.0") + 
  geom_point(aes(step, TP, color = "VP")) + 
  geom_point(aes(step, TN, color = "VN")) +
  geom_point(aes(step, FP, color = "FP")) +
  geom_point(aes(step, FN, color = "FN")) + 
  #xlab("Pasos temporales") + 
  scale_y_continuous(breaks=c(0, 0.25,0.5,0.75,1), limits = c(0,1))+
  xlab(expression("Pasos"*" ["*log[10]*"]"))+
#  geom_vline(xintercept = 7e+06)+
  scale_x_log10(breaks=c(0,1e+07), labels=c(0,expression("1x"*10^7)))+
  ylab("Cantidad de partículas") + 
  ggtitle("Valores de MC para RF")+
  theme_bw() 
p1 <- p1 + facet_grid(fill~velocity, scales = "free")
p1 <- p1+ scale_color_brewer(palette = "Dark2", direction = 1) +  guides(colour = guide_legend(title="Valores MC", override.aes = list(size=3))) +
  theme(legend.position="bottom", text= element_text(size=15))
p1


p1 <-filter(output_predict, model=="OCSVM"& fill == 0.35) %>%
  ggplot() + 
  # ggtitle("v=1.0") + 
  geom_point(aes(step, TP, color = "VP")) + 
  geom_point(aes(step, TN, color = "VN")) +
  geom_point(aes(step, FP, color = "FP")) +
  geom_point(aes(step, FN, color = "FN")) + 
#  geom_vline(xintercept =  1.5e7)+
  xlab(expression("Pasos"*" ["*log[10]*"]")) + 
  scale_y_continuous(breaks=c(0, 0.25,0.5,0.75,1), limits = c(0,1))+
  scale_x_log10(breaks=c(0,1e+07), labels=c(0,expression("1x"*10^7)))+
  ylab("Cantidad de partículas") + 
  ggtitle("Valores de MC para OCSVM")+
  theme_bw() 
p1 <- p1 + facet_grid(fill~velocity, scales = "free")
p1 <- p1+ scale_color_brewer(palette = "Dark2", direction = 1) +  guides(colour = guide_legend(title="Valores MC", override.aes = list(size=3))) +
  theme(legend.position="bottom", text= element_text(size=15))
p1


p1 <- filter(output_predict, velocity==-1 & fill == 0.15) %>%
  ggplot() + 
  ggtitle("Evolución temporal de OCSVM") + 
  xlab(expression("Pasos"*" ["*log[10]*"]")) + 
  scale_x_log10(breaks=c(0,1e+07), labels=c(0,expression("1x"*10^7))) +
  ylab("Desempeño") + 
  theme_bw() + 
  labs(colour="Method") +
  geom_line(aes(step, recall, color = "Sensibilidad"), size = 1.5, linetype = "solid") +
  ylim(c(0,1)) + geom_point(aes(step, accuracy, color = "Exactitud"), shape =21, size =4)+
  geom_point(aes(step, precision, color = "Precisión"), size = 1.5, shape= 4) 
p1 <- p1 + facet_grid(fill~velocity) 
p1 <- p1 + guides(colour = guide_legend(title="Métrica", override.aes = list(shape = c(21, 4, NA) ,linetype = c("blank", "blank", "solid")))) +
  #theme(legend.position="none",text= element_text(size=16))
  # theme(legend.position = "bottom" , text= element_text(size=15))
  theme(legend.position="bottom",text= element_text(size=20))
p1 <- p1 + scale_color_brewer(palette = "Set1")+theme(strip.background =element_rect(fill="#F2F2F2"))
print(p1)


p1 <-ggplot(filter(output_predict, model == "OCSVM",velocity==-1 & fill == 0.35), aes(step, accuracy, color = model)) + 
  ggtitle(expression("Evolución temporal: entrenamiento con "*v[z]* " y z")) + 
  xlab("Pasos temporales") + 
  ylab("Desempeño") + 
  theme_bw() + 
  geom_line(aes(step, recall, color = "Sensibilidad"), size = 1.5, linetype = "solid") +
  ylim(c(0,1)) + geom_point( size = 2)
#  geom_point(aes(step, precision, color = "Precisión"), size = 1.5, shape= 4) 
p1 <- p1 + facet_grid(fill~velocity) 
p1 <- p1 + guides(colour = guide_legend(title="Métrica")) +
  #theme(legend.position="none",text= element_text(size=16))
  # theme(legend.position = "bottom" , text= element_text(size=15))
  theme(legend.position="bottom",text= element_text(size=15))
p1 <- p1 + scale_color_brewer(palette = "Set2")+theme(strip.background =element_rect(fill="#F2F2F2"))
print(p1)



p1 <- filter(output_predict, model == "SVM" & velocity==-1 & fill==0.35) %>%
  ggplot() + 
  ggtitle("Evolución temporal de SVM") + 
  xlab(expression("Pasos"*" ["*log[10]*"]")) + 
  #scale_x_log10(breaks=c(0,1e+07), labels=c(0,expression("1x"*10^7))) +
  ylab("Desempeño") + 
  theme_bw() + 
  labs(colour="Method") +
  geom_point(aes(step, TN, color = "VN")) +
  geom_point(aes(step, FP, color = "FP")) +
  geom_line(aes(step, recall, color = "Sensibilidad"), size = 1.5, linetype = "solid") +
  ylim(c(0,1)) + geom_point(aes(step, accuracy, color = "Exactitud"), shape =21, size =4)+
  geom_point(aes(step, precision, color = "Precisión"), size = 1.5, shape= 4) 
p1 <- p1 + facet_grid(fill~velocity) 
p1 <- p1 + guides(colour = guide_legend(title="Métrica", override.aes = list(shape = c(4,4,21, 4, NA) ,linetype = c("blank", "blank","blank", "blank", "solid")))) +
  #theme(legend.position="none",text= element_text(size=16))
  # theme(legend.position = "bottom" , text= element_text(size=15))
  theme(legend.position="bottom",text= element_text(size=20))
p1 <- p1 + scale_color_brewer(palette = "Set1")+theme(strip.background =element_rect(fill="#F2F2F2"))
print(p1)

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
