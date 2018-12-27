library(dplyr)
library(ggplot2)
library(readr)
library(purrr)
library(caret)
library(stats)
library(factoextra)

df <- read.csv("~/THESIS/all_log_lamps.csv")

###función para tomar 10 rows anteriores, hacer el promedio y reeplazar###
###en la celda actual####

fun1 <- function(df, col){
  df <- as.data.frame(df)
  df1 <- as.data.frame(df[[as.numeric(col)]])
  v <- vector("double", nrow(df1))
    for (i in 1:(nrow(df1)-10)) {
      if(i <= 10){
        v[i] <- print(NA)
      }else{
        v[i] <- ((abs(mean(df1[(i-10):(i-1),]) - df1[i,]))/df1[i,])*100
      }
    }
  v <- as.data.frame(v)
  names(v) <- paste("chnge_per100")
  v <- print(v)
}
  

plot_end <- function(vel, ff){
  df <- as.data.frame(filter(df, velocity == vel & fill.factor == ff))
  df2 <- fun1(df,8)
  names(df2) <- paste("perc_chge")
  df <- bind_cols(df, df2)
  c <- as.numeric((abs(max(df[["TotEng"]])) + abs(min(df[["TotEng"]])))/2)
  df1 <- filter(df, TotEng <= c)
  ###ojo, no siempre los dumps van a ser cada 10000 steps####
  df1 <- filter(df1, abs(perc_chge) <= 0.0001) 
  df3 <- as.data.frame(filter(df1, Step %% 10000 == 0 ))
  if(nrow(df3)==0){
    min_step <- round(as.numeric(median(df1[4])), digits = -4)
  }else{
      min_step <- as.numeric(median(df3[[4]]))
  }
  print(paste("el step de corte sugerido es", min_step))
  p1 <- ggplot(df, aes(Step,TotEng)) +
    geom_line() +
    xlab("Step") +
    ylab("TotEng")
  p1 <- p1 + geom_vline(xintercept = as.numeric(min_step), color = "red", size =1)
  p1 <- p1 + ggtitle(as.character(paste("velocity ", vel, " & fill factor ", ff)))
  p1 
}

setwd("/home/daniela/simulaciones_daniela/")

###OJO: tamaño small###

class_kmeans <- function(vel, ff, min_step){
  
  fill.factor <- as.numeric(ff)
  velocity  <- as.numeric(vel)

  directory <- paste(getwd(),"/granular_small_",fill.factor,"_",velocity,sep ="")
  print("DIRECTORY:")
  print(directory)
  setwd(directory)

  lo <- as.character(paste("output.",min_step,".gz", sep = ""))
  last_output <- as.data.frame(read.table(lo, header= FALSE, sep=' ', strip.white = TRUE, skip = 9, skipNul = TRUE,
                                        col.names=c("id", "type", 
                                                    "x", "y", "z",
                                                    "vx", "vy", "vz",
                                                    "omegax", "omegay", "omegaz", "NA")))
  last_output <- select(last_output,-NA.)
  #ggplot(last_output, aes(id, vz)) + geom_point()
  ###############kmeans##############

  ##no descomentar a menos que no se sepa el nro de clusters que se quieran########
  ###(o que se tenga una compu que resista sin trabarse)###########################
  #fviz_nbclust(last_output, kmeans, method = "wss") +
  # geom_vline(xintercept = 2, linetype = 2)
  set.seed(123)

  km.res <- kmeans(select(last_output, vz), 2, nstart = 25)
  clust <- as.data.frame(km.res$cluster) 
  names(clust) <- paste("clust")
  last_output <- bind_cols(last_output, clust)
  ggplot(last_output, aes(id, vz, color = clust)) + geom_point()
}

