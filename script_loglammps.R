library(dplyr)
library(ggplot2)
library(readr)
library(purrr)
library(caret)
library(e1071)
library(corrplot)

###Set directory#############################################################
setwd("/home/daniela/simulaciones_daniela/")
dirs <- list.files(path="./", full.names = TRUE, pattern = "granular_*")
root_dir <- getwd()
log_list <- list()

  for (f in 1:length(dirs)){
 #   for (f in 1:1){
    print("DIRECTORY:")
    print(dirs[f])
  
      size <- unlist(strsplit(dirs[f],"[_]"))[2]
      fill.factor <- as.numeric(unlist(strsplit(dirs[f],"[_]"))[3])
      velocity  <- as.numeric(unlist(strsplit(dirs[f],"[_]"))[4])
    
      directory <- paste(getwd(),"/granular_",size,"_",fill.factor,"_",velocity,sep ="")
      setwd(directory)
    
      #limpio mis dataframes
      logl = list.files(pattern="log.lammps")
      logl = read_table2(logl,skip = 52)
      logl = logl[-8]
      logl = logl[!is.na(rowSums(logl)),]
      velocity <- as.data.frame(rep(velocity, nrow(logl)))
      fill.factor <- as.data.frame(rep(fill.factor, nrow(logl)))
      df <- bind_cols(velocity, fill.factor)
      names(df) <- paste(c("velocity", "fill.factor"))
      logl = as.data.frame(logl)
      logl <- bind_cols(df, logl)
      log_list[[f]] <- logl
      
      setwd(root_dir)
  }  


plot_all <- function(name1= "x", name2 = "y", vel, ff){
  if(length(vel) == 1 & length(ff) == 1){
    for (i in 1:length(log_list)) {
      if(log_list[[i]][[1]][1] == vel & log_list[[i]][[2]][1] == ff){
        x1 <- log_list[[i]][[name1]]
        x2 <- log_list[[i]][[name2]]
        p <- ggplot(log_list[[i]], aes(x1,x2)) + 
          geom_line() +
          xlab(as.character(name1)) +
          ylab(as.character(name2))
        p <- p + ggtitle(as.character(paste("velocity ", vel, " & fill factor ", ff)))
        return(p)  
      }}}else{
      df <- as.data.frame(do.call(rbind, log_list))
      df <- as.data.frame(filter(df, velocity == vel))
      x1 <- as.vector(df[[name1]])
      x2 <- as.vector(df[[name2]])
      p1 <- ggplot(df, aes(x1,x2)) + geom_line() +
        xlab(as.character(name1)) +
        ylab(as.character(name2))
      p1 <- p1 + facet_grid(velocity ~ fill.factor)
      return(p1)
        }
  }


####para Luis#######
  df <- read.csv("~/THESIS/all_log_lamps.csv")
  plot_all <- function(name1= "x", name2 = "y", vel, ff){
          df <- as.data.frame(filter(df, velocity == vel & fill.factor == ff))
          x1 <- as.vector(df[[name1]])
          x2 <- as.vector(df[[name2]])
          p1 <- ggplot(df, aes(x1,x2)) +
            geom_line() +
            xlab(as.character(name1)) +
            ylab(as.character(name2))
          if(length(vel) == 1 && length(ff) == 1){
            p1 <- p1 + ggtitle(as.character(paste("velocity ", vel, " & fill factor ", ff)))
            return(p1)  
          }else{
          p1 <- p1 + facet_grid(velocity ~ fill.factor)
          return(p1)
        }
  }



