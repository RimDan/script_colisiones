perc_gain_012  <- data.frame(   perc= numeric(),
                                model= character(),
                                fill= numeric(),
                                velocity=numeric(),
                                stepc=numeric(),
                                time_cut=numeric(),
                                perc_gain=numeric(),
                                stringsAsFactors=FALSE)


percent <- as.vector(c(0.90,0.95,0.99)) 


  df1 <- output_predict
  for (i in 1:length(unique(output_predict[[1]]))) {
      model <- as.character(unique(output_predict[[1]])[i])
      print(model)
        for (i in 1:length(unique(output_predict[[3]]))){
          fil <- as.numeric(unique(output_predict[[3]])[i])
            for (i in 1:length(unique(output_predict[[4]]))){
              vl <- as.numeric(unique(output_predict[[4]])[i])
              for (f in 1:length(percent)) {
              perc <- percent[f]
              m <-  cut_time(df = df1, vel = vl, ff = fil, mdl = model, perc)
              step_cut <- m[1]
              time_cut <- m[2]
              perc_gain <- m[3]
              new_data <- data.frame(perc = perc, model = model, fill= fil, velocity=vl, stepc = step_cut,
                                    time_cut = time_cut, perc_gain = perc_gain)
              perc_gain_012 <- rbind(perc_gain_012, new_data)
              
              }
        }
  }
}

p2 <- ggplot() + geom_line(data= filter(perc_gain_012, model == "svm", perc ==0.99), aes(velocity, perc_gain, color = "0.99")) +
  geom_point(data= filter(perc_gain_012, model == "svm", perc ==0.99), aes(velocity, perc_gain, color = "0.99")) +
  geom_line(data= filter(perc_gain_012, model == "svm", perc ==0.95), aes(velocity, perc_gain, color = "0.95")) +
  geom_point(data= filter(perc_gain_012, model == "svm", perc ==0.95), aes(velocity, perc_gain, color = "0.95")) +
  geom_line(data= filter(perc_gain_012, model == "svm", perc ==0.90), aes(velocity, perc_gain, color = "0.90")) +
  geom_point(data= filter(perc_gain_012, model == "svm", perc ==0.90), aes(velocity, perc_gain, color = "0.90")) +
  xlim(-1,0) + ylim(0,100) + xlab("Velocidad inicial") + ylab("Porcentaje de ganacia") + ggtitle("SVM: Prueba 012")
p2 <- p2 + facet_grid(fill~.) + theme_bw()
p2 <- p2 +  guides(colour = guide_legend(title="kappa", override.aes = list(size=1)))
p2

p2 <- ggplot() + geom_line(data= filter(perc_gain_012, model == "svm" & fill == 0.15), aes(velocity, perc_gain, color = "0.15")) +
  geom_point(data= filter(perc_gain_012, model == "svm" & fill == 0.15), aes(velocity, perc_gain, color = "0.15")) +
  geom_line(data= filter(perc_gain_012, model == "svm" & fill == 0.25), aes(velocity, perc_gain, color = "0.25")) +
  geom_point(data= filter(perc_gain_012, model == "svm" & fill == 0.25), aes(velocity, perc_gain, color = "0.25")) +
  geom_line(data= filter(perc_gain_012, model == "svm" & fill == 0.35), aes(velocity, perc_gain, color = "0.35")) +
  geom_point(data= filter(perc_gain_012, model == "svm" & fill == 0.35), aes(velocity, perc_gain, color = "0.35")) +
  xlim(-1,0) + ylim(0,100) + xlab("Velocidad inicial") + ylab("Porcentaje de ganacia") + ggtitle("SVM: Prueba 012")
p2 <- p2 + facet_grid(perc~.) + theme_bw()
p2 <- p2 +  guides(colour = guide_legend(title="Factor de llenado", override.aes = list(size=1)))
p2
  

cut_time <- function(df, vel, ff, mdl, perc){
  perc <- as.numeric(perc)
  mdl <- as.character(mdl)
  df <- as.data.frame(df)
  vel <- as.numeric(vel)
  ff <- as.numeric(ff)
  df <- filter(df, velocity == vel & fill==ff & model == mdl & kappa >= perc)
    if(nrow(df)==0){
      print(NA)
    }else{
      m <- vector(mode = "numeric", length = 3)
      m[1] <- min(df[["step"]])
      v <- min(df[["step"]])
      last_step <- filter(cut_analyst_time, velocity == vel & fill == ff)[[4]]
      finalt <- filter(cut_analyst_time, velocity == vel & fill == ff)[[5]]
      new_time <- (v*finalt)/last_step
      m[2] <- new_time
      perc_gain <- (new_time*100)/finalt
      m[3] <- perc_gain
      print(m)
    }
  }

options(scipen = 999)
