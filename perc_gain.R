perc_gain_005  <- data.frame(   perc= numeric(),
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
              for (i in 1:length(percent)) {
              percent <- percent[i]
              m <-  cut_time(df = df1, vel = vl, ff = fil, mdl = model, percent)
              step_cut <- m[1]
              time_cut <- m[2]
              perc_gain <- m[3]
              new_data <- data.frame(perc = percent, model = model, fill= fil, velocity=vl, stepc = step_cut,
                                    time_cut = time_cut, perc_gain = perc_gain)
              perc_gain_006 <- rbind(output_predict, new_data)
              
              }
        }
  }
}

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
