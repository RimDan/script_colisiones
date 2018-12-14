df <- read.csv("~/THESIS/all_log_lamps.csv")

plot_end <- function(vel, ff){
  df <- as.data.frame(filter(df, velocity == vel & fill.factor == ff))
  c <- as.numeric((abs(max(df[["TotEng"]])) + abs(min(df[["TotEng"]])))/2)
  df1 <- filter(df, TotEng <= c)
  x <- as.vector(df1[["TotEng"]])
  y <- as.vector(lag(df1[["TotEng"]]))
  z <- as.data.frame(x-y)
  names(z) <- paste("stap")
  df1 <- bind_cols(df1, z)
  z1 <- filter(df1, z == 0)
  min_step <- as.numeric(min(z1[4]))
  return(paste("el step de corte sugerido es", min_step))
  p1 <- ggplot(df, aes(Step,TotEng)) +
    geom_line() +
    xlab("Step") +
    ylab("TotEng")
  p1 <- p1 + geom_vline(xintercept = as.numeric(min_step), color = "red", size =1)
  p1 <- p1 + ggtitle(as.character(paste("velocity ", vel, " & fill factor ", ff)))
    return(p1)  
  }
