df <- read.csv("~/THESIS/all_log_lamps.csv")

plot_end <- function(vel, ff){
  df <- as.data.frame(filter(df, velocity == vel & fill.factor == ff))
  c <- as.numeric((abs(max(df[["TotEng"]])) + abs(min(df[["TotEng"]])))/2)
  df1 <- filter(df, TotEng <= c)
  ###ojo, no siempre los dumps van a ser cada 1000 steps####
  df1 <- as.data.frame(filter(df1, Step %% 10000 == 0 ))
  x <- as.vector(df1[["TotEng"]])
  y <- as.vector(lag(df1[["TotEng"]]))
  z <- as.data.frame(x-y)
  names(z) <- paste("stap")
  df1 <- bind_cols(df1, z)
  z1 <- filter(df1, z == 0)
  min_step <- as.numeric(min(z1[4]))
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
dirs <- list.files(path="./", full.names = TRUE, pattern = "granular_*")

###OJO: tamaño small###
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

