###########PLOTSSSS############################
path <- as.character("/home/daniela/simulaciones_daniela/plots_stepvsmetric/")

setwd("/home/daniela/simulaciones_daniela/S_001_vz_all3/")

root_dir <- getwd()

dirs <- list.files(path="./", full.names = TRUE, pattern = "S_001_*")

for (f in 1:length(dirs)) {
  print("DIRECTORY:")
  print(dirs[f])
  
  code <- as.character(unlist(strsplit(unlist(strsplit(dirs[f],"[_]"))[3], "[.]"))[1])
  #fill <- as.numeric(unlist(strsplit(dirs[f],"[_]"))[3])
  #velocity  <- as.numeric(unlist(strsplit(dirs[f],"[_]"))[4])
  rm(output_predict)
  rm(output_predict_svm1)
  load(paste(getwd(),"/S_001_", code, ".Rda", sep = ""))
  df1 <- output_predict
  #if(exists('output_predict_svm1') == TRUE){
  #  df1 <- output_predict_svm1
  #}else{
  #  df1 <- output_predict
  #}
  
  for (i in 1:length(unique(df1[[2]]))) {
    s <- as.character(unique(df1[[2]])[i])
    for (i in 1:length(unique(df1[[1]]))) {
      mdl <- as.character(unique(df1[[1]])[i])
     # if(mdl == "svm1class"){
      #  mod <- "ocsvm"
      #}else{
      #  mod <- as.character(unique(df1[[1]])[i])
      #}
      print(mdl)
            p1 <- filter(df1, model == mdl & size == s) %>%
              ggplot() + 
              ggtitle(paste( "Evolución temporal de", mdl, sep = " ")) + 
              geom_point(aes(step, kappa, color = "Kappa"), shape = 23) + 
              xlab("Steps") + 
              ylab("Valores") + 
              theme_bw() + 
              labs(colour="Method")+
             # geom_line(aes(step, accuracy, color = "Precisión"), size = 3) +
              #geom_jitter(aes(step, accuracy, color= model))+
              geom_point(aes(step, accuracy, color = "Exactitud"), shape =21)# +
             # scale_x_continuous(labels = scales::scientific)
              #geom_area(aes(step, precision))
            #scale_x_continuous(breaks = scales::pretty_breaks(n = 4))
            #+ geom_vline(xintercept = 3e+06)
            p1 <- p1 + geom_line(aes(step, precision, color = "Precisión"), size = 1) 
            p1 <- p1 + facet_grid(fill~velocity, scales="free") 
            p1 <- p1 + guides(colour = guide_legend(title="Métrica"))+
              theme(axis.text.x=element_blank(),text= element_text(size=15))
            #theme(axis.text.x = element_text(face="bold", size=9, angle = 90)
            p1 <- p1 + scale_color_brewer(palette = "Set1")
            
            esteplot <- p1
            plot <- as.character(paste(code, "KvsS", mdl, sep = "_"))
            plot <- as.character(paste(plot, "png", sep="."))
            ggsave(plot, plot = esteplot, path = path)
          }
        }
  setwd(root_dir)
}




##############plots del output predict##########
p1 <- filter(df1, model == mdl) %>%
  ggplot() + 
  # ggtitle("v=1.0") + 
  geom_point(aes(step, accuracy, color= model)) + 
  xlab("Steps") + 
  ylab("accuracy") + 
  theme_bw() + 
  labs(colour="Method") +
  ylim(0,1)
  #geom_jitter(aes(step, accuracy, color= model))+
  #geom_line(aes(step, accuracy, color= model)) 
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 4))
  #+ geom_vline(xintercept = 3e+06)
p1 <- p1 + facet_grid(fill~velocity, scales="free") 
p1 <- p1 + theme(axis.text.x = element_text(face="bold", color="#993333", 
                                            size=9, angle = 90))
p1 <- p1 + scale_color_brewer(palette = "Set1")
p1
options(scipen = 99)
