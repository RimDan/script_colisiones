##############plots del output predict##########
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