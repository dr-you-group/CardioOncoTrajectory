LCTMPlot <- function(sample, bestModelOption) {
  
  # bestModelOption = "Adjusted_TRUE_TRUE"
  
  ## group2
  
  modelName = paste(bestModelOption,"_2", sep="")
  
  people2 <- as.data.frame(get(modelName)$pprob[,1:2])
  
  sample$group2 <- factor(people2$class[sapply(as.numeric(sample$ptno), function(x) which(people2$ptno == x))])
  
  
  p1 <- ggplot(sample, aes(x=diffYear, y=ef, group = ptno, colour = group2)) +
    geom_line() +
    scale_x_continuous(limits = c(0,2),breaks=c(0,1,2)) +
    theme_classic() +
    theme(text = element_text(size=15)) +
    labs(x = "", y = "", colour = "Latent Class", title = "Raw") 
  
  p2 <- ggplot(sample, aes(x=diffYear, y=ef, group = ptno, colour = group2)) +
    geom_smooth(aes(group = ptno, colour = group2),size = 0.5, se = F) +
    geom_smooth(aes(group = group2), method = "loess", size = 2, se = T)  +
    scale_x_continuous(limits = c(0,2),breaks=c(0,1,2)) +
    labs(x = "",y = "",colour = "Latent Class", title = "Smoothed") +
    theme_classic() +
    theme(legend.position = "none", text = element_text(size=15)) 
  
  plot_2 <- grid.arrange(p1,p2,ncol=2)
  
  ggsave(paste0("./plot/",modelName,".png"), plot_2, width=26, height=12, units="cm", dpi=300)
  
  ## group3
  
  modelName = paste(bestModelOption,"_3", sep="")
  
  people3 <- as.data.frame(get(modelName)$pprob[,1:2])
  
  sample$group3 <- factor(people3$class[sapply(as.numeric(sample$ptno), function(x) which(people3$ptno == x))])
  
  
  p3 <- ggplot(sample, aes(x=diffYear, y=ef, group = ptno, colour = group3)) +
    geom_line() +
    scale_x_continuous(limits = c(0,2),breaks=c(0,1,2)) +
    theme_classic() +
    theme(text = element_text(size=15)) +
    labs(x = "", y = "", colour = "Latent Class", title = "Raw") 
  
  p4 <- ggplot(sample, aes(x=diffYear, y=ef, group = ptno, colour = group3)) +
    geom_smooth(aes(group = ptno, colour = group2),size = 0.5, se = F) +
    geom_smooth(aes(group = group3), method = "loess", size = 2, se = T)  +
    scale_x_continuous(limits = c(0,2),breaks=c(0,1,2)) +
    labs(x = "",y = "",colour = "Latent Class", title = "Smoothed") +
    theme_classic() +
    theme(legend.position = "none", text = element_text(size=15)) 
  
  plot_3 <- gridExtra::grid.arrange(p3,p4,ncol=2)
  
  ggsave(paste0("./plot/",modelName,".png"), plot_3, width=26, height=12, units="cm", dpi=300)
 
  ## group4
  
  modelName = paste(bestModelOption,"_4", sep="")
  
  people4 <- as.data.frame(get(modelName)$pprob[,1:2])
  
  sample$group4 <- factor(people4$class[sapply(as.numeric(sample$ptno), function(x) which(people4$ptno == x))])
  
  
  p5 <- ggplot(sample, aes(x=diffYear, y=ef, group = ptno, colour = group4)) +
    geom_line() +
    scale_x_continuous(limits = c(0,2),breaks=c(0,1,2)) +
    theme_classic() +
    theme(text = element_text(size=15)) +
    labs(x = "", y = "", colour = "Latent Class", title = "Raw") 
  
  p6 <- ggplot(sample, aes(x=diffYear, y=ef, group = ptno, colour = group4)) +
    geom_smooth(aes(group = ptno, colour = group4),size = 0.5, se = F) +
    geom_smooth(aes(group = group4), method = "loess", size = 2, se = T)  +
    scale_x_continuous(limits = c(0,2),breaks=c(0,1,2)) +
    labs(x = "",y = "",colour = "Latent Class", title = "Smoothed") +
    theme_classic() +
    theme(legend.position = "none", text = element_text(size=15)) 
  
  plot_4 <- gridExtra::grid.arrange(p5,p6,ncol=2)
  
  ggsave(paste0("./plot/",modelName,".png"), plot_4, width=26, height=12, units="cm", dpi=300)
  
  return(sample)
}

