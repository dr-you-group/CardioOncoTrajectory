LCTM_Basic(sample, nwg = TRUE, idiag = TRUE)
LCTM_Adjusted(sample, nwg = TRUE, idiag = TRUE)
LCTM_Adjusted(sample, nwg = TRUE, idiag = FALSE)

LCTMPlot(sample,"Basic_TRUE_TRUE")
LCTMPlot(sample,"Adjusted_TRUE_TRUE")
LCTMPlot(sample,"Quadra_TRUE_TRUE")

#### calculate when EF decreases below 50 ####

class1 <- sample %>% filter(group3 == 1)
time <- seq(0,1,by = 0.001)
loessModel <- loess(ef~diffYear, data = class1)

result <- predict(loessModel, timePoint)

timePoint <- time[which(result < 50)[1]]

p4 <- ggplot(sample, aes(x=diffYear, y=ef, group = ptno, colour = group3)) +
  geom_smooth(aes(group = ptno, colour = group3),size = 0.5, se = F) +
  geom_smooth(aes(group = group3), method = "loess", size = 2, se = T)  +
  geom_hline(yintercept = 50, linetype = 'dashed' ) +
  geom_vline(xintercept = timePoint, linetype = 'dashed') +
  scale_x_continuous(limits = c(0,2),breaks=c(0,0.411,1,2)) +
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70)) +
  labs(x = "year",y = "EF",colour = "Latent Class", title = "Smoothed") +
  theme_classic() +
  theme(text = element_text(size=15)) 
