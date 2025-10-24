BSI.stats <- by(data = BSI.sig.data$BSI.Total, BSI.sig.data$Age.Group,
   FUN = function(x) round(stat.desc(x, norm = TRUE), 3))

BSI.stats <- data.frame(do.call(rbind, BSI.stats))


Sig.stats <- by(data = BSI.sig.data$Sig.Scale, BSI.sig.data$Age.Group,
   FUN = function(x) round(stat.desc(x, norm = TRUE), 3))

Sig.stats <- data.frame(do.call(rbind, Sig.stats))

BSI.plot <- BSI.sig.data %>% 
    ggplot(aes(x = Age.Group, y = BSI.Total, fill = Age.Group)) + 
    stat_summary(fun = mean, geom = "bar", width = .7) +
  #this next line is the problem
    stat_summary(fun = mean_se, geom = "errorbar", width = .4, color = "red") +
    scale_y_continuous(limits = c(0, 140), breaks = seq(from = 0, to = 140, by = 5))+
      scale_fill_manual(values = c("forestgreen", "blue")) +
      labs(main = "BSI Scores by Age Group", x = "Age Group" , y = "Mean Score by Age") + 
      theme(legend.position = "none")

BSI.plot                                          
  


Sig.Scale.plot <- BSI.sig.data %>% 
  ggplot(aes(x = Age.Group, y = Sig.Scale, fill = Age.Group)) +
  stat_summary(fun = mean, geom = "bar", width = .7) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2, color = "tomato")+
  scale_y_continuous(limits = c(-1, 10), breaks = seq(from = 0, to = 10, by = 1))+
  scale_fill_manual(values = c("skyblue", "green")) +
  labs(title = "SIG Scores by Age Group", x = "Age Group" , y = "Mean Score by Age") + 
  theme(legend.position = "none")

Sig.Scale.plot
