library(lawstat)
library(ggplot2)

abund<-read.table("data/Abundance.txt", h=T)
abund$Treatment <- factor(abu$Treatment, labels = c("Forest", "Oil palm", "Pasture"))

# Checking the Normality of the data #
qqnorm(abund$N)
qqline(abund$N)
shapiro.test(abund$N)

qqnorm(log(abund$N))
qqline(log(abund$N))
shapiro.test(log(abund$N))

# Checking the Homocedasticity of the data #
levene.test(log(abund$N), abund$Treatment)

# One-way ANOVA #
fit <- aov(log(N) ~ Treatment, data = abund)
summary(fit)

# Posthoc test (Tukey)
tukey <- TukeyHSD(fit)
tukey

# Graph
Figure2<- ggplot(abund, aes(x=Treatment, y=log(N), fill = Treatment)) + 
  geom_boxplot() +
  xlab("") +
  ylab("Log(Abundance)") +
  annotate(geom="text", x=c("Forest", "Oil palm", "Pasture"), y=4, label=c("a", "ab", "b"), 
           size = 5, family = "serif") +
  theme(panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(fill = NA, colour = "black"),
        text = element_text(color ="black", size = 14, family = "serif"),
        axis.text.x = element_text(color ="black", size = 12, angle = 0),
        axis.text.y = element_text(color ="black", size = 12, angle = 0),
        legend.position = "none")

Figure2 

ggsave(plot = Figure2, "figures/Figure 2.tiff", dpi = 600, width = 5, height = 4, compression = "lzw")
