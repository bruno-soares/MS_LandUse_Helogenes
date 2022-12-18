library(vegan)
library(lawstat)
library(ggplot2)

abu <- read.table("clipboard", h=T)
abu

abu$Treatment <- factor(abu$Treatment, labels = c("Forest", "Oil palm", "Pasture"))
str(abu)

# Homocedasticity

levene.test(abu$N, abu$Treatment)

# One-way ANOVA

fit <- aov(N ~ Treatment, data = abu)
summary(fit)

# Normality

par(mfrow = c(2,2))
plot(fit)
shapiro.test(resid(fit))

# Posthoc test (Tukey)

tukey <- TukeyHSD(fit)
tukey

# Graph

Figure2 <- ggplot(abu, aes(x=Treatment, y=N, fill = Treatment)) + 
  geom_boxplot() + scale_fill_manual(values=c("Forest"="#00BA38","Oil palm"="#619CFF","Pasture"="#F8766D"))+
  xlab("") +
  ylab("Abundance") +
  annotate(geom="text", x=c("Forest", "Oil palm", "Pasture"), y=40, label=c("a", "ab", "b"), 
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
