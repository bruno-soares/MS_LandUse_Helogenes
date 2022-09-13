#### Loading packages and data ####
library(ggplot2)
size<-read.table("data/size.txt",header=T)

# Calculating Length-Weight relationship and the Condition Factor #
eq<-lm(log(size$P.T)~log(size$C.T/10))
k<-size$P.T/(exp(eq$coefficients[1])*(size$C.T/10)^eq$coefficients[2])
k_forest<-k[which(size$Treatment=="FLORESTA")]
k_palm<-k[which(size$Treatment=="PALMA")]
k_pasture<-k[which(size$Treatment=="PASTO")]
all_data<-as.data.frame(cbind(size$Treatment,k))
colnames(all_data)<-c("Treatment","k")
all_data$k<-as.numeric(all_data$k)
anova<-aov(k~Treatment,data=all_data)
summary(anova)
levels(all_data)

# Plotting the condition factor #
ggplot(aes(x=Treatment,y=k),data=all_data)+
  geom_violin(aes(fill=Treatment))+
  geom_jitter(alpha=0.3,shape=16, position=position_jitter(0.2))+
  scale_fill_manual(values=c("FLORESTA"="#00BA38","PALMA"="#619CFF","PASTO"="#F8766D"))+
  stat_summary(fun=mean, geom="crossbar",size=0.3)+
  ylab("Condition factor")+  xlab("Land use")+
  scale_x_discrete(labels = c('Forest','Oil Palm','Pasture'))+
  theme(legend.position="none",
        panel.background = element_rect(fill = "white", colour = "black", size = 0.5), # opcoes graficas
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(face = "bold", colour = "black", size = 8))
ggsave(filename="figures/Figure 5.tiff",dpi=600,unit=c("cm"),width=15,height=15,compression = "lzw")
