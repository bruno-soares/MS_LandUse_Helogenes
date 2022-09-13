# Loading functions, packages, and formatting data #
source('script/utils.R')
library(dplyr)
library(vegan)
library(MASS)
library(iNEXT)
library(ggplot2)
diet_table<-read.table("data/diet.txt",header=T)
land_use<-as.data.frame(diet_table$Treatment)
diet<-diet_table[,c(12:24)]
land_use<-land_use[-which(rowSums(diet)==0),]
diet<-diet[-which(rowSums(diet)==0),]

# Calculating the Alimentary Index #
tableIA<-data.frame(cbind(t(IA(diet))*100,t(data.frame(IA(diet,as.data.frame(land_use)[,1])*100))))
names(tableIA)[1]<-"General"
write.table(tableIA,"tables/Table 1.csv")

# Bootstrapping dietary data for statistical testing #
diet_palm<-diet[c(1:46),]
diet_pasture<-diet[c(47:65),]
diet_forest<-diet[c(66:104),]
bootstrap_palm<-data.frame()
bootstrap_pasture<-data.frame()
bootstrap_forest<-data.frame()

for(i in 1:100){
  bootstrap_palm<-rbind(bootstrap_palm,ia(sample_n(diet_palm,size=10,replace=TRUE)))
  bootstrap_pasture<-rbind(bootstrap_pasture,ia(sample_n(diet_pasture,size=10,replace=TRUE)))
  bootstrap_forest<-rbind(bootstrap_forest,ia(sample_n(diet_forest,size=10,replace=TRUE)))
}

bootstrap_total<-data.frame()
bootstrap_total<-rbind(bootstrap_palm,bootstrap_pasture,bootstrap_forest)
bootstrap_total

# Testing bootstrapped samples #
bootstrap_distance<-vegdist(sqrt(bootstrap_total),method="bray")
grupos<-c(rep("Oil palm",100),rep("Pasture",100),rep("Forest",100))
adonis(bootstrap_distance~grupos)

# Plotting NMDS #
nmds<-isoMDS(bootstrap_distance,k=2)
ggplot()+
  geom_point(aes(x=nmds$points[,1],y=nmds$points[,2],color=as.factor(grupos)))+
  xlab("NMDS 1")+  ylab("NMDS 2")+
  labs(color="Land use")+
  scale_color_manual(values=c("Forest"="#00BA38","Oil palm"="#619CFF","Pasture"="#F8766D"))+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5), # opcoes graficas
        panel.grid.major = element_line(colour = NA),legend.position="bottom",
        panel.grid.minor = element_line(colour = NA),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 14, face = "bold"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(face = "bold", colour = "black", size = 10))
ggsave(filename="figures/Figure 3.tiff",dpi=600,unit=c("cm"),width=13,height=10,compression = "lzw")

# Testing variation in the number of consumed items between land uses #
diet_palm2<-diet_palm %>% mutate_if(is.numeric, ~1 * (. != 0))
diet_palm2<-colSums(diet_palm2)
teste_palm<-iNEXT(diet_palm2,q=0,datatype="abundance")
ggiNEXT(teste_palm, type=1, se=TRUE, facet.var="none", color.var="site", grey=FALSE)

diet_pasture2<-diet_pasture %>% mutate_if(is.numeric, ~1 * (. != 0))
diet_pasture2<-colSums(diet_pasture2)
teste_pasture<-iNEXT(diet_pasture2,q=0,datatype="abundance")
ggiNEXT(teste_pasture, type=1, se=TRUE, facet.var="none", color.var="site", grey=FALSE)

diet_forest2<-diet_forest %>% mutate_if(is.numeric, ~1 * (. != 0))
diet_forest2<-colSums(diet_forest2)
teste_forest<-iNEXT(diet_forest2,q=0,datatype="abundance")
ggiNEXT(teste_forest, type=1, se=TRUE, facet.var="none", color.var="site", grey=FALSE)

diet_list<-list(diet_palm2[!diet_palm2==0],diet_pasture2[!diet_pasture2==0],diet_forest2[!diet_forest2==0])
names(diet_list)<-c("Oil palm","Pasture","Forest")
inext_list<-iNEXT(diet_list,q=0,datatype="abundance")
ggiNEXT(inext_list, type=1)+
  ylab("Number of Food Items")+
  scale_color_manual(values=c("Forest"="#00BA38","Oil palm"="#619CFF","Pasture"="#F8766D"))+
  scale_fill_manual(values=c("Forest"="#00BA38","Oil palm"="#619CFF","Pasture"="#F8766D"))+
      theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5), # opcoes graficas
        panel.grid.major = element_line(colour = NA),legend.position="bottom",
        panel.grid.minor = element_line(colour = NA),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12, face = "bold"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(face = "bold", colour = "black", size = 8))
ggsave(filename="figures/Figure 4.tiff",dpi=600,unit=c("cm"),width=15,height=15,compression = "lzw")
