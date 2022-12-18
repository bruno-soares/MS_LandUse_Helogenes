# Loading functions, packages, and formatting data #
source('script/utils.R')
library(dplyr)
library(vegan)
library(MASS)
library(iNEXT)
library(ggplot2)
diet_table<-read.table("data/diet.txt",header=T)
land_use<-as.data.frame(diet_table$Treatment)
names(diet_table)
diet<-diet_table[,c(12:24)]
size<-diet_table[,c(6:7)]
land_use<-land_use[-which(rowSums(diet)==0),]
size<-size[-which(rowSums(diet)==0),]
diet<-diet[-which(rowSums(diet)==0),]
diet2<-cbind(diet,size$C.T)

# Calculating the Alimentary Index #
tableIA<-data.frame(cbind(t(IA(diet))*100,t(data.frame(IA(diet,as.data.frame(land_use)[,1])*100))))
names(tableIA)[1]<-"General"
write.table(tableIA,"tables/Table 1.csv")

# Bootstrapping dietary data for statistical testing #
diet_palm<-diet2[c(1:46),]
diet_pasture<-diet2[c(47:65),]
diet_forest<-diet2[c(66:104),]
bootstrap_palm<-data.frame()
bootstrap_pasture<-data.frame()
bootstrap_forest<-data.frame()
names(diet_palm)

for(i in 1:100){
  sample_palm<-sample_n(diet_palm,size=10,replace=TRUE)
  sample_pasture<-sample_n(diet_pasture,size=10,replace=TRUE)
  sample_forest<-sample_n(diet_forest,size=10,replace=TRUE)
  bootstrap_palm<-rbind(bootstrap_palm,cbind(ia(sample_palm[,-14]),sample_palm[,14]))
  bootstrap_pasture<-rbind(bootstrap_pasture,cbind(ia(sample_pasture[,-14]),sample_pasture[,14]))
  bootstrap_forest<-rbind(bootstrap_forest,cbind(ia(sample_forest[,-14]),sample_forest[,14]))
}

bootstrap_total<-data.frame()
bootstrap_total<-rbind(bootstrap_palm,bootstrap_pasture,bootstrap_forest)
bootstrap_total

# Testing bootstrapped samples #
names(bootstrap_total)
bootstrap_distance<-vegdist(sqrt(bootstrap_total[,-14]),method="bray")
grupos<-c(rep("Oil palm",100),rep("Pasture",100),rep("Forest",100))
adonis2(bootstrap_distance~grupos+bootstrap_total$V14,permutations=10000)
permdisp_test<-betadisper(bootstrap_distance,grupos)
permutest(permdisp_test,pairwise=TRUE)
permdisp_test$group.distances

# Plotting NMDS #
nmds<-isoMDS(bootstrap_distance,k=2)
env.fit<-envfit(nmds$points[,1:2],sqrt(bootstrap_total))
spp.scrs <- as.data.frame(scores(env.fit, display = "vectors"))
spp.scrs <- cbind(spp.scrs, Items = rownames(spp.scrs))
spp.scrs <- spp.scrs[c(1,5,11),]
spp.scrs$Items<-c("Insect fragments","Coleoptera","Formicidae")

ggplot()+
  geom_point(aes(x=nmds$points[,1],y=nmds$points[,2],color=as.factor(grupos)))+
  geom_segment(data = spp.scrs,
               aes(x = 0, xend = Dim1*0.6, y = 0, yend = Dim2*0.6),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black",
               size=0.9) +
  geom_text(data = spp.scrs, aes(x = Dim1*0.6+0.05, y = Dim2*0.6-0.05, label = Items),
            size = 2)+
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
ggsave(filename="figures/Figure 3.png",dpi=600,unit=c("cm"),width=13,height=10)

# Testing variation in the number of consumed items between land uses #
diet_palm2<-diet_palm %>% mutate_if(is.numeric, ~1 * (. != 0))
diet_palm2<-colSums(diet_palm2)
teste_palm<-iNEXT(diet_palm2,q=0,datatype="abundance",nboot = 999)
ggiNEXT(teste_palm, type=1, se=TRUE, facet.var="none", color.var="site", grey=FALSE)

diet_pasture2<-diet_pasture %>% mutate_if(is.numeric, ~1 * (. != 0))
diet_pasture2<-colSums(diet_pasture2)
teste_pasture<-iNEXT(diet_pasture2,q=0,datatype="abundance")
ggiNEXT(teste_pasture, type=1, se=TRUE, facet.var="none", color.var="site", grey=FALSE)

diet_forest2<-diet_forest %>% mutate_if(is.numeric, ~1 * (. != 0))
diet_forest2<-colSums(diet_forest2)
teste_forest<-iNEXT(diet_forest2,q=0,datatype="abundance",nboot=999)
ggiNEXT(teste_forest, type=1, se=TRUE, facet.var="none", color.var="site", grey=FALSE)

diet_list<-list(diet_palm2[!diet_palm2==0],diet_pasture2[!diet_pasture2==0],diet_forest2[!diet_forest2==0])
names(diet_list)<-c("Oil palm","Pasture","Forest")
inext_list<-iNEXT(diet_list,q=0,datatype="abundance",nboot=999)
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
