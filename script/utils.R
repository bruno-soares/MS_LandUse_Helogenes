#### basic function for calculating the Freq. of Occurrence ####
fc<-function(x){
  samples<-nrow(x)
  apply(x,2,function(x) sum(x!=0)/samples)
}

#### function for calculating the Freq. of Occurrence per group ###
FOi<-function(x,y){
  if(missing(y))
  {    
    fc(x)
  }
  else
  {
    t(as.data.frame(lapply(split(x,list(y)),FUN=fc)))
  }
}

#### function for calculating the gravimetric frequency ####
fp<-function(x){
  vol<-sum(x)
  apply(x,2,function(x) (sum(x))/vol)
}

#### function for calculating the gravimetric frequency per group #####
Fq<-function(x,f1,f2){
  fp<-function(n){vol<-sum(n)
  apply(n,2,function(n) (sum(n))/vol)}
  if(missing(f2))
  {if(missing(f1))
  {fp(x)}
    else
    {t(as.data.frame(lapply(split(x,list(f1),drop=TRUE),FUN=fp)))}}
  else
  {as.data.frame(na.omit(t(as.data.frame(lapply(split(x,list(f1,f2),drop=TRUE),FUN=fp)))))}}

### basic function for calculating the AI ###
ia<-function(x){
  samples<-nrow(x)
  vol<-sum(x)
  FO<-apply(x,2,function(x) (sum(x!=0))/samples)
  FV<-apply(x,2,function(x) (sum(x))/vol) 
  numerador<-FO*FV
  denominador<-sum(FO*FV)
  t(as.matrix(numerador/denominador))
}

#### function for calculating AI for each group ###
IA<-function(x,f1,f2){
  ia<-function(n){samples<-nrow(n)
  vol<-sum(n)
  FO<-apply(n,2,function(n) (sum(n!=0))/samples)
  FV<-apply(n,2,function(n) (sum(n))/vol) 
  num<-FO*FV
  den<-sum(FO*FV)
  t(as.matrix(num/den))}
  if(missing(f2))
  {if(missing(f1))
  {ia(x)}
    else
    {index<-lapply(split(x,list(f1),drop=TRUE),FUN=ia)
    index2<-do.call(rbind,index)
    row.names(index2)<-names(index)
    return(index2)}}
  else
  {index<-lapply(split(x,list(f1,f2),drop=TRUE),FUN=ia)
  index2<-do.call(rbind,index)
  row.names(index2)<-names(index)
  index2<-as.data.frame(na.omit(index2))
  return(index2)}}