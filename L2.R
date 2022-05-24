library (ggplot2)
library(reshape2)
library(devtools) # if "devtools" is not installed already
# devtools::install_github("uqrmaie1/admixtools")
library(admixtools)

system("./plink --bfile subset1KGPApril2016NewMapAutoChr2Geno01Missing01HWELD --thin-count 5000 \\
       --make-bed --out subset1KGPApril2016NewMapAutoChr2Geno01Missing01HWELD5kSNPs")

for (k in (1:5)){
  system(paste0("./admixture subset1KGPApril2016NewMapAutoChr2Geno01Missing01HWELD5kSNPs.bed --cv=3 ",k,
                "| tee subset1KGPApril2016NewMapAutoChr2Geno01Missing01HWELD5kSNPsK",k,".log"))
}


CV <- system("grep -h CV subset1KGPApril2016NewMapAutoChr2Geno01Missing01HWELD5kSNPsK*.log",intern=T)

CV=data.frame(k=1:5,CV=as.numeric(do.call(rbind,strsplit(CV,": "))[,2]))



p <- ggplot(CV)+
  geom_line(aes(x=k,y=CV))+
  geom_point(aes(x=k,y=CV,col=CV),pch=19,size=2)+
  scale_color_gradient2(low = "brown",high = "blue",mid="white",midpoint=(max(CV$CV)+min(CV$CV))/2)+
  theme_light()+
  geom_label(aes(x=k,y=CV+.001,label=k))+
  NULL  

p

Q <- read.table("subset1KGPApril2016NewMapAutoChr2Geno01Missing01HWELD.6.Q")


fam <-read.table("subset1KGPApril2016NewMapAutoChr2Geno01Missing01HWELD.fam") 

Q=cbind(fam[,1:2],Q)

colnames(Q)[1:2]=c("Pop","ID")

#Q2 <- data.frame(Pop = fam[,1],ID=fam[,2],K1=Q2$V1,K2=Q2$V2)

Q <- Q[order(Q$Pop),]

Q[1:5,]

Q <- melt(Q)

Q

q <- ggplot(data=Q,aes(x=ID,y=value,fill=variable))

q <- q+
  geom_bar(stat="identity",width = 1.1)+
  facet_wrap(~Pop,scales = "free_x",strip.position = "bottom",nrow = 1)+
  theme(axis.text.x = element_blank(),axis.ticks.x  = element_blank())+
  theme(strip.background = element_blank(),strip.text.x = element_text(angle=90))+
  theme(panel.spacing.x = unit(0.01,"lines"),
        panel.background = element_blank(),)+
  NULL

q

admixtureplot=function(Q,fam){
  Q <- read.table(Q)
  fam <-read.table(fam) 
  Q=cbind(fam[,1:2],Q)
  colnames(Q)[1:2]=c("Pop","ID")
  Q <- Q[order(Q$Pop),]
  Q <- melt(Q)
  q <- ggplot(data=Q,aes(x=ID,y=value,fill=variable))
  
  q <- q+
    geom_bar(stat="identity",width = 1.1)+
    facet_wrap(~Pop,scales = "free_x",strip.position = "bottom",nrow = 1)+
    theme(axis.text.x = element_blank(),axis.ticks.x  = element_blank())+
    theme(strip.background = element_blank(),strip.text.x = element_text(angle=90))+
    theme(panel.spacing.x = unit(0.01,"lines"),
          panel.background = element_blank(),)+
    NULL
  
  return(q)
}

plot2 <- admixtureplot(Q="subset1KGPApril2016NewMapAutoChr2Geno01Missing01HWELD.2.Q",
                       fam ="subset1KGPApril2016NewMapAutoChr2Geno01Missing01HWELD.fam" )

plot2


#D statistics
genotype_data="v50subset4Practicals"
f2_dir="./f2"

#extract_f2(genotype_data,f2_dir)

x=f2_from_precomp("f2",afprod=T)

qpdstat(x,"Dinka.DG",c("YRI.SG","GBR.SG","TSI.SG","CHB.SG","Papuan.DG"),c("Altai_Neanderthal.DG","Denisova.DG"),"Chimp.REF")