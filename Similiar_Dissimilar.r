# load('/Users/yolee/Desktop/NYU/bl1206/1/files/raws/fwdgradedhomework1andbothdatasets/baa.ratios.rda') 
bg.surv <-read.csv('big-data-survey-2014-fall-interests.csv')
data<-as.data.frame(bg.surv)

name<-data$name
table<-data[,8:21]
data<-as.data.frame(cbind(name, table))
for ( i in 1:length(data)) {data[,i]<-as.character(data[,i])}
for ( i in 2:length(data)) {data[,i]<-as.numeric(data[,i])}

v1<-as.numeric(data[data$name=='B.Lai',2:length(data)])

bing<-as.data.frame(matrix('', ncol=2, nrow=length(data[data$name!='B.Lai',1])))
colnames(bing)<-c('name','corr')
bing$corr<-as.numeric(0)
bing$name<-as.character(data$name[data$name!='B.Lai'])

for ( i in 1:length(bing[,1]) ) 
{ 
  v2<-as.numeric(data[data$name==bing$name[i],2:length(data)])
  bing$corr[i]<-as.numeric(cor(v1, v2)) 
}

# MOST SIMILAR
bing[bing$corr==max(bing$corr),1:length(bing)]
print(paste("Most correlated:", bing$name[bing$corr==max(bing$corr)]))

# MOST DIFFERENT
bing[bing$corr==min(bing$corr),1:length(bing)]
print(paste("Most uncorrelated:", bing$name[bing$corr==min(bing$corr)]))

library(ggplot2) 
bing$name<-factor(bing$name) 

gg<- ggplot() 
g1<- gg + geom_bar(data=bing, aes(x=reorder(name, corr), y=corr, colour=corr, fill=corr), stat="identity", position="dodge") 
g2<- g1 + theme(panel.background=element_rect(fill="white"), axis.text.x=element_text(angle=90))
g3<- g2 + ggtitle("Correlation Coefficients between Students and Bing Lai")
g4<- g3 + xlab("Name") + ylab("Coefficient Correlation")

print(g4)