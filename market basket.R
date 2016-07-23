library(arules)
library(tidyr)
library(dplyr)

#Replacing product URL to category
clickurl=merge(ClickStreamData,ProductCategoryData, by="URL")
newclickurl=clickurl[order(clickurl$Timestamp),]            #ordering by timestamp
newclickurl=newclickurl[,-2]
unique=newclickurl[!duplicated(newclickurl),]               #removing duplicate transactions
duplicate=newclickurl[duplicated(newclickurl),]
duplicate=duplicate[,c(2,3,4,5,16)]
unique1=unique[order(unique$IP.Address),]
col=c("accessories","automotive","books","clothing","computers", "electronics", "games", "grocery", "handbags", "home&garden","movies","outdoors","shoes","tools")

y15=matrix(NA,nrow=22743,ncol=15)

 for(n in 1:15)           #gathering transactions of each user....where ncol is the no. of last 
 {                        #transactions where the items are searched 
   for(i in 1:22743)
   {
     if(unique1[i,3]==unique1[i+n,3])
       y15[i,n]=paste(unique1[i,16],unique1[i+n,16], sep = ",")
   }
 }
output15=y15
 
 r15=matrix(NA,nrow=22743,ncol=1)
 r15=paste(y15[,1],y15[,2],y15[,3],y15[,4],y15[,5],y15[,6],y15[,7],y15[,8],y15[,9],y15[,10],y15[,11],y15[,12],y15[,13],y15[,14],y15[,15], sep=",")
 r15=matrix(r15)   
 r15=as.data.frame(r15)                  #baskets of unique transactions collected
 s=separate(r15,V1,into=c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15","V16","V17","V18","V19","V20","V21","V22","V23","V24","V25","V26","V27","V28","V29","V30"), sep=",")
 
s[is.na(s)]="NA"
category=matrix(NA,nrow=22743, ncol=15)
category1=matrix(c("accessories","automotive","books","clothing","computers", "electronics", "games", "grocery", "handbags", "home&garden","movies","outdoors","shoes","tools","NA"),ncol=1)

for(i in 1:22743) 
 {
  for(j in 1:30)
  {
  for(n in 1:15)
   {
    if((s[i,j])==category1[n])
category[i,n]=(category1[n,])
    }
   }
 }
 
   trx=category
   category=category[,1:14]     #matrix of baskets
   colnames(category)=c("accessories","automotive","books","clothing","computers", "electronics", "games", "grocery", "handbags", "home&garden","movies","outdoors","shoes","tools")
   ran=as.matrix(paste(category[,1],category[,2],category[,3],category[,4],category[,5],category[,6],category[,7],category[,8],category[,9],category[,10],category[,11],category[,12],category[,13],category[,14], sep = ","))
   ran=gsub(",NA","",ran)
   ran=gsub("NA,","",ran)
   ran=gsub("NA","",ran)
   ran=(ran[!ran[,1]==""])      #cleaning the data
   ran=as.matrix(ran)
   write(ran, "transactions.csv")
   trans=read.transactions(file.choose(), "basket", sep=",")
   summary(trans)


aprior=apriori(trans,parameter = list(support=0.01, confidence=0.25, minlen=2))
summary(aprior)
inspect(aprior)
inspect(sort(aprior, by = "lift"))

frequentItems <- eclat (trans, parameter = list(supp = 0.01, maxlen = 15)) # calculates support for frequent items
summary(frequentItems)
itemFrequencyPlot (trans,topN=13,type="absolute") # plot frequent items
