
#data.dir<-"D:/bdtest/pred.sim/risk engine/data/"
#data.dir<-"risk engine/data/"


#data.x<-readRDS(str_c(data.dir,"demo.RDS"))
#data.y<-readRDS(str_c(data.dir,"out.RDS"))
#MDL.setup<-readRDS(str_c(data.dir,"MDL.setup.RDS"))
#MDL.setup$VarType<-c("ID","age",rep("factor",3),rep("binary",9),rep("factor",6),"binary","binary","factor","factor",rep("numeric",6),"riskcat")
#saveRDS(MDL.setup,str_c(data.dir,"MDL.setup.RDS"))
#na.rpl<-function(x){
#  ind<-is.na(x)
#  x[ind]<-x[!ind]%>%head(sum(ind))
#  print(str_c(sum(ind)," replaced!"))
#  return(x)
#}

#for(c in 1:ncol(data.x)){
#  data.x[,c]<-data.x[,c]%>%na.rpl
#}
#saveRDS(data.x,str_c(data.dir,"demo.RDS"))

data.dir<-"data creation/data/chenny model/"
spind<-sample(1:639995,10000)
data.x<-readRDS(str_c(data.dir,"data2014_2.RDS"))[spind,]
data.y<-readRDS(str_c(data.dir,"data2015_2.RDS"))[spind,]
data.x$RiskCat<-factor(data.x$RiskCat,1:9)
MDL.setup<-readRDS(str_c(data.dir,"MDL.setup_1.RDS"))

source("risk engine/RE.gen.function.R")
source("risk engine/strat3.RE.R")
source("risk engine/strat3.sim.R")
source("visualization/sim vis/SimVis.function.R")

riskcat<-MDL.setup$Colname[MDL.setup$VarCat=="Risk Category"]
bin.var<-MDL.setup%>%
  filter(VarCat%in%c("Risk Factor","Absorb")&VarType=="binary")%>%
  select(Colname)%>%
  unlist
fac.var<-MDL.setup%>%
  filter(VarCat%in%c("Risk Factor","Risk Category")&VarType!="binary")%>%
  select(Colname)%>%
  unlist

eng.list<-get.eng.list.strat3(data.x,data.y,MDL.setup,simp=T)
run.fd<-sim.n.year(30,data.x,eng.list,MDL.setup,50000)
sim.data<-get.sim(run.fd)
oc.aggr<-get.oc.aggr()
ind.list<-get.ind.list()
sim.sum<-get.sum.list(ind.list)



trans<-list()
for(v in c(bin.var,fac.var)){
  trans[[v]]<-get.trans(v,data.x,data.y)
  print(v)
}


