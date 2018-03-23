##########################################################################
##########################################################################

# Title: Model visualization & validation
# Description: to show the coefficient and validate the model

##########################################################################
##########################################################################

require(tidyverse)
require(reshape2)
require(grid)

source("D:/bdtest/AIwR/RiskPrediction.R")
source("D:/bdtest/AIwR/UniVarAnal.R")
source("D:/bdtest/pred.sim/risk engine/risk.engine.R")

########################################################
# Data
########################################################

data.dir<-"D:/bdtest/pred.sim/risk engine/data/"
#data.dir<-"risk engine/data/"

data.x<-readRDS(str_c(data.dir,"demo.RDS"))
data.y<-readRDS(str_c(data.dir,"out.RDS"))
MDL.setup<-readRDS(str_c(data.dir,"MDL.setup.RDS"))
eng.list<-readRDS("D:/bdtest/pred.sim/risk engine/RUN/PRED/PRED20180322230702.RDS")
setwd("D:/bdtest/pred.sim/visualization/model vis/")

EL.coef.fun<-function(eng.list,MDL.setup){
  MDL.setup.sub<-MDL.setup%>%
    filter(VarCat%in%c("Risk Factor","Risk Category"))
  eng.list.simp<-list()
  eng.list.simp[["MDL.setup"]]<-MDL.setup
  for(i in 1:nrow(MDL.setup.sub)){
    type=MDL.setup.sub$VarType[i]
    var=MDL.setup.sub$Colname[i]
    if(type=="binary"){
      eng.list.simp[[var]]<-eng.list[[var]]%>%
        summary%>%.$coef%>%coef.proc%>%coef.beaut 
    }else if(type=="factor"){
      eng.list.simp[[var]]<-eng.list[[var]]
    }else{
      rname<-names(eng.list[[var]])
      for(r in rname){
        elrn<-str_c(var,"_",r)
        eng.list.simp[[elrn]]<-eng.list[[var]][[r]]%>%
          summary%>%.$coef%>%coef.proc%>%coef.beaut 
      }
    }
  }
  saveRDS(eng.list.simp,"data/eng.list.simp.RDS")
  return(eng.list.simp)
}

eng.list.simp<-EL.coef.fun(eng.list,MDL.setup)

mdl.val.pred<-function(data.x,eng.list,MDL.setup){
  data.x.new<-data.x%>%
    mutate(AGE=AGE%>%sim.age)
  bin.out<-MDL.setup%>%
    filter(VarCat%in%c("Risk Factor","Risk Category"),VarType=="binary")%>%
    select(Colname)%>%
    unlist
  riskcat<-MDL.setup$Colname[MDL.setup$VarType=="riskcat"]
  data.x.new[,riskcat]<-sim.RCE(data.x,eng.list[[riskcat]])
  for(oc in bin.out){
    data.x.new[,oc]<-sim.bin(oc,data.x,eng.list,cal.prob=T)
  }
  data.x.new<-data.x.new[,c(bin.out,riskcat)]
  return(data.x.new)
}

mv.ggsave<-function(gg,path,width,height){
  tiff(path,width=width,height=height,res=120)
  gg%>%grid.draw
  dev.off()
}

oc<-"CKD"

mdl.val<-function(eng.list,data.x,data.y,MDL.setup){
  fit<-mdl.val.pred(data.x,eng.list,MDL.setup)
  
  
  oc.list<-MDL.setup%>%
    filter(VarCat=="Risk Factor",VarType=="binary")%>%
    select(Colname)%>%unlist
  
  for(oc in oc.list){
    ind<-data.x[,oc]==0
    pred<-fit[ind,oc]
    pwr<-find.power(pred)
    mdl.pred<-data.frame(
      prediction=pred,
      pred.scale=pred^pwr,
      outcome=sapply(data.y[ind,oc],function(x)ifelse(x,"Y","N"))
    )
    plt.dir<-str_c("data/graph/",oc,"/")
    if(!dir.exists(plt.dir))dir.create(plt.dir)
    mdl.pred%>%
      pred.score.dist.vis%>%
      mv.ggsave(str_c(plt.dir,"score.tiff"),400,400)
    mdl.pred%>%
      roc.vis%>%
      mv.ggsave(str_c(plt.dir,"roc.tiff"),400,400)
    mdl.pred%>%
      perf.sum.vis%>%
      mv.ggsave(str_c(plt.dir,"perf.tiff"),1000,400)
    mdl.pred%>%
      threshold.sel.vis%>%
      mv.ggsave(str_c(plt.dir,"thres.tiff"),1000,350)
  }
}

mdl.val(eng.list,data.x,data.y,MDL.setup)

