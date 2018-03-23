##########################################################################
##########################################################################

# Title: Risk Engine & Simulation Engine
# Description: Risk Engine & Simulation Engine

##########################################################################
##########################################################################

require(tidyverse)
require(reshape2)

source("D:/bdtest/AIwR/RiskPrediction.R")
source("D:/bdtest/AIwR/UniVarAnal.R")

########################################################
# Data
########################################################

#data.dir<-"D:/bdtest/pred.sim/risk engine/data/"
#data.dir<-"risk engine/data/"

#data.x<-readRDS(str_c(data.dir,"demo.RDS"))
#data.y<-readRDS(str_c(data.dir,"out.RDS"))
#MDL.setup<-readRDS(str_c(data.dir,"MDL.setup.RDS"))
#eng.list<-readRDS("D:/bdtest/pred.sim/risk engine/RUN/PRED/PRED20180323092940.RDS")

eng.list$Dyslipidemia%>%summary

EL.coef.fun<-function(eng.list,MDL.setup){
  MDL.setup.sub<-MDL.setup%>%
    filter(VarCat%in%c("Risk Factor","Risk Category"))
  eng.list.simp<-list()
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
  return(eng.list.simp)
}

eng.list.simp<-EL.coef.fun(eng.list,MDL.setup)
