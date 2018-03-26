##########################################################################
##########################################################################

# Title: Simulation visualization
# Description: to show the coefficient and validate the model

##########################################################################
##########################################################################

require(tidyverse)
require(reshape2)
require(grid)

########################################################
# Data
########################################################
data.dir<-"D:/bdtest/pred.sim/risk engine/data/"
MDL.setup<-readRDS(str_c(data.dir,"MDL.setup.RDS"))

sim.fd<-"D:/bdtest/pred.sim/risk engine/RUN/SIM/SIM20180323172001/"
fn.list<-dir(sim.fd)
fn.simp<-str_replace(fn.list,"\\.RDS","")

sim.result<-list()
for(y in 0:20){
  fn<-fn.simp[[y+1]]
  sim.result[[fn]]<-readRDS(str_c(sim.fd,fn.list[y+1]))
  print(paste(fn.list[y+1],"read."))
}





########################################################
# Visualization of simulation
########################################################
bin.var<-MDL.setup%>%
  filter(VarCat=="Risk Factor"&VarType=="binary")%>%
  select(Colname)%>%
  unlist
fac.var<-MDL.setup%>%
  filter(VarCat%in%c("Risk Factor","Risk Category")&VarType!="binary")%>%
  select(Colname)%>%
  unlist

prev.tb<-tibble(
  year=0:20
)
for(var in bin.var){
  prev<-sapply(sim.result,function(x)x[,var]%>%mean(na.rm=T))
  prev.tb<-cbind(prev.tb,prev)
  names(prev.tb)[ncol(prev.tb)]<-var
}


# vis
# prev.tb%>%melt(id.var="year")%>%ggplot(aes(x=year,y=value,colour=variable))+geom_line(size=.8)+ylim(0,1)


cat.prop.list<-list()
for(var in fac.var){
  df<-sapply(sim.result,function(x)x[,var]%>%table)%>%t%>%as.tibble
  df<-cbind(year=0:20,df)
  cat.prop.list[[var]]<-df
}

vis.prog<-function(x){
  cat.prop.list[[x]]%>%
    melt(id.vars="year")%>%
    ggplot(aes(x=year,y=value,fill=variable))+
    geom_area(position="fill")
}

########################################################
# Visualization of outcome
########################################################

outcome<-MDL.setup%>%
  filter(VarCat=="Outcome")%>%
  select(Colname)%>%
  unlist

rc.name<-MDL.setup$Colname[MDL.setup$VarType=="riskcat"]
riskcat<-sim.result$Y000[,rc.name]%>%unique%>%sort
oc.aggr<-tibble()
for(rc in riskcat){
  rind<-sim.result$Y000[,rc.name]==rc
  for(oc in outcome){
    temp<-tibble(
      RiskCat=rc,
      Outcome=oc,
      Mean=sim.result$Y000[rind,oc]%>%mean(na.rm=T),
      SD=sim.result$Y000[rind,oc]%>%sd(na.rm=T)
    )
    oc.aggr<-rbind(oc.aggr,temp)
  }
}

oc.aggr.mean<-oc.aggr[,1:3]%>%dcast(RiskCat~Outcome)



# by year
# cummulative
# end













