##########################################################################
##########################################################################

# Title: Simulation visualization
# Description: to show the coefficient and validate the model

##########################################################################
##########################################################################

require(tidyverse)
require(reshape2)
require(grid)
source("visualization/sim vis/SimVis.function.R")

########################################################
# Data
########################################################

##############################
# Model setup

#data.dir<-"D:/bdtest/pred.sim/risk engine/data/"
data.dir<-"risk engine/data/"
MDL.setup<-readRDS(str_c(data.dir,"MDL.setup.RDS"))

bin.var<-MDL.setup%>%
  filter(VarCat=="Risk Factor"&VarType=="binary")%>%
  select(Colname)%>%
  unlist
fac.var<-MDL.setup%>%
  filter(VarCat%in%c("Risk Factor","Risk Category")&VarType!="binary")%>%
  select(Colname)%>%
  unlist



################################
# Simulation folder

#sim.fd<-"D:/bdtest/pred.sim/risk engine/RUN/SIM/SIM20180323172001/"
sim.fd<-"risk engine/RUN/SIM/SIM20180326220713/"

sim.data<-get.sim(sim.fd)

oc.aggr<-get.oc.aggr()

ind.list<-get.ind.list()

sim.sum<-get.sum.list(ind.list)

savedir<-"visualization/sim vis/SIM.sum/"
dir.create(savedir)
saveRDS(sim.sum,str_c(savedir,"SIM20180326220713.RDS"))
########################################################
# Visualization of simulation
########################################################



