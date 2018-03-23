sim.fd<-"D:/bdtest/pred.sim/risk engine/RUN/SIM/SIM20180323172001/"



for(fn in dir(sim.fd)){
  readRDS(str_c(sim.fd,fn))
    
}



ckd.prev<-sapply(dir(sim.fd),function(fn){
  readRDS(str_c(sim.fd,fn))%>%
    .[,"CKD"]%>%
    mean(na.rm=T)
})

lipid<-sapply(dir(sim.fd),function(fn){
  readRDS(str_c(sim.fd,fn))%>%
    .[,"Dyslipidemia"]%>%
    mean(na.rm=T)
})

plot(lipid,type="l")


