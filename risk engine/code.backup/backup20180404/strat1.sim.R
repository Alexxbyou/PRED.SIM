##########################################################################
##########################################################################

# Title: Simulation strat1
# Description: Strategy: stratefy the transition matrix by age
## Functions ===
##	#na.rpl	50
##	p.slash	63
##	get.var	68
##	fml.gen.glmer	77
##	fml.gen.glm	88
##	trans.cal	97
##	simp.age.proc	104
##	get.risk.eng	115
##	get.risk.eng.glm	153
##	get.risk.eng.glmer	161
##	get.eng.list	167
##	tm.gen	186
##	get.risk.cat.eng	213
##	#get.eng.list.test	239
##	sim.bin	259
##	sim.vec	273
##	sim.fac	275
##	sim.age	286
##	sim.RCE	292
##	sim.1y	305
##	sim.n.year	334
## ===

##########################################################################
##########################################################################

sim.1y.strat1<-function(data.x,eng.list){
  eng.list.long<-eng.list%>%eng.list.trans.strat1
  data.x[,riskcat]<-sim.strat1(data.x,eng.list.long)
  return(data.x)
}

eng.list.trans.strat1<-function(eng.list){
  tm.name<-names(eng.list)
  eng.list.long<-tibble()
  for(i in 2:length(eng.list)){
    temp<-cbind(
      Age=tm.name[i],
      riskcat=rownames(eng.list[[i]]),
      eng.list[[i]]%>%as.tibble
    )
    names(temp)[2]<-riskcat
    eng.list.long<-rbind(eng.list.long,temp)
  }
  return(eng.list.long)
}

sim.strat1<-function(data.x,eng.list.long){
  dice<-data.x%>%
    mutate(Age=Age%>%simp.age.proc)%>%
    .[,c("Age",riskcat)]%>%
    left_join(eng.list.long)%>%
    .[,-c(1:2)]
  ind<-dice%>%apply(1,sum)%>%as.logical
  out<-data.x[,riskcat]
  out[ind]<-apply(dice[ind,],1,sim.vec)
  lvl<-data.x[,riskcat]%>%levels
  result<-factor(lvl[out],lvl)
  return(result)
}

sim.n.year<-function(n=20,data.x,eng.list,MDL.setup,save="risk engine/RUN/SIM"){
  
  ## dir
  save<-save%>%p.slash
  if(!dir.exists(save))dir.create(save)
  
  ## run id
  run.id<-str_c("SIM",Sys.time()%>%str_replace_all("[^[:digit:]]",""))
  run.fd<-str_c(save,run.id,"/")
  if(!dir.exists(run.fd))dir.create(run.fd)
  
  ## simulation
  data.x.curr<-data.x
  saveRDS(data.x,str_c(run.fd,"Y000.RDS"))
  for(yr in 1:n){
    data.x.curr<-sim.1y.strat1(data.x.curr,eng.list)
    fn<-str_c("Y",sprintf("%03d",yr))
    saveRDS(data.x.curr,str_c(run.fd,fn,".RDS"))
    print(str_c(fn," simulated and saved. (",yr,"/",n,")"))
  }
}
