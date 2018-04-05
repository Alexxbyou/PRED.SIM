##########################################################################
##########################################################################

# Title: Risk Engine strat1
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

# require riskcat
get.eng.list.strat1<-function(data.x,data.y,MDL.setup,save="risk engine/RUN/PRED"){
  eng.list<-list(type="strat1")
  reg.data<-data.x%>%
    mutate(Age=Age%>%simp.age.proc)
  age.grp<-reg.data$Age%>%unique%>%sort
  for(ag in age.grp){
    ind<-reg.data$Age==ag
    eng.list[[ag]]<-get.trans(riskcat,reg.data,data.y,ind)
    print(str_c(ag," group transition matrix calculated"))
  }
  ## dir
  save<-save%>%p.slash
  if(!dir.exists(save))dir.create(save)
  
  run.id<-str_c("PRED",Sys.time()%>%str_replace_all("[^[:digit:]]",""))
  run.fl<-str_c(save,run.id,".RDS")
  saveRDS(eng.list,run.fl)
  print(str_c(run.fl," saved!"))
  return(eng.list)
}



get.trans<-function(cn,data.x,data.y,ind=NULL){
  if(!is.null(ind)){
    data.x<-data.x[ind,]
    data.y<-data.y[ind,]
  }
  mat<-tibble(
    y0=data.x[,cn],
    y1=data.y[,cn]
  )%>%
    table%>%
    apply(1,function(x)x/sum(x))%>%
    t
  mat[is.na(mat)]<-0
  return(mat)
}

#eng.list<-get.eng.list.strat1(data.x,data.y,MDL.setup)
