##########################################################################
##########################################################################

# Title: Risk Engine & Simulation Engine
# Description: Risk Engine & Simulation Engine
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


sim.bin<-function(outcome,data.x,eng.list,cal.prob=F){
  rind<-which(data.x[,outcome]==0)
  result<-data.x[,outcome]
  data.x.sub<-data.x%>%
    mutate(Age=simp.age.proc(Age))%>%
    .[rind,]
  model<-eng.list[[outcome]]
  prob<-predict(model,data.x.sub,type = "response",allow.new.levels=T)
  if(cal.prob){
    result[rind]<-prob
  }else{
    result[rind]<-(runif(length(prob))<prob)%>%as.numeric
  }
  print(str_c(outcome," updated."))
  return(result)
}


sim.vec<-function(x)sample(1:length(x),1,prob=x)

sim.fac<-function(outcome,data.x,eng.list){
  trans.mat<-eng.list[[outcome]]%>%as.tibble
  new.out<-tibble(y0=data.x[,outcome])%>%
    left_join(trans.mat)%>%
    .[,-1]%>%
    apply(1,sim.vec)
  result<-trans.mat$y0[new.out]
  print(str_c(outcome," updated."))
  return(result)
}

sim.age<-function(age,age.max=90){
  age<-(age%>%as.character%>%as.numeric)+1
  age[age>90]<-90
  age
}

#sim.RCE<-function(data.x,RCE.list,simp=T){
#  if(simp)data.x<-data.x%>%mutate(Age=Age%>%simp.age.proc)
#  lvl<-names(RCE.list)
#  lvl<-factor(lvl,levels=lvl)
#  pred<-sapply(lvl,function(l){
#    predict(RCE.list[[l]],data.x,type="response")
#  })%>%as.tibble
#  out<-lvl[apply(pred,1,sim.vec)]
#  print("RiskCat Updated")
#  return(out)
#}

# customize function
dss.check<-function(data.x){
  dss1<-c("Diabetes","Hypertension")
  dss2<-c("CKD","CHD","Heart_F","Stroke","AF")
  ind<-data.x[,dss1]%>%apply(1,sum)%>%as.logical
  data.x[ind,dss2]<-0
  return(data.x)
}

sim.risk<-function(data.x){
  cc<-c("CKD","CHD","Heart_F","Stroke","AF")
  # BMI
  ind1<-data.x$BMI_cat%in%c("23-27.4","27.5-32.4","32.5-39.9",">=40")
  # Diabetes
  ind2<-data.x$Diabetes==1
  # Hypertension
  ind3<-data.x$Hypertension==1
  # CC
  ind4<-data.x[,cc]%>%apply(1,sum)%>%as.logical
  # cat9: death
  cat9<-data.x$Death==1
  # cat8: DM+HTP+CC
  cat8<-ind2&ind3&ind4&(!cat9)
  # cat7: HTP+CC
  cat7<-ind3&ind4&(!(cat9|cat8))
  # cat6: DM+CC
  cat6<-ind2&ind4&(!(cat9|cat8))
  # cat5: DM+HTP
  cat5<-ind2&ind3&(!(cat9|cat8))
  # cat4: HTP
  cat4<-ind3&(!(cat9|cat8|cat7|cat5))
  # cat3: DM
  cat3<-ind2&(!(cat9|cat8|cat6|cat5))
  # cat2: at risk
  cat2<-ind1&(!(cat9|cat8|cat7|cat6|cat5|cat4|cat3))
  
  risk.cat<-rep("1",nrow(data.x))%>%
    rc.rplc(cat2,"2")%>%
    rc.rplc(cat3,"3")%>%
    rc.rplc(cat4,"4")%>%
    rc.rplc(cat5,"5")%>%
    rc.rplc(cat6,"6")%>%
    rc.rplc(cat7,"7")%>%
    rc.rplc(cat8,"8")%>%
    rc.rplc(cat9,"9")%>%
    factor(1:9)
  print("Risk Category updated.")
  return(risk.cat)
}

rc.rplc<-function(x,ind,lab){
  x[ind]<-lab
  return(x)
}


sim.1y<-function(data.x,eng.list,MDL.setup){
  data.x.new<-data.x%>%
    mutate(Age=Age%>%sim.age)
  fac.out<-MDL.setup%>%
    filter(VarCat%in%c("Risk Factor"),VarType=="factor")%>%
    select(Colname)%>%
    unlist
  bin.out<-MDL.setup%>%
    filter(VarCat%in%c("Risk Factor","Absorb"),VarType=="binary")%>%
    select(Colname)%>%
    unlist
  #riskcat<-MDL.setup$Colname[MDL.setup$VarType=="riskcat"]
  #data.x.new[,riskcat]<-sim.RCE(data.x,eng.list[[riskcat]])
  for(oc in fac.out){
    data.x.new[,oc]<-sim.fac(oc,data.x,eng.list)
  }
  for(oc in bin.out){
    data.x.new[,oc]<-sim.bin(oc,data.x,eng.list)
  }
  data.x.new[,riskcat]<-sim.risk(data.x.new)
  return(data.x.new)
}

#tibble(y0=data.x$BMI_cat,y1=sim.fac("BMI_cat",data.x,eng.list)$BMI_cat)%>%View

#data.x%>%View
#data.x%>%sim.1y(eng.list,MDL.setup)%>%View



sim.n.year<-function(nyear,data.x,eng.list,MDL.setup,N=NULL,save="risk engine/RUN/SIM"){
  
  ## dir
  save<-save%>%p.slash
  if(!dir.exists(save))dir.create(save)
  
  ## run id
  run.id<-str_c("SIM",Sys.time()%>%str_replace_all("[^[:digit:]]",""))
  run.fd<-str_c(save,run.id,"/")
  if(!dir.exists(run.fd))dir.create(run.fd)
  
  ## simulation
  if(!is.null(N)){
    set.seed(527)
    sp.ind<-sample(1:nrow(data.x),N,replace=T)
    data.x.curr<-data.x[sp.ind,]
  }else{
    data.x.curr<-data.x
  }
  saveRDS(data.x.curr,str_c(run.fd,"Y000.RDS"))
  for(yr in 1:nyear){
    data.x.curr<-sim.1y(data.x.curr,eng.list,MDL.setup)
    fn<-str_c("Y",sprintf("%03d",yr))
    saveRDS(data.x.curr,str_c(run.fd,fn,".RDS"))
    print(str_c(fn," simulated and saved. (",yr,"/",nyear,")"))
  }
  print(str_c("Simulation saved in ",run.fd))
  return(run.fd)
}

#setwd("risk engine/")
#sim.n.year(30,data.x,eng.list,MDL.setup,50000)
