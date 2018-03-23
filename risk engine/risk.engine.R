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

require(tidyverse)
require(reshape2)
#require(lme4)  # for glmer
#require(nlme)  # for random.effect

########################################################
# Data
########################################################

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


p.slash<-function(x)
  x%>%
  str_replace("/$","")%>%
  str_c("/")

get.var<-function(outcome,MDL.setup){
  x<-MDL.setup$Colname[!MDL.setup$VarCat%in%c("ID","Outcome")]
  var.set<-list(
    x=x[x!=outcome],
    y=outcome
  )
  return(var.set)
}

fml.gen.glmer<-function(var.set){
  x<-var.set$x
  y<-var.set$y
  x.age<-x[grep("AGE",x,ignore.case = T)]
  x.oth<-x[x!=x.age]
  fml<-as.formula(
    str_c(y,"~ (1|",x.age,")+",paste(x.oth,collapse="+"))
  )
  return(fml)
}

fml.gen.glm<-function(var.set){
  x<-var.set$x
  y<-var.set$y
  fml<-as.formula(
    str_c(y,"~",paste(x,collapse="+"))
  )
  return(fml)
}

trans.cal<-function(x){
  n<-ncol(x)
  x[,2:n]<-x[,2:n]%>%
    apply(1,function(a)a/sum(a))%>%t
  return(x)
}

simp.age.proc<-function(age){
  age<-age%>%
    as.character%>%
    as.numeric
  na.ind<-is.na(age)|age<0
  age[na.ind]<-mean(age[!na.ind])
  lvl=c("<40","40-49","50-59","60-69","70-79","80-89",">=90")
  age<-cut(age,breaks=c(0,4:9*10,999),labels=lvl,right=F)
  return(age)
}

get.risk.eng<-function(outcome,data.x,data.y,MDL.setup,simp=T){
  len<-data.x[,outcome]%>%unique%>%length
  type<-MDL.setup$VarType[MDL.setup$Colname==outcome]
  
  if(type=="riskcat"){
    risk.eng<-get.risk.cat.eng(outcome,data.x,data.y,MDL.setup)
  }else if(type=="factor"){
    risk.eng<-tibble(
      y0=data.x[,outcome],
      y1=data.y[,outcome],
      count=as.integer(1)
    )%>%
      filter(!is.na(y0),!is.na(y1))%>%
      dcast(y0~y1)%>%
      trans.cal
    print(str_c(outcome," transition matrix calculated."))
  }else{
    id.col<-MDL.setup$Colname[MDL.setup$VarCat=="ID"]
    oc.col<-which(names(data.x)==outcome)
    r.sub<-data.x[,oc.col]==0
    data.x.sub<-data.x[r.sub,-oc.col]
    reg.data<-left_join(data.x.sub,data.y[,c(id.col,outcome)])
    var.set<-get.var(outcome,MDL.setup)
    risk.eng<-get.risk.eng.glm(reg.data,var.set,MDL.setup)
    print(str_c(outcome," GLM model fitted."))
  }
  return(risk.eng)
}


#if(simp){
#  risk.eng<-get.risk.eng.glm(reg.data,var.set)
#  print(str_c(outcome," GLM model fitted."))
#}else{
#  risk.eng<-get.risk.eng.glmer(reg.data,var.set)
#  print(str_c(outcome," GLMER model fitted."))
#}

# Age check and var check, when factor variable does not all levels, it will be discarded

get.risk.eng.glm<-function(reg.data,var.set,MDL.setup){
  
  #age check and update
  age.col<-grep("age",names(reg.data),ignore.case = T)
  age.band<-reg.data[,age.col]%>%levels%>%as.numeric%/%10%>%unique
  if(any(!(4:9)%in%age.band)){
    var.set$x<-var.set$x[var.set$x!=names(reg.data)[age.col]]
  }else{
    reg.data[,age.col]<-reg.data[,age.col]%>%simp.age.proc
  }
  
  #var check
  var.set<-var.check(var.set,reg.data,MDL.setup)
  fml<-fml.gen.glm(var.set)
  risk.eng<-glm(fml,data=reg.data,family="binomial")
  return(risk.eng)
}

var.check<-function(var.set,reg.data,MDL.setup){
  fac.var<-MDL.setup$Colname[MDL.setup$VarCat=="Risk Factor"&MDL.setup$VarType=="factor"]
  bin.var<-MDL.setup$Colname[MDL.setup$VarCat=="Risk Factor"&MDL.setup$VarType=="binary"]
  bin.var<-bin.var[bin.var!=var.set$y]
  var.x<-var.set$x
  fac.ind<-sapply(fac.var,function(v){
    !identical(
      reg.data[,v]%>%unique%>%sort%>%as.character,
      reg.data[,v]%>%levels
    )
  })
  if(any(fac.ind))var.x<-var.x[!var.x%in%fac.var[fac.ind]]
  bin.ind<-sapply(bin.var,function(v){
    (reg.data[,v]%>%unique%>%length)==1
  })
  if(any(bin.ind))var.x<-var.x[!var.x%in%bin.var[bin.ind]]
  var.set$x<-var.x
  return(var.set)
}

get.risk.eng.glmer<-function(reg.data,var.set){
  fml<-fml.gen.glmer(var.set)
  risk.eng<-glmer(fml,data=reg.data,family="binomial",control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e7)))
  return(risk.eng)
}

get.eng.list<-function(data.x,data.y,MDL.setup,simp=T,save="RUN/PRED"){
  ## dir
  save<-save%>%p.slash
  if(!dir.exists(save))dir.create(save)
  
  oc.list<-MDL.setup$Colname[MDL.setup$VarCat%in%c("Risk Factor","Risk Category")]
  risk.eng.list<-list()
  for(oc in oc.list){
    risk.eng<-get.risk.eng(oc,data.x,data.y,MDL.setup,simp)
    risk.eng.list[[oc]]<-risk.eng
  }
  
  run.id<-str_c("PRED",Sys.time()%>%str_replace_all("[^[:digit:]]",""))
  run.fl<-str_c(save,run.id,".RDS")
  saveRDS(risk.eng.list,run.fl)
  print(str_c(run.fl," saved!"))
  return(risk.eng.list)
}

tm.gen<-function(lvl,para){
  if(is.matrix(para)){
    if(ncol(para)==length(lvl)&nrow(para)==length(lvl)){
      rownames(para)<-colnames(para)<-lvl
      result<-para
    }else{
      stop("trans.mask has different rows and columns than # levels.")
    }
  }else if(para=="default"){
    mat<-matrix(1,length(lvl),length(lvl))
    rownames(mat)<-colnames(mat)<-lvl
    result<-mat
  }else if(para=="upper.triangle"){
    mat<-matrix(1,length(lvl),length(lvl))
    rownames(mat)<-colnames(mat)<-lvl
    mat[lower.tri(mat)]<-0
    result<-mat
  }else{
    stop("para only have two types: default, upper.triangle")
  }
  result.melt<-result%>%melt
  colnames(result.melt)<-c("y0","y1","train")
  return(result.melt)
}

#trans.mask=c("default","upper.triangle")

get.risk.cat.eng<-function(riskcat,data.x,data.y,MDL.setup){
  lvl<-data.x[,riskcat]%>%levels
  RCE.list<-list()
  reg.data<-data.x%>%
    mutate(AGE=AGE%>%simp.age.proc)
  for(l in lvl){
    out.c<-data.y[,riskcat]%>%
      as.character
    reg.data<-reg.data%>%
      mutate(out=(out.c==l)%>%as.numeric)
    fml<-get.var("out",MDL.setup)%>%fml.gen.glm
    model<-glm(fml,data=reg.data,family="binomial")
    RCE.list[[l]]<-model
    print(str_c("Level ",l," model trained!"))
  }
  return(RCE.list)
}


#RCE.list<-get.risk.cat.eng("RiskCat",data.x,data.y,MDL.setup)






#get.eng.list.test<-function(data.x,data.y,MDL.setup,simp=F){
#  oc.list<-MDL.setup$Colname[MDL.setup$VarCat%in%c("Risk Factor","Risk Category")]
#  risk.eng.list<-list()
#  for(oc in oc.list){
#    len<-data.x[,oc]%>%.[!is.na(.)]%>%unique%>%length
#    if(len==2){
#      data.x1<-data.x[1:1000,]
#    }else{
#      data.x1<-data.x
#    }
#    risk.eng<-get.risk.eng(oc,data.x1,data.y,MDL.setup,simp)
#    risk.eng.list[[oc]]<-risk.eng
#  }
#  return(risk.eng.list)
#}


#eng.list<-get.eng.list(data.x,data.y,MDL.setup,simp=T)


sim.bin<-function(outcome,data.x,eng.list,cal.prob=F){
  rind<-which(data.x[,outcome]==0)
  result<-data.x[,outcome]
  data.x.sub<-data.x%>%
    mutate(AGE=simp.age.proc(AGE))%>%
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

sim.RCE<-function(data.x,RCE.list,simp=T){
  if(simp)data.x<-data.x%>%mutate(AGE=AGE%>%simp.age.proc)
  lvl<-names(RCE.list)
  lvl<-factor(lvl,levels=lvl)
  pred<-sapply(lvl,function(l){
    predict(RCE.list[[l]],data.x,type="response")
  })%>%as.tibble
  out<-lvl[apply(pred,1,sim.vec)]
  print("RiskCat Updated")
  return(out)
}


sim.1y<-function(data.x,eng.list,MDL.setup){
  data.x.new<-data.x%>%
    mutate(AGE=AGE%>%sim.age)
  fac.out<-MDL.setup%>%
    filter(VarCat%in%c("Risk Factor","Risk Category"),VarType=="factor")%>%
    select(Colname)%>%
    unlist
  bin.out<-MDL.setup%>%
    filter(VarCat%in%c("Risk Factor","Risk Category"),VarType=="binary")%>%
    select(Colname)%>%
    unlist
  riskcat<-MDL.setup$Colname[MDL.setup$VarType=="riskcat"]
  data.x.new[,riskcat]<-sim.RCE(data.x,eng.list[[riskcat]])
  for(oc in fac.out){
    data.x.new[,oc]<-sim.fac(oc,data.x,eng.list)
  }
  for(oc in bin.out){
    data.x.new[,oc]<-sim.bin(oc,data.x,eng.list)
  }
  return(data.x.new)
}

#tibble(y0=data.x$BMI_cat,y1=sim.fac("BMI_cat",data.x,eng.list)$BMI_cat)%>%View

#data.x%>%View
#data.x%>%sim.1y(eng.list,MDL.setup)%>%View



sim.n.year<-function(n,data.x,eng.list,MDL.setup,save="RUN/SIM"){
  
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
    data.x.curr<-sim.1y(data.x.curr,eng.list,MDL.setup)
    fn<-str_c("Y",sprintf("%03d",yr))
    saveRDS(data.x.curr,str_c(run.fd,fn,".RDS"))
    print(str_c(fn," simulated and saved. (",yr,"/",n,")"))
  }
}

#sim.n.year(20,data.x,eng.list,MDL.setup)
