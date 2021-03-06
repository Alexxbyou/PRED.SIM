##########################################################################
##########################################################################

# Title: Risk Engine & Simulation Engine
# Description: Risk Engine & Simulation Engine
## Functions ===
##	#na.rpl	46
##	get.var	58
##	fml.gen.glmer	67
##	fml.gen.glm	78
##	trans.cal	87
##	simp.age.proc	94
##	get.risk.eng	105
##	get.risk.eng.glm	136
##	get.risk.eng.glmer	144
##	get.eng.list	150
##	#get.eng.list.test	161
##	sim.bin	181
##	sim.vec	195
##	sim.fac	197
##	sim.age	208
##	sim.1y	214
##	sim.n.year	241
##	p.slash	244
## ===

##########################################################################
##########################################################################

require(tidyverse)
require(reshape2)
require(lme4)  # for glmer
require(nlme)  # for random.effect

########################################################
# Data
########################################################

#data.dir<-"D:/bdtest/pred.sim/risk engine/data/"

#data.x<-readRDS(str_c(data.dir,"demo.RDS"))
#data.y<-readRDS(str_c(data.dir,"out.RDS"))
#MDL.setup<-readRDS(str_c(data.dir,"MDL.setup.RDS"))
#MDL.setup$VarType<-c("ID","age",rep("factor",3),rep("binary",9),rep("factor",6),"binary","binary","factor","factor",rep("numeric",6),"factor")
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
  if(len>2){
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
    if(simp){
      risk.eng<-get.risk.eng.glm(reg.data,var.set)
      print(str_c(outcome," GLM model fitted."))
    }else{
      risk.eng<-get.risk.eng.glmer(reg.data,var.set)
      print(str_c(outcome," GLMER model fitted."))
    }
  }
  return(risk.eng)
}


get.risk.eng.glm<-function(reg.data,var.set){
  age.col<-grep("age",names(reg.data),ignore.case = T)
  reg.data[,age.col]<-reg.data[,age.col]%>%simp.age.proc
  fml<-fml.gen.glm(var.set)
  risk.eng<-glm(fml,data=reg.data,family="binomial")
  return(risk.eng)
}

get.risk.eng.glmer<-function(reg.data,var.set){
  fml<-fml.gen.glmer(var.set)
  risk.eng<-glmer(fml,data=reg.data,family="binomial",control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e7)))
  return(risk.eng)
}

get.eng.list<-function(data.x,data.y,MDL.setup,simp=T){
  oc.list<-MDL.setup$Colname[MDL.setup$VarCat%in%c("Risk Factor","Risk Category")]
  risk.eng.list<-list()
  for(oc in oc.list){
    risk.eng<-get.risk.eng(oc,data.x,data.y,MDL.setup,simp)
    risk.eng.list[[oc]]<-risk.eng
  }
  return(risk.eng.list)
}


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


sim.bin<-function(outcome,data.x,eng.list){
  rind<-which(data.x[,outcome]==0)
  result<-data.x[,outcome]
  data.x.sub<-data.x%>%
    mutate(AGE=simp.age.proc(AGE))%>%
    .[rind,]
  model<-eng.list[[outcome]]
  prob<-predict(model,data.x.sub,type = "response")
  result[rind]<-(runif(length(prob))<prob)%>%as.numeric
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



sim.n.year<-function(n,data.x,eng.list,MDL.setup,temp="tempfile",save="run.save"){
  
  ## dir
  p.slash<-function(x)
    x%>%
    str_replace("/$","")%>%
    str_c("/")
  temp<-temp%>%p.slash
  save<-save%>%p.slash
  
  if(!dir.exists(temp))dir.create(temp)
  if(!dir.exists(save))dir.create(save)
  
  ## run id
  run.id<-str_c("RUN",Sys.time()%>%str_replace_all("[^[:digit:]]",""))
  run.fd<-str_c(save,run.id,"/")
  if(!dir.exists(run.fd))dir.create(run.fd)
  
  ## simulation
  data.x.curr<-data.x
  saveRDS(data.x,str_c(run.fd,"Y000.RDS"))
  for(yr in 1:n){
    data.x.curr<-sim.1y(data.x,eng.list,MDL.setup)
    fn<-str_c("Y",sprintf("%03d",yr))
    saveRDS(data.x,str_c(run.fd,fn,".RDS"))
    print(str_c(fn," simulated and saved. (",yr,"/",n,")"))
  }
}

#sim.n.year(20,data.x,eng.list,MDL.setup)


