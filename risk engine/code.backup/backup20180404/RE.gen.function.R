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






p.slash<-function(x)
  x%>%
  str_replace("/$","")%>%
  str_c("/")

get.var<-function(outcome,MDL.setup,not.x=NULL){
  x<-MDL.setup$Colname[!MDL.setup$VarCat%in%c("ID","Outcome",not.x)]
  var.set<-list(
    x=x[x!=outcome],
    y=outcome
  )
  return(var.set)
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
