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


########################################################
# Data
########################################################

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
    var.set<-get.var(outcome,MDL.setup,c("Risk Category","Absorb"))
    risk.eng<-get.risk.eng.glm(reg.data,var.set,MDL.setup)
    print(str_c(outcome," GLM model fitted."))
  }
  return(risk.eng)
}


get.risk.eng.glm<-function(reg.data,var.set,MDL.setup){
  
  #age check and update
  age.col<-grep("age",names(reg.data),ignore.case = T)
  age.band<-reg.data[,age.col]%>%as.numeric%/%10%>%unique
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


get.eng.list.strat3<-function(data.x,data.y,MDL.setup,simp=T,save="risk engine/RUN/PRED"){
  ## dir
  save<-save%>%p.slash
  if(!dir.exists(save))dir.create(save)
  
  # 20180402 remove risk category ,"Risk Category"
  oc.list<-MDL.setup$Colname[MDL.setup$VarCat%in%c("Risk Factor","Absorb")]
  risk.eng.list<-list(type="strat3")
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
    mutate(Age=Age%>%simp.age.proc)
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







#eng.list<-get.eng.list(data.x,data.y,MDL.setup,simp=T)
#eng.list<-get.eng.list.strat3(data.x,data.y,MDL.setup,simp=T)
