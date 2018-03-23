require(tidyverse)

root<-"D:/bdtest/pred.sim/data creation/"
data.dir<-str_c(root,"data/")


# View column
View.col<-function(demo)
  tibble(
    no=1:ncol(demo),
    col=names(demo)
  )%>%View


demo<-readRDS(str_c(data.dir,"demo.proc.RDS"))
trans<-read_csv(str_c(data.dir,"transition.csv"))

ind<-sample(1:nrow(trans),nrow(demo),replace=T)

demo<-demo[,c(2:21,25:26,28:31)]

demo<-demo[,c(1:5,8:16,20:26,6,19)]
demo%>%View.col

demo<-cbind(
  demo,
  trans[ind,c(21,15:20)]
)

demo$RiskCat<-trans$group_2008[ind]

saveRDS(demo,str_c(data.dir,"demo.RDS"))

MDL.setup<-tibble(
  n=1:ncol(demo),
  Colname=names(demo),
  VarCat=c("ID",rep("Demographics",4),rep("Risk Factor",16),rep("Intervention Variable",3),rep("Outcome",6),"Risk Category")
)

saveRDS(MDL.setup,str_c(data.dir,"MDL.setup.RDS"))

###########################################
# Outcome
rf<-which(MDL.setup$VarCat%in%c("ID","Risk Factor","Risk Category"))
out<-demo[,rf]


bin.prog<-function(x,rate){
  z.ind<-x==0
  x[z.ind]<-sample(0:1,sum(z.ind),prob=c(1-rate,rate),replace = T)
  return(x)
}

cat.prog<-function(x,rate){
  f.ind<-is.factor(x)
  if(f.ind)lvl=levels(x)
  x<-as.numeric(x)
  x.max<-max(x)
  prog.ind<-sample(0:1,length(x),prob=c(1-rate,rate),replace = T)%>%as.logical
  n.prog<-sum(prog.ind)
  prog.del<-sample(1:5,n.prog,prob=c(.9,.05,.02,.01,.01),replace = T)
  x[prog.ind]<-x[prog.ind]+prog.del
  x[x>x.max]<-x.max
  if(f.ind)
    x<-factor(
      lvl[x],levels=lvl
    )
  return(x)
}

# Binary prog

for(c in c(2:10,17)){
  out[,c]<-out[,c]%>%bin.prog(runif(1,0,0.3))
}

for(c in c(11:16,18)){
  out[,c]<-out[,c]%>%cat.prog(runif(1,0,0.3))
}

saveRDS(out,str_c(data.dir,"out.RDS"))

tgt.dir<-"D:/bdtest/pred.sim/risk engine/data/"

for(f in c("demo.RDS","out.RDS","MDL.setup.RDS")){
  file.copy(str_c(data.dir,f),str_c(tgt.dir,f))
}




