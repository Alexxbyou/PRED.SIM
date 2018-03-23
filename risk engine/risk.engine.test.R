require(lme4)
###############################################
# test GLMER performance
###############################################
data.dir<-"D:/bdtest/pred.sim/risk engine/data/"

demo<-readRDS(str_c(data.dir,"demo.RDS"))
MDL.setup<-readRDS(str_c(data.dir,"MDL.setup.RDS"))
out<-readRDS(str_c(data.dir,"out.RDS"))

outcome<-"CKD"
id.col<-MDL.setup$Colname[MDL.setup$VarCat=="ID"]
demo.temp<-left_join(demo[,names(demo)!=outcome],out[,c(id.col,outcome)])
var.set<-get.var(outcome,MDL.setup)
fml<-fml.gen.glmer(var.set)

age.proc<-function(age,intvl){
  age<-age%>%as.character%>%as.numeric
  age[age>90]<-90
  age[age<30]<-30
  age<-age%/%intvl*intvl
  age<-age%>%as.factor
  return(age)
}

t.mdl.list<-list()
perf<-tibble()
for(int in 5:2){
  demo.t<-demo.temp%>%
    mutate(AGE=AGE%>%age.proc(int))
  t0<-Sys.time()
  risk.eng<-glmer(fml,data=demo.t,family="binomial",control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e7)))
  t.mdl.list[[str_c("int",int)]]<-risk.eng
  t1<-Sys.time()
  print(str_c("int",int," done!"))
  perf<-rbind(perf,tibble(interval=int,time=t1-t0))
}

test.dir<-"D:/bdtest/pred.sim/risk engine/test/"

# dir.create(test.dir)

saveRDS(perf,str_c(test.dir,"perf.RDS"))
saveRDS(t.mdl.list,str_c(test.dir,"t.mdl.list.RDS"))


vis.RE<-function(mdl)
  mdl%>%random.effects%>%unlist%>%plot

t.mdl.list$int5%>%vis.RE
t.mdl.list$int4%>%vis.RE
t.mdl.list$int3%>%vis.RE
t.mdl.list$int2%>%vis.RE


nlist<-c(10^(2:6),10^(2:6)/2,10^(2:6)*.25,10^(2:6)*.75)%>%round%>%sort
perf<-tibble()
nlist<-c(10^(2:6),10^(2:6)/2)%>%sort
for(i in nlist){
  t0<-Sys.time()
  model<-glmer(fml,data=demo.temp[sample(1:nrow(demo.temp),i),],family="binomial",control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e7)))
  t1<-Sys.time()
  perf<-rbind(perf,tibble(n=i,time=t1-t0))
}

saveRDS(perf,str_c(test.dir,"perf.time.RDS"))
