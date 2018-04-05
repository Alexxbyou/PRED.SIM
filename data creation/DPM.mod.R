data.dir<-"data creation/data/chenny model/"


# add diabetes, derived from risk category
readRDS(str_c(data.dir,"data2014.RDS"))%>%
  mutate(Diabetes=RiskCat%in%c("3","5","6","8")%>%as.numeric)%>%
  .[,c(1:4,23,5:22)]%>%
  saveRDS(str_c(data.dir,"data2014_1.RDS"))

readRDS(str_c(data.dir,"data2015.RDS"))%>%
  mutate(Diabetes=RiskCat%in%c("3","5","6","8")%>%as.numeric,
         Hypertension=RiskCat%in%c("4","5","7","8")%>%as.numeric)%>%
  .[,c(1,16,2:15)]%>%
  saveRDS(str_c(data.dir,"data2015_1.RDS"))

MDL.setup%>%
  rbind(c("Diabetes","Risk Factor","binary"))%>%
  .[c(1:4,23,5:22),]%>%
  saveRDS(str_c(data.dir,"MDL.setup_1.RDS"))



# add mortality
readRDS(str_c(data.dir,"data2014_1.RDS"))%>%
  mutate(Death=(RiskCat=="9")%>%as.numeric)%>%
  saveRDS(str_c(data.dir,"data2014_1.RDS"))

readRDS(str_c(data.dir,"data2015_1.RDS"))%>%
  mutate(Death=(RiskCat=="9")%>%as.numeric)%>%
  saveRDS(str_c(data.dir,"data2015_1.RDS"))

MDL.setup%>%
  rbind(c("Death","Absorb","binary"))%>%
  saveRDS(str_c(data.dir,"MDL.setup_1.RDS"))


# update risk category
data2014<-readRDS(str_c(data.dir,"data2014_1.RDS"))
data2014$RiskCat<-data2014%>%sim.risk
saveRDS(data2014,str_c(data.dir,"data2014_2.RDS"))

data2015<-readRDS(str_c(data.dir,"data2015_1.RDS"))
data2015$RiskCat<-data2015%>%sim.risk
saveRDS(data2015,str_c(data.dir,"data2015_2.RDS"))

