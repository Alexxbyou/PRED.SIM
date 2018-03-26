##########################################################################
##########################################################################

# Title: Simulation visualization
# Description: to show the coefficient and validate the model

##########################################################################
##########################################################################

require(tidyverse)
require(reshape2)
require(grid)

get.oc.aggr<-function(...){
  rc.name<-MDL.setup$Colname[MDL.setup$VarType=="riskcat"]
  riskcat<-sim.result$Y000[,rc.name]%>%unique%>%sort
  oc.aggr.temp<-tibble()
  for(rc in riskcat){
    rind<-sim.result$Y000[,rc.name]==rc
    for(oc in outcome){
      temp<-tibble(
        RiskCat=rc,
        Outcome=oc,
        Mean=sim.result$Y000[rind,oc]%>%mean(na.rm=T),
        SD=sim.result$Y000[rind,oc]%>%sd(na.rm=T)
      )
      oc.aggr.temp<-rbind(oc.aggr.temp,temp)
    }
  }
  
  oc.aggr<-list(
    mean=oc.aggr.temp[,1:3]%>%dcast(RiskCat~Outcome),
    sd=oc.aggr.temp[,-3]%>%dcast(RiskCat~Outcome)
  )
  return(oc.aggr)
}

#oc.aggr<-get.oc.aggr()
########################################################
# get sim data
########################################################


# get simulation data
get.sim<-function(fd){
  fn.list<-dir(fd)
  fn.simp<-str_replace(fn.list,"\\.RDS","")
  n.year<-length(fn.list)-1
  sim.result<-list()
  for(y in 0:n.year){
    fn<-fn.simp[[y+1]]
    sim.result[[fn]]<-readRDS(str_c(sim.fd,fn.list[y+1]))
    print(paste(fn.list[y+1],"read."))
  }
  return(sim.result)
}

#sim.fd<-"D:/bdtest/pred.sim/risk engine/RUN/SIM/SIM20180323172001/"
#sim.data<-get.sim(sim.fd)

ind.fun<-function(x){
  x.uniqe<-x%>%unique%>%sort
  df<-sapply(x,function(a)x.uniqe==a)%>%t%>%as.tibble
  names(df)<-x.uniqe
  return(df)
}

get.ind.list<-function(...){
  ind.list<-list(All=rep(T,nrow(sim.data$Y000)))
  IV<-MDL.setup$Colname[MDL.setup$VarCat=="Intervention Variable"]
  if(length(IV)>0){
    for(iv in IV){
      ind.list[[iv]]<-sim.data$Y000[,iv]%>%ind.fun
    }
  }
  return(ind.list)
}

#ind.list<-get.ind.list()

get.prev<-function(ind){
  sim.data.sub<-lapply(sim.data,function(x)x[ind,])
  nyear<-length(sim.data.sub)-1
  prev.tb<-tibble(
    year=0:nyear
  )
  for(var in bin.var){
    prev<-sapply(sim.data.sub,function(x)x[,var]%>%mean(na.rm=T))
    prev.tb<-cbind(prev.tb,prev)
    names(prev.tb)[ncol(prev.tb)]<-var
  }
  return(prev.tb)
}

get.cat.prog<-function(ind){
  sim.data.sub<-lapply(sim.data,function(x)x[ind,])
  nyear<-length(sim.data.sub)-1
  cat.prop.list<-list()
  for(var in fac.var){
    df<-sapply(sim.data.sub,function(x)x[,var]%>%table)%>%t%>%as.tibble
    df<-cbind(year=0:nyear,df)
    cat.prop.list[[var]]<-df
  }
  return(cat.prop.list)
}

get.OC.by.yr<-function(ind){
  RiskCat<-get.cat.prog(ind)$RiskCat
  nyear<-nrow(RiskCat)-1
  OC.by.yr<-cbind(
    year=0:nyear,
    as.matrix(RiskCat[,-1])%*%as.matrix(oc.aggr$mean[,-1])
  )%>%as.tibble
  OC.by.yr$year<-as.integer(OC.by.yr$year)
  return(OC.by.yr)
}

cumu<-function(x){
  sapply(1:length(x),function(a)sum(x[1:a]))
}

get.oc.cumu<-function(OC.by.yr){
  OC.by.yr[,-1]<-OC.by.yr[,-1]%>%sapply(cumu)
  return(OC.by.yr)
}

get.sum<-function(ind){
  if(is.vector(ind)){
    prev<-get.prev(ind)
    prog<-get.cat.prog(ind)
    oc.by.yr<-get.OC.by.yr(ind)
    oc.cumu<-get.oc.cumu(oc.by.yr)
    list(
      prev=prev,
      prog=prog,
      oc.by.yr=oc.by.yr,
      oc.cumu=oc.cumu
    )
  }else{
    lapply(ind,get.sum)
  }
}

get.sum.list<-function(ind.list){
  result<-lapply(ind.list,get.sum)
  result$MDL.setup<-MDL.setup
  return(result)
}


#sim.sum<-get.sum.list(ind.list)
########################################################
# visualization
########################################################


###################################
# prevalence

prev.all<-function(title){
  v.data<-sim.sum$All$prev%>%melt(id.var="year")
  names(v.data)<-c("Year","Factor","prevalence")
  ggplot(v.data,aes(x=Year,y=prevalence,colour=Factor))+
    geom_line(size=.8)+ylim(0,1)+
    ylab("Prevalence (%)")+ggtitle(title)+
    theme(legend.position = "bottom")
}

prev.int<-function(int,var){
  sim.sum.sub<-sim.sum[[int]]
  int.c<-names(sim.sum.sub)
  vis.df<-tibble()
  for(i in int.c){
    temp<-sim.sum.sub[[i]][["prev"]][,c("year",var)]
    temp$Intervention<-i
    vis.df<-rbind(vis.df,temp)
  }
  names(vis.df)<-c("Year","prevalence","Intervention")
  ggplot(vis.df,aes(x=Year,y=prevalence,colour=Intervention))+
    geom_line(size=.8)+
    ylab("Prevalence (%)")+ggtitle(var)+
    theme(legend.position = "bottom")
}

#prev.int("iswellcontrolled","Dyslipidemia","")


vis.prog<-function(x,title){
  x%>%
    melt(id.vars="year")%>%
    ggplot(aes(x=year,y=value,fill=variable))+
    geom_area(position="fill")+ggtitle(title)+ylab("Proportion")+
    theme(legend.position = "bottom",
          legend.title = element_blank())
}


prog.all<-function(var){
  sim.sum$All$prog[[var]]%>%vis.prog(var)
}

fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}

out.all<-function(var,cumu=T){
  if(cumu){
    out<-sim.sum$All$oc.cumu
  }else{
    out<-sim.sum$All$oc.by.yr
  }
  v.data<-out[,c("year",var)]
  names(v.data)[2]<-"out"
  ggplot(v.data,aes(x=year,y=out))+geom_path()+
    scale_y_continuous(name=var,labels=scales::comma)+ggtitle(var)
}

out.int<-function(int,var,cumu=T){
  int.data<-sim.sum[[int]]
  sl<-ifelse(cumu,"oc.cumu","oc.by.yr")
  v.data<-tibble()
  for(s in names(int.data)){
    df<-int.data[[s]][[sl]][,c("year",var)]
    df$cat<-s
    v.data<-rbind(v.data,df)
  }
  names(v.data)[2]<-"out"
  ggplot(v.data,aes(x=year,y=out,colour=cat))+geom_line(size=.8)+
    ylab(var)+ggtitle(var)+
    theme(legend.position = "bottom")
}








