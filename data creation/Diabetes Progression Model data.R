##################################################################
# Diabetes Progression Model data
##################################################################

####################################
# Dimension

#####
# Demographics: Age, Gender, Race, Maritial status, mediFundHX
# Risk Factor: 
### Chronic Condition: Dyslipidemia, Hypertension, Asthma, COPD, CKD, CHD, Stroke, Heart_F, AF
### Test: HbA1c, HDL, LDL, TG, BP
# Outcome: cost x6
# RiskCat: Healthy, At risk, DM, HTP, DM+HTP, DM+CC, HTP+CC, DM+HTP+CC, Death
### CC: CKD, CHD, HF, Stroke, AF
require(reshape2)
require(RODBC)
require(tidyverse)
rhs<-odbcConnect("rhs")
data.dir<-"data creation/data/chenny model/"

qry2014_1<-"SELECT [GLOBAL_PATIENT_KEY2015] GLOBAL_PATIENT_KEY
,AGE Age
,GENDER Gender
,RACE Ethnicity
,MARITAL_STATUS Marital
,CASE WHEN YEAR(LIPID)<=2014 THEN 1 ELSE 0 END Dyslipidemia
,CASE WHEN YEAR(HYPERTENSION)<=2014 THEN 1 ELSE 0 END Hypertension
,CASE WHEN YEAR(ASTHMA)<=2014 THEN 1 ELSE 0 END Asthma
,CASE WHEN YEAR(COPD)<=2014 THEN 1 ELSE 0 END COPD
,CASE WHEN YEAR(CHRONIC_KIDNEY_DISEASE)<=2014 THEN 1 ELSE 0 END CKD
,CASE WHEN YEAR(CORONARY_HEART_DISEASE)<=2014 THEN 1 ELSE 0 END CHD
,CASE WHEN YEAR(HEART_FAILURE)<=2014 THEN 1 ELSE 0 END Heart_F
,CASE WHEN YEAR(STROKE)<=2014 THEN 1 ELSE 0 END Stroke
,CASE WHEN YEAR(ATRIAL_FIBRILLATION)<=2014 THEN 1 ELSE 0 END AF
FROM PRED_SIM_PAT_LIST pl
LEFT JOIN [dbo].[2008_2016_patients] pat
ON [GLOBAL_PATIENT_KEY] = GPK2016 
LEFT JOIN [RHS_2016].[dbo].[LK_UP_GPK_15_16] gpklk
ON [GLOBAL_PATIENT_KEY2016] = GPK2016
WHERE [GLOBAL_PATIENT_KEY2015] IS NOT NULL"

qry2015<-"SELECT [GLOBAL_PATIENT_KEY2015] GLOBAL_PATIENT_KEY
,CASE WHEN YEAR(LIPID)<=2015 THEN 1 ELSE 0 END Dyslipidemia
,CASE WHEN YEAR(HYPERTENSION)<=2015 THEN 1 ELSE 0 END Hypertension
,CASE WHEN YEAR(ASTHMA)<=2015 THEN 1 ELSE 0 END Asthma
,CASE WHEN YEAR(COPD)<=2015 THEN 1 ELSE 0 END COPD
,CASE WHEN YEAR(CHRONIC_KIDNEY_DISEASE)<=2015 THEN 1 ELSE 0 END CKD
,CASE WHEN YEAR(CORONARY_HEART_DISEASE)<=2015 THEN 1 ELSE 0 END CHD
,CASE WHEN YEAR(HEART_FAILURE)<=2015 THEN 1 ELSE 0 END Heart_F
,CASE WHEN YEAR(STROKE)<=2015 THEN 1 ELSE 0 END Stroke
,CASE WHEN YEAR(ATRIAL_FIBRILLATION)<=2015 THEN 1 ELSE 0 END AF
FROM PRED_SIM_PAT_LIST pl
LEFT JOIN [dbo].[2008_2016_patients] pat
ON [GLOBAL_PATIENT_KEY] = GPK2016 
LEFT JOIN [RHS_2016].[dbo].[LK_UP_GPK_15_16] gpklk
ON [GLOBAL_PATIENT_KEY2016] = GPK2016
WHERE [GLOBAL_PATIENT_KEY2015] IS NOT NULL
"

data2014<-sqlQuery(rhs,qry2014_1)
saveRDS(data2014,str_c(data.dir,"data2014.RDS"))
saveRDS(data2014,str_c(data.dir,"data2014.bk.RDS"))
data2014<-readRDS(str_c(data.dir,"data2014.bk.RDS"))

data2015<-sqlQuery(rhs,qry2015)
saveRDS(data2015,str_c(data.dir,"data2015.RDS"))
saveRDS(data2015,str_c(data.dir,"data2015.bk.RDS"))
data2015<-readRDS(str_c(data.dir,"data2015.bk.RDS"))

# qry_cost omited

# GPK lookup
qry.gpk<-"SELECT [GLOBAL_PATIENT_KEY2016]
,[GLOBAL_PATIENT_KEY2015]
FROM PRED_SIM_PAT_LIST pl
LEFT JOIN [RHS_2016].[dbo].[LK_UP_GPK_15_16] gpklk
ON [GLOBAL_PATIENT_KEY2016] = GPK2016"

GPK.lkup<-sqlQuery(rhs,qry.gpk)
saveRDS(GPK.lkup,str_c(data.dir,"GPK.lkup.RDS"))

riskcat<-read.csv("data creation/data/chenny model/s1.0.1.csv")

riskcat.sub<-riskcat%>%
  left_join(GPK.lkup,by=c("GLOBAL_PATIENT_KEY"="GLOBAL_PATIENT_KEY2016"))%>%
  mutate(GLOBAL_PATIENT_KEY=GLOBAL_PATIENT_KEY2015)%>%
  filter(!is.na(GLOBAL_PATIENT_KEY))%>%
  select(GLOBAL_PATIENT_KEY,group_2014,group_2015)

data2014<-data2014%>%
  left_join(riskcat.sub[,1:2])%>%
  rename(RiskCat=group_2014)

data2015<-data2015%>%
  left_join(riskcat.sub[,-2])%>%
  rename(RiskCat=group_2015)

# BMI
bmi.qry<-"SELECT bmi.*
FROM PRED_SIM_PAT_LIST pl
INNER JOIN [RHS_2015].[dbo].[PATIENTS_HEIGHT_WEIGHT_BMI_CDMS] bmi
ON pl.[gpk2016] = bmi.[gpk2016]
WHERE [GLOBAL_PATIENT_KEY] IS NOT NULL"

#BMI.data<-sqlQuery(rhs,bmi.qry)
#saveRDS(BMI.data,str_c(data.dir,"BMI.data.RDS"))
BMI.data<-readRDS(str_c(data.dir,"BMI.data.RDS"))

bmi.cut<-c(0,18.5,23,27.5,32.5,40,100)
A1c.cut<-c(0,5.1,6,7,8.5,100)
LDL.cut<-c(0,2.6,3.3,4,100)
BP.cut<-c(0,120,140,160,180,1000)

cutoff2label<-function(cutoff,digit=1){
  np<-length(cutoff)
  result<-c(
    paste("<",cutoff[2],sep=""),
    paste(cutoff[2:(np-2)],"-",cutoff[3:(np-1)]-10^(-digit),sep=""),
    paste(">=",cutoff[np-1],sep="")
  )
  return(result)
}

na2unknown<-function(x){
  x<-as.character(x)
  x[is.na(x)]<-"Unknown"
  return(x)
}

col.num2cat<-function(x,cut){
  cut(x,breaks=cut,labels=cut%>%cutoff2label)
}

BMI2014<-BMI.data%>%
  as.tibble%>%
  filter(!is.na(BMI_ADJ),(DATE_OF_PHYSICAL_EXAMINATION%>%as.Date)<=as.Date("2014-12-31"))%>%
  group_by(GLOBAL_PATIENT_KEY)%>%
  mutate(ord=(DATE_OF_PHYSICAL_EXAMINATION%>%as.Date-as.Date("2014-12-31"))%>%abs%>%order)%>%
  filter(ord==1)%>%
  mutate(BMI_cat=BMI_ADJ%>%cut(bmi.cut,bmi.cut%>%cutoff2label))%>%
  select(GLOBAL_PATIENT_KEY,BMI_cat)
  

BMI2015<-BMI.data%>%
  as.tibble%>%
  filter(!is.na(BMI_ADJ))%>%
  group_by(GLOBAL_PATIENT_KEY)%>%
  mutate(ord=(DATE_OF_PHYSICAL_EXAMINATION%>%as.Date-as.Date("2015-12-31"))%>%abs%>%order)%>%
  filter(ord==1)%>%
  mutate(BMI_cat=BMI_ADJ%>%cut(bmi.cut,bmi.cut%>%cutoff2label))%>%
  select(GLOBAL_PATIENT_KEY,BMI_cat)

data2014<-data2014%>%
  left_join(BMI2014)%>%
  mutate(BMI_cat=BMI_cat%>%na2unknown)
  
data2015<-data2015%>%
  left_join(BMI2015)%>%
  mutate(BMI_cat=BMI_cat%>%na2unknown)


# LDL HbA1c

ldl.a1c.qry<-"
SELECT [GLOBAL_PATIENT_KEY]
,[DATE_LAB_TEST_RESULT]
,[TEST_CD]
,[TEST_DESC]
,[LAB_TEST_RESULT_NUM] RESULT
FROM PRED_SIM_PAT_LIST pl
LEFT JOIN [RHS_2016].[dbo].[LK_UP_GPK_15_16] gpklk
ON [GLOBAL_PATIENT_KEY2016] = GPK2016
LEFT JOIN [RHS_2015].[dbo].[LAB_TTSH_CDMS] lab
ON [GLOBAL_PATIENT_KEY2015] = [GLOBAL_PATIENT_KEY]
WHERE [TEST_CD] = 'LDL'
OR TEST_DESC = 'HBA1C'

UNION

SELECT [GLOBAL_PATIENT_KEY]
,DATE_OF_LAB_TEST_RESULT
,[TEST_CODE] TEST_CD
,[TEST_DESC] TEST_DESC
,LAB_TEST_RSLT_NUM RESULT
FROM PRED_SIM_PAT_LIST pl
LEFT JOIN [RHS_2016].[dbo].[LK_UP_GPK_15_16] gpklk
ON [GLOBAL_PATIENT_KEY2016] = GPK2016
LEFT JOIN [RHS_2015].[dbo].LAB_NHGP_CDMS lab
ON [GLOBAL_PATIENT_KEY2015] = [GLOBAL_PATIENT_KEY]
WHERE [TEST_CODE] = 'LDL'
OR TEST_DESC = 'HBA1C'"

#LDL.A1c.data<-sqlQuery(rhs,ldl.a1c.qry)
#LDL.A1c.data$TEST_CD<-as.character(LDL.A1c.data$TEST_CD)
LDL.A1c.data$TEST_CD[LDL.A1c.data$TEST_DESC=="HbA1c"]<-"HbA1c"
saveRDS(LDL.A1c.data,str_c(data.dir,"LDL.A1c.data.RDS"))

# no 2015 data, use 2013-2014 progression instead
LDL.A1c2014<-LDL.A1c.data%>%
  as.tibble%>%
  rename(test_date=DATE_LAB_TEST_RESULT)%>%
  select(GLOBAL_PATIENT_KEY,test_date,TEST_CD,RESULT)%>%
  filter(test_date%>%as.Date<=as.Date("2013-12-31"))%>%
  group_by(GLOBAL_PATIENT_KEY,TEST_CD)%>%
  mutate(ord=(test_date%>%as.Date-as.Date("2013-12-31"))%>%abs%>%order)%>%
  filter(ord==1)%>%
  dcast(GLOBAL_PATIENT_KEY~TEST_CD,value.var = "RESULT")

data2014<-data2014%>%
  left_join(LDL.A1c2014)%>%
  mutate(HbA1c=HbA1c%>%col.num2cat(A1c.cut)%>%na2unknown,LDL=LDL%>%col.num2cat(LDL.cut)%>%na2unknown)

LDL.A1c2015<-LDL.A1c.data%>%
  as.tibble%>%
  rename(test_date=DATE_LAB_TEST_RESULT)%>%
  select(GLOBAL_PATIENT_KEY,test_date,TEST_CD,RESULT)%>%
  group_by(GLOBAL_PATIENT_KEY,TEST_CD)%>%
  mutate(ord=(test_date%>%as.Date-as.Date("2014-12-31"))%>%abs%>%order)%>%
  filter(ord==1)%>%
  dcast(GLOBAL_PATIENT_KEY~TEST_CD,value.var = "RESULT")

data2015<-data2015%>%
  left_join(LDL.A1c2015)%>%
  mutate(HbA1c=HbA1c%>%col.num2cat(A1c.cut)%>%na2unknown,LDL=LDL%>%col.num2cat(LDL.cut)%>%na2unknown)

#LDL update
data2014<-data2014%>%.[order(.$GLOBAL_PATIENT_KEY),]
data2015<-data2015%>%.[order(.$GLOBAL_PATIENT_KEY),]

ind<-data2014$LDL!="Unknown"&data2015$LDL=="Unknown"
data2015$LDL[ind]<-data2014$LDL[ind]
# bp

BP.qry<-"SELECT [GLOBAL_PATIENT_KEY2015] GLOBAL_PATIENT_KEY
      ,[REG_DATE_TIME]
      ,[PAT_BP_SYSTOLIC]
      ,[PAT_BP_DIASTOLIC]
FROM PRED_SIM_PAT_LIST pl
LEFT JOIN [RHS_2016].[dbo].[LK_UP_GPK_15_16] gpklk
ON [GLOBAL_PATIENT_KEY2016] = GPK2016
LEFT JOIN [RHS_INCREMENTAL].[dbo].[TXV_RHS_NHGP_MAP] bp
ON [GLOBAL_PATIENT_KEY] = GPK2016
WHERE ([PAT_BP_SYSTOLIC] <> '-'
OR [PAT_BP_DIASTOLIC] <> '-')
AND [GLOBAL_PATIENT_KEY2015] IS NOT NULL"

BP.data<-sqlQuery(rhs,BP.qry)
saveRDS(BP.data,str_c(data.dir,"BP.data.RDS"))

BP2014<-BP.data%>%
  rename(BP=PAT_BP_SYSTOLIC,test.date=REG_DATE_TIME)%>%
  filter(test.date%>%as.Date<=as.Date("2014-12-31"))%>%
  mutate(BP=BP%>%as.numeric)%>%
  group_by(GLOBAL_PATIENT_KEY)%>%
  mutate(ord=(test.date%>%as.Date-as.Date("2014-12-31"))%>%abs%>%order)%>%
  filter(ord==1)%>%
  select(GLOBAL_PATIENT_KEY,BP)
  
BP2015<-BP.data%>%
  rename(BP=PAT_BP_SYSTOLIC,test.date=REG_DATE_TIME)%>%
  mutate(BP=BP%>%as.numeric)%>%
  group_by(GLOBAL_PATIENT_KEY)%>%
  mutate(ord=(test.date%>%as.Date-as.Date("2015-12-31"))%>%abs%>%order)%>%
  filter(ord==1)%>%
  select(GLOBAL_PATIENT_KEY,BP)
  
data2014<-data2014%>%
  left_join(BP2014)%>%
  mutate(BP=BP%>%col.num2cat(BP.cut)%>%na2unknown)

data2015<-data2015%>%
  left_join(BP2015)%>%
  mutate(BP=BP%>%col.num2cat(BP.cut)%>%na2unknown) 
  

#saveRDS(data2014,str_c(data.dir,"data2014.RDS"))
#saveRDS(data2015,str_c(data.dir,"data2015.RDS"))
#data2014<-readRDS(str_c(data.dir,"data2014.RDS"))
#data2015<-readRDS(str_c(data.dir,"data2015.RDS"))
#data2014$LDL[data2014$LDL=="4-99.9"]<-">=4"
#data2015$LDL[data2015$LDL=="4-99.9"]<-">=4"


age.brk<-c(0,4:9*10,999)
age.lbl<-age.brk%>%cutoff2label(0)

age.proc<-function(age)
  age%>%
  cut(breaks=age.brk,labels=age.lbl)

fac.proc<-function(x,known,unknown,uf=T){
  x<-str_to_title(x)
  x[!x%in%known]<-unknown
  if(uf){
    lvl<-c(unknown,known)
  }else{
    lvl<-c(known,unknown)
  }
  x<-factor(x,levels=lvl)
  return(x)
}

marital.proc<-function(x){
  mar.ind<-names(x)=="Marital"
  x<-x[,!mar.ind]
  return(x)
}

#chronic.proc<-function(x,chr.list){
#  
#}
data2014<-data2014%>%
  marital.proc%>%
  mutate(
    Age_cat=Age%>%age.proc,
    Gender=Gender%>%fac.proc("Male","Female"),
    Ethnicity=Ethnicity%>%fac.proc(c("Chinese","Malay","Indian"),"Others",F),
    BMI_cat=BMI_cat%>%fac.proc(bmi.cut%>%cutoff2label,"Unknown"),
    HbA1c=HbA1c%>%fac.proc(A1c.cut%>%cutoff2label,"Unknown"),
    LDL=LDL%>%fac.proc(LDL.cut%>%cutoff2label,"Unknown"),
    BP=BP%>%fac.proc(BP.cut%>%cutoff2label,"Unknown")
  )

data2015<-data2015%>%
  marital.proc%>%
  mutate(
    BMI_cat=BMI_cat%>%fac.proc(bmi.cut%>%cutoff2label,"Unknown"),
    HbA1c=HbA1c%>%fac.proc(A1c.cut%>%cutoff2label,"Unknown"),
    LDL=LDL%>%fac.proc(LDL.cut%>%cutoff2label,"Unknown"),
    BP=BP%>%fac.proc(BP.cut%>%cutoff2label,"Unknown")
  )

#saveRDS(data2014,str_c(data.dir,"data2014.RDS"))
#saveRDS(data2015,str_c(data.dir,"data2015.RDS"))

view.prog<-function(cn){
  y0<-data2014[,c("GLOBAL_PATIENT_KEY",cn)]
  y1<-data2015[,c("GLOBAL_PATIENT_KEY",cn)]
  names(y0)[2]<-str_c(cn,"_y0")
  names(y1)[2]<-str_c(cn,"_y1")
  left_join(y0,y1)%>%
    .[,2:3]%>%
    table%>%
    apply(1,function(x)x/sum(x))%>%
    t
}

names(data2015)%>%.[-1]%>%
  sapply(view.prog)

data2014$Death<-as.numeric(data2014$RiskCat==9)
data2015$Death<-as.numeric(data2015$RiskCat==9)


############################################
# cost, intervention
qry2014_2<-"SELECT [GLOBAL_PATIENT_KEY2015] GLOBAL_PATIENT_KEY
,[mediFundHx] MediFund
,[TOTAL_2014_GROSS_AMOUNT]
,[TOTAL_2014_TTSH_GROSS_AMOUNT]
,[TOTAL_2014_POLY_GROSS_AMOUNT]
FROM PRED_SIM_PAT_LIST pl
LEFT JOIN [dbo].[2008_2016_patients] pat
ON [GLOBAL_PATIENT_KEY] = GPK2016 
LEFT JOIN [RHS_2016].[dbo].[LK_UP_GPK_15_16] gpklk
ON [GLOBAL_PATIENT_KEY2016] = GPK2016
WHERE [GLOBAL_PATIENT_KEY2015] IS NOT NULL"

cost.int.data<-sqlQuery(rhs,qry2014_2)

data2014<-left_join(data2014,cost.int.data)
data2014$RiskCat<-factor(data2014$RiskCat,unique(data2014$RiskCat)%>%sort)
data2015$RiskCat<-factor(data2015$RiskCat,unique(data2015$RiskCat)%>%sort)
gpk.death<-data2014$GLOBAL_PATIENT_KEY[data2014$Death==1]
data2014<-data2014[!data2014$GLOBAL_PATIENT_KEY%in%gpk.death,]
data2015<-data2015[!data2015$GLOBAL_PATIENT_KEY%in%gpk.death,]

data2014<-data2014[!duplicated(data2014$GLOBAL_PATIENT_KEY),]
data2015<-data2015[!duplicated(data2015$GLOBAL_PATIENT_KEY),]


#saveRDS(data2014,str_c(data.dir,"data2014.RDS"))
#saveRDS(data2015,str_c(data.dir,"data2015.RDS"))


####################################
# Model setup

MDL.setup<-tibble(
  Colname=names(data2014)[-19],
  VarCat=c("ID",rep("Demographics",3),rep("Risk Factor",9),"Risk Category",rep("Risk Factor",4),"Absorb","Intervention",rep("Outcome",3)),
  VarType=c("ID","age",rep("factor",2),rep("binary",9),"riskcat",rep("factor",4),"binary","binary",rep("numeric",3))
)

saveRDS(MDL.setup,str_c(data.dir,"MDL.setup.RDS"))





