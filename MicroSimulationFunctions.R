#########################################                                   #########################################
#########################################                                   #########################################
#########################################          SimulationKm             #########################################
#########################################
#########################################

#' @export
Read_Scenario_VKT<<-function(y)
{
  Scenario_VKT=c(y[4],y[6],y[9])
  return(as.vector(data.frame(Scenario_VKT)))

}

#########################################                                   #########################################
#########################################        Infrastructure             #########################################
#########################################                                   #########################################
#' @export
Infrastructure<<-function(hh_syn,hh_yearBefore,k,house_attribute,attr_private,attr_punblic)
{
  beta=c(	0.167,	0,	-0.856,	-0.813,	-1.21,	-1.37,	-1.88,	-2.13,	-2.02,	-2.26,	-2.58,	-2.7,	-2.86,	-2.65,	-2.31,0.0000876,-0.019,	-0.073,	0.054,	-0.0000291,	0.152,	-0.0000799,	-0.000178,	0.001,	0.01,	16.6)
  ASC=-2.522
  beta_private=c(-0.413, 0.276, -0.689,2.06e-05,0.434,-3.23e-04,-5.26e-05,-0.031,0.068,0.436,  -0.015,  1.650)
  beta_public=c(0.256,-0.043, 0.299,-0.427,-0.042,-0.143,0.101,0.506,0.068,-1.80e-04,-3.740,0.436,  -0.015,  1.650)
  Asc_private=-5.8007
  Asc_public=-5.5451

  hh_syn$charger_house=0;
  hh_syn$charger_house=ifelse(is.element(hh_syn$abshid,hh_yearBefore$abshid), hh_yearBefore$charger_house, hh_syn$charger_house)
  XB=cbind(hh_syn$abshid,hh_syn$n_children,hh_syn$`16-24 years`,hh_syn$`25-29 years`,hh_syn$`30-34 years`,hh_syn$`35-39 years`,hh_syn$`40-44 years`,hh_syn$`45-49 years`,hh_syn$`50-54 years`,hh_syn$`55-59 years`)
  XB=cbind(XB,hh_syn$`60-64 years`,hh_syn$`65-69 years`,hh_syn$`70-74 years`,hh_syn$`75-79 years`,hh_syn$`80-84 years`,hh_syn$`85 years and over`,hh_syn$income)
  attr=as.vector(unlist(house_attribute[k,]))
  XB_attribute=rep.row(attr,nrow(XB))
  XB=cbind(XB,XB_attribute)
  P=XB[,2:ncol(XB)]%*%beta + ASC
  numerator=exp(P)
  denominator=1+exp(P)
  P_charger=numerator/denominator
  Rdraws_charger=runif(nrow(XB),0,1)
  hh_syn$charger_house=ifelse(hh_syn$charger_house!=1 & hh_syn$parkingspot==1 & hh_syn$houseEstimation==1 &  Rdraws_charger<=P_charger,1,hh_syn$charger_house)
  uptake_charger_house_Print=sum(hh_syn$charger_house)
  #print(uptake_charger_house_Print)
  XB=NULL;
  P_charger=NULL
  numerator=NULL
  denominator=NULL
  P=NULL
  #hh_syn=NULL;
  Rdraws_charger=NULL
  XB_attribute=NULL;
  #hh_yearBefore=NULL

  ######

  hh_syn$solarPanel=ifelse(is.element(hh_syn$abshid,hh_yearBefore$abshid),hh_yearBefore$solarPanel, 0)
  hh_syn$charger_apa_new=ifelse(is.element(hh_syn$abshid,hh_yearBefore$abshid),  hh_yearBefore$charger_apa_new, 0)

  attr_private=as.vector(unlist(apart_private[k,]))
  attr_punblic=as.vector(unlist(apart_public[k,]))
  XB_attribute_private=rep.row(attr_private,nrow(hh_syn))
  XB_attribute_public=rep.row(attr_punblic,nrow(hh_syn))

  XB_private=cbind(XB_attribute_private,hh_syn$n_children,hh_syn$medianAge,hh_syn$solarPanel)
  XB_public=cbind(XB_attribute_public,hh_syn$n_children,hh_syn$medianAge,hh_syn$solarPanel)

  P_private=exp(XB_private%*%beta_private +Asc_private)
  P_public=  exp(XB_public%*%beta_public +Asc_public)

  P_private= P_private/(1+P_public+P_private)
  P_public= P_public/(1+P_public+P_private)

  Rdraws_charger=runif(nrow(hh_syn),0,1)
  hh_syn$charger_apa_new=ifelse(hh_syn$charger_apa_new!=2 & hh_syn$charger_apa_new!=1 & hh_syn$parkingspot==1 & hh_syn$ApartEstimation==1 & Rdraws_charger<=P_private,2,
                                ifelse(hh_syn$charger_apa_new!=2 & hh_syn$charger_apa_new!=1 & hh_syn$parkingspot==1 & hh_syn$ApartEstimation==1 &
                                         Rdraws_charger>P_private & Rdraws_charger<=P_private+P_public,1,hh_syn$charger_apa_new))

  #file=paste("hh_syn_",letters[k],"_infras",".csv",sep = "")
  #setwd("/Users/andreapellegrini/MicrosimulationNSW/Infrastructure/Infrastructure_files")
  #fwrite(hh_syn,file, nThread = 8)
  apartmentPrint=c(sum( hh_syn$charger_apa_new==1),sum( hh_syn$charger_apa_new==2),sum( hh_syn$charger_apa_new==1)+sum( hh_syn$charger_apa_new==2))
  #print(apartmentPrint)

  P_private=NULL
  P_public=NULL
  XB_private=NULL
  XB_attribute_private=NULL
  XB_attribute_public=NULL
  XB_public=NULL
  Rdraws_charger=NULL
  hh_yearBefore=NULL
  return(hh_syn)



}

#########################################                                   #########################################
#########################################        UpdateFunction             #########################################
#########################################                                   #########################################
#' @export
UpdateFunction<<-function(hh_syn,hh_syn_base)
{
  #test=ifelse(is.element(SOURCE_new$abshid,hh_syn$abshid),1,0)
  #table(test)
  hh_syn$checkHH=ifelse(is.element(hh_syn$abshid,hh_syn_base$abshid),1,0)
  hh_syn=hh_syn[hh_syn$checkHH==0,];
  Sydney_location=c(11501,
                    11502,
                    11503,
                    11504,
                    11601,
                    11602,
                    11603,
                    11701,
                    11702,
                    11703,
                    11801,
                    11802,
                    11901,
                    11902,
                    11903,
                    11904,
                    12001,
                    12002,
                    12003,
                    12101,
                    12102,
                    12103,
                    12104,
                    12201,
                    12202,
                    12203,
                    12301,
                    12302,
                    12303,
                    12401,
                    12402,
                    12403,
                    12404,
                    12405,
                    12501,
                    12502,
                    12503,
                    12504,
                    12601,
                    12602,
                    12701,
                    12702,
                    12703,
                    12801,
                    12802)
  hh_syn$Sydney=ifelse(is.element(hh_syn$sa3_code2016,Sydney_location),1,0)
  hh_syn=suppressMessages(full_join(hh_syn_base,hh_syn));
  return(hh_syn);


}

#########################################                                   #########################################
#########################################         Read2020                  #########################################
#########################################                                   #########################################
#' @export
Read2020<<-function(hh_syn,Scenario_VKT)
{

  #Apply the Vehicle Transaction Model
  hh_syn_base=VehicleTransactionModel(hh_syn);
  #Read the scenario Exelspreadsheet
  #Apply the Action model
  hh_syn_base=ApplyActions(hh_syn_base,Scenario_VKT)
  return(hh_syn_base)

}
#########################################                                   #########################################
#########################################         Read2021                  #########################################
#########################################                                   #########################################
#' @export
ReadYears<<-function(hh_syn,Scenario_VKT)
{

  #Apply the Vehicle Transaction Model
  hh_syn_base=VehicleTransactionModel(hh_syn);
  #Read the scenario Exelspreadsheet
  #Apply the Action model
  hh_syn_base=ApplyActions(hh_syn_base,Scenario_VKT)
  return(hh_syn_base)

}

#########################################                                   #########################################
#########################################     Replicate Matrix              #########################################
#########################################                                   #########################################
#' @export
rep.row<<-function(x,n)

{
  matrix(rep(x,each=n),nrow=n)
}

#' @export
rep.col<<-function(x,n)
{
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

#########################################                                   #########################################
#########################################           FuelType                #########################################
#########################################                                   #########################################
#' @export
Fueltype<<-function(hh_syn,Sources,k)
{
set.seed(as.integer(runif(1,1,4444)));
Rdraws=matrix(runif(nrow(hh_syn),0,1),nrow(hh_syn),1);
###NOW WE
Charging_station=as.vector(as.data.frame(Sources[[9]])) [k,2];
#Charging_station=Charging_station[2]
Charging_station=ifelse(Charging_station==0,0.05,Charging_station*10)

Ev_chargher=ifelse(hh_syn$charger_house==1 | hh_syn$charger_apa_new==1 | hh_syn$charger_apa_new==2 ,1,0)
Ev_chargher=ifelse(Ev_chargher!=1 & Rdraws[,1]<=Charging_station,1,Ev_chargher)

#########################################                                   #########################################
#########################################     STEP 2- Class Ass. Model      #########################################
#########################################                                   #########################################

Income_scale_1000=hh_syn$income/1000;
X=cbind(hh_syn$n_children,hh_syn$`40-44 years`,hh_syn$`45-49 years`,hh_syn$`70-74 years`,hh_syn$parkingspot,hh_syn$`#Vehicles`,Income_scale_1000)
b_class_1=c(-0.1908,	0.2583,	0.2045,	0.0899,	-0.0075,	0.0848,	-0.0812)
Asc_class_1=-0.2385
XB=X%*%b_class_1 + Asc_class_1
p=exp(XB)
Prob_class_1=p/(1+p)
Prob_class_2=1-Prob_class_1
XB=NULL;
p=NULL;
X=NULL;
b_class_1=NULL;
Asc_class_1=NULL;

#########################################                                   #########################################
#########################################     STEP 4- Price                 #########################################
#########################################                                   #########################################

P_models_class1=matrix(NA,nrow(hh_syn),length(2:7))
P_models_class2=matrix(NA,nrow(hh_syn),length(2:7))
Prob_class_1=rep.col(Prob_class_1,ncol(P_models_class1))
Prob_class_2=rep.col(Prob_class_2,ncol(P_models_class2))

for (i in 1:5)
{
  Rdraws_2=matrix(runif(nrow(hh_syn)*7,0,1),nrow(hh_syn),7);
  ##CONVERT DRAWS TAKEN INTO EV1 DRAWS
  Rdraws_2[,1:6]=-log(-log(Rdraws_2[,1:6]))
  X=cbind(Income_scale_1000,Income_scale_1000^2)
  b_price=c(-0.04,	0.001)
  Asc_price=-0.42
  XB=X%*%b_price
  XB_j= rep.col(XB,length(2:7)) + Rdraws_2[,1:6] + Asc_price
  XB=NULL;
  X=NULL;
  b_price=NULL;
  XB_j=exp(XB_j)/(exp(XB_j)+1)
  XB_j=ifelse(XB_j<0.50,sqrt(2*XB_j)-1,1-sqrt(2*(1-XB_j)))
  price_load=as.matrix(as.data.frame(Sources[[1]]))

  Price=price_load[1,] + ifelse(XB_j<0,XB_j*price_load[1,]*0.6,
                                ifelse(XB_j>0,XB_j*price_load[1,]*1,0))
  XB_j=NULL;
  #########################################                                   #########################################
  #########################################     STEP 5- Models                #########################################
  #########################################                                   #########################################
  ##PETROL
  accelleration=as.matrix(as.data.frame(Sources[[6]])) [k,1];
  emission=as.matrix(as.data.frame(Sources[[5]])) [k,1];
  charger_home=as.matrix(as.data.frame(Sources[[7]])) [k,1];
  charger_station=as.matrix(as.data.frame(Sources[[8]])) [k,1];
  cost_km=as.matrix(as.data.frame(Sources[[4]])) [k,1];
  insurance=as.matrix(as.data.frame(Sources[[2]])) [k,1];
  reg_fee=as.matrix(as.data.frame(Sources[[3]])) [k,1];
  X=rep.row((as.vector(cbind(accelleration,emission,charger_home,charger_station,cost_km,insurance,reg_fee))),nrow(hh_syn))
  X=cbind(X,Price[,1])
  #########################################                                   #########################################
  #########################################     STEP 3- Petrol                #########################################
  #########################################                                   #########################################
  beta_class1_scenario=-0.7145
  beta_class2_scenario=-0.1903
  petrol_scenario=as.vector(as.data.frame(Sources[[9]])) [k,9];

  #########################################                                   #########################################
  Asc_class1=3.9172;
  Asc_class2=4.0003;
  beta_class1=c(-0.028,	-0.6783,	-0.0002,	-0.0132,	-0.0954,	-0.0002,	-0.0006,	-0.3677)
  beta_class2=c(-0.0292,	-0.6069,	-0.0003,	-0.0165,	-0.0585,	-0.0006,	-0.0005,	-0.2748)

  P_models_class1[,1]=exp(X%*%beta_class1 + beta_class1_scenario*petrol_scenario +Asc_class1)
  P_models_class2[,1]=exp(X%*%beta_class2 + beta_class2_scenario*petrol_scenario +Asc_class2)
  X=NULL;

  ##Diesel
  accelleration=as.matrix(as.data.frame(Sources[[6]])) [k,2];
  emission=as.matrix(as.data.frame(Sources[[5]])) [k,2];
  charger_home=as.matrix(as.data.frame(Sources[[7]])) [k,2];
  charger_station=as.matrix(as.data.frame(Sources[[8]])) [k,2];
  cost_km=as.matrix(as.data.frame(Sources[[4]])) [k,2];
  insurance=as.matrix(as.data.frame(Sources[[2]])) [k,2];
  reg_fee=as.matrix(as.data.frame(Sources[[3]])) [k,2];
  X=rep.row((as.vector(cbind(accelleration,emission,charger_home,charger_station,cost_km,insurance,reg_fee))),nrow(hh_syn))
  X=cbind(X,Price[,2])

  Asc_class1=0;
  Asc_class2=0.4463;

  P_models_class1[,2]=exp(X%*%beta_class1 +Asc_class1)
  P_models_class2[,2]=exp(X%*%beta_class2 +Asc_class2)
  X=NULL;

  ##Hybrid-electric

  accelleration=as.matrix(as.data.frame(Sources[[6]])) [k,3];
  emission=as.matrix(as.data.frame(Sources[[5]])) [k,3];
  charger_home=as.matrix(as.data.frame(Sources[[7]])) [k,3];
  charger_station=as.matrix(as.data.frame(Sources[[8]])) [k,3];
  cost_km=as.matrix(as.data.frame(Sources[[4]])) [k,3];
  insurance=as.matrix(as.data.frame(Sources[[2]])) [k,3];
  reg_fee=as.matrix(as.data.frame(Sources[[3]])) [k,3];
  X=rep.row((as.vector(cbind(accelleration,emission,charger_home,charger_station,cost_km,insurance,reg_fee))),nrow(hh_syn))
  X=cbind(X,Price[,3])

  Asc_class1=-1.3957;
  Asc_class2=-1.5164;

  P_models_class1[,3]=exp(X%*%beta_class1 +Asc_class1)
  P_models_class2[,3]=exp(X%*%beta_class2 +Asc_class2)

  X=NULL;
  ##Hybrid-plug in
  accelleration=as.matrix(as.data.frame(Sources[[6]])) [k,4];
  emission=as.matrix(as.data.frame(Sources[[5]])) [k,4];
  charger_home=as.matrix(as.data.frame(Sources[[7]])) [k,4];
  charger_station=as.matrix(as.data.frame(Sources[[8]])) [k,4];
  cost_km=as.matrix(as.data.frame(Sources[[4]])) [k,4];
  insurance=as.matrix(as.data.frame(Sources[[2]])) [k,4];
  reg_fee=as.matrix(as.data.frame(Sources[[3]])) [k,4];
  X=rep.row((as.vector(cbind(accelleration,emission,charger_home,charger_station,cost_km,insurance,reg_fee))),nrow(hh_syn))
  X=cbind(X,Price[,4])

  Asc_class1=-1.12131;
  Asc_class2=-1.1144;

  P_models_class1[,4]=exp(X%*%beta_class1 +Asc_class1)
  P_models_class2[,4]=exp(X%*%beta_class2 +Asc_class2)
  X=NULL;

  ##Battery electric

  accelleration=as.matrix(as.data.frame(Sources[[6]])) [k,5];
  emission=as.matrix(as.data.frame(Sources[[5]])) [k,5];
  charger_home=as.matrix(as.data.frame(Sources[[7]])) [k,5];
  charger_station=as.matrix(as.data.frame(Sources[[8]])) [k,5];
  cost_km=as.matrix(as.data.frame(Sources[[4]])) [k,5];
  insurance=as.matrix(as.data.frame(Sources[[2]])) [k,5];
  reg_fee=as.matrix(as.data.frame(Sources[[3]])) [k,5];
  X=rep.row((as.vector(cbind(accelleration,emission,charger_home,charger_station,cost_km,insurance,reg_fee))),nrow(hh_syn))
  X=cbind(X,Price[,5])
  #########################################                                   #########################################
  #########################################     STEP 3- Battery electric      #########################################
  #########################################                                   #########################################
  eletric_scenario=as.vector(as.data.frame(Sources[[9]])) [k,1:8];
  #beta_class1_scenario=c(0,	0.0381,	0,0,	0.0989,	0.6372,	0.0006,	0.0005)
  beta_class1_scenario=c(0,	0.0381,	0,-11.6708,	0.0989,	0.6372,	0.0006,	0.0005)
  #beta_class2_scenario=c(0.1207,	0.0177,	0.00002,	-0.1254,	0.0895,	0,	0.0023,	0.0008)
  beta_class2_scenario=c(0.1207,	0.0177,	0.00002,	-12.4254,	0.0895,	0,	0.0023,	0.0008)

  #########################################           READ INPUT              #########################################
    fleet1=sum(hh_syn$Veh_1_Age!=99)
    fleet2=sum(hh_syn$Veh_2_Age!=99)
    fleet3=sum(hh_syn$Veh_3_Age!=99)
    fleet4=sum(hh_syn$Veh_4_Age!=99)
    total_vehicles=fleet1 + fleet2 + fleet3+ fleet4
    Battery_vehicles=sum(hh_syn$Vehicle1==5)+sum(hh_syn$Vehicle2==5)+sum(hh_syn$Vehicle3==5)+sum(hh_syn$Vehicle4==5)
    eletric_scenario[1]=(Battery_vehicles/total_vehicles)*100
    eletric_scenario[2]=Charging_station
    ###FILL THIS FUNCTION IN


  #########################################                                   #########################################
  Asc_class1=-0.8401;
  Asc_class2=0;

  P_models_class1[,5]=exp(X%*%beta_class1 +Asc_class1 +sum(eletric_scenario*beta_class1_scenario))
  P_models_class2[,5]=exp(X%*%beta_class2 +Asc_class2 +sum(eletric_scenario*beta_class2_scenario))

  P_models_class1[Ev_chargher==0,5]=0
  P_models_class2[Ev_chargher==0,5]=0
  X=NULL;
  ##Natural Gas
  accelleration=as.matrix(as.data.frame(Sources[[6]])) [k,6];
  emission=as.matrix(as.data.frame(Sources[[5]])) [k,6];
  charger_home=as.matrix(as.data.frame(Sources[[7]])) [k,6];
  charger_station=as.matrix(as.data.frame(Sources[[8]])) [k,6];
  cost_km=as.matrix(as.data.frame(Sources[[4]])) [k,6];
  insurance=as.matrix(as.data.frame(Sources[[2]])) [k,6];
  reg_fee=as.matrix(as.data.frame(Sources[[3]])) [k,6];
  X=rep.row((as.vector(cbind(accelleration,emission,charger_home,charger_station,cost_km,insurance,reg_fee))),nrow(hh_syn))
  X=cbind(X,Price[,6])

  Asc_class1=-1.60;
  Asc_class2=-1.60;

  P_models_class1[,6]=exp(X%*%beta_class1 +Asc_class1)
  P_models_class2[,6]=exp(X%*%beta_class2 +Asc_class2)
  X=NULL;

  #############
  P_models_class1=P_models_class1/rowSums(P_models_class1)
  P_models_class2=P_models_class2/rowSums(P_models_class2)
  P_models_class=P_models_class1*Prob_class_1 +P_models_class2*Prob_class_2

  #########################################                                   #########################################
  #########################################     STEP 6- Purchase Action       #########################################
  #########################################                                   #########################################


  FuelType=ifelse(Rdraws_2[,7]<=P_models_class[,1],1,
                  ifelse(Rdraws_2[,7]>P_models_class[,1] & Rdraws_2[,7]<=(P_models_class[,1]+P_models_class[,2]),2,
                         ifelse(Rdraws_2[,7]>(P_models_class[,1]+P_models_class[,2]) & Rdraws_2[,7]<=(P_models_class[,1]+P_models_class[,2]+P_models_class[,3]),3,
                                ifelse(Rdraws_2[,7]>(P_models_class[,1]+P_models_class[,2]+P_models_class[,3]) & Rdraws_2[,7]<=(P_models_class[,1]+P_models_class[,2]+P_models_class[,3]+P_models_class[,4]),4,
                                       ifelse(Rdraws_2[,7]>(P_models_class[,1]+P_models_class[,2]+P_models_class[,3]+P_models_class[,4]) & Rdraws_2[,7]<=(P_models_class[,1]+P_models_class[,2]+P_models_class[,3]+P_models_class[,4]+P_models_class[,5]),5,6
                                       )))))
  if (i==1)
  {
    FuelType=ifelse(hh_syn$Action1==2,FuelType,0)
    FuelT1=FuelType
    Fuel_Market_share_action1=c(sum(FuelType==1),sum(FuelType==2),sum(FuelType==3),sum(FuelType==4),sum(FuelType==5),sum(FuelType==6))

  }

  if (i==2)
  {

    FuelType=ifelse(hh_syn$Action2==2,FuelType,0)
    FuelT2=FuelType
    Fuel_Market_share_action2=c(sum(FuelType==1),sum(FuelType==2),sum(FuelType==3),sum(FuelType==4),sum(FuelType==5),sum(FuelType==6))
  }


  if (i==3)
  {

    FuelType=ifelse(hh_syn$Action3==2,FuelType,0)
    FuelT3=FuelType
    Fuel_Market_share_action3=c(sum(FuelType==1),sum(FuelType==2),sum(FuelType==3),sum(FuelType==4),sum(FuelType==5),sum(FuelType==6))
  }

  if (i==4)
  {
    FuelType=ifelse(hh_syn$Action4==2,FuelType,0)
    FuelT4=FuelType
    Fuel_Market_share_action4=c(sum(FuelType==1),sum(FuelType==2),sum(FuelType==3),sum(FuelType==4),sum(FuelType==5),sum(FuelType==6))
  }

  if (i==5)
  {
    FuelType=ifelse(hh_syn$Action1_buy==1,FuelType,0)
    FuelT5=FuelType
    Fuel_Market_share_action5=c(sum(FuelType==1),sum(FuelType==2),sum(FuelType==3),sum(FuelType==4),sum(FuelType==5),sum(FuelType==6))
  }

  #print(i)
}


###Calibration Tasks

    hh_syn$newhouseholds=ifelse(hh_syn$Vehicle1==99 & hh_syn$Action1_buy==1,1,0)

    A=ifelse(hh_syn$Action1==3 & hh_syn$newhouseholds==0,hh_syn$Vehicle1,0)
    B=ifelse(hh_syn$Action2==3 & hh_syn$newhouseholds==0,hh_syn$Vehicle2,0)
    C=ifelse(hh_syn$Action3==3 & hh_syn$newhouseholds==0,hh_syn$Vehicle3,0)
    D=ifelse(hh_syn$Action4==3 & hh_syn$newhouseholds==0,hh_syn$Vehicle4,0)
  #Fleet_upadte=cbind(hh_syn$abshid,FuelT1+A,FuelT2+B,FuelT3+C,FuelT4+D)
  Fleet_upadte=cbind(FuelT1+A,FuelT2+B,FuelT3+C,FuelT4+D)

  #
  check_space=ifelse(Fleet_upadte[,1]==0 & FuelT5!=0 & hh_syn$newhouseholds==1,1,
               ifelse(Fleet_upadte[,2]==0 & FuelT5!=0,2,
                      ifelse(Fleet_upadte[,3]==0 & FuelT5!=0,3,
                             ifelse(Fleet_upadte[,4]==0 & FuelT5!=0,4,0))))
  Fleet_upadte[check_space==1,1]= FuelT5[check_space==1]
  Fleet_upadte[check_space==2,2]= FuelT5[check_space==2]
  Fleet_upadte[check_space==3,3]= FuelT5[check_space==3]
  Fleet_upadte[check_space==4,4]= FuelT5[check_space==4]

  hh_syn$Vehicle1=Fleet_upadte[,1]
  hh_syn$Vehicle2=Fleet_upadte[,2]
  hh_syn$Vehicle3=Fleet_upadte[,3]
  hh_syn$Vehicle4=Fleet_upadte[,4]

 Fleet_upadte=NULL;
 A=NULL;
 B=NULL;
 C=NULL;
 D=NULL;


############
#########################################                                   #########################################
#########################################     CLEAR FUNCTIONS               #########################################
#########################################                                   #########################################
P_models_class=NULL;
P_models_class1=NULL;
P_models_class2=NULL;
Price=NULL;
Prob_class_1=NULL;
Prob_class_2=NULL;
Rdraws=NULL;
Rdraws_2=NULL;
Income_scale_1000=NULL;
FuelType=NULL;
return(hh_syn)
}


