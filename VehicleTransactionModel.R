#' @export
Read_Scenario_VKT<<-function(y)
{
  Scenario_VKT=c(y[4],y[6],y[9])
  return(as.vector(data.frame(Scenario_VKT)))


}


#########################################                                   #########################################
#########################################          SimulationKm             #########################################
#########################################                                   #########################################
#' @export
KmDrivenFunction<<-function(x)
{

  kmstart=ifelse(x<=0.0193321616871705,1000,
                 ifelse(x>0.0193321616871705 & x<=0.101230228471002, 4754.71698113208,
                        ifelse(x>0.101230228471002 & x<=0.372231985940246,8509.43396226415,
                               ifelse(x>0.372231985940246 & x<=0.650615114235501,12264.1509433962,
                                      ifelse(x>0.650615114235501 & x<=0.803866432337434,16018.8679245283,
                                             ifelse(x>0.803866432337434 & x<=0.814059753954306,19773.5849056604,
                                                    ifelse(x>0.814059753954306 & x<=0.900175746924429,23528.3018867925,
                                                           ifelse(x>0.900175746924429 & x<=0.936028119507909,27283.0188679245,
                                                                  ifelse(x>0.936028119507909 & x<=0.953602811950791,31037.7358490566,
                                                                         ifelse(x>0.953602811950791 & x<=0.954305799648506,34792.4528301887,
                                                                                ifelse(x>0.954305799648506 & x<=0.961335676625659,38547.1698113208,
                                                                                       ifelse(x>0.961335676625659 & x<=0.971880492091388,42301.8867924528,
                                                                                              ifelse(x>0.971880492091388 & x<=0.974692442882249,46056.6037735849,
                                                                                                     ifelse(x>0.974692442882249 & x<=0.975043936731107,49811.320754717,
                                                                                                            ifelse(x>0.975043936731107 & x<=0.985237258347979,53566.0377358491,
                                                                                                                   ifelse(x>0.985237258347979 & x<=0.985588752196837,57320.7547169811,
                                                                                                                          ifelse(x>0.985588752196837 & x<=0.987697715289982,61075.4716981132,
                                                                                                                                 ifelse(x>0.987697715289982 & x<=0.987697715289982,64830.1886792453,
                                                                                                                                        ifelse(x>0.987697715289982 & x<=0.98804920913884,68584.9056603774,
                                                                                                                                               ifelse(x>0.98804920913884 & x<=0.989455184534271,72339.6226415094,
                                                                                                                                                      ifelse(x>0.989455184534271 & x<=0.989806678383128,76094.3396226415,
                                                                                                                                                             ifelse(x>0.989806678383128 & x<=0.990861159929701,79849.0566037736,
                                                                                                                                                                    ifelse(x>0.990861159929701 & x<=0.992267135325132,83603.7735849057,
                                                                                                                                                                           ifelse(x>0.992267135325132 & x<=0.992618629173989,92990.56604,
                                                                                                                                                                                  ifelse(x>0.992618629173989 & x<=0.996485061511424,124905.6604,
                                                                                                                                                                                         ifelse(x>0.996485061511424 & x<=0.997891036906854,154943.3962,
                                                                                                                                                                                                ifelse(x>0.997891036906854 & x<=0.998945518453427,162452.830188679,
                                                                                                                                                                                                       ifelse(x>0.998945518453427 & x<=0.999297012302285,177471.6981,
                                                                                                                                                                                                              ifelse(x>0.999297012302285 & x<=0.999648506151142,194367.9245,
                                                                                                                                                                                                                     ifelse(x>0.999648506151142,200000,999)
                                                                                                                                                                                                              )))))))))))))))))))))))))))))









  return(kmstart)

}

#########################################                                   #########################################
#########################################          VKT                      #########################################
#########################################                                   #########################################
#' @export
VKT<<-function(hh_syn,ScenarioVKT,index,Mean_price)
{
  Sociodemographic=cbind(hh_syn$n_children,hh_syn$`16-24 years`,hh_syn$`25-29 years`,hh_syn$`30-34 years`,hh_syn$`35-39 years`,hh_syn$`40-44 years`,hh_syn$`45-49 years`,hh_syn$`50-54 years`,
                         hh_syn$`55-59 years`,hh_syn$`60-64 years`,hh_syn$`65-69 years`,hh_syn$`70-74 years`,hh_syn$`75-79 years`,hh_syn$`80-84 years`,hh_syn$income,hh_syn$Sydney,hh_syn$`#Vehicles`)

  Beta_Socio=c(2.59,
               -4.79,
               -1.44,
               5.71,
               -7.23,
               1.42,
               6.49,
               3.49,
               -4.6,
               -4.12,
               -3.81,
               -2.2,
               -5.93,
               -1.93,
               5.09,
               -0.57,
               12.88)

  Beta_scenario=c(-710.898,
                  3526.92,
                  388.595,
                  939.552,
                  481.819)
  Rebate_Scenario=c(1991.51,
                    12298.3,
                    314.311,
                    1254.14,
                    -5122.27)

  Ev_Km_charge_Scenario=c(897.857,
                          5331.75,
                          -5376.51,
                          -4974.52,
                          -2024.35)

  Petrol_Km_charge_Scenario=c(-1956.38,
                              -25642.5,
                              3215.07,
                              867.001,
                              -51.4876)


  ASC_VTK=1099.3



  if(index==1)
  {
    VTK1_1=(hh_syn$Vehicle1==1)*Beta_scenario[1]+(hh_syn$Vehicle1==2)*Beta_scenario[2]+(hh_syn$Vehicle1==3)*Beta_scenario[3]+ (hh_syn$Vehicle1==4)*Beta_scenario[4]+ (hh_syn$Vehicle1==5)*Beta_scenario[5]
    VTK2_1=(hh_syn$Vehicle1==1)*Petrol_Km_charge_Scenario[1]+(hh_syn$Vehicle1==2)*Petrol_Km_charge_Scenario[2]+(hh_syn$Vehicle1==3)*Petrol_Km_charge_Scenario[3]+ (hh_syn$Vehicle1==4)*Petrol_Km_charge_Scenario[4]+ (hh_syn$Vehicle1==5)*Petrol_Km_charge_Scenario[5]
    VTK3_1=(hh_syn$Vehicle1==1)*Ev_Km_charge_Scenario[1]+(hh_syn$Vehicle1==2)*Ev_Km_charge_Scenario[2]+(hh_syn$Vehicle1==3)*Ev_Km_charge_Scenario[3]+ (hh_syn$Vehicle1==4)*Ev_Km_charge_Scenario[4]+ (hh_syn$Vehicle1==5)*Ev_Km_charge_Scenario[5]
    VTK4_1=(hh_syn$Vehicle1==1)*Rebate_Scenario[1]+(hh_syn$Vehicle1==2)*Rebate_Scenario[2]+(hh_syn$Vehicle1==3)*Rebate_Scenario[3]+ (hh_syn$Vehicle1==4)*Rebate_Scenario[4]+ (hh_syn$Vehicle1==5)*Rebate_Scenario[5]
    VTK4_1=-VTK4_1
    VTK=ASC_VTK +VTK1_1 +VTK2_1+VTK3_1+VTK4_1+ Sociodemographic%*%Beta_Socio
    return(VTK)
  }
  if(index==2)
  {
    VTK1_1=(hh_syn$Vehicle2==1)*Beta_scenario[1]+(hh_syn$Vehicle2==2)*Beta_scenario[2]+(hh_syn$Vehicle2==3)*Beta_scenario[3]+ (hh_syn$Vehicle2==4)*Beta_scenario[4]+ (hh_syn$Vehicle2==5)*Beta_scenario[5]
    VTK2_1=(hh_syn$Vehicle2==1)*Petrol_Km_charge_Scenario[1]+(hh_syn$Vehicle2==2)*Petrol_Km_charge_Scenario[2]+(hh_syn$Vehicle2==3)*Petrol_Km_charge_Scenario[3]+ (hh_syn$Vehicle2==4)*Petrol_Km_charge_Scenario[4]+ (hh_syn$Vehicle2==5)*Petrol_Km_charge_Scenario[5]
    VTK3_1=(hh_syn$Vehicle2==1)*Ev_Km_charge_Scenario[1]+(hh_syn$Vehicle2==2)*Ev_Km_charge_Scenario[2]+(hh_syn$Vehicle2==3)*Ev_Km_charge_Scenario[3]+ (hh_syn$Vehicle2==4)*Ev_Km_charge_Scenario[4]+ (hh_syn$Vehicle2==5)*Ev_Km_charge_Scenario[5]
    VTK4_1=(hh_syn$Vehicle2==1)*Rebate_Scenario[1]+(hh_syn$Vehicle2==2)*Rebate_Scenario[2]+(hh_syn$Vehicle2==3)*Rebate_Scenario[3]+ (hh_syn$Vehicle2==4)*Rebate_Scenario[4]+ (hh_syn$Vehicle2==5)*Rebate_Scenario[5]
    VTK4_1=-VTK4_1
    VTK=ASC_VTK +VTK1_1 +VTK2_1+VTK3_1+VTK4_1+ Sociodemographic%*%Beta_Socio
    return(VTK)
  }
  if(index==3)
  {
    VTK1_1=(hh_syn$Vehicle3==1)*Beta_scenario[1]+(hh_syn$Vehicle3==2)*Beta_scenario[2]+(hh_syn$Vehicle3==3)*Beta_scenario[3]+ (hh_syn$Vehicle3==4)*Beta_scenario[4]+ (hh_syn$Vehicle3==5)*Beta_scenario[5]
    VTK2_1=(hh_syn$Vehicle3==1)*Petrol_Km_charge_Scenario[1]+(hh_syn$Vehicle3==2)*Petrol_Km_charge_Scenario[2]+(hh_syn$Vehicle3==3)*Petrol_Km_charge_Scenario[3]+ (hh_syn$Vehicle3==4)*Petrol_Km_charge_Scenario[4]+ (hh_syn$Vehicle3==5)*Petrol_Km_charge_Scenario[5]
    VTK3_1=(hh_syn$Vehicle3==1)*Ev_Km_charge_Scenario[1]+(hh_syn$Vehicle3==2)*Ev_Km_charge_Scenario[2]+(hh_syn$Vehicle3==3)*Ev_Km_charge_Scenario[3]+ (hh_syn$Vehicle3==4)*Ev_Km_charge_Scenario[4]+ (hh_syn$Vehicle3==5)*Ev_Km_charge_Scenario[5]
    VTK4_1=(hh_syn$Vehicle3==1)*Rebate_Scenario[1]+(hh_syn$Vehicle3==2)*Rebate_Scenario[2]+(hh_syn$Vehicle3==3)*Rebate_Scenario[3]+ (hh_syn$Vehicle3==4)*Rebate_Scenario[4]+ (hh_syn$Vehicle3==5)*Rebate_Scenario[5]
    VTK4_1=-VTK4_1
    VTK=ASC_VTK +VTK1_1 +VTK2_1+VTK3_1+VTK4_1+ Sociodemographic%*%Beta_Socio
    return(VTK)
  }
  if(index==4)
  {
    VTK1_1=(hh_syn$Vehicle4==1)*Beta_scenario[1]+(hh_syn$Vehicle4==2)*Beta_scenario[2]+(hh_syn$Vehicle4==3)*Beta_scenario[3]+ (hh_syn$Vehicle4==4)*Beta_scenario[4]+ (hh_syn$Vehicle4==5)*Beta_scenario[5]
    VTK2_1=(hh_syn$Vehicle4==1)*Petrol_Km_charge_Scenario[1]+(hh_syn$Vehicle4==2)*Petrol_Km_charge_Scenario[2]+(hh_syn$Vehicle4==3)*Petrol_Km_charge_Scenario[3]+ (hh_syn$Vehicle4==4)*Petrol_Km_charge_Scenario[4]+ (hh_syn$Vehicle4==5)*Petrol_Km_charge_Scenario[5]
    VTK3_1=(hh_syn$Vehicle4==1)*Ev_Km_charge_Scenario[1]+(hh_syn$Vehicle4==2)*Ev_Km_charge_Scenario[2]+(hh_syn$Vehicle4==3)*Ev_Km_charge_Scenario[3]+ (hh_syn$Vehicle4==4)*Ev_Km_charge_Scenario[4]+ (hh_syn$Vehicle4==5)*Ev_Km_charge_Scenario[5]
    VTK4_1=(hh_syn$Vehicle4==1)*Rebate_Scenario[1]+(hh_syn$Vehicle4==2)*Rebate_Scenario[2]+(hh_syn$Vehicle4==3)*Rebate_Scenario[3]+ (hh_syn$Vehicle4==4)*Rebate_Scenario[4]+ (hh_syn$Vehicle4==5)*Rebate_Scenario[5]
    VTK4_1=-VTK4_1
    VTK=ASC_VTK +VTK1_1 +VTK2_1+VTK3_1+VTK4_1+ Sociodemographic%*%Beta_Socio
    return(VTK)
  }

}

#########################################                                   #########################################
#########################################          VKT_update               #########################################
#########################################                                   #########################################
#' @export
VKT_update<<-function(hh_syn,ScenarioVKT,Mean_price)
{
  Sociodemographic=cbind(hh_syn$n_children,hh_syn$`16-24 years`,hh_syn$`25-29 years`,hh_syn$`30-34 years`,hh_syn$`35-39 years`,hh_syn$`40-44 years`,hh_syn$`45-49 years`,hh_syn$`50-54 years`,
                         hh_syn$`55-59 years`,hh_syn$`60-64 years`,hh_syn$`65-69 years`,hh_syn$`70-74 years`,hh_syn$`75-79 years`,hh_syn$`80-84 years`,hh_syn$income,hh_syn$Sydney,hh_syn$`#Vehicles`)

  Beta_Socio=c(2.59,
               -4.79,
               -1.44,
               5.71,
               -7.23,
               1.42,
               6.49,
               3.49,
               -4.6,
               -4.12,
               -3.81,
               -2.2,
               -5.93,
               -1.93,
               5.09,
               -0.57,
               12.88)

  Beta_scenario=c(-710.898,
                  3526.92,
                  388.595,
                  939.552,
                  481.819)
  Rebate_Scenario=c(1991.51,
                    12298.3,
                    314.311,
                    1254.14,
                    -5122.27)

  Ev_Km_charge_Scenario=c(897.857,
                          5331.75,
                          -5376.51,
                          -4974.52,
                          -2024.35)

  Petrol_Km_charge_Scenario=c(-1956.38,
                              -25642.5,
                              3215.07,
                              867.001,
                              -51.4876)


  ASC_VTK=1099.3


  VTK1_1=(hh_syn$Vehicle1==1)*Beta_scenario[1]+(hh_syn$Vehicle1==2)*Beta_scenario[2]+(hh_syn$Vehicle1==3)*Beta_scenario[3]+ (hh_syn$Vehicle1==4)*Beta_scenario[4]+ (hh_syn$Vehicle1==5)*Beta_scenario[5]
  VTK2_1=(hh_syn$Vehicle1==1)*Petrol_Km_charge_Scenario[1]+(hh_syn$Vehicle1==2)*Petrol_Km_charge_Scenario[2]+(hh_syn$Vehicle1==3)*Petrol_Km_charge_Scenario[3]+ (hh_syn$Vehicle1==4)*Petrol_Km_charge_Scenario[4]+ (hh_syn$Vehicle1==5)*Petrol_Km_charge_Scenario[5]
  VTK3_1=(hh_syn$Vehicle1==1)*Ev_Km_charge_Scenario[1]+(hh_syn$Vehicle1==2)*Ev_Km_charge_Scenario[2]+(hh_syn$Vehicle1==3)*Ev_Km_charge_Scenario[3]+ (hh_syn$Vehicle1==4)*Ev_Km_charge_Scenario[4]+ (hh_syn$Vehicle1==5)*Ev_Km_charge_Scenario[5]
  VTK4_1=(hh_syn$Vehicle1==1)*Rebate_Scenario[1]+(hh_syn$Vehicle1==2)*Rebate_Scenario[2]+(hh_syn$Vehicle1==3)*Rebate_Scenario[3]+ (hh_syn$Vehicle1==4)*Rebate_Scenario[4]+ (hh_syn$Vehicle1==5)*Rebate_Scenario[5]
  VTK4_1=-VTK4_1
  VTK=ASC_VTK +VTK1_1 +VTK2_1+VTK3_1+VTK4_1+ Sociodemographic%*%Beta_Socio
  hh_syn$Veh_1_Km[hh_syn$newhouseholds==1]=VTK[hh_syn$newhouseholds==1]

  return(hh_syn)



}

#########################################                                   #########################################
#########################################     VehicleTransactionModel       #########################################
#########################################                                   #########################################
#' @export
VehicleTransactionModel<<-function(hh_syn,Mean_price)
{

  N_alternative=4
  ##Draws need to be taken in order to work out the shares
  set.seed(as.integer(runif(1,1,4444)));
  Rdraws_prob=matrix(runif(nrow(hh_syn)*4,0,1),nrow(hh_syn),4);
  ##For each vehicle included in the household vehicle fleet, we need to calculate the probabilities associated with the discrete alternative as per previous point
  ###Based upon the vehicle fleet, change the variables below
  MatrixAge=cbind(hh_syn$Veh_1_Age,hh_syn$Veh_2_Age,hh_syn$Veh_3_Age,hh_syn$Veh_4_Age)
  MatrixKm=cbind(hh_syn$Veh_1_Km,hh_syn$Veh_2_Km,hh_syn$Veh_3_Km,hh_syn$Veh_4_Km)
  MatrixDummy=cbind(hh_syn$Veh_1_Age,hh_syn$Veh_2_Age,hh_syn$Veh_3_Age,hh_syn$Veh_4_Age);
  ActionSave=cbind(hh_syn$Veh_1_Age,hh_syn$Veh_2_Age,hh_syn$Veh_3_Age,hh_syn$Veh_4_Age);
  ## I start off by computing the probability values for Ev
  for (k in 1:4)
  {

    km=MatrixKm[,k]/1000
    InteractionPrice=km*Mean_price;
    ageCar=MatrixAge[,k]
    XB_1_VEHICLE=cbind(ageCar,km)
    ysdummy=MatrixDummy[,k]
    ChosenDraws=Rdraws_prob[,k]

    XB_VEHICLE=cbind(hh_syn$abshid,hh_syn$n_children,hh_syn$`16-24 years`,hh_syn$`25-29 years`,hh_syn$`30-34 years`,hh_syn$`35-39 years`,hh_syn$`40-44 years`,hh_syn$`45-49 years`,hh_syn$`50-54 years`,hh_syn$`55-59 years`)
    I_ncome=hh_syn$income/1000
    XB_VEHICLE=cbind(XB_VEHICLE,hh_syn$`60-64 years`,hh_syn$`65-69 years`,hh_syn$`70-74 years`,hh_syn$`75-79 years`,hh_syn$`80-84 years`,hh_syn$`85 years and over`,hh_syn$houseEstimation,hh_syn$ApartEstimation,hh_syn$parkingspot,I_ncome)
    XB_2_VEHICLE=cbind(hh_syn$CentralCoast,hh_syn$CentralWest,hh_syn$HunterValley,hh_syn$Murray,hh_syn$Northwest,hh_syn$Riverina)
    XB_VEHICLE_TRMODEL=cbind(XB_VEHICLE,XB_1_VEHICLE,XB_2_VEHICLE,InteractionPrice)
    Beta_VEHICLE_alt1=c(0.0948418,
                        -0.538642,
                        -0.488076,
                        0,
                        0,
                        -0.253688,
                        0.357516,
                        0.487004,
                        0.369065,
                        0.254363,
                        0.642165,
                        0,
                        0.469996,
                        0,
                        0,
                        0,
                        0.190017,
                        0.0243519,
                        0.090605,
                        0,
                        0,
                        0.367402,
                        0,
                        0.903361,
                        0,
                        0.639582,
                        0,
                        0)

    Beta_VEHICLE_alt2=c( 0.094538,
                         -0.348701,
                         -0.681153,
                         -0.473097,
                         -0.465486,
                         -0.518518,
                         0,
                         0,
                         0,
                         0,
                         0.537057,
                         0.326948,
                         0.54625,
                         -0.365356,
                         0,
                         0.150523,
                         -0.243604,
                         0.0244506,
                         0.257647,
                         0.232775,
                         0.0125727,
                         0.274141,
                         0.812412,
                         0.369313,
                         0.931189,
                         0,
                         0.274889,
                         -0.009)

    Beta_VEHICLE_alt3=c(0.359738,
                        -0.451303,
                        -0.354808,
                        -0.655284,
                        0,
                        -0.850462,
                        -0.589659,
                        0,
                        0,
                        -0.211876,
                        0.634394,
                        0,
                        0,
                        0,
                        0,
                        0.590356,
                        0,
                        0.0297774,
                        0,
                        0,
                        0.0192072,
                        0.367402,
                        0,
                        0.903361,
                        0,
                        0.639582,
                        0,
                        0)

    Beta_VEHICLE_alt4=c(rep(0,length(Beta_VEHICLE_alt3)))
    Beta_matrix=rbind(Beta_VEHICLE_alt1,Beta_VEHICLE_alt2,Beta_VEHICLE_alt3,Beta_VEHICLE_alt4)
    Prob_VEHICLE_TRMODEL=matrix(99,nrow(hh_syn),N_alternative)
    a=-4.96
    #b=-4.45
    b=-6.64
    c=a*2.526805712
    ASCs=c(a,
           b,
           c,
           0)

    LogitProb=function(X,B,pmatrix,ASCs,ysdummy,Mean_price)
    {
      for (i in 1:ncol(pmatrix))
      {
        if (i==2)
          pmatrix[,i]=X%*%B[i,] +ASCs[i]
        else
          pmatrix[,i]=X%*%B[i,] +ASCs[i]
      }

      pmatrix=exp(pmatrix)
      denominator=rowSums(pmatrix)

      for (i in 1:ncol(pmatrix))
        pmatrix[,i]= pmatrix[,i]/denominator

      for (i in 1:ncol(pmatrix))
        pmatrix[,i]=ifelse(ysdummy!=99, pmatrix[,i],99)

      return(pmatrix)

    }
    Prob_VEHICLE_TRMODEL=LogitProb(XB_VEHICLE_TRMODEL[,2:ncol(XB_VEHICLE_TRMODEL)],Beta_matrix,Prob_VEHICLE_TRMODEL,ASCs,ysdummy,Mean_price)

    ########HERE I compute the probability that a household purchases a vehicle
    ##First to be reported is the matrix of explanatory variables X. Unlike the previous matrix, here I modify the last column as the geographical variables are being changed.
    XB_3_VEHICLE=cbind(hh_syn$CentralCoast,hh_syn$CentralWest,hh_syn$HunterValley,hh_syn$Murray,hh_syn$Northwest,hh_syn$Riverina,hh_syn$SouthernHighlands)
    XB_VEHICLE_buy=cbind(XB_VEHICLE,XB_1_VEHICLE,XB_3_VEHICLE)
    Beta_buy=c(0,
               0,
               0,
               0,
               0,
               0,
               0,
               0,
               0.178052,
               0.141455,
               0.210915,
               0.174035,
               0.166989,
               0,
               0,
               -0.0807079,
               -0.316799,
               0.00337235,
               0.116079,
               0.0227904,
               0.0140618,
               0,
               -0.31738,
               -0.283084,
               0,
               0.165743,
               -0.349309,
               -0.304605
    )



    #############Here I calculate the conditional probabilities
    #Prob_VEHICLE_TRMODEL[,1]= (Prob_VEHICLE_TRMODEL[,1]*  Prob_VEHICLE_buy[,1]) / (Prob_VEHICLE_TRMODEL[,2] + Prob_VEHICLE_TRMODEL[,1])
    #Prob_VEHICLE_TRMODEL[,2]= (Prob_VEHICLE_TRMODEL[,2]*  Prob_VEHICLE_buy[,1]) / (Prob_VEHICLE_TRMODEL[,2] + Prob_VEHICLE_TRMODEL[,1])
    #Prob_VEHICLE_TRMODEL[,3]= (Prob_VEHICLE_TRMODEL[,3] * Prob_VEHICLE_buy[,2]) / (Prob_VEHICLE_TRMODEL[,3] + Prob_VEHICLE_TRMODEL[,4])
    #Prob_VEHICLE_TRMODEL[,4]= (Prob_VEHICLE_TRMODEL[,4] * Prob_VEHICLE_buy[,2]) / (Prob_VEHICLE_TRMODEL[,3] + Prob_VEHICLE_TRMODEL[,4])

    ###Once the prob_ev has been calculated, we now work out the actions for each household

    ActionFunction=function(Actions,Rdraws_prob,Prob_ev,ysdummy)
    {

      Actions[,1]=ifelse(Rdraws_prob <= Prob_ev[,1],1,0)
      Actions[,2]=ifelse(Rdraws_prob > Prob_ev[,1] & Rdraws_prob <=(Prob_ev[,1] + Prob_ev[,2]) ,1,0)
      Actions[,3]=ifelse(Rdraws_prob > (Prob_ev[,1] + Prob_ev[,2]) & Rdraws_prob <=(Prob_ev[,1] + Prob_ev[,2]+ Prob_ev[,3]) ,1,0)
      Actions[,4]=ifelse(Rdraws_prob > (Prob_ev[,1] + Prob_ev[,2]+ Prob_ev[,3])  ,1,0)

      for (i in 1:ncol(Actions))
        Actions[,i]=ifelse(ysdummy!=99,  Actions[,i],99)

      return(Actions)
    }
    Actions=matrix(99,nrow(hh_syn),N_alternative)
    Actions=ActionFunction(Actions,ChosenDraws,Prob_VEHICLE_TRMODEL,ysdummy)
    Actions=data.frame(Actions)

    ###
    N_cars=c(rep(0,N_alternative))
    N_cars=colSums(Actions==1)
    NN=c(N_cars[1],N_cars[2], N_cars[3] ,N_cars[4])
    names(NN)=c("Sell no replace","Sell  replace","Sell replace (other)","No change")
    #print(NN)
    #####Create action variables
    ActionSave[,k]=ifelse(Actions[,1]==1 | Actions[,3]==1,1,
                          ifelse(Actions[,2]==1 ,2,
                                 ifelse(Actions[,4]==1,3,99)))





  }


  Prob_VEHICLE_buy=matrix(99,nrow(hh_syn),2)
  #ASC_buy=-4.85836;
  ASC_buy=-5.35836;
  BinaryLogitBuy=function(X,B,pmatrix,ASC,Mean_price)
  {

    pmatrix[,1]=X%*%B + ASC -0.005*Mean_price
    pmatrix[,1]=exp(pmatrix[,1])
    denominator=pmatrix[,1]+1

    #calculate the probability for the first alternative
    pmatrix[,1]= pmatrix[,1]/denominator
    pmatrix[,2]=1-pmatrix[,1];

    return(pmatrix)
  }

  hh_syn$Action1=ActionSave[,1]
  hh_syn$Action2=ActionSave[,2]
  hh_syn$Action3=ActionSave[,3]
  hh_syn$Action4=ActionSave[,4]

  Prob_VEHICLE_buy=BinaryLogitBuy(XB_VEHICLE_buy[,2:ncol(XB_VEHICLE_buy)],Beta_buy,Prob_VEHICLE_buy,ASC_buy,Mean_price)
  set.seed(as.integer(runif(1,1,4444)));
  R_buy=matrix(runif(nrow(hh_syn)*1,0,1),nrow(hh_syn),1);
  hh_syn$Action1_buy=ifelse(hh_syn$Action1==3 & R_buy<=Prob_VEHICLE_buy[,1] & hh_syn$`#Vehicles`<4,1,0)
  #print("New vehicles")
  #print(sum(hh_syn$Action1_buy==1))

  return(hh_syn)
}
#' @export
ApplyActions<<-function(hh_syn,ScenarioVKT,Mean_price)
{

  ########SELL NO REPLACE ###############
  hh_syn$Veh_1_Age[(hh_syn$Action1==1)]=99;
  hh_syn$Veh_1_Km[(hh_syn$Action1==1)]=99;
  hh_syn$Vehicle1[(hh_syn$Action1==1)]=0;
  hh_syn$`#Vehicles`=ifelse(hh_syn$Action1==1,hh_syn$`#Vehicles`-1,hh_syn$`#Vehicles`)
  #table(hh_syn$`#Vehicles`)

  hh_syn$Veh_2_Age[(hh_syn$Action2==1)]=99;
  hh_syn$Veh_2_Km[(hh_syn$Action2==1)]=99;
  hh_syn$Vehicle2[(hh_syn$Action2==1)]=0;
  hh_syn$`#Vehicles`=ifelse(hh_syn$Action2==1,hh_syn$`#Vehicles`-1,hh_syn$`#Vehicles`)
  #table(hh_syn$`#Vehicles`)

  hh_syn$Veh_3_Age[(hh_syn$Action3==1)]=99;
  hh_syn$Veh_3_Km[(hh_syn$Action3==1)]=99;
  hh_syn$Vehicle3[(hh_syn$Action3==1)]=0;
  hh_syn$`#Vehicles`=ifelse(hh_syn$Action3==1,hh_syn$`#Vehicles`-1,hh_syn$`#Vehicles`)
  #table(hh_syn$`#Vehicles`)

  hh_syn$Veh_4_Age[(hh_syn$Action4==1)]=99;
  hh_syn$Veh_4_Km[(hh_syn$Action4==1)]=99;
  hh_syn$Vehicle4[(hh_syn$Action4==1)]=0;
  hh_syn$`#Vehicles`=ifelse(hh_syn$Action4==1,hh_syn$`#Vehicles`-1,hh_syn$`#Vehicles`)
  #table(hh_syn$`#Vehicles`)

  ### SELL AND REPLACE

  hh_syn$Veh_1_Age=ifelse(hh_syn$Action1==2,0,hh_syn$Veh_1_Age)
  hh_syn$Veh_1_Km=ifelse(hh_syn$Action1==2,VKT(hh_syn,ScenarioVKT,1,Mean_price),hh_syn$Veh_1_Km)

  hh_syn$Veh_2_Age=ifelse(hh_syn$Action2==2,0,hh_syn$Veh_2_Age)
  hh_syn$Veh_2_Km=ifelse(hh_syn$Action2==2,VKT(hh_syn,ScenarioVKT,2,Mean_price),hh_syn$Veh_2_Km)

  hh_syn$Veh_3_Age=ifelse(hh_syn$Action3==2,0,hh_syn$Veh_3_Age)
  hh_syn$Veh_3_Km=ifelse(hh_syn$Action3==2,VKT(hh_syn,ScenarioVKT,3,Mean_price),hh_syn$Veh_3_Km)

  hh_syn$Veh_4_Age=ifelse(hh_syn$Action4==2,0,hh_syn$Veh_4_Age)
  hh_syn$Veh_4_Km=ifelse(hh_syn$Action4==2,VKT(hh_syn,ScenarioVKT,4,Mean_price),hh_syn$Veh_4_Km)

  ###NO CHANGE - NO BUY

  hh_syn$Veh_1_Age=ifelse(hh_syn$Action1==3 & hh_syn$Action1_buy==0 ,hh_syn$Veh_1_Age+1,hh_syn$Veh_1_Age)
  hh_syn$Veh_1_Km=ifelse(hh_syn$Action1==3 & hh_syn$Action1_buy==0,hh_syn$Veh_1_Km*1.05,hh_syn$Veh_1_Km)

  hh_syn$Veh_2_Age=ifelse(hh_syn$Action2==3 ,hh_syn$Veh_2_Age+1,hh_syn$Veh_2_Age)
  hh_syn$Veh_2_Km=ifelse(hh_syn$Action2==3 ,hh_syn$Veh_2_Km*1.05,hh_syn$Veh_2_Km)

  hh_syn$Veh_3_Age=ifelse(hh_syn$Action3==3 ,hh_syn$Veh_3_Age+1,hh_syn$Veh_3_Age)
  hh_syn$Veh_3_Km=ifelse(hh_syn$Action3==3 ,hh_syn$Veh_3_Km*1.05,hh_syn$Veh_3_Km)

  hh_syn$Veh_4_Age=ifelse(hh_syn$Action4==3 ,hh_syn$Veh_4_Age+1,hh_syn$Veh_4_Age)
  hh_syn$Veh_4_Km=ifelse(hh_syn$Action4==3 ,hh_syn$Veh_4_Km*1.05,hh_syn$Veh_4_Km)


  ##NO CHANGE - BUY
  ### Buy( Add a car)
  hh_syn$check=ifelse(hh_syn$Action1==3 & hh_syn$Action1_buy==1 & hh_syn$`#Vehicles`<4  & hh_syn$Veh_1_Age==99,1,
                      ifelse(hh_syn$Action1==3 & hh_syn$Action1_buy==1 & hh_syn$`#Vehicles`<4  & hh_syn$Veh_2_Age==99,2,
                             ifelse(hh_syn$Action1==3 & hh_syn$Action1_buy==1 & hh_syn$`#Vehicles`<4  & hh_syn$Veh_3_Age==99,3,
                                    ifelse(hh_syn$Action1==3 & hh_syn$Action1_buy==1 & hh_syn$`#Vehicles`<4  & hh_syn$Veh_4_Age==99,4,0))))

  #table(hh_syn$check)


  hh_syn$Veh_2_Age[(hh_syn$check==2)]=0
  hh_syn$Veh_2_Km=ifelse(hh_syn$check==2,VKT(hh_syn,ScenarioVKT,2,Mean_price),hh_syn$Veh_2_Km)
  hh_syn$`#Vehicles`=ifelse(hh_syn$check==2,hh_syn$`#Vehicles`+1,hh_syn$`#Vehicles`)
  #table(hh_syn$`#Vehicles`)

  hh_syn$Veh_3_Age[(hh_syn$check==3)]=0
  hh_syn$Veh_3_Km=ifelse(hh_syn$check==3,VKT(hh_syn,ScenarioVKT,3,Mean_price),hh_syn$Veh_3_Km)
  hh_syn$`#Vehicles`=ifelse(hh_syn$check==3,hh_syn$`#Vehicles`+1,hh_syn$`#Vehicles`)
  #table(hh_syn$`#Vehicles`)

  hh_syn$Veh_4_Age[(hh_syn$check==4)]=0
  hh_syn$Veh_4_Km=ifelse(hh_syn$check==4,VKT(hh_syn,ScenarioVKT,4,Mean_price),hh_syn$Veh_4_Km)
  hh_syn$`#Vehicles`=ifelse(hh_syn$check==4,hh_syn$`#Vehicles`+1,hh_syn$`#Vehicles`)
  return(hh_syn)
}

