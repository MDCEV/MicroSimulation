#########################################                                   #########################################
#########################################                                   #########################################
#########################################         doit                      #########################################
#########################################                                   #########################################
#########################################                                   #########################################

doit<<-function(listcsv,Sources,house_attribute,attr_private,attr_punblic)
{
  ####Output Matrixes
  Market_shares_overtime=matrix(NA,length(listcsv),6)
  VKT_overtime=matrix(NA,length(listcsv),6)
  VKT_overtime_Mean=matrix(NA,length(listcsv),1)
  uptake_charger_house=c(rep(0,length(listcsv)))
  apartment=matrix(NA,21,3)
  #Rdraws_KmDriven_years=matrix(runif(nrow(hh_syn)*4,0,1),nrow(hh_syn),4)
  count=2020;
  start_time <- Sys.time()
  for (t in 2:length(listcsv))
  {
    if (count==2020)
    {
      #Read 2020

      hh_syn= (fread(listcsv[t-1], nThread = 8));
      #Read the scenario Exelspreadsheet
      Scenario_VKT=(Read_Scenario_VKT(as.vector(as.data.frame(Sources[[9]])) [t-1,]));
      ##Save statiscs
      Market_shares_overtime[t-1,]=OutPut_MarketShare(hh_syn)
      VKT_overtime[t-1,]=OutPut_VKT(hh_syn)
      VKT_overtime_Mean[t-1,1]=OutPut_VKT_Mean(hh_syn)
      uptake_charger_house[t-1]=sum(hh_syn$charger_house)
      apartment[t-1,]=c(sum( hh_syn$charger_apa_new==1),sum( hh_syn$charger_apa_new==2),sum( hh_syn$charger_apa_new==1)+sum( hh_syn$charger_apa_new==2))
      ##UpdateFunction
      hh_syn_base=Read2020(hh_syn,Scenario_VKT)
      hh_syn=NULL;
      count=2020+1;
      cat("Year 2020","\n")
    }

    hh_syn=fread(listcsv[t], nThread = 8)
    hh_syn=UpdateFunction(hh_syn,hh_syn_base)
    hh_syn=Infrastructure(hh_syn,hh_syn_base,t-1,house_attribute,attr_private,attr_punblic)
    hh_syn=Fueltype(hh_syn,Sources,t)
    Market_shares_overtime[t,]=OutPut_MarketShare(hh_syn)
    VKT_overtime[t,]=OutPut_VKT(hh_syn)
    VKT_overtime_Mean[t,1]=OutPut_VKT_Mean(hh_syn)
    uptake_charger_house[t]=sum(hh_syn$charger_house)
    apartment[t,]=c(sum( hh_syn$charger_apa_new==1),sum( hh_syn$charger_apa_new==2),sum( hh_syn$charger_apa_new==1)+sum( hh_syn$charger_apa_new==2))
    hh_syn_base=VKT_update(hh_syn,Scenario_VKT)
    Scenario_VKT=(Read_Scenario_VKT(as.vector(as.data.frame(Sources[[9]])) [t,]));
    hh_syn_base=ReadYears(hh_syn,Scenario_VKT)
    hh_syn=NULL;
    cat("Year ",count, "\n" )
    count=count+1;

  }

  hh_syn=NULL;
  hh_syn_base=NULL;

  end_time <- Sys.time()
  Runtime=end_time - start_time;
  cat("Run time",Runtime, "\n")

  rownames(Market_shares_overtime)=c(2020:2040);
  colnames(Market_shares_overtime)=c("Petrol",	"Diesel"	,"Hybrid-electric",	"Plug-in hybrid-electric",	"Battery electric"	,"Natural gas")
  print(Market_shares_overtime)

  colnames(VKT_overtime)=c("Petrol",	"Diesel"	,"Hybrid-electric",	"Plug-in hybrid-electric",	"Battery electric"	,"Natural gas")
  rownames(VKT_overtime)=c(2020:2040)
  #print(VKT_overtime)

  rownames(VKT_overtime_Mean)=c(2020:2040)
  colnames(VKT_overtime_Mean)=c("Average VKT")

  cat("Infrastructure:Houses","\n")
  uptake_charger_house=t(t(uptake_charger_house));
  rownames(uptake_charger_house)=c(2020:2040)
  colnames(uptake_charger_house)=c("Houses")
  cat("Infrastructure:Apartments","\n")
  colnames(apartment)=c("Communal Bays","Private spots","Total")
  rownames(apartment)=c(2020:2040);
  #print(apartment,"\n")
  list_of_datasets <- list("Vehicle Market" = Market_shares_overtime, "VKT" = VKT_overtime,
                           "Mean_VKT" =VKT_overtime_Mean,"Infrastructur_Houses" =uptake_charger_house,
                           "Infrastructure_Apartments" =apartment);
    write.xlsx(list_of_datasets, file = "Microsimulation.xlsx", row.names=TRUE)
  cat("Excel files are saved in your folder")
  Market_shares_overtime=NULL;
  VKT_overtime=NULL;
  VKT_overtime_Mean=NULL;
  apartment=NULL;
  uptake_charger_house=NULL;

}
