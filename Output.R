
#########################################                                   #########################################
#########################################        OutPut                     #########################################
#########################################                                   #########################################

#' @export
OutPut_MarketShare<<-function(hh_syn)
{

  A=hh_syn$Vehicle1;
  B=hh_syn$Vehicle2;
  C=hh_syn$Vehicle3;
  D=hh_syn$Vehicle4;

  AA=c(sum(A==1),sum(A==2),sum(A==3),sum(A==4),sum(A==5),sum(A==6))
  BB=c(sum(B==1),sum(B==2),sum(B==3),sum(B==4),sum(B==5),sum(B==6))
  CC=c(sum(C==1),sum(C==2),sum(C==3),sum(C==4),sum(C==5),sum(C==6))
  DD=c(sum(D==1),sum(D==2),sum(D==3),sum(D==4),sum(D==5),sum(D==6))
  AA=rbind(AA,BB,CC,DD)
  Market_shares_overtime=c(colSums(AA))

  names(Market_shares_overtime)=c("Petrol",	"Diesel"	,"Hybrid-electric",	"Plug-in hybrid-electric",	"Battery electric"	,"Natural gas")
  #rownames(Market_shares_overtime)=c(2021:2040)
  #round(as.data.frame(Market_shares_overtime),3)
  #print(Market_shares_overtime)
  return(Market_shares_overtime)


}
#' @export
OutPut_VKT<<-function(hh_syn)
{


  A=hh_syn$Veh_1_Km;
  B=hh_syn$Veh_2_Km;
  C=hh_syn$Veh_3_Km;
  D=hh_syn$Veh_4_Km;

  AA=c(sum(A[hh_syn$Vehicle1==1]),sum(A[hh_syn$Vehicle2==1]),sum(A[hh_syn$Vehicle1==3]),sum(A[hh_syn$Vehicle1==4]),sum(A[hh_syn$Vehicle1==5]),sum(A[hh_syn$Vehicle1==6]))
  BB=c(sum(B[hh_syn$Vehicle2==1]),sum(B[hh_syn$Vehicle2==1]),sum(B[hh_syn$Vehicle2==3]),sum(B[hh_syn$Vehicle2==4]),sum(B[hh_syn$Vehicle2==5]),sum(B[hh_syn$Vehicle2==6]))
  CC=c(sum(C[hh_syn$vehicle3==1]),sum(C[hh_syn$vehicle3==1]),sum(C[hh_syn$vehicle3==3]),sum(C[hh_syn$vehicle3==4]),sum(C[hh_syn$vehicle3==5]),sum(C[hh_syn$Vehicle3==6]))
  DD=c(sum(D[hh_syn$vehiDle4==1]),sum(D[hh_syn$vehiDle4==1]),sum(D[hh_syn$vehiDle4==3]),sum(D[hh_syn$vehiDle4==4]),sum(D[hh_syn$vehiDle4==5]),sum(D[hh_syn$Vehicle4==6]))
  AA=rbind(AA,BB,CC,DD)
  VKT_sum=c(colSums(AA))
  names(VKT_sum)=c("Petrol",	"Diesel"	,"Hybrid-electric",	"Plug-in hybrid-electric",	"Battery electric"	,"Natural gas")
  #rownames(Market_shares_overtime)=c(2021:2040)
  #round(as.data.frame(Market_shares_overtime),3)
  #print(VKT_sum)
  return(VKT_sum)



}
#' @export
OutPut_VKT_Mean<<-function(hh_syn)
{

  mylist=c(hh_syn$Veh_1_Km[hh_syn$Vehicle1!=0],hh_syn$Veh_2_Km[hh_syn$Vehicle2!=0],hh_syn$Veh_3_Km[hh_syn$Vehicle3!=0],hh_syn$Veh_4_Km[hh_syn$Vehicle4!=0])
  #print(mean(mylist))
  return(mean(mylist))

}

#' @export
OutPut_MarketShare_NewVehiclesOnly<<-function(hh_syn)
{

  A=ifelse(hh_syn$Action1==2  | hh_syn$Action1_buy==1,hh_syn$Vehicle1,0)
  B=ifelse(hh_syn$Action2==2  | hh_syn$Action1_buy==1,hh_syn$Vehicle2,0)
  C=ifelse(hh_syn$Action3==2  | hh_syn$Action1_buy==1,hh_syn$Vehicle3,0)
  D=ifelse(hh_syn$Action4==2  | hh_syn$Action1_buy==1,hh_syn$Vehicle4,0)

  AA=c(sum(A==1),sum(A==2),sum(A==3),sum(A==4),sum(A==5),sum(A==6))
  BB=c(sum(B==1),sum(B==2),sum(B==3),sum(B==4),sum(B==5),sum(B==6))
  CC=c(sum(C==1),sum(C==2),sum(C==3),sum(C==4),sum(C==5),sum(C==6))
  DD=c(sum(D==1),sum(D==2),sum(D==3),sum(D==4),sum(D==5),sum(D==6))
  AA=rbind(AA,BB,CC,DD)
  Market_shares_overtime_NEW=c(colSums(AA))

  names(Market_shares_overtime_NEW)=c("Petrol",	"Diesel"	,"Hybrid-electric",	"Plug-in hybrid-electric",	"Battery electric"	,"Natural gas")
  #rownames(Market_shares_overtime)=c(2021:2040)
  #round(as.data.frame(Market_shares_overtime),3)
  #print(Market_shares_overtime)
  return(Market_shares_overtime_NEW)

}
