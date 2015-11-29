#include "fun_head.h"

MODELBEGIN


/******************************************
*
*	DEMAND
*
******************************************/


EQUATION("x")
/*
Characteristics' values, constant unless they are a of a specified Id.
*/

v[0]=V("IdCh");
if(v[0]==1)
 v[1]=VLS(p->up->up,"price",1);
else
 {
  v[1]=CURRENT;
 } 
RESULT(v[1] )


EQUATION("TotIterations")
/*
Total number of iterations for a class
Thios is computed separately, in order to take into account the redistribution of expenditure due to missing goods, and avoid that expenditure level is modified by rounding the number of iterations to an integer
*/

v[5]=0;
CYCLE(cur, "Need")
 {
  v[1]=VS(cur,"ProdExists");
  if(v[1]==1)
   {
    v[2]=VS(cur,"NumIterations");
    v[3]=VS(cur,"TempIterations");
    v[4]=v[2]+v[3];
    v[5]+=v[4];
   }
 }

RESULT(v[5] )




EQUATION("TTB_multiplWinner")
/*

Standard TTB, but all the winners remain marking "app" to 1, and returning the number of winners.

Choose one option on the base of their characteristics "x", using "tau" as tolerance and "Delta" as random bias of the observed characteristics' values.

Consumers' preferences are the order by which the characteristics are used to filter out (perceived) inferior products.

For each characteristics, the perceived value for each option is a random value drawn from a normal centered around the true value and variance equal to Delta.

All options scoring more than tau times the max value for the current characteristics are considered equivalent to the maximum, and therefore not filtered away.

Before starting the selection procedure, options are scanned to remove those scoring less than a minimum on some characteristic.
*/

V("MinimumPriceSet"); // make sure the minimum parameter for the price characteristic has been set
v[0]=0;
v[31]=VS(c->up,"Expenditure")/VS(c->up,"TotIterations"); //amount to spend on this iteration
v[30]=VS(c,"IdNeed");
//cur4=SEARCH_CND("IdSector",v[30]); // search for the sector that produces the good that satisfies the need that the iteration is buying
v[38]=0;
CYCLE(cur, "Firm")
 { // compute the number of firms producing the product that satisfy the need the consumer is looking for
  v[37]=VS(cur,"product"); // the product (addressing a particular need) the firm is producing
  if(v[37]==v[30])
    v[38]++; 
 }
//v[35]=SUMS(cur4,"IdFirm"); // check if there is any firm producing the good
if(v[38]<1)
 { // if there is no firm in the sector
  v[36]=INCRS(c->up,"NoConsumption",v[31]); // add this expenditure to the savings
  END_EQUATION(-2); // exit the purchase and move to next one
 }
 // in all other cases continue the purhase

//select out the options scoring less than the minimum on some characteristic
CYCLE(cur, "Firm")
{
 v[24]=1; //assume the option to be viable
 //cur3=SEARCH_CNDS(cur,"IdPNeed",v[30]);
 v[37]=VS(cur,"product");
 if(v[37]!=v[30])
  {// if the the firm does not produce the product the consumer is looking for, exclude it from the avaialble options
   WRITES(cur,"app",0);
  }
 else
  { // if the firm produces the required good
   CYCLES(cur, cur1, "Ch")
    {//for any characteristic
     v[20]=VS(cur1,"IdCh");
     cur2=SEARCH_CNDS(c,"IdDCh",v[20]); //find the ch. of the option you are browsing
     v[4]=VS(cur2,"Delta"); // observation error in the quality of the good
  
     v[21]=VS(cur2,"Minimum"); // this is the minimum for the consumer on this characteristic
     v[22]=VS(cur1,"x");
     v[23]=norm(v[22],v[4]*v[22]); //this is the observed value
     WRITES(cur1,"obs_x",v[23]); //write the observed value   
     v[25]=VS(cur2,"NegativeQuality"); //
     if(v[21]*v[25]>v[23]*v[25]) //
      { //option too low
       v[24]=0;
       break;
      }
  
    }
    if(v[24]==1)
     { //option viable
      WRITES(cur,"app",1);
      v[0]++;
     }
    else
      WRITES(cur,"app",0);
  }
}	
if(v[0]==0) //no option viable
 {
  v[34]=INCRS(c->up,"NoConsumption",v[31]); /// given that the expenditure for the class for the current period is already computed, add the non consumed income to savings
  END_EQUATION(-1);
 }
 
if(v[0]==1)
 {//only one option viable, choose it outrightly
  cur=SEARCH_CND("app",1);
  INCRS(cur,"MonetarySales",v[31]);
  END_EQUATION(1);
 } 
  
//Do a proper choice among the (more than one) viable options

//INTERACT("First", v[0]);

CYCLES(c, cur, "DCh")
 {//for each characteristic, in the order of the decision maker
  v[27]=1;
  v[1]=VS(cur,"IdDCh");
  v[25]=VS(cur,"NegativeQuality"); // to control for negative value of quality such as pice
  v[3]=VS(cur,"tau"); // the tolerance parameter

  CYCLE(cur1, "Firm")
   { //find the (observed) maximum in respect of IdDCh, excluding the non viable, or already removed, options
    v[7]=VS(cur1,"app");
    if(v[7]==1)
     {//if it is still active (a potential choice)
     //cur3=SEARCH_CNDS(cur1,"IdPNeed",v[30]);
     cur2=SEARCH_CNDS(cur1,"IdCh",v[1]); //find the ch. of the option
     v[5]=VS(cur2,"obs_x");//read its value
     if(v[5]<0)
      v[5]=0; //negative values mess up the threshold system. Only positive values can be considered.
     if(v[27]==1)
      {
       v[27]=0;
       v[6]=v[5];
      } 
     if(v[5]*v[25]>v[6]*v[25])
      v[6]=v[5];//record in v[6] the maximum (observed) value
     WRITES(cur1,"curr_x",v[5]); //write the observed value
     } 
    else 
     WRITES(cur1,"curr_x",-1); //write a default value for non-active options
   }
  CYCLE(cur1, "Firm")
   { //second cycle: remove options below maximum * tau
    v[7]=VS(cur1,"app");
    if(v[7]==1)
    {
     v[8]=VS(cur1,"curr_x");
     if(v[25]==-1)
      v[33]=1/v[3];
     else
      v[33]=v[3]; 
     if(v[8]*v[25]<v[6]*v[33]*v[25])
      {//too low value: remove
       WRITES(cur1,"app",0);
       v[0]--;
      }
    }
   }
//INTERACT("Subsequ", v[0]);
 }
if(v[0]==0)
 INTERACT("No firms left",v[0]);//error control: v[0] must be >=1

CYCLE(cur, "Firm")
 {
  v[32]=VS(cur,"app");
  if(v[32]==1)
   INCRS(cur,"MonetarySales",v[31]/v[0]);
 }

RESULT(v[0] )


EQUATION("Trade")
/*
Set a trading cycle:
- initialize "sales" to zero in firms;
- compute the sales for each firm as the total of classes and needs 
*/
V("ExpectedSales"); // given that "UnitSales" is a parameter, firms need first to compute expected sales to use the value of "UnitSales" from t-1 (see the "ExpectedSales" equation)
CYCLE(cur1, "Supply")
 {
  CYCLES(cur1, cur, "Firm")
  {
   v[1]=VS(cur,"MonetarySales");
   WRITES(cur,"MonetarySalesL",v[1]); // before setting the sales to 0 for the current period computation, register the lagged value of monetary sales
   v[2]=VS(cur,"UnitSales");
   WRITES(cur,"UnitSalesL",v[2]); // before setting the unit sales to 0 for the current computation, register the lagged value
   WRITES(cur,"MonetarySales",0);
   WRITES(cur,"UnitSales",0);
  }
 }

CYCLE(cur, "Class")
 {
  v[4]=VS(cur,"Expenditure");
  WRITES(cur,"NoConsumption",0); // after having computed the expenditure set the non expenditure to 0, to be computed again inthis period for the following period expenditures
  v[14]=v[21]=0;
  CYCLES(cur, cur1, "Need")
   { // make a first cycle through needs to check which products are available in the market
    v[10]=VS(cur1,"IdNeed");
    v[12]=0;
    CYCLE(cur2, "Supply")
     {
      CYCLES(cur2, cur3, "Firm")
       { // cycle through all firms
        v[11]=VS(cur3,"product");
        if(v[11]==v[10]) //count how many firms sell the product needed
         v[12]++;
       }

     }
    if(v[12]<1)
     {
      v[13]=VS(cur1,"NumIterations");
      v[14]+=v[13];
      v[20]=VS(cur1,"Share");
      v[21]+=v[20];
      WRITES(cur1,"ProdExists",0); // write that the product for the need does not exists
     }
    else
     WRITES(cur1,"ProdExists",1);

   }
  CYCLES(cur, cur1, "Need")
   { // cycle again to redistribute the iterations to the needs for which a product exists
    v[15]=VS(cur1,"Share"); // use the share
    v[22]=1-v[21];
    v[23]=v[15]/v[22];
    v[16]=v[23]*v[14]; // compute the number of iterations non sde from the other needs
    v[17]=round(v[16]); // round them
    WRITES(cur1,"TempIterations",v[17]);
   }


  CYCLES(cur, cur1, "Need")
   {
    v[19]=VS(cur1,"ProdExists");
    if(v[19]==1)
     {
      v[0]=0;   
      //v[5]=VS(cur1,"Share");
      //v[6]=v[5]*v[4];  
      v[9]=VS(cur1,"NumIterations");
      v[18]=VS(cur1,"TempIterations");
   
      for(v[8]=0; v[8]<v[9]+v[18]; v[8]++)
        V_CHEAT("TTB_multiplWinner", cur1);
     }
   }
 }

CYCLE(cur1, "Supply")
 {  
  CYCLES(cur1, cur, "Firm")
  {
   v[5]=VS(cur,"MonetarySales");
   v[6]=VLS(cur,"price",1);
   if(v[5]>0)
     WRITES(cur,"UnitSales",v[5]/v[6]);
   else
     WRITES(cur,"UnitSales",0); 
  }
}

RESULT( 1)


EQUATION("Stocks")
/*
Stocks
*/

V("Trade");
v[0]=VL("Stocks",1);
v[1]=V("Q");
v[3]=V("UnitSales");
v[6]=V("backlog");

v[5]=v[0]+v[1]-v[3]-v[6];
if(v[5]<0)
 {//increase backlog
 v[6]=INCR("backlog",-v[0]-v[1]+v[3]);
 v[5]=0;
 }
if(v[5]>0 && v[6]>0)
 WRITE("backlog",0);
 
RESULT(v[5] )


EQUATION("DesiredQ")
/*
Production, as a function of the difference between past stocks and desired ones
*/
V("Trade");
v[0]=VL("Stocks",1);
v[1]=V("DesiredStocks"); // percentage of expected sales firms want to stock to face unexpected demand
v[2]=V("backlog");
v[4]=V("ExpectedSales");
v[3]=v[1]*v[4];
v[6]=max(0,v[3]+v[2]+v[4]-v[0] );

RESULT(v[6])


EQUATION("ExpectedSales")
/*
Smoothed level of sales
*/
v[0]=VL("ExpectedSales",1);
v[1]=VL("UnitSales",1);
v[2]=V("aES");
v[3]=v[0]*v[2]+(1-v[2])*v[1];

RESULT(v[3] )


EQUATION("Q")
/*
Actual production, which is the minimum between desired production and constraints
*/

v[0]=V("DesiredQ");
v[1]=V("LaborCapacity");
v[2]=min(v[0],v[1]);
v[3]=V("CapitalCapacity");
v[5]=min(v[3],v[2]);

RESULT(v[5] )


EQUATION("LaborCapacity")
/*
Maximum capacity allowed by currently employed workers on current capital stock
*/
v[0]=0;
v[1]=V("MaxLaborProductivity");
v[2]=VL("NumWorkers",1); // refers only to first layer workers, not to executives
v[4]=v[2]*v[1];

RESULT(v[4] )


EQUATION("NumWorkersFirm")
/*
Ttal number of workers in the firm
*/

v[1]=SUM("NumWorkers");

RESULT(v[1] )


EQUATION("NumWorkers")
/*
Number of workers, computed as the 120% of the workers required to fill expected sales
*/

v[15]=0;
CYCLES(p->up, cur, "Labor")
 { //check how many tiers already exist
  v[15]++;
 }
v[14]=V("IdLabor");
if(v[14]==1)
 { // compute the first tier workers given their productivity and production needs
  //V("aNWDynamic");
  v[0]=VL("NumWorkers",1);
  //v[1]=V("ExpectedSales");
  v[1]=V("DesiredQ");
  //v[10]=V("backlog");
  //v[11]=v[1]+v[10];
  v[8]=V("CapitalCapacity");
  v[9]=min(v[1],v[8]);
  v[2]=VL("MaxLaborProductivity",1);
  v[4]=V("DesiredUnusedCapacity");
  v[12]=VL("SkillIntensity",1); 
  v[7]=v[2]/v[12]; // suppressed for the moment the use of skill bias, which should become a continuous variable, rather than a dummy
  v[3]=v[4]*(v[9]/v[2]);
  v[5]=V("aNW");
  v[6]=v[0]*v[5]+(1-v[5])*v[3]; // number of workers in the first layer
 }

else
 {// when above the first tier workers...
  v[18]=V("IdLabor");
  cur=SEARCH_CNDS(p->up,"IdLabor",v[18]-1);
  v[23]=VS(p->up,"IdFirm");
  v[24]=VS(cur->up,"IdFirm");
  if(v[23]!=v[24])
   v[25]=INTERACT("the firm ID is different from the current firm", v[24]);
  v[21]=VS(cur,"nu"); //given the worker ratio between tiers (defined by the tier below)
  v[19]=VS(cur,"NumWorkers"); //and the number of workers in the previous tier
  v[6]=v[19]/v[21]; // compute the required executives for the current tier
  v[17]=V("nu");
  if(v[6]>=v[17] && v[18]==v[15])
   { // if they are above the workers ratio of this tier and this is the last tier, create a new working class
    cur1=ADDOBJS_EX(p->up,"Labor",p);
    WRITES(cur1,"IdLabor",v[18]+1);
    v[20]=v[6]/v[17];
    WRITELS(cur1,"NumWorkers",v[20], t);
    WRITELLS(cur1,"NumWorkers",0, t,1); // write also that the number of workers in the previous period is equal to 0, as it is used in the inequality statistics
    v[22]=0;
    CYCLES(p->up->up->up,cur2, "Class")
     {
      v[22]++;
     }
    if(v[22]-1<v[18]+1)
     { //starting from the second class (the first are engineers), if it does not exist a class that represnt the new layer of executives, create it
      cur2=SEARCH_CNDS(p->up->up->up,"NumClass",v[18]);
      cur3=ADDOBJS_EX(cur2->up,"Class",cur2);
      WRITES(cur3,"NumClass",v[18]+1);
      WRITELS(cur3,"Expenditure",0, t-1);
      WRITELS(cur3,"ShareWageIncome",0, t-1);
      WRITELS(cur3,"ShareNonWageIncome",0, t-1);
      //WRITELS(cur3,"ShareProfitIncome",0, t-1);
      WRITELS(cur3,"Savings",0, t-1);
      // WRITELS(cur3,"NumIterations",0, t-1); to reactivate when NumIterations report the number of consumers in the labour class, as given in equation "ShareWageIncome". Otherwise the number of iterations simply define the number of representative conusmers (groups) in a class
      WRITELS(cur3,"ShareIncome",0, t-1); // reset the share income to be recomputed
      WRITES(cur3,"Individuals",v[20]); // set the number of individuals to nu;ber of workers of the new class
      v[35]=VS(cur2,"LorenzInd"); 
      WRITES(cur3,"LorenzInd",v[35]+v[20]); // set total number of workers as previous total plus new workers
      WRITELS(cur3,"SavingsTot",0, t-1); // set the total savings to 0
      WRITELS(cur3,"NoConsumption",0, t-1); // set the savongs due to unavailability of the good to 0
      CYCLES(cur3, cur, "Need")
       { // enter in neds and characterisitcs to change the tau parameter (the minimum is set every period)
        v[27]=VS(cur,"IdNeed");
        CYCLES(cur, cur1, "DCh")
         {
          v[26]=VS(cur1,"IdDCh");
          CYCLES(cur2, cur4, "Need")
           { // cycle among the needs of the preceding class
            v[28]=VS(cur4,"IdNeed");
            if(v[28]==v[27])
             { // when in the same need as the one the new class is cycling cycle through the char of the preceding class
              CYCLES(cur4, cur5, "DCh")
               { 
                v[29]=VS(cur5,"IdDCh");
                if(v[29]==v[26])// when in the same characteristic the new class is cycling
                 v[30]=VS(cur5,"tau"); // read the value of the tau parameter
               }

             }
           }
          v[31]=VS(cur1,"tauMultiplier"); // the speed of adjustment of the tolerance level from one class to the following one
          v[34]=VS(cur1,"NegativeQuality");
          if(v[34]>0)
           v[33]=VS(p->up->up->up,"tauMax"); // the asympthotic level of the tolerance level (1 for qualities and 0 for price)
          if(v[34]<0)
           v[33]=VS(p->up->up->up,"tauMin"); // the asympthotic level of the tolerance level (1 for qualities and 0 for price)
          v[32]=v[30]*(1-v[31])+v[31]*v[33]; // adjustment in the treshold level of tolerance
          WRITES(cur1,"tau",v[32]); // finally write the tau for the new consumer class in each of its characteristic for each need

         }

       }
     v[35]=VS(cur3,"ComputeShare"); // set the distribution of expenditure shares across needs for the new class

     }

   }
  if(v[18]>2 && v[19]<v[21])
   v[6]=0;
 }

RESULT(v[6] )


EQUATION("MinimumPriceSet")
/*
System equation that mantains the `minimum' parameter for the price characteristic, of the first tyer working class and engineers, above the minimum price among firms (times the variance of the consumer percetion error). 
It also sets the Minimum parameter across needs for the price
*/


v[4]=1000000;
CYCLE(cur, "Supply")
 {
  CYCLES(cur, cur1, "Firm")
   {
    v[3]=VLS(cur1,"price",1);
    if(v[3]<v[4])
     v[4]=v[3];
   }

 }
v[10]=V("AvWagecoeff");
CYCLE(cur, "Demand")
 {
  CYCLES(cur, cur1, "Class")
   {
    v[1]=VS(cur1,"NumClass");
    if(v[1]<=1)
     { // start with the engineers and the first tier working class (they both set their minimum independently)
      CYCLES(cur1, cur2, "Need")
       {
        CYCLES(cur2, cur3, "DCh")
         {
          v[2]=VS(cur3,"IdDCh");
          if(v[2]==1)
           { // maintain the minimum above the minimum of the characteritic (price) only for the fist (price) characteritic
            v[5]=VS(cur3,"Minimum"); // check the initialised value of the minimum
            v[6]=VS(cur3,"Delta"); // variance of the distribution of the perceived characteritic around the actaul value
            v[7]=v[6]*v[4];
            v[8]=max(v[5],v[7]); // use either the initially set minimum or the price of the cheapest firm
            WRITES(cur3,"Minimum",v[8]);
           }
         }

       }

     }
    if(v[1]>1)
     { // layers above the first one (different types of executives)
      CYCLES(cur1, cur2, "Need")
       {
        CYCLES(cur2, cur3, "DCh")
         {
          v[9]=VS(cur3,"IdDCh");
          if(v[9]==1)
           {
            cur4=SEARCH_CNDS(cur,"NumClass",v[1]-1);
            CYCLES(cur4, cur5, "DCh")
             { // cycle the DCh for any need (we assume that the minimum for price is equal across needs)
              v[11]=VS(cur5,"IdDCh");
              if(v[11]==1)
               v[12]=VS(cur5,"Minimum"); // read the minimum par of the previsous class for char price
             }
            v[8]=v[12]*v[10];
            WRITES(cur3,"Minimum",v[8]); // write the value of the parameter, as the previous Minimum times the multiplier
           }
         }

       }

     }
   }

 }


RESULT(v[8] )


EQUATION("AvWagecoeff")
/*
Average of the wage multiplier, in case they are different between firms (or capital and final good firms), to be used as a multiplier of the `minimum' parameter for consumers classes of executives
*/

v[3]=v[4]=0;
CYCLE(cur, "Supply")
 {
  CYCLES(cur, cur1, "Firm")
   {
    CYCLES(cur1, cur2, "Labor")
     {
      v[1]=VS(cur2,"IdLabor");
      if(v[1]>1)
       {
        v[2]=VS(cur2,"wagecoeff");
        v[3]+=v[2];
        v[4]++;
       }
     }

   }

 }
CYCLE(cur, "Machinery")
 {
  CYCLES(cur, cur1, "KFirm")
   {
    CYCLES(cur1, cur2, "KLabor")
     {
      v[5]=VS(cur2,"IdKLabor");
      if(v[5]>1)
       {
        v[6]=VS(cur2,"KWagecoeff");
        v[3]+=v[6];
        v[4]++;
       }
     }

   }

 }
v[7]=v[3]/v[4];

RESULT(v[7] )


EQUATION("Expenditure")
/*
Total money spent by consumers, computed as a combination of past consumption and available resources (from wages and stock options)
*/
v[0]=VL("Expenditure",1);
v[1]=VLS(p->up->up, "WageIncome",1);
v[3]=VL("ShareWageIncome",1);
v[4]=VLS(p->up->up, "NonWageIncome",1);
v[5]=VL("ShareNonWageIncome",1);
v[13]=VS(p->up->up,"ExIncome"); // exogenous income
v[14]=V("ShareExIncome"); // share of exogenous income (should be modelled to change while classes increase)
//v[7]=V("ShareProfitIncome");
v[9]=VL("SavingsTot",1);
v[11]=V("Savings");
v[12]=V("NoConsumption");
v[2]=V("aEx");
v[10]=v[0]*v[2]+(1-v[2])*(v[1]*v[3]+v[4]*v[5]+v[12]+v[5]*v[13]);
//v[10]=v[0]*v[2]+(1-v[2])*(v[1]*v[3]+v[4]*v[5]+v[12]+v[11]);

v[8]=v[2]*(v[1]*v[3]+v[4]*v[5]-v[0]); // residual (excess consumption) income not consumed (overconsumed) due to the smoothing factor, increase (decreases) savings
//v[8]=v[2]*(v[1]*v[3]+v[4]*v[5]-v[0]+v[11]);
WRITE("Savings",v[8]); // saving are first computed here as smoothed consumption, and in the ollowing step during the puchase, in case a need cannot be satisfied

RESULT(v[10] )


EQUATION("SavingsTot")
/*
Total amount of savings, which sums:
the amount of available income (wage, non wage, and past savings) not consumed due to smoothing
the amount of income not used due to non availability of the good
*/

V("Trade");
v[1]=V("Savings"); // smoothed consumption
v[2]=V("NoConsumption"); // good not available to satisfy a specific need
v[3]=v[1]+v[2];

RESULT(v[3] )



EQUATION("WageIncome")
/*
Income from the wage
*/

V("Trade");
v[0]=0;
CYCLE(cur2, "Supply")
 {
  CYCLES(cur2, cur, "Firm")
   {
   CYCLES(cur, cur1, "Labor")
    {
     v[1]=VS(cur1,"wage");
     v[2]=VS(cur1,"NumWorkers");
     v[0]+=v[1]*v[2]; 
    }
   }
 }
v[3]=0;
CYCLE(cur, "Machinery")
	{
	CYCLES(cur, cur1, "KFirm")
		{
    		CYCLES(cur1, cur2, "KLabor")
       {
        v[5]=VS(cur2, "KNbrWorkers");
    		  v[6]=VS(cur2, "KWage");
        v[3]+=v[5]*v[6];
       }
    		v[7]=VS(cur1, "KNbrEngineers");
    		v[8]=VS(cur1, "KWageEngineers");
    		v[3]+=v[7]*v[8];
		}
	}

RESULT((v[0]+v[3]) )


EQUATION("ShareWageIncome")
/*
Divides the overall income across the classes, according to the different jobs, i.e. wage class
*/

v[7]=VS(p->up->up,"WageIncome");
v[1]=V("NumClass");
if(v[1]>0)
 { //if we are in one of the labour classes (not engineers)
  v[4]=v[11]=0;
  CYCLES(p->up->up, cur2, "Supply")
   {
    CYCLES(cur2, cur, "Firm")
     { //cycle all firms
      CYCLES(cur, cur1, "Labor")
       { // determine the number of labor classes / layers
        v[14]=VS(cur1,"IdLabor");
        if(v[14]==v[1])
         { // for the layer corresponding to the class under computation compute the income
          v[2]=VS(cur1,"wage");
          v[3]=VS(cur1,"NumWorkers");
          v[4]+=v[2]*v[3];
          v[11]+=v[3];
         }
       }
     }
   }
  CYCLES(p->up->up, cur, "KFirm")
   {
    CYCLES(cur, cur1, "KLabor")
     { // determine the number of labor classes / layers
      v[15]=VS(cur1,"IdKLabor");
      if(v[15]==v[1])
       {
        v[5]=VS(cur1,"KWage");
        v[6]=VS(cur1,"KNbrWorkers");
        v[4]+=v[5]*v[6];
        v[11]+=v[6];
       }
     }
   }
 }
else
 {
  v[4]=v[11]=0;
  CYCLES(p->up->up, cur, "KFirm")
   {
    cur1=SEARCHS(cur,"KEngineers");
    v[9]=VS(cur1,"KWageEngineers");
    v[10]=VS(cur1,"KNbrEngineers");
    v[4]+=v[9]*v[10];
    v[11]+=v[10];
   }

 }
v[8]=v[4]/v[7];
v[12]=v[11]-0.5;
v[13]=round(v[12]);
//WRITE("NumIterations",v[13]); to maintain the same individual demand to all firms. When "NumIterations" is kept fix, the individual demand, as the expenditure of the class increases, becomes 'groups' demand all to selected firms.

RESULT(v[8] )


EQUATION("NonWageIncome")
/*
Sum of earnings not included in to wages, such as wage premia
*/

v[2]=0;
CYCLE(cur2, "Supply")
 {
  CYCLES(cur2, cur, "Firm")
   {
    CYCLES(cur, cur1, "Labor")
     {
      v[1]=VS(cur1,"WagePrem");
      v[2]+=v[1];
     }
   }
 }
CYCLE(cur, "KFirm")
 {
  CYCLES(cur, cur1, "KLabor")
   {
    v[3]=VS(cur1,"KWagePrem");
    v[2]+=v[3];
   }
 }

RESULT(v[2] )


EQUATION("ShareNonWageIncome")
/*
Shre distributed to each class of wage premia
*/

v[1]=VS(p->up->up,"NonWageIncome");
v[2]=V("NumClass");
v[5]=0;
if(v[2]>0)
 {
  CYCLES(p->up->up, cur2, "Supply")
   {
    CYCLES(cur2, cur, "Firm")
     {
      CYCLES(cur, cur1, "Labor")
       {
        v[3]=VS(cur1,"IdLabor");
        if(v[3]==v[2])
         {
          v[4]=VS(cur1,"WagePrem");
          v[5]+=v[4];
         }
       }
  
     }
   }
  CYCLES(p->up->up, cur, "KFirm")
   {
    CYCLES(cur, cur1, "KLabor")
     {
      v[6]=VS(cur1,"IdKLabor");
      if(v[6]==v[2])
       {
        v[7]=VS(cur1,"KWagePrem");
        v[5]+=v[7];
       }
     }

   }
 }
if(v[1]>0)
 v[8]=v[5]/v[1];
else
 v[8]=0;

RESULT(v[8] )


EQUATION("price")
/*
Markup on the unit production cost
*/
v[10]=V("markup");
v[21]=v[22]=0;
CYCLE(cur, "Labor")
 {
  v[16]=VS(cur,"wage");
  v[17]=VS(cur,"NumWorkers");
  v[21]+=v[16]*v[17];
  //v[22]+=v[17]; // total labour force
 }
v[22]=V("NumWorkers"); // labour in the first tier (the ones which define the production capacity)
if(v[21]==0)
 v[11]=V("AvWage");
else
 v[11]=v[21]/v[22];  //av. wage in the firm
WRITE("AvWage",v[11]);
/*
if(v[2]==0)
 {
  v[12]=V("IncProductivity"); //productivity of the first (the best) K in the list
  v[13]=v[11]*v[10]/v[12];
  END_EQUATION(v[13]);
 }
 */
v[59]=V("CapitalIntens");
v[52]=V("CapitalDepress");
v[0]=v[31]=v[32]=0;
CYCLE(cur, "Capital")
 {
  v[3]=VS(cur,"MaxKQ");
  v[5]=VS(cur,"IncProductivity");
  v[0]+=v[3];
  v[32]+=v[5]*v[3];
 }

v[9]=v[32]/v[0]; //weighted av. incProductivity
WRITE("AvIncProd",v[9]);

v[14]=v[11]*v[10]/v[9];
if(v[14]<0)
 v[14]=INTERACT("nEG. price", v[14]);

RESULT(v[14] )


EQUATION("Profit")
/*
Profit, difference between revenues and total costs
*/
v[0]=V("UnitSales");
v[1]=V("price");
v[2]=V("LaborCost");
v[3]=v[0]*v[1]-v[2];
v[4]=INCR("CumProfit",v[3]);

RESULT(v[3] )


EQUATION("MovAvProfit")
/*
Moving average of the profits.
Test to be uases as an indicator of profits that is not oscillating as unit sales, and allows to use profits to trigger exit from market
*/

v[1]=VL("MovAvProfit",1);
v[2]=VL("Profit",1);
v[3]=V("aProfit");
v[4]=v[3]*v[2]+(1-v[3])*v[1];

RESULT(v[4] )


EQUATION("ExpectedProfit")
/*
Level of profits perceived by the firm, which reflects the expected gains from the sales.
To be used as an indicator of profit that is not oscillating as unit sales, and that would allow to use profits to trigger exit from markets 
*/

v[0]=V("ExpectedSales");
v[1]=V("price");
v[2]=V("LaborCost");
v[3]=v[0]*v[1]-v[2];

RESULT(v[3] )


EQUATION("Workforce")
/*
Number of workers employed, and actually working in the production process
*/

V("NumWorkers");
v[2]=V("IdLabor");
if(v[2]==1)
 { // number of workers in the first tier
  v[1]=V("NumWorkers");
  v[3]=V("DesiredUnusedCapacity");
  v[4]=VL("MaxLaborProductivity",1);
  v[5]=V("CapitalCapacity");
  v[6]=V("DesiredQ");
  v[7]=min(v[5],v[6]);
  v[8]=V("aNW");
  v[9]=v[1]-(v[3]-1)*(v[7]/v[4])*(1-v[8]);
 }
else
 { // number of workers in the tiers above
  CYCLES(p->up, cur, "Labor")
   {
    v[10]=VS(cur,"IdLabor");
    if(v[10]==v[2]-1)
     {
      v[11]=VS(cur,"nu");
      v[12]=VS(cur,"Workforce");
     }
   }
  v[9]=v[12]/v[11];

 }

RESULT(v[9] )


EQUATION("ExpectedLCost")
/*
Labor cost without the reserve labour force
*/

v[0]=0;
CYCLE(cur, "Labor")
 {
  v[1]=VS(cur,"Workforce");
  v[2]=VS(cur,"wage");
  v[0]+=v[1]*v[2];
 }

RESULT(v[0] )


EQUATION("ExpectedProfit4")
/*
Level of profits perceived by the firms as if they did not have the excess lbour capacity
*/

v[0]=V("UnitSales");
v[1]=V("price");
v[2]=V("ExpectedLCost");
v[3]=v[0]*v[1]-v[2];

RESULT(v[3] )


EQUATION("LaborCost")
/*
Comment
*/
v[0]=0;
CYCLE(cur, "Labor")
 {
  v[1]=VS(cur,"NumWorkers");
  v[2]=VS(cur,"wage");
  v[0]+=v[1]*v[2];
 }

RESULT(v[0] )


EQUATION("ExpectedProfit2")
/*
Level of profits perceived by the firm, which reflects the gains from the desired production
*/

v[0]=V("DesiredQ");
v[1]=V("price");
v[2]=V("LaborCost");
v[3]=v[0]*v[1]-v[2];

RESULT(v[3] )


EQUATION("ExpectedProfit3")
/*
Level of profits perceived by the firm, which reflects the gains from the actual production
*/

v[0]=V("Q");
v[1]=V("price");
v[2]=V("LaborCost");
v[3]=v[0]*v[1]-v[2];

RESULT(v[3] )


EQUATION("MaxKQ")
/*
Defines the Theoretical Labor Productivity of the Firm as incorporated in the various capital vintages of the firm.
*/
v[0]=0;
v[1]=0;
v[2]=V("CapitalDepress");//defines the depression rate of capital
v[3]=V("K");
v[4]=V("KAge");
v[9]=V("CapitalIntens");
v[6]=pow((1-v[2]),v[4]);//computes the depressiation of capital
v[7]=v[3]*v[6];//computes the actual stock of this capital vintage that can be used 

v[8]=v[7]/v[9];

RESULT(v[8] )

EQUATION("CapitalCapacity")
/*
Comment
*/

RESULT(SUM("MaxKQ") )


EQUATION("MaxLaborProductivity")
/*
Defines the Theoretical Labor Productivity of the Firm as incorporated in the various capital vintages of the firm.
*/
v[0]=0;
v[1]=0;
v[2]=V("CapitalDepress");//defines the depression rate of capital
CYCLE(cur, "Capital")
 {
  v[3]=VS(cur,"K");
  v[4]=VS(cur,"KAge");
  v[5]=VS(cur, "IncProductivity");
  v[6]=pow((1-v[2]),v[4]);//computes the depressiation of capital
  v[7]=v[3]*v[6];//computes the actual stock of this capital vintage that can be used 
  v[0]+=(v[7]*v[5]);
  v[1]+=v[7];
 }
v[8]=v[0]/v[1];//Max Labor productivity computed as the weighted average of the incorporated productivity in every capital vintages
WRITE("CapitalStock",v[1]);
v[9]=V("CapitalIntens");
v[10]=v[1]/v[9];

RESULT(v[8] )


EQUATION("LaborComposition")
/*
Defines the Labor composition by skills of the Firm as function of the various capital vintages of the firm.
Measured as the share of Skill Labor required to use the capital combination 
*/
v[0]=0;
v[1]=0;
v[2]=V("CapitalDepress");//defines the depression rate of capital
CYCLE(cur, "Capital")
 {
  v[3]=VS(cur,"K");
  v[4]=VS(cur,"KAge");
  v[5]=VS(cur, "IncSkillBiais");
  v[6]=pow((1-v[2]),v[4]);//computes the depressiation of capital
  v[7]=v[3]*v[6];//computes the actual stock of this capital vintage that can be used 
  v[0]+=(v[7]*v[5]);
  v[1]+=v[7];
 }
v[8]=v[0]/v[1];//Skill biais : defines the proportion of skilled workers (skill type 2)
CYCLE(cur, "Labor")
 {
  v[9]=VS(cur,"SkillType");
  if (v[9]<=1)//if the labor type is unskilled
  	v[10]=1-v[8];
  else
  	v[10]=v[8];
  v[11]=max(v[10],0.00001);
  v[12]=min(v[11],1);
  WRITES(cur,"SkillIntensity",v[12]);
 }

RESULT(v[8] )


EQUATION("KAge")
v[0]=VL("KAge",1);
RESULT((v[0]+1) );


EQUATION("wage")
V("NumWorkers");
v[3]=VS(p->up,"IdFirm");
v[2]=V("IdLabor");
if(v[2]==1)
 { // first tier workers
  v[0]=V("MinWage");
  v[1]=V("wagecoeff"); // wage coefficient as the minimum wage multiplier
 }
else
 { // executives
  cur=SEARCH_CNDS(p->up,"IdLabor",v[2]-1);
  v[4]=VS(cur->up,"IdFirm");
  if(v[4]!=v[3])
   INTERACT("Firms is different", v[4]);
  v[0]=VS(cur,"wage");
  v[1]=V("wagecoeff"); // wage coefficient as the wage tier multiplier
 }

RESULT((v[0]*v[1]) )


EQUATION("WagePrem")
/*
Wage premia distributed, when available to all classes of executives. Premia for single worker should be divided by the number of workers
*/

V("Profit");
V("InvestmentDecision");
v[5]=VS(p->up,"CumProfit");
if(v[5]>0)
 {
  v[1]=V("IdLabor");
  if(v[1]>1)
   {
    v[3]=0;
    CYCLES(p->up, cur, "Labor")
     { // make a fisrt cycle to compute the overall wage paied to the different layers of executives
      v[10]=VS(cur,"IdLabor");
      if(v[10]>1)
       {
        v[2]=VS(cur,"wage");
        v[3]+=v[2];
       }
     }
    v[9]=V("wage");
    v[4]=VS(p->up,"ro"); //share of profits dedicated to R&D
    v[5]=VS(p->up,"CumProfit"); // cumulated profits after subtracting investment expenditures
    v[6]=(1-v[4])*v[5]; // amount of profits to pay premia
    v[7]=v[6]*v[9]/v[3];
    v[8]=INCRS(p->up,"CumProfit",-v[7]);
   }
  else
   v[7]=0;
 }
else
 v[7]=0;

RESULT(v[7] )




/****************************************************************/
/******************** PROUCT INNOVATION *************************/
/****************************************************************/


EQUATION("RdExpenditure")
/*
Cumulated profits (not used to invest in kapital) devoted to product R&D
*/

V("Profit");
V("InvestmentDecision");
v[1]=V("CumProfit");
if(v[1]>0)
 {
  v[2]=V("ro"); // share of profits devoted to R&D
  v[3]=v[1]*v[2];
  v[4]=INCR("CumProfit",-v[3]);
 }
else
 v[3]=0;

RESULT(v[3] )


EQUATION("ExploreSector")
/*
The equation represent the first step of product R&D: exploration of sectors (needs)
The firm decide to innovate on a sector that has increasing sales. The number of sectors on which the firm can acquire information depends on the resources invested in R&D. The more the firm invests the further from its knowledge it can explore
*/

v[1]=V("RdExpenditure"); // amount of R&D in the current period
if(v[1]>0)
 { // only if the firm had some profirs to devote to R&D
  v[2]=V("product"); // the good the firm is currently producing
  v[3]=VS(p->up->up,"totNeeds"); // the total amount of needs (goods to be produced)
  v[4]=V("zProd"); // Product innovation probability 
  v[5]=exp(-v[4]*v[1]);
  v[6]=v[3]*(1-v[5])/2;
  v[7]=round(v[6]);
  v[8]=v[2]-v[7];
  v[9]=max(1,v[8]); // the minimum value of the good the firm can explore (exploration on the left side of the need spectrum)
  v[10]=v[2]+v[7];
  v[11]=min(v[3],v[10]); // the maximum value of the good (need) the firm can explore (exploration on the right side of the need spectrum - starting from the produced good)
  v[12]=-1000;
  CYCLES(p->up->up, cur, "Sectors")
   { // cycle through the sectoral sales to identify the one in which the sales have grown most in the last period
    v[14]=VS(cur,"IdGood");
    if(v[14]>=v[9] && v[14]<=v[11])
     { // include in the extraction the sectors in which the firm can innovate
      v[13]=VS(cur,"DeltaPotD");
      v[16]=VLS(cur,"SSales",1);
      v[17]=VLS(cur,"PotD",1);
      v[13]=v[17]-v[16];
      v[18]=abs(v[13]);
      if(v[13]<0)
       WRITES(cur,"sectorProb",1/v[18]+1);
      else
       WRITES(cur,"sectorProb",v[13]*100);
     }
    else
     WRITES(cur,"sectorProb",0.01);
   }
  cur1=RNDDRAWS(p->up->up,"Sectors","sectorProb"); // randomly draw the sector with probabilities that depend on the escess potential demand
  v[15]=VS(cur1,"IdGood");
  if(v[15]<v[9] || v[15]>v[11])
   v[15]=v[2];
 }
else
 v[15]=0;

RESULT(v[15] )


EQUATION("ExcessD")
/*
Excess demand in a sector (need). The variable on which the prototype is chosone
*/

v[1]=V("PotD");
v[2]=V("SSales");

RESULT(v[1]-v[2] )



EQUATION("GrowthSSales")
/*
Rate of growth of the sectol sales
*/

v[1]=VL("SSales",1);
v[2]=V("SSales");
v[3]=v[2]-v[1];

RESULT(v[3] )


EQUATION("ProdInno")
/*
The actual product innovation: extraction of a quality given the quality of the good the firm is currently producing
*/

v[17]=V("RdExpenditure");
if(v[17]>0)
 {
  V("Trade");
  cur=SEARCH("PNeed"); // go in the product currently produced
  CYCLES(cur, cur1, "Ch")
   {
    v[1]=VS(cur1,"IdCh");
    if(v[1]>1)
     v[2]=VS(cur1,"x"); // check the current quality level of the prouced good
   }
  v[3]=V("ExploreSector"); // the sector for which the firm is drawing the prototipe
  v[4]=V("product"); // the sector in which the firm is currently producing
  v[5]=abs(v[4]-v[3])+1; // the distance between the two sectors
  v[6]=V("ProdShockP"); // productivity shock that determines the variance of the product innovation
  v[7]=v[6]/v[5]; // variance of the product innovation
  v[8]=norm(v[2],v[7]); // outcome of the product innovation
  v[13]=max(0,v[8]);
  if(v[3]==v[4])
   { // in case in which the innvoation has occured in the current sector of production, retain the prototype only if better then the quality currently produced
    if(v[13]<=v[2])
     END_EQUATION(-1);
   }
  v[12]=1000;
  CYCLE(cur, "PNeed")
   { 
    v[9]=VS(cur,"IdPNeed");
    if(v[9]>0) // cycle only trhourgh the prototipes and leave alone the current prouction
     {
      CYCLES(cur, cur1, "Ch")
       {
        v[10]=VS(cur1,"IdCh");
        if(v[10]>1) //check the value of the quality characteristic
         {
          v[11]=VS(cur1,"x");
          if(v[11]<=v[12])
           {
            v[12]=v[11];
            v[14]=v[9]; // retain the Id of the prototipe with lower quality
           }
         }
       }
  
     }
   }
  CYCLE(cur, "PNeed")
   {
    v[15]=VS(cur,"IdPNeed");
    if(v[15]==v[14])
     { // in the prototipe with the lowest quality
      WRITES(cur,"productProt",v[3]); // write the sector to which the prototipe pertains
      CYCLES(cur, cur1, "Ch")
       { // serach for the quality characteristic
        v[16]=VS(cur1,"IdCh");
        if(v[16]>1)
         { 
          v[17]=VS(cur1,"x");
          if(v[13]>v[17])
           { // change the quality only if better then the one of the lowest prototipe
            WRITELS(cur1,"x",v[13],t); // write its quality
            WRITES(cur,"productProt",v[3]); // and write the sector of the prototipe
           }
          else
           v[14]=-2;
          
         }
       }
  
     }
   }
 }
else
 v[14]=0;

RESULT(v[14] )


EQUATION("PotD")
/*
Potential demand for a specific good (need) given by the total income of a class per the share of expenditure on the specific neede
*/

v[1]=V("IdGood");
v[5]=0;
CYCLES(p->up, cur, "Demand")
 {
  CYCLES(cur, cur1, "Class")
   { // cycle all classes
    CYCLES(cur1, cur2, "Need")
     { // cycle the needs and select the one satisfied by the calling sector
      v[2]=VS(cur2,"IdNeed");
      if(v[2]==v[1])
       { // if the need represents the calling good
        v[3]=VS(cur2,"Share"); // share of expenditure devoted to this need
        v[6]=VS(cur1,"TotIterations");
        v[7]=VS(cur2,"NumIterations");
        v[3]=v[7]/v[6]; // share of expenditure devoted to this need as a result of the roundedc onsumption iterations
        v[4]=VS(cur1,"Expenditure"); // total expenditure
        v[5]+=v[4]*v[3];
       }
     }

   }

 }

RESULT(v[5] )


EQUATION("DeltaPotD")
/*
Change in the potential demand
*/

v[1]=VL("PotD",1);
v[2]=V("PotD");
v[3]=v[2]-v[1];

RESULT(v[3] )


EQUATION("MovAvExpSales")
/*
Moving average of expected sales.
Used by the firm to decide wether to adopt a prototipe
*/

v[1]=VL("MovAvExpSales",1);
v[2]=VL("ExpectedSales",1);
v[3]=V("aExpSales");
v[4]=v[3]*v[2]+(1-v[3])*v[1];

RESULT(v[4] )


EQUATION("DMonetarySales")
/*
Change in monetary sales
*/

v[1]=V("MonetarySalesL");
v[2]=V("MonetarySales");
v[3]=v[2]-v[1];

RESULT(v[3] )


EQUATION("MovAvMonSales")
/*
Moving average of the Monetary sales.
To be used as a measure f the probability to insert an innovation in the market (if oscillations are not too large)
*/

v[1]=VL("MovAvMonSales",1);
v[2]=V("MonetarySalesL");
v[3]=V("aMonSales");
v[4]=v[3]*v[2]+(1-v[3])*v[1];

RESULT(v[4] )


EQUATION("DMovAvMonSales")
/*
Growth of the moving average of monetary sales
*/

v[1]=V("MovAvMonSales");
v[2]=VL("MovAvMonSales",1);
v[3]=v[1]-v[2];

RESULT(v[3] )


EQUATION("DUnitSales")
/*
Growth of unit sales
*/

v[1]=V("UnitSales");
v[2]=V("UnitSalesL"); // lagged unit sales
v[3]=v[1]-v[2];

RESULT(v[3] )





EQUATION("Innovate")
/*
Introduction of the innovation: one of the prototipes is adopted by the firm, replacing the current poduction.
It occurs (in probability) when the growth of unit sales is decreasing
*/

v[15]=INCR("tInno",-1);
if(v[15]<=0)
 {
  
  v[3]=V("DUnitSales");
  v[17]=VL("DUnitSales",1);
  if(v[3]<0 && v[17]<0)
   { // if the expected sales are declining for the second consecutive period (they do not decline only for )
    v[18]=V("zInno"); /// parameter that defines the likelihood of introducing an innovation
    v[19]=exp(v[18]*1/v[3]); // probability of introducing an innovation increases the higher is the reduction in sales
    v[20]=UNIFORM(0,1);
    if(v[20]<v[19])
     {
      v[7]=0;
      CYCLE(cur, "PNeed")
       {
        v[4]=VS(cur,"IdPNeed");
        v[8]=VS(cur,"productProt");
        if(v[4]>0)
         {// cycle through the prototipes
          CYCLES(cur, cur1, "Ch")
           {
            v[5]=VS(cur1,"IdCh");
            if(v[5]>1)
             {// cycle through the quality characteristics
              v[6]=VS(cur1,"x");
              if(v[6]>v[7])
               {
                v[7]=v[6];
                v[9]=v[8];
                v[12]=v[4];
               }
             }
           }
    
         }
       }
     
      if(v[7]>0)
       {// if at least one prototipe was active
        v[22]=v[24]=0;
        v[23]=V("product");
        CYCLES(p->up->up, cur2, "Supply")
         {
          CYCLES(cur2, cur3, "Firm")
           {
            v[21]=VS(cur3,"product");
            if(v[21]==v[9])
             { // count ht enumber of firms producing the same product of the prototipe to e introduced
              v[22]++;
             }
            if(v[21]==v[23])
             v[24]++; // count the number of firms that are in the same market in which the firms is
           }
  
         }
        if(v[9]!=v[23] && v[22]<v[24])
         { // if the market of the prototipe os less competitive then the one in which the frms is producing
          cur=SEARCH("PNeed");
          CYCLES(cur, cur1, "Ch")
           {
            v[10]=VS(cur1,"IdCh");
            if(v[10]>1) // write the new quality on the quality characteristic
             WRITELS(cur1,"x",v[7], t);
           }
          WRITE("product",v[9]); // write the new product
          CYCLE(cur, "PNeed")
           { // do another cycle to set to 0 the prototipe that has been introduced, so that it is not used again in the next period, an it is the first one to be replaced
            v[11]=VS(cur,"IdPNeed");
            if(v[11]==v[12])
             { // gp back to the prototipe used
              CYCLES(cur, cur1, "Ch")
               {
                v[13]=VS(cur1,"IdCh");
                if(v[13]>1)
                 { // go to the quality characteristic
                  v[14]=VS(cur1,"x");
                  WRITELS(cur1,"x",0,t);
                  //if(v[14]!=v[7])
                   //INTERACT("This was not the prototipe, check the IdPNeed", v[12]);
                 }
               }
        
             }
           }
          v[16]=VS(p->up,"innoInterval"); // minimum interval between the introduction of two prototipes
          WRITE("tInno",v[16]);
        }
        if(v[9]==v[23])
         { // if the prototipe is in the same market in which the firm is producing 
          cur=SEARCH("PNeed");
          CYCLES(cur, cur1, "Ch")
           {
            v[10]=VS(cur1,"IdCh");
            v[25]=VS(cur1,"x");
            if(v[10]>1 && v[7]>v[25]) // write the new quality on the quality characteristic
             WRITELS(cur1,"x",v[7], t);
           }
          WRITE("product",v[9]); // write the new product
          CYCLE(cur, "PNeed")
           { // do another cycle to set to 0 the prototipe that has been introduced, so that it is not used again in the next period, an it is the first one to be replaced
            v[11]=VS(cur,"IdPNeed");
            if(v[11]==v[12])
             { // gp back to the prototipe used
              CYCLES(cur, cur1, "Ch")
               {
                v[13]=VS(cur1,"IdCh");
                if(v[13]>1)
                 { // go to the quality characteristic
                  v[14]=VS(cur1,"x");
                  WRITELS(cur1,"x",0,t);
                  //if(v[14]!=v[7])
                   //INTERACT("This was not the prototipe, check the IdPNeed", v[12]);
                 }
               }
        
             }
           }
          v[16]=VS(p->up,"innoInterval"); // minimum interval between the introduction of two prototipes
          WRITE("tInno",v[16]);
        }
       if(v[9]!=v[23] && v[22]>=v[24]-1)
        v[9]=-3; // the new market is more competitve than the one in which the firm is now
      }
      else
       v[9]=-1; // no prototipe available
    }
    else
     v[9]=-4; // low probability to innovate (reduction in sale was not so large)
   }	
  else
   v[9]=0; // no need to innovate, sales are not decreasing
   }
else
 v[9]=-2; // to close to the previous innovation

RESULT(v[9] )


EQUATION("ProbInnovate")
/*
Probability that a firm introduces an innovation, given a deterioration of its market conditions (expected sales)
We use different indicators
*/

v[1]=V("NegSales");
v[2]=V("DeltaExpSales");


RESULT(1 )



EQUATION("DeltaExpSales")
/*
Rate of change of the firms' expected sales.
To be used as a trigger of the introuction of an innovation
*/

v[1]=VL("ExpectedSales",1);
v[2]=V("ExpectedSales");
v[3]=v[2]/v[1]-1;

RESULT(v[3] )


EQUATION("SumDeltaExpSales")
/*
Sum of the negative rate of growth of expected sales, for consecutive periods of ngeative growth rate
*/

v[1]=V("DeltaExpSales");
v[2]=VL("SumDeltaExpSales",1);
if(v[1]<0)
 v[3]=v[2]+v[1];
else
 v[3]=0;

RESULT(v[3] )


EQUATION("NegSales")
/*
Number of consecutive periods during which the growth rate of the expected sales of a firm is negative
*/

v[1]=VL("NegSales",1);
v[2]=V("DeltaExpSales");
if(v[2]<0)
 v[3]=v[1]+1;
else
 v[3]=0;

RESULT(v[3] )





EQUATION("KapitalNeed")
/*
Decide whether to order new capital.
*/
/*
v[3]=VL("CapitalStock",1);
v[4]=V("ExpectedSales");
v[7]=V("DesiredUnusedCapacity");
v[8]=V("CapitalIntens");
v[9]=(v[4]/v[8])*v[7];
v[10]=v[9]-v[3];
*/
v[11]=max(v[10],0);
//Cannot understand, and in any case does not work

V("MaxLaborProductivity");
v[3]=V("CapitalCapacity");
v[4]=V("ExpectedSales");
v[7]=V("DesiredUnusedCapacity");
v[8]=V("CapitalIntens");

v[9]=(v[4])*v[7];

v[10]=v[9]-v[3];
v[11]=max(v[10],0);
v[12]=v[11]*v[8];
RESULT(v[12] )


EQUATION("InvestmentDecision")
/*
Place on order of K if you need it and did not place an order as yet
*/
v[0]=V("Waiting");
if(v[0]==1)
 END_EQUATION(1); //skip the equation if you already placed an order. To be edited to give the possibility to remove a too late order
//we are here only if there is no pending order

v[1]=V("KapitalNeed");
if(v[1]>0)
 {
  v[3]=V("PlaceOrder");
  WRITE("Waiting",1);
 } 

RESULT( 1)



EQUATION("PlaceOrder")
/*
Place the order from the calling firm to a Kapital producer adopting the technology of the firm
*/

v[0]=VS(c,"IdTech"); //this is the technology of the firm

//assuming there are many firms producing K with the same technologies, firm select the one they prefer in terms of price and productivity of the capital, and waiting time (insert also durability of the capital if we include depreciation as a function of production quantity and not time)
v[51]=v[53]=v[59]=v[60]=v[58]=v[70]=0;
//given the preference of the buyer firm wth respect to the features of the capital production, namely price and current productivity of the capital, and approximate time to wait for receiving the order
v[30]=VS(c,"betaPrice");
v[31]=VS(c,"betaProd");
v[32]=VS(c,"betaTime");
//check and evaluate the available supply
CYCLE(cur, "KFirm")
 {
  v[50]=VLS(cur,"KPrice",1);
  v[51]+=v[50];
  cur2=SEARCHS(cur,"KCapital");
  v[52]=VS(cur2,"CurrentProductivity");
  v[53]+=v[52];
  v[60]++;
  v[64]=VS(cur,"NumOrders");
  if(v[64]>0)
   {
    CYCLES(cur, cur1, "Order")
     {
      v[54]=VS(cur1,"TimeWaited");
      v[55]=VS(cur1,"KCompletion");
      v[56]=VS(cur1,"KAmount");
      v[57]=(v[56]-v[55])*v[54];
      WRITES(cur,"WaitTime",v[57]+1);
     }
   }
  else
   WRITES(cur,"WaitTime",1);
  v[65]=VS(cur,"WaitTime");
  v[58]+=v[65];
// an index that gives the amount of time needed or a firm to complete the production of the capital already ordered
 }
 // write the average values for price  and producitivty of capital, and for the index of awaiting time. The averates are updated each time a firm place an order, as this would affectthe choice of a following firm in the same time period.
// NOTE THAT THIS PROVIDES A FIRST MOVER 'ADVANTAGE' TO THE FIRST FIRMS.
v[61]=v[51]/v[60];
WRITE("AvKPrice",v[61]);
v[62]=v[53]/v[60];
WRITE("AvCurrProd",v[62]);
v[63]=v[58]/v[60];
WRITE("AvWaitTime",v[63]);
 
CYCLE(cur, "KFirm")
 {
  v[1]=VS(cur,"IdKTech");
  if(v[0]!=v[1])
   WRITES(cur,"kapp",0);
  else
   WRITES(cur,"kapp",1);
  v[20]=VLS(cur,"KPrice",1); 
  v[21]=VS(cur,"CurrentProductivity");
  v[26]=VS(cur,"WaitTime");
  // normalise relative values (shares) in order to compare firms on the different indicators using the same evaluation for different units of measurement
  v[27]=v[20]/v[61]+1;
  v[28]=v[21]/v[62]+1;
  v[29]=v[26]/v[63]+1;
  v[33]=pow(v[28],v[31])*pow(v[29],-v[32])*pow(v[27],-v[30]);
  WRITES(cur,"kselect",v[33]*VS(cur,"kapp"));
  v[70]+=v[33];

 }
v[73]=0;
CYCLE(cur, "KFirm")
 {
  v[71]=VS(cur,"kselect");
  v[72]=v[71]/v[70];
  WRITES(cur,"kchoice",v[72]);
  v[73]+=v[72];
 }

v[73]=SUM("kchoice");

//if((double)t>1)
 //INTERACT("check that kchoice has been allocated", v[73]);

v[81]=0;
CYCLE(cur, "KFirm")
 {
  v[80]=VS(cur,"kchoice");
  if(v[80]>v[81])
   v[81]=v[80];
 }


cur=SEARCH_CND("kchoice",v[81]);
//if((double)t>1)
 //INTERACTS(cur,"check that it selects the max value", VS(cur,"kchoice")-v[81]);

//cur=RNDDRAWTOT("Kfirm","kchoice", 1);
//if((double)t>1)
 //INTERACTS(cur,"correct firm?", V("kchoice"));
//cur=RNDDRAW("KFirm","kapp"); when there is no evalutaion of the capital acquired

//assuming there is only one K firm for each technology, use the following, which is faster
//cur=SEARCH_CND("IdKTech",v[0]);



//Legend:
//c: it is the final producer firm ordering the K
//cur: is the K producer
//cur1: is the order of K under production

//cur1=ADDOBJS(cur,"Order");
v[6]=VLS(cur,"KPrice",1);

//cur1=cur->add_an_object("Order"); FIXED
cur1=ADDOBJS(cur,"Order");

v[2]=VS(c,"IdFirm");
v[3]=VS(c,"KapitalNeed");
v[7]=VS(c,"Ishare");
v[8]=VS(c,"Profit");
v[11]=max(v[8],0);
v[9]=(v[11]*v[7])/v[6];
v[10]=max(v[3],v[9]);

WRITES(cur1,"KAmount",v[3]);
WRITES(cur1,"KCompletion",0);
WRITES(cur1,"TimeWaited",1);
cur1->hook=c; //useful to retrieve quickly the ordering firm

INCRS(cur,"NumOrders",1);
v[4]=VLS(cur,"CurrentProductivity",1); //current state of the K art
WRITES(cur1,"Kproductivity",v[4]); //tech characteristics of the capital stock order
v[5]=VLS(cur,"CurrentSkillBiais",1);
WRITES(cur1,"KSkillBiais",v[5]);
WRITES(cur1,"KP",v[6]);// write the price of the capital in the period in which it is ordered, and use it to compute the actual expenditure using the `agreed' price.
v[12]=INCRS(c,"CumProfit",-(v[6]*v[3]));

RESULT(1 )



EQUATION("KProductionFlow")
/*
*/
//Activity of the K producing firm
V("Vacancies");
v[0]=V("KQ"); //production capacity of the firm
v[1]=V("NumOrders");
v[2]=v[0]/v[1]; //one way to determine the amount of K production capacity per order. Otherwise...

v[3]=0;
CYCLE(cur, "Order")
 {
  v[4]=VS(cur,"KAmount");
  v[5]=VS(cur,"KCompletion");
  v[3]+=v[4]-v[5];
 }


CYCLE_SAFE(cur, "Order")
 {//increase the level of advancement of the orders and, if completed, remove the order. Given the production capacity, devote it respecting oreders' order (first comes first go, which allows to respect the priority given by customers, on side, and to reduce the dofferences between the price agreed upon ordering and the price at which the kapital is sold)
  v[4]=VS(cur,"KAmount");
  v[5]=VS(cur,"KCompletion");
  v[6]=(v[4]-v[5]); // given the missing quantity of the current order
  //v[7]=v[6]*v[0]; //share of production capacity devoted to this order
  v[8]=min(v[0], v[4]-v[5]); //use the production capacity needed actually neded to produce the order, or exhaust here the production capacity (for the current period)
  INCRS(cur,"KCompletion",v[8]);
  v[0]=v[0]-v[8];
  v[5]=VS(cur,"KCompletion"); //update the completion level  in order to cancel the order if done
  if(v[5]>=v[4])
   {//order fulfilled. Either search for the ordering firm, or simply use the hook
    if(v[5]>0)
     {//stupid control needed to not be confused by the very initial object
      INCRS(cur->hook,"NumK",1);
      //cur1=ADDOBJS(cur->hook,"Capital");
//      cur1=cur->hook->add_an_object("Capital");
      cur1=ADDOBJS(cur->hook,"Capital");
      WRITELS(cur1,"K",v[5],t);
      v[9]=VS(cur,"Kproductivity");
      WRITELS(cur1,"IncProductivity",v[9],t);
      v[10]=VS(cur,"KSkillBiais");
      WRITELS(cur1,"IncSkillBiais",v[10],t);
      WRITELS(cur1,"KAge",0,t);
      v[11]=VS(cur,"KP");
      v[12]=v[11]*v[5];
      WRITELS(cur1,"KExpenditures",v[12], t);
      WRITES(cur->hook,"Waiting",0); //tell the firms it has the new capital
      INCR("NumOrders",-1);
      SORTS(cur->hook,"Capital","IncProductivity", "DOWN");
      DELETE(cur);
     }
   }
  else
   {
    if(v[4]>0)
     INCRS(cur,"TimeWaited",1); // if orders remain non completed increase the time needed to go through future orders
   }
   
 }

v[13]=min(V("KQ"),v[3]); 
v[15]=V("KQ")-v[3];
v[16]=v[15]-v[0];
//if(v[15]>0 && v[15]!=v[0])
 //INTERACT("check the correspondence between production and KQ",v[16]);
//if(v[15]<0 && v[0]!=0)
 //INTERACT("check the correspondence between production and KQ",v[0]);
 
RESULT(v[13] )


EQUATION("KInnovation")
/*
Changes in the characteristics of kapital
*/

v[0]=VL("CurrentProductivity",1);
v[1]=VL("CurrentSkillBiais",1);
v[2]=VL("KNbrEngineers",1);
v[3]=V("z");
v[4]=V("ProductivityShock");
v[5]=V("SkillBiaisShock");
v[6]=-(v[2]*v[3]);
v[7]=1-exp(v[6]);//Proba to innovate increases with the nbr of engineers
//first random draw to define the outcome of R&D i.e. success or failure
if (RND<v[7])
	{
	v[8]=norm(0,v[4]);//second stockastic variable, defines the level of productivity out of the successful R&D process	
	v[9]=norm(0,v[5]);
	}
else
	{
	v[8]=0;
	v[9]=0;
	}
v[10]=max(v[8],0);
v[11]=v[0] + v[10];
WRITE("CurrentProductivity",v[11]);
v[12]=v[1] +v[9];
v[13]=max(v[12],0);
v[14]=min(v[13],1);
WRITE("CurrentSkillBiais",v[14]);
RESULT(1 )


EQUATION("KNbrEngineers")
/*
Number of enginers is a share of the number of blue collars. Though they are mantained only when there are the available cumulated profits (their cost should not enter in teh price determination)
*/

v[1]=V("KCumProfit");
v[2]=VL("KWageEngineers",1);
v[3]=V("EngineersHiring"); // share of cuimulated profits devoted to increase the amount of engineers
//v[4]=V("KProfit");

v[4]=0;
v[7]=max(v[1],v[4]); // in case in which profits are negative for a long time but the firm achieve to sell some macineries, we assume it decides to increase the attractiveness of its capital
v[5]=v[3]*(v[7]/v[2]);	
v[8]=V("EngineersShare");
//v[9]=VS(p->up->son,"KNbrWorkers"); // number of first tier worker as a max to chose the number of engineers
v[9]=VS(p->up,"KNbrWorkers"); // number of first tier worker as a max to chose the number of engineers
v[10]=v[9]*v[8];
v[6]=max(v[5],0);
v[11]=min(v[10],v[6]);
v[12]=INCRS(p->up,"KCumProfit",-(v[11]*v[2]));

RESULT(v[11] )


EQUATION("KWageEngineers")
v[0]=V("MinWage");
v[1]=V("KEWagecoeff");
RESULT((v[0]*v[1]) )


EQUATION("KProfit")
/*
Comment
*/

V("KNbrEngineers"); // compute first the number of engineers which use the past value of cumulated profits
v[0]=V("KPrice");
v[1]=V("KProductionFlow");
v[8]=0;
CYCLE(cur, "KLabor")
 {
  v[2]=VS(cur,"KNbrWorkers");
  v[8]+=v[2];
 }
v[5]=V("AvKWage");
v[3]=V("KNbrEngineers");
v[4]=V("KWageEngineers");

v[6]=(v[0]*v[1])-(v[8]*v[5])-(v[3]*v[4]);
v[7]=INCR("KCumProfit",v[6]);

RESULT(v[6] )


EQUATION("KPrice")
/*
Comment
*/
v[0]=V("Kmarkup");
v[8]=V("KQ"); // productive capacity of the firm
v[9]=V("KLaborProductivity");
v[10]=v[9]/v[8];
v[18]=1;
v[17]=0;
CYCLE(cur, "KLabor")
 {
  v[11]=V("KWage");
  v[16]=VS(cur,"KNbrWorkers");
  v[17]+=v[11]*v[16];
  v[18]+=v[16];
 }
v[19]=v[17]/v[18];
WRITE("AvKWage",v[19]);
v[12]=V("KWageEngineers");
v[13]=V("KNbrEngineers");
v[14]=v[12]*v[13];
v[15]=v[14]/v[8];
v[7]=v[0]*(v[19]/v[9]+v[15]);

RESULT(v[7] )


EQUATION("KWage")
/*
Comment
*/
V("KNbrWorkers");
v[2]=V("IdKLabor");
if(v[2]==1)
 {
  v[0]=V("MinWage");
  v[1]=V("KWagecoeff");
 }
else
 {
  cur=SEARCH_CNDS(p->up,"IdKLabor",v[2]-1);
  v[0]=VS(cur,"KWage");
  v[1]=V("KWagecoeff"); // wage coefficient as the wage tier multiplier
 }
 
RESULT((v[0]*v[1]) )


EQUATION("KWagePrem")
/*
Wage premia distributed, when available to all classes of executives. Premia for single worker should be divided by the number of workers
*/

V("KProfit");
v[11]=VS(p->up,"EngineersHiring"); // ratio of profits used to invest in engineers in the following period
v[5]=VS(p->up,"KCumProfit");
if(v[5]>0)
 {
  v[1]=V("IdKLabor");
  if(v[1]>1)
   {
    v[3]=0;
    CYCLES(p->up, cur, "KLabor")
     { // make a fisrt cycle to compute the overall wage paied to the different layers of executives
      v[10]=VS(cur,"IdKLabor");
      if(v[10]>1)
       {
        v[2]=VS(cur,"KWage");
        v[3]+=v[2];
       }
     }
    v[9]=V("KWage");
    v[5]=VS(p->up,"KCumProfit"); // cumulated profits after subtracting investment expenditures, and adding current profits
    v[6]=(1-v[11])*v[5]; // amount of cumulated profits to pay premia
    v[7]=v[6]*v[9]/v[3]; // amount of premium to this class of workers
    v[8]=INCRS(p->up,"KCumProfit",-v[7]);
   }
  else
   v[7]=0;
 }
else
 v[7]=0;

RESULT(v[7] )


EQUATION("KNbrWorkers")
/*
Number of workesr in the capital sector firms
*/
v[1]=0;
CYCLES(p->up, cur, "Order")
 {
  v[11]=VS(cur,"KAmount");
  v[12]=VS(cur,"KCompletion"); 
  v[1]+=v[11]-v[12];
 }
v[15]=0;
CYCLES(p->up, cur, "KLabor")
 { //check how many tiers already exist
  v[15]++;
 }
v[14]=V("IdKLabor");
if(v[14]==1)
 {
  v[0]=VL("KNbrWorkers",1);
  v[2]=VL("KLaborProductivity",1);
  v[4]=V("KDesiredUnusedCapacity");
  v[3]=v[4]*(v[1]/v[2]);
  v[5]=V("KaNW");
  v[6]=v[0]*v[5]+(1-v[5])*v[3];
 }
else
 {// when above the first tier workers...
  v[18]=V("IdKLabor");
  cur=SEARCH_CNDS(p->up,"IdKLabor",v[18]-1); 
  v[19]=VS(cur,"KNbrWorkers"); //and the number of workers in the previous tier
  v[21]=VS(cur,"knu"); //given the worker ratio between tiers n the tier below
  v[6]=v[19]/v[21]; // compute the required executives
  v[17]=V("knu"); //given the worker ratio between tiers in the present tier
  if(v[6]>=v[17] && v[18]==v[15])
   { 
    cur1=ADDOBJS_EX(p->up,"KLabor",p);
    WRITES(cur1,"IdKLabor",v[18]+1);
    v[20]=v[6]/v[17];
    WRITELS(cur1,"KNbrWorkers",v[20], t);
    WRITELLS(cur1,"KNbrWorkers",0, t,1); // write also that the number of workers in the previous period is 0, to be used in the statistics
    v[22]=0;
    CYCLES(p->up->up->up,cur2, "Class")
     {
      v[22]++;
     }
    if(v[22]-1<v[18]+1)
     { /// if there is still not a class for the new tyoe of wage earner create one
      cur2=SEARCH_CNDS(p->up->up->up,"NumClass",v[18]);
      cur3=ADDOBJS_EX(cur2->up,"Class",cur2);
      WRITES(cur3,"NumClass",v[18]+1);
      WRITELS(cur3,"Expenditure",0, t-1);
      WRITELS(cur3,"ShareWageIncome",0, t-1);
      WRITELS(cur3,"ShareNonWageIncome",0, t-1);
      //WRITELS(cur3,"ShareProfitIncome",0, t-1);
      WRITELS(cur3,"Savings",0, t-1);
      // WRITELS(cur3,"NumIterations",0, t-1); to reactivate when NumIterations report the number of consumers in the labour class, as given in equation "ShareWageIncome". Otherwise the number of iterations simply define the number of representative conusmers (groups) in a class
      WRITELS(cur3,"ShareIncome",0, t-1); // reset the share income to be recomputed
      WRITES(cur3,"Individuals",v[20]); // set the number of individuals to nu;ber of workers of the new class
      v[35]=VS(cur2,"LorenzInd"); 
      WRITES(cur3,"LorenzInd",v[35]+v[20]); // set total number of workers as previous total plus new workers
      WRITELS(cur3,"SavingsTot",0, t-1); // set the total savings to 0
      WRITELS(cur3,"NoConsumption",0, t-1); // set the available income due to unavailability of goods to 0
      CYCLES(cur3, cur, "Need")
       { // enter in neds and characterisitcs to change the tau parameter (the minimum is set every period)
        v[27]=VS(cur,"IdNeed");
        CYCLES(cur, cur1, "DCh")
         {
          v[26]=VS(cur1,"IdDCh");
          CYCLES(cur2, cur4, "Need")
           { // cycle among the needs of the preceding class
            v[28]=VS(cur4,"IdNeed");
            if(v[28]==v[27])
             { // when in the same need as the one the new class is cycling cycle through the char of the preceding class
              CYCLES(cur4, cur5, "DCh")
               { 
                v[29]=VS(cur5,"IdDCh");
                if(v[29]==v[26])// when in the same characteristic the new class is cycling
                 v[30]=VS(cur5,"tau"); // read the value of the tau parameter
               }

             }
           }
          v[31]=VS(cur1,"tauMultiplier"); // the speed of adjustment of the tolerance level from one class to the following one
          v[34]=VS(cur1,"NegativeQuality");
          if(v[34]>0)
           v[33]=VS(p->up->up->up,"tauMax"); // the asympthotic level of the tolerance level (1 for qualities and 0 for price)
          if(v[34]<0)
           v[33]=VS(p->up->up->up,"tauMin"); // the asympthotic level of the tolerance level (1 for qualities and 0 for price)
          v[32]=v[30]*(1-v[31])+v[31]*v[33]; // adjustment in the treshold level of tolerance
          WRITES(cur1,"tau",v[32]); // finally write the tau for the new consumer class in each of its characteristic for each need      
         }

       }
     v[35]=VS(cur3,"ComputeShare"); // set the expenditure shares for the needs in the new class
     
     
     }
   }
  if(v[18]>2 && v[19]<v[21])
   v[6]=0;
 }

RESULT(v[6] )


EQUATION("KQ")
/*
Comment
*/

v[0]=V("KNbrWorkers");
v[1]=V("KLaborProductivity");

RESULT((v[0]*v[1]) )



EQUATION("MinWage")
/*
Sets the minimum wage for all categories, as an aggregate relation. Variables influecing overall wage are: aggregate productivity, inflation, and unemployment. 
Aggregate productivity?
Unemployment: to account for Beveridge curves we could use the suggishness in the hiring process, which gnerates rates of vacancies...
NOTE: probably it makes sense to use levels for all variables. That is, when the variable reaches a certain level, a wage resetting is unedergone: if inflation runs too high, wages are renegotiated, if aggregate productivity increase evidently, wage are renegotiated.

*/
V("NbrWorkers");
v[6]=(double)t;
v[0]=VL("MinWage",1);
v[10]=V("InitAggProd"); //the reference level of productivity 
v[2]=V("MovAvAggProd");
v[11]=V("IncrAggProd"); 
v[12]=v[10]+v[10]*v[11]; //required increase in productity to change the min wage
v[13]=V("MovAvPrice");
v[14]=V("InitAvPrice"); //the reference level of prices
v[15]=V("IncrAvPrice");
v[16]=v[14]+v[14]*v[15]; //required increase in prices to change the min wage
v[17]=V("MovAvUnemp2");
v[18]=VL("MovAvUnemp2",1);
v[19]=V("aMWL"); //weight of unemployment on change in min wage
v[20]=(v[17]/v[18])-1;

v[5]=(1-v[19])*v[0]+v[19]*(v[0]*(1-v[20])); //change in min wage due to changes in the labour market (as proxy of labour (excess) demand) although it should include the available number of workers, or use the beveridge curve versione, or whatever..

if(v[2]>v[12] && v[13]>v[16]) 
 { // discrete changes in the minimum wage occur when the wage is renegotiated due to changes in poductivity and in consumables prices
  v[3]=V("aMWA"); // weight of average productivity on changes in min wage
  v[4]=(v[2]/v[10])-1;
  v[21]=V("aMWP"); // weight of price on changes in min wage
  v[22]=(v[13]/v[14])-1;
  v[5]=(1-v[19]-v[3]-v[21])*v[0]+v[19]*(v[0]*(1-v[20]))+v[3]*(v[0]*(1+v[4]))+v[21]*(v[0]*(1+v[22]));
  WRITE("InitAggProd",v[2]);
  WRITE("InitAvPrice",v[13]);
 }

RESULT(v[5] )


EQUATION("AggProductivity")
/*
Comment
*/
v[0]=0;
v[1]=0;
CYCLE(cur2, "Supply")
 {
  CYCLES(cur2, cur, "Firm")
   {
    v[2]=VS(cur,"Q");
    v[0]+=v[2];
    CYCLES(cur, cur1, "Labor")
     {
      v[3]=VS(cur1,"NumWorkers");
      v[1]+=v[3];
     }
   }
 }
v[4]=0;
v[5]=0;
v[10]=0;
CYCLE(cur, "KFirm")
 {
 	v[7]=VS(cur,"KNbrEngineers");	
 	v[10]+=v[7];
    v[8]=VS(cur,"KProductionFlow");
    v[5]+=v[8];
   CYCLES(cur, cur1, "KLabor")
    {
     v[6]=VS(cur1,"KNbrWorkers");
  	  v[4]+=v[6];
    }

 }
v[9]=(v[0]+v[5])/(v[1]+v[4]+v[10]);
RESULT(v[9] )


EQUATION("MovAvAggProd")
/*
Exponential Moving Average of the aggregate productivity
*/

v[1]=VL("MovAvAggProd",1);
v[2]=VL("AggProductivity",1);
v[3]=V("aAgProd");
v[4]=v[3]*v[2]+(1-v[3])*v[1];

RESULT(v[4] )



EQUATION("Ms")
/*
Market share of each firm, computed differently only for firms in the consumable market
*/

v[1]=V("UnitSales");
v[2]=VS(p->up->up,"TotUSales");
v[3]=VL("Ms",1);
if(v[2]>0)
 v[4]=v[1]/v[2];
else
 v[4]=v[3];

RESULT(v[4] )


EQUATION("TotUSales")
/*
Total unit sold by the consumabe sector
*/

v[2]=0;
CYCLE(cur1, "Supply")
 {
  CYCLES(cur1, cur, "Firm")
   {
    v[1]=VS(cur,"UnitSales");
    v[2]+=v[1];
   }
 }

RESULT(v[2] )



EQUATION("AvPrice")
/*
Average prices
*/

v[3]=0;
CYCLE(cur1, "Supply")
 {
  CYCLES(cur1, cur, "Firm")
   {
    v[1]=VS(cur,"price");
    v[2]=VS(cur,"Ms");
    v[3]+=v[1]*v[2];
   }
 }

RESULT(v[3] )


EQUATION("MovAvPrice")
/*
Exponential Moving Average of the inflation
*/

v[1]=VL("MovAvPrice",1);
v[2]=VL("AvPrice",1);
v[3]=V("aAvPrice");
v[4]=v[3]*v[2]+(1-v[3])*v[1];


RESULT(v[4] )

EQUATION("Inflation")
/*
One period change in price in the conumables market
*/

v[1]=VL("AvPrice",1);
v[2]=V("AvPrice");
v[3]=v[2]/v[1]-1;

RESULT(v[3] )


EQUATION("Unemployment")
/*
The equation is used to design a very sketchy labour market in which unemployment may reduce wages. 
The highest number of workers reached in the economy through time is assumed to be the growing working population
When emplyment drops from this highest level, there is unemployment which is not the fucking 'frictional'
A more elaborate alternative for this rough proxy is in equation unemp
*/

v[2]=0;
CYCLE(cur2, "Supply")
 {
  CYCLES(cur2, cur, "Firm")
   {
    CYCLES(cur, cur1, "Labor")
     {
      v[1]=VS(cur1,"NumWorkers");
      v[2]+=v[1];
     }
  
   }
 }
CYCLE(cur, "KFirm")
 {
  CYCLES(cur, cur1, "KLabor")
   {
    v[3]=VS(cur1,"KNbrWorkers");
    v[2]+=v[3];
   }
  cur2=SEARCHS(cur,"KEngineers");
  v[4]=VS(cur2,"KNbrEngineers");
  v[2]+=v[4];
 }

v[5]=V("Employed");
//WRITE("NbrWorkers",v[2]);
if(v[2]>v[5])
 {
  WRITE("Employed",v[2]);
  v[6]=0;
  //if(V("Employed") != V("NbrWorkers"))
   //INTERACT("whats wong?", v[2]-v[5]);
 }
else
 v[6]=(v[5]-v[2])/v[5]*100;

RESULT(v[6] )


EQUATION("Vacancies")
/*
Rate of vacancies over the maximum number of workers (since t=0)
*/

V("Unemployment");
v[10]=v[11]=v[15]=v[16]=0;
CYCLE(cur3, "Supply")
 {
  CYCLES(cur3, cur, "Firm")
   {
    v[51]=0;
    CYCLES(cur, cur2, "Labor")
     { //check how many tiers already exist
      v[51]++;
     }
    CYCLES(cur, cur1, "Labor")
     {
      v[52]=V("IdLabor");
      if(v[52]==1)
       {// compute the vacancies in the first tier
        v[1]=VS(cur1,"NumWorkers");
        v[2]=VS(cur1,"ExpectedSales");
        v[3]=VS(cur1,"CapitalCapacity");
        v[4]=min(v[2],v[3]);
        v[5]=VLS(cur1,"SkillIntensity",1);
        v[6]=VLS(cur1,"MaxLaborProductivity",1);
        v[7]=v[5]/v[6]; // suppressed for the moment the use of skill bias, which should become a continuous variable
        v[13]=VS(cur,"DesiredUnusedCapacity");
        v[8]=v[4]*v[13]/v[6]; //the actual number of workers needed
        v[9]=VLS(cur1,"NumWorkers",1);
        v[12]=v[8]-v[9];
        if(v[12]>0) 
         v[10]+=v[8]-v[9]; // number of vacancies as the number of new workers that the firm needs with repsect to the previous period
        v[11]+=v[1]; // total number of workers
        v[14]=v[1]-v[9]; 
        if(v[14]>0)
         v[15]+=v[1]-v[9]; // number of workers hired, i.e. matched in teh labour market
        else
         v[16]+=v[9]-v[1];
       }
      else
       {//compute the vacancies in the executives tires
        v[1]=VS(cur1,"NumWorkers");
        v[9]=VLS(cur1,"NumWorkers",1);
        v[53]=V("nu"); //given the worker ratio between tiers
        if(v[8]>=v[53])
         v[8]=v[8]/v[53]; // compute the number of executies needed if in the previous layer there are more than nu workers
        else
         v[8]=v[8];
        v[12]=v[8]-v[9];
        if(v[12]>0) 
         v[10]+=v[8]-v[9]; // number of vacancies as the number of new workers that the firm needs with repsect to the previous period
        v[11]+=v[1]; // total number of executives
        if(v[14]>0)
         v[15]+=v[1]-v[9]; // number of executives hired, i.e. matched in teh labour market
        else
         v[16]+=v[9]-v[1];
       }
     }
  
   }
 }

CYCLE(cur, "KFirm")
 {
  v[23]=v[54]=0;
  CYCLES(cur, cur1, "Order")
   {
    v[21]=VS(cur1,"KAmount");
    v[22]=VS(cur1,"KCompletion");
    v[23]+=v[21]-v[22];
   }
  CYCLES(cur, cur2, "KLabor")
   { //check how many tiers already exist
    v[54]++;
   }
  CYCLES(cur, cur3, "KLabor")
   {
    v[55]=V("IdKLabor");
    if(v[55]==1)
     {
      v[24]=VS(cur3,"KNbrWorkers");
      v[25]=VS(cur3,"KLaborProductivity");
      v[32]=VS(cur3,"KDesiredUnusedCapacity");
      v[26]=v[23]/v[25]*v[32]; // actual number of kapital workers needed
      v[27]=VLS(cur3,"KNbrWorkers",1);
      v[28]=v[26]-v[27];
      if(v[28]>0)
       v[10]+=v[26]-v[27]; // sum to number of vacancies with respect to period t-1
      v[11]+=v[24]; // sum to number of hired workers
      v[33]=v[24]-v[27]; 
      if(v[33]>0) 
       v[15]+=v[24]-v[27]; // sum to number of matches
      else
       v[16]+=v[27]-v[24]; // sum to number of discarded
     }
    else
     {// when above the first tier K workers...
      v[24]=VS(cur3,"KNbrWorkers");
      v[27]=VLS(cur3,"KNbrWorkers",1);
      v[56]=V("knu"); //given the worker ratio between tiers
      if(v[26]>=v[56]) // if there is need for K executives
       v[26]=v[26]/v[56]; // compute the required K executive vacancies with respective to previous tire vacancies
      else
       v[26]=v[26];
      v[28]=v[26]-v[27];
      if(v[28]>0)
       v[10]+=v[26]-v[27]; // sum to number of vacancies with respect to period t-1
      v[11]+=v[24]; // sum to number of hired workers
      v[33]=v[24]-v[27]; 
      if(v[33]>0) 
       v[15]+=v[24]-v[27]; // sum to number of matches
      else
       v[16]+=v[27]-v[24]; // sum to number of discarded
     }
   }

  cur4=SEARCHS(cur,"KEngineers");
  v[29]=VLS(cur4,"KNbrEngineers",1);
  v[30]=VS(cur4,"KNbrEngineers");
  v[31]=v[30]-v[29];
  if(v[31]>0)
   v[10]+=v[31];
  v[11]+=v[30];
  v[34]=v[30]-v[29];
  if(v[34]>0)
   v[15]+=v[30]-v[29];
  else
   v[16]+=v[29]-v[30];
 }

v[40]=V("Employed");
v[41]=V("NbrWorkers");
if(v[41]-v[11]>0.00001)
 INTERACT("why two different computations for NbrWorkers", v[41]-v[11]);
WRITE("Hired",v[15]/v[40]); // ratio of new hired over the maximum number of workers (since t=0)
v[50]=V("TotHired");
WRITE("TotHiredLag",v[50]);
WRITE("TotHired",v[15]);
WRITE("JobLoss",v[16]);

RESULT(v[10]/v[40])


EQUATION("TotVacancies")
/*
Number of vacancies
*/

v[1]=V("Employed");
v[2]=V("Vacancies");

RESULT(v[2]*v[1] )


EQUATION("MovAvTotVac")
/*
Exponential Moving Average of Total vacancies
*/

v[1]=VL("MovAvTotVac",1);
v[2]=VL("TotVacancies",1);
v[3]=V("aTotVac");
v[4]=v[3]*v[2]+(1-v[3])*v[1];

RESULT(v[4] )


EQUATION("MovAvTotHired")
/*
Exponential Moving Average of Total people hired
*/
V("Vacancies");
v[1]=VL("MovAvTotHired",1);
v[2]=V("TotHiredLag");
v[3]=V("aTotHir");
v[4]=v[3]*v[2]+(1-v[3])*v[1];

RESULT(v[4] )



EQUATION("MovAvUnemp2")
/*
We derive the level of unemployment using directly the Beveridge curve, without generateing it. Two options:
Linear equation: U = Constant - beta*V (Nickell et al: beta=0.23; Wall & Zoega: beta=0.5; Teo et al: 0.3>beta<0.9)
Hyperbolic equation: U = Constant + beta/V (Börsch-Supan: c<1, beta=6)
Note that both Constant and beta should be quite different when using one or the other relation. See the Earnings_and_Consumption file
*/

v[1]=V("c");
v[2]=V("MovAvTotVac");
v[3]=V("beta");
//v[4]=v[1]-v[3]*v[2];
v[4]=v[1]+v[3]/v[2];

RESULT(v[4] )



EQUATION("NbrWorkers")
/*

*/

v[3]=v[8]=0;
CYCLE(cur2, "Supply")
 {
  CYCLES(cur2, cur, "Firm")
   {
    CYCLES(cur, cur1, "Labor")
     {
      v[1]=VS(cur1,"NumWorkers");
      v[2]=VLS(cur1,"NumWorkers",1);
      v[3]+=v[1];
      v[8]+=v[2];
     }
  
   }
 }
CYCLE(cur, "KFirm")
 {
  CYCLES(cur, cur1, "KLabor")
   {
    v[4]=VS(cur1,"KNbrWorkers");
    v[5]=VLS(cur1,"KNbrWorkers",1);
    v[3]+=v[4];
    v[8]+=v[5];
   }
  cur2=SEARCHS(cur,"KEngineers");
  v[6]=VS(cur2,"KNbrEngineers");
  v[7]=VLS(cur2,"KNbrEngineers",1);
  v[3]+=v[6];
  v[8]+=v[7];
 }

RESULT(v[3] )


EQUATION("MovAvNbrWork")
/*
Exponential Moving Average of the number of worwers
*/

v[1]=VL("MovAvNbrWork",1);
v[2]=VL("NbrWorkers",1);
v[3]=V("aNbrWork");
v[4]=v[3]*v[2]+(1-v[3])*v[1];

RESULT(v[4] )


EQUATION("ComputeShare")
/*
The Share is a function that is called when a classed is formed to determine the shares of expenditure across needs, as a change with respect to the previous class. 
he dynamic mimics engels laws (shiting the share of expenditures toward different needs, thus reducing the expenditure on 'basic' needs, as income increases)
This Share function simply normalise the shares computed in ExpShares
*/

v[1]=SUM("ExpShare"); //compute the sum of the newly computed expenditure shares
v[4]=v[7]=0;
CYCLE(cur, "Need")
 { // cycle through the needs
  v[2]=VS(cur,"ExpShare"); 
  v[3]=v[2]/v[1]; // normalise the share
  WRITES(cur,"Share",v[3]); // and fix it
  v[4]+=v[3]; // check that the sum is equal to 1
  v[5]=v[3]*100; // compute the number of iterations given the consumption shares
  v[6]=round(v[5]); // round to the closest integer
  WRITES(cur,"NumIterations",v[6]);
  v[7]+=v[6]; // check that the sum of interations is 100
 }

RESULT(v[4]+v[7] )


EQUATION("ExpShare")
/*
Change in the expenditure shares across classes
*/

v[1]=VS(p->up->up,"eta"); // iterrelations between the changes in the shares
v[2]=VS(p->up, "NumClass"); // check the current class for which the share is computed
v[5]=V("IdNeed");
v[3]=V("endExpShare"); // asymptotic value of the expenditure share for the current need (defined in the beginning symmetric to the first class distribution of shares)
CYCLES(p->up->up, cur, "Class")
 {
  v[4]=VS(cur,"NumClass"); 
  if(v[4]==v[2]-1)
   { // select the class below the one for which the shares are computed
    CYCLES(cur, cur1, "Need")
     { //  cycle through the different needs
      v[6]=VS(cur1,"IdNeed");
      if(v[6]==v[5])
       v[7]=VS(cur1,"Share"); // read the value of the Share
     }

   }
 }
v[8]=v[1]*(v[7]-v[3]);
v[9]=v[7]*(1-v[8]);

RESULT(v[9] )




/*******************************************************************************
INITIALISATIONS
********************************************************************************/




EQUATION("Init")
/*
Initialisation of initial values
*/

V("Init_x");

v[22]=VL("MinWage",1);
CYCLE(cur4, "Supply")
 {
  CYCLES(cur4, cur1, "Firm")
   {// run a first cycle trough firms to set the number of labor in t-1
    v[15]=0;
    CYCLES(cur1,cur, "Labor")
     { //check how many tiers already exist
      v[15]++;
     }
    CYCLES(cur1, cur, "Labor")
     {
      v[14]=V("IdLabor");
      if(v[14]==1)
       { // compute the first tier workers given their productivity and production needs
        //V("aNWDynamic");
        //v[0]=VL("NumWorkers",1);
        v[1]=VLS(cur1,"ExpectedSales",1); //use the value of expected sales to compute the number of workers in the initial period
        //v[1]=VS(cur1,"DesiredQ");
        //v[10]=V("backlog");
        //v[11]=v[1]+v[10];
        v[8]=VS(cur1,"CapitalCapacity");
        v[9]=min(v[1],v[8]);
        v[2]=VLS(cur1,"MaxLaborProductivity",1);
        v[4]=VS(cur1,"DesiredUnusedCapacity");
        v[12]=VS(cur,"SkillIntensity"); 
        v[7]=v[2]/v[12]; // suppressed for the moment the use of skill bias, which should become a continuous variable, rather than a dummy
        v[3]=v[4]*(v[9]/v[2]); // number of workers in the first layer in the first period	
        WRITELLS(cur,"NumWorkers",v[3],t,1);
        WRITELS(cur,"NumWorkers",v[3],t);
        v[21]=VS(cur,"wagecoeff");
        v[23]=v[22]*v[21];
        WRITELS(cur,"wage",v[23], t-1);
       }
      else
       {// when above the first tier workers...
        v[18]=VS(cur,"IdLabor");
        cur2=SEARCH_CNDS(cur->up,"IdLabor",v[18]-1); 
        v[17]=VS(cur2,"nu"); //given the worker ratio between tiers of the tier below
        v[19]=VS(cur2,"NumWorkers"); //and the number of workers in the previous tier
        v[6]=v[19]/v[17]; // compute the required executives
        v[24]=VS(cur,"nu"); // given the worker ration between tiers of the present tier (use different worker ratios in case we want to change it along the hierarchical structure)
        WRITELLS(cur,"NumWorkers",v[6], t,1);
        WRITELS(cur,"NumWorkers",v[6],t);
        v[25]=VLS(cur2,"wage",1);
        v[26]=VS(cur,"wagecoeff");
        v[27]=v[25]*v[26];
        WRITELS(cur,"wage",v[27], t-1);
        if(v[6]>=v[24] && v[18]==v[15])
         {
          cur3=ADDOBJS_EX(cur->up,"Labor",cur);
          WRITES(cur3,"IdLabor",v[18]+1);
          v[20]=v[6]/v[24];
          WRITELLS(cur3,"NumWorkers",v[20], t,1);
          WRITELS(cur3,"NumWorkers",v[20],t);
          v[28]=v[27]*v[26];
          WRITELS(cur3,"wage",v[28], t-1);
         }
       }
  
     }
   }
 }

CYCLE(cur2, "Supply")
 {
  CYCLES(cur2, cur, "Firm")
   {// a second cycle to set the price in t-1
    v[10]=VS(cur,"markup");
    v[21]=v[22]=0;
    CYCLES(cur,cur1, "Labor")
     {
      v[16]=VLS(cur1,"wage",1);
      v[17]=VLS(cur1,"NumWorkers",1);
      v[21]+=v[16]*v[17];
      v[22]+=v[17];
     }
    //if(v[21]==0)
     //v[11]=V("AvWage");
    //else
     v[11]=v[21]/v[22];  //av. wage in the firm
    WRITES(cur,"AvWage",v[11]);
    v[0]=v[31]=v[32]=0;
    CYCLES(cur,cur1, "Capital")
     {
      v[3]=VS(cur1,"MaxKQ");
      v[5]=VS(cur1,"IncProductivity");
      v[0]+=v[3];
      v[32]+=v[5]*v[3];
     }
    v[9]=v[32]/v[0]; //weighted av. incProductivity
    WRITES(cur,"AvIncProd",v[9]);
    v[14]=v[11]*v[10]/v[9];
    if(v[14]<0)
     v[14]=INTERACT("nEG. price", v[14]);
    WRITELS(cur,"price",v[14], t-1);
   }
 }

v[3]=v[8]=0;
CYCLE(cur2, "Supply")
 {
  CYCLES(cur2, cur, "Firm")
   {
    CYCLES(cur, cur1, "Labor")
  	{
      v[1]=VS(cur1,"NumWorkers");
      v[2]=VLS(cur1,"NumWorkers",1);
      v[3]+=v[1];
      v[8]+=v[2];
     }
  
   }
 }
CYCLE(cur, "KFirm")
 {
  CYCLES(cur, cur1, "KLabor")
   {
    v[4]=VS(cur1,"KNbrWorkers");
    v[5]=VLS(cur1,"KNbrWorkers",1);
    v[3]+=v[4];
    v[8]+=v[5];
   }
  cur2=SEARCHS(cur,"KEngineers");
  v[6]=VS(cur2,"KNbrEngineers");
  v[7]=VLS(cur2,"KNbrEngineers",1);
  v[3]+=v[6];
  v[8]+=v[7];
 }
cur=SEARCH("Country");
WRITELS(cur,"MovAvNbrWork",v[3],t);
v[10]=V("AvPrice");
WRITELS(cur,"AvPrice",v[10], t-1);
WRITELS(cur,"MovAvPrice",v[10], t-1);
WRITES(cur,"InitAvPrice",v[10]);
//WRITELS(cur,"MinWage",v[10],1);
v[11]=V("AggProductivity");
WRITELS(cur,"MovAvAggProd",v[11], t-1);
WRITELS(cur,"AggProductivity",v[11], t-1);
WRITES(cur,"InitAggProd",v[11]);

V("MinimumInit"); // assign the value for the Minimum parameter to all classes
V("TauInit");
V("ShareInit"); // assign the expenniture shares to the classes above the first one

v[30]=0;
CYCLE(cur, "Need")
 {
  v[30]++;
 }
WRITE("totNeeds",v[30]); // write the total number of needs in the economy (to be used in the product innovation)

PARAMETER;
RESULT(1 )


EQUATION("Init_x")
/*
Initialisation of the charactheristics (quality)
*/

CYCLE(cur, "Supply")
 {
  CYCLES(cur, cur1, "Firm")
   {
    CYCLES(cur1, cur2, "PNeed")
     {
      v[5]=VS(cur2,"IdPNeed");
      if(v[5]==0)
       { // set the initial value of the quality characteristic only for the product that is currently produced. The others are set to 0
        CYCLES(cur2, cur3, "Ch")
         {
          v[1]=VS(cur3,"IdCh");
          if(v[1]>1)
           { // if it is not a price characteristic set the initial value
            v[2]=V("Min_x");
            v[3]=V("Max_x");
            v[4]=UNIFORM(v[2],v[3]);
            WRITELS(cur3,"x",v[4], t-1);
           }
         }
       }

     }

   }

 }

PARAMETER;
RESULT(1 )


EQUATION("InitMarkup")
/*
Set the initial level of mark-up
*/

v[1]=V("Avx"); // average value of the quality characteristic across firms (computed from the initial setting)
v[2]=V("AvMarkup"); // average markup, used as a parameter to change the difference in firms pricing with respect to quality setting
v[8]=v[9]=0;
CYCLE(cur, "Supply")
 {
  CYCLES(cur, cur1, "Firm")
   {
    v[3]=VS(cur1,"AvxFirm");
    v[4]=(v[3]-v[1])/v[3]; // diffrence from the average value of quality
    v[5]=v[2]*v[4];
    v[6]=v[2]+v[5]; // equal change in the markup
    WRITES(cur1,"markup",1+v[6]);
    v[7]=VS(cur1,"markup");
    if(v[7]<1)
     INTERACT("A negative Markup setting. Check equation InitMarkup", v[6]);
    v[8]+=v[7];
    v[9]++;
   }

 }
v[10]=v[8]/v[9]; // check it is the same value of AvMarkup

PARAMETER;
RESULT(v[10] )


EQUATION("AvxFirm")
/*
Average level of quality characteristic in the firm (used to set the initial mark-up), turns into a parameter
*/

v[3]=v[4]=0;
CYCLE(cur, "PNeed")
 {
  CYCLES(cur, cur1, "Ch")
   {
    v[1]=VS(cur1,"IdCh");
    if(v[1]>1)
     {
      v[2]=VS(cur1,"x");
      v[3]+=v[2];
      v[4]++;
     }
   }

 }
v[5]=v[3]/v[4];

PARAMETER;
RESULT(v[5] )


EQUATION("MinimumInit")
/*
Initialisation of the minimum parameter across classes, needs and chars
*/

v[2]=V("MinimumPMinE"); // minimum value of the minimum parameter on price for engineers
v[3]=V("MinimumPMaxE"); // maximum value of the minimum parameter on price for engineers
v[4]=V("MinimumPMinW"); // minimum value of the minimum parameter on price for workers
v[5]=V("MinimumPMaxW"); // maximum value of the minimum parameter on price for workers
v[13]=V("MinimumCMinE"); // minimum value of the minimum parameter on any quality characteristic for engineers
v[14]=V("MinimumCMaxE"); // maximum value of the minimum parameter on any quality characteristic for engineers
v[15]=V("MinimumCMinW"); // minimum value of the minimum parameter on any quality characteristic for workers
v[16]=V("MinimumCMaxW"); // maximum value of the minimum parameter on any quality characteristic for workers
v[6]=1000000;
CYCLE(cur, "Supply")
 {
  CYCLES(cur, cur1, "Firm")
   {
    v[7]=VLS(cur1,"price",1);
    if(v[7]<v[6])
     v[6]=v[7];
   }

 }
v[10]=V("AvWagecoeff");
CYCLE(cur, "Demand")
 {
  CYCLES(cur, cur1, "Class")
   {
    v[1]=VS(cur1,"NumClass");
    if(v[1]<1)
     { // engineers
      CYCLES(cur1, cur2, "Need")
       {
        CYCLES(cur2, cur3, "DCh")
         {
          v[2]=VS(cur3,"IdDCh");
          if(v[2]==1)
           { // maintain the minimum above the minimum of the characteritic (price) only for the fist (price) characteritic
            v[8]=UNIFORM(v[2],v[3]);
            v[9]=VS(cur3,"Delta"); // variance of the distribution of the perceived characteritic around the actaul value
            v[11]=v[9]*v[6];
            v[12]=max(v[8],v[11]); // use either the initially set minimum or the price of the cheapest firm
            WRITES(cur3,"Minimum",v[12]);
           }
          else
           {
            v[17]=UNIFORM(v[13],v[14]);
            WRITES(cur3,"Minimum",v[17]);
           }
         }

       }

     }
    if(v[1]==1)
     {// first tier workers
      CYCLES(cur1, cur2, "Need")
       {
        CYCLES(cur2, cur3, "DCh")
         {
          v[17]=VS(cur3,"IdDCh");
          if(v[17]==1)
           {
            v[18]=UNIFORM(v[4],v[5]);
            v[19]=VS(cur3,"Delta");
            v[20]=v[19]*v[6];
            v[21]=max(v[18],v[20]);
            WRITES(cur3,"Minimum",v[21]);
           }
          else
           {
            v[22]=UNIFORM(v[15],v[16]);
            WRITES(cur3,"Minimum",v[22]);
           }
         }

       }

     }
    if(v[1]>1) 
     { // exectuives, above the first tier workers
      CYCLES(cur1, cur2, "Need")
       {
        CYCLES(cur2, cur3, "DCh")
         {
          v[23]=VS(cur3,"IdDCh");
          if(v[23]==1)
           {
            cur4=SEARCH_CNDS(cur,"NumClass",v[1]-1);
            CYCLES(cur4, cur5, "DCh")
             {
              v[24]=VS(cur5,"IdDCh");
              if(v[24]==1)
               v[25]=VS(cur5,"Minimum");
             }
            v[26]=v[25]*v[10];
            WRITES(cur3,"Minimum",v[26]);
           }
          else
           {
            v[27]=UNIFORM(v[15],v[16]);
            WRITES(cur3,"Minimum",v[27]);
           }
         }

       }

     }
   }
 }

PARAMETER
RESULT(1 )


EQUATION("TauInit")
/*
Set the initial values for the preferences (tolerance toward difference from max char in the market) across classes, needs and characteristics
*/

v[1]=V("tauMinE"); // minimum level of tolerance toward the best product in the market, for engineers
v[2]=V("tauMaxE"); // maximum level of tolerance toward the best product in the market, for engineers
CYCLE(cur, "Demand")
 {
  CYCLES(cur, cur1, "Class")
   {
    v[6]=VS(cur1,"NumClass");
    if(v[6]<1)
     {// engineers
      CYCLES(cur1, cur2, "Need")
       {
        CYCLES(cur2, cur3, "DCh")
         {
          v[7]=UNIFORM(v[1],v[2]);
          WRITES(cur3,"tau",v[7]);
         }

       }

     }
    if(v[6]==1)
     { // first tier workers
      CYCLES(cur1, cur2, "Need")
       {
        CYCLES(cur2, cur3, "DCh")         
        {
          v[3]=VS(cur3,"tauMinW"); // minimum level of tolerance toward the best product in the market, for workers
          v[4]=VS(cur3,"tauMaxW"); // maximum level of tolerance toward the best product in the market, for wokers
          v[8]=UNIFORM(v[3],v[4]);
          WRITES(cur3,"tau",v[8]);
         }

       }

     }
    if(v[6]>1)
     { // exectuives, above first tier
      CYCLES(cur1, cur2, "Need")
       {
        v[9]=VS(cur2,"IdNeed");
        CYCLES(cur2, cur3, "DCh")
         {
          v[10]=VS(cur3,"IdDCh");
          cur4=SEARCH_CNDS(cur,"NumClass",(v[6]-1));
          CYCLES(cur4, cur5, "Need")
           {
            v[11]=VS(cur5,"IdNeed");
            if(v[11]==v[9])
             { // check in the same need
              CYCLES(cur5, cur6, "DCh")
               { // and in the same characteristic
                v[12]=VS(cur6,"IdDCh");
                if(v[12]==v[10]) // if it is in the same characteristic and in the same need, in the previous class, take the value of tau
                 v[13]=VS(cur6,"tau");
               }

             }
           }
          v[5]=VS(cur3,"tauMultiplier"); // the speed of adjustment of the tolerance level from one class to the following one
          v[16]=VS(cur3,"NegativeQuality");
          if(v[16]>0)
           v[15]=V("tauMax"); // the asympthotic level of the tolerance level (1 for qualities and 0 for price)
          if(v[16]<0)
           v[15]=V("tauMin"); // the asympthotic level of the tolerance level (1 for qualities and 0 for price)
          v[14]=v[13]*(1-v[5])+v[5]*v[15]; // adjustment in the treshold level of tolerance
          WRITES(cur3,"tau",v[14]); // finally write the tau for the new consumer class in each of its characteristic for each need
         }

       }

     }
   }

 }

PARAMETER
RESULT(1 )


EQUATION("ShareInit")
/*
Set the initial distribution of shares (in the class above the first tier, engineers and firts tier are set exogenously)
*/

CYCLE(cur, "Demand")
 {
  CYCLES(cur, cur1, "Class")
   {
    v[1]=VS(cur1,"NumClass");
    if(v[1]>1)
     { // if the class is above the first tier of workers, compute the change in the expenditure shares with resepct to the first class
      v[2]=VS(cur1,"ComputeShare");
     }
   }

 }

PARAMETER
RESULT(1 )




/***** SOME STATISTICS ******/


EQUATION("IncomeDistribution")
/*
Herfindahl Index for the Income
*/
v[0]=V("WageIncome");
v[1]=V("NonWageIncome");
v[2]=0;
v[3]=0;
v[4]=0;
CYCLE(cur, "Class")
 {
  v[5]=VS(cur,"ShareWageIncome");
  v[6]=VS(cur,"ShareNonWageIncome");
  v[7]=(v[5]*v[0])+(v[6]*v[1]);
  v[8]=v[7]/(v[0]+v[1]);
  v[2]+=(v[8]*v[8]);
  v[3]+=(v[5]*v[5]);
  v[4]+=(v[6]*v[6]);
 }
WRITE("HerfTotalIncome",v[2]);
WRITE("HerfWageIncome",v[3]);
WRITE("HerfNonWageIncome",v[4]);
RESULT(1 )

EQUATION("InvHerfIndex")
/*
Inverse Herfindahl Index for firm sales in each sector
*/
v[0]=0;
v[1]=0;
CYCLE(cur, "Firm")
 {
  v[2]=VS(cur,"UnitSales");
  v[0]+=v[2];
 }
WRITE("TotalSales",v[0]);
CYCLE(cur, "Firm")
 {
 v[3]=VS(cur,"UnitSales");
 v[4]=v[3]/v[0];
 v[1]+=(v[4]*v[4]);
 }
v[5]=1/v[1];

RESULT(v[5] )


EQUATION("Andre")
/*
Lorenz curve
*/

v[10]=(double)t;
if(v[10]>1)
 {
  v[5]=v[6]=v[9]=0;
  v[4]=VL("NonWageIncome",1);
  v[11]=VL("WageIncome",1);
  CYCLE(cur, "Demand")
   {
    SORTS(cur,"Class","ShareIncome", "UP");
    CYCLES(cur, cur1, "Class")
     {
      v[1]=VLS(cur1,"ShareWageIncome",1);
      v[2]=VLS(cur1,"ShareNonWageIncome",1);
      v[12]=(v[1]*v[11])/(v[4]+v[11]);
      v[13]=(v[2]*v[4])/(v[4]+v[11]);
      v[3]=(v[12]+v[13]);
      WRITES(cur1,"Lorenz",v[3]+v[6]);
      v[6]=v[3]+v[6];
      v[5]+=v[3];
      v[8]=VS(cur1,"Individuals");
      v[9]+=v[8];
      WRITES(cur1,"LorenzInd",v[9]);
     }
    SORTS(cur,"Class","NumClass", "UP");
  
   }
 }

RESULT(v[5] )


EQUATION("ShareIncome")
/*
Share of income for each individual
*/

v[5]=v[8]=0;
v[1]=VL("ShareWageIncome",1);
v[2]=VL("ShareNonWageIncome",1);
v[11]=VLS(p->up->up,"WageIncome",1);
v[12]=VLS(p->up->up,"NonWageIncome",1);
v[13]=(v[1]*v[11])/(v[11]+v[12]);
v[14]=(v[2]*v[12])/(v[11]+v[12]);
v[4]=V("NumClass");
if(v[4]==0)
 {
  CYCLES(p->up->up, cur, "Machinery")
   {
    CYCLES(cur, cur1, "KFirm")
     {
      cur2=SEARCHS(cur1,"KEngineers");
      v[5]+=VLS(cur2,"KNbrEngineers",1);
     }
   } 
  if(v[5]>0)
   v[3]=(v[13]+v[14])/v[5];
  else
   v[3]=0;
  WRITE("Individuals",v[5]);
 }
else
 {
  CYCLES(p->up->up, cur, "Supply")
   {
    CYCLES(cur, cur1, "Firm")
     {
      CYCLES(cur1, cur2, "Labor")
       {
        v[6]=VS(cur2,"IdLabor");
        if(v[6]==v[4])
         {
          v[7]=VLS(cur2,"NumWorkers",1);
          v[8]+=v[7];
         }
       }

     }

   }
  CYCLES(p->up->up, cur, "Machinery")
   {
    CYCLES(cur, cur1, "KFirm")
     {
      CYCLES(cur1, cur2, "KLabor")
       {
        v[9]=VS(cur2,"IdKLabor");
        if(v[9]==v[4])
         {
          v[10]=VLS(cur2,"KNbrWorkers",1);
          v[8]+=v[10];
         }
                  
       }

     }

   }
  WRITE("Individuals",v[8]);
  if(v[8]>0)
   v[3]=(v[13]+v[14])/v[8];
  else
   v[3]=0;
 }

RESULT(v[3] )


EQUATION("TotIndividuals")
/*
Total number of consumers (workers) in t-1
*/

V("ShareIncome");
v[2]=0;
CYCLE(cur, "Demand")
 {
  CYCLES(cur, cur1, "Class")
   {
    v[1]=VS(cur1,"Individuals");
    v[2]+=v[1];
   }

 }

RESULT(v[2] )


EQUATION("AvIncome")
/*
Average income across classes in time t-1
*/

v[5]=(double)t;
if(v[5]>1)
 {
  V("ShareIncome");
  v[1]=VL("WageIncome",1);
  v[2]=VL("NonWageIncome",1);
  v[3]=V("TotIndividuals");
  v[4]=(v[1]+v[2])/v[3];
 }
else
 v[4]=1;

RESULT(v[4] )


EQUATION("Atkinson")
/*
Atkinson index of inequality for income in period t-1
*/

v[16]=(double)t;
if(v[16]>1)
 {
  v[10]=0;
  v[6]=V("AvIncome");
  v[8]=V("Aversion"); // parmeter for the aversion to inequality (changes which end of the income distribution has a higher weight in the index computation)
  CYCLE(cur, "Demand")
   {
    CYCLES(cur, cur1, "Class")
     {
      v[1]=VS(cur1,"ShareIncome");
      if(v[1]>0)
       {
        v[2]=VL("WageIncome",1);
        v[3]=VL("NonWageIncome",1);
        v[4]=VS(cur1,"Individuals");
        v[5]=v[1]*(v[2]+v[3]); 
        v[7]=v[1]/v[6]; // relation with average income
        v[9]=pow(v[5],(1-v[8]));
        v[17]=v[9]*v[4]; // sum of incomes for the whole class
       }
      else
       v[17]=0;
      v[10]+=v[17];
     }
  
   }
  v[11]=V("TotIndividuals");
  v[12]=v[10]/v[11];
  v[13]=1/(1-v[8]);
  v[14]=pow(v[12],v[13]);
  v[15]=1-v[14]/v[6];
 }
else
 v[15]=0;

RESULT(v[15] )


EQUATION("Gini")
/*
Gini coefficient in t-1
*/

v[20]=(double)t;
if(v[20]>1)
{
CYCLE(cur, "Demand")
 {
  v[14]=v[21]=0;
  CYCLES(cur, cur1, "Class")
   {
    v[1]=VS(cur1,"ShareIncome");
    if(v[1]>0)
     {
      v[2]=VL("WageIncome",1);
      v[3]=VL("NonWageIncome",1);
      v[22]=VLS(cur1,"ShareWageIncome",1);
      v[23]=VLS(cur1,"ShareNonWageIncome",1);
      v[24]=v[22]*v[2];
      v[25]=v[23]*v[3];
      v[5]=VS(cur1,"Individuals");
      v[6]=(v[24]+v[25]); // total class income of the comparing class
      v[7]=VS(cur1,"NumClass");
      v[21]++;
      CYCLES(cur, cur2, "Class")
       {
        v[8]=VS(cur2,"NumClass");
        if(v[8]!=v[7])
         { // if it is a different class 
          v[9]=VS(cur2,"ShareIncome");
          if(v[9]>0)
           {
            v[26]=VLS(cur2,"ShareWageIncome",1);
            v[27]=VLS(cur2,"ShareNonWageIncome",1);
            v[28]=v[26]*v[2];
            v[29]=v[27]*v[3];
            v[11]=VS(cur2,"Individuals");
            v[12]=(v[28]+v[29]); // total class income of the compared class
            v[13]=abs(v[6]-v[12]);
            v[14]+=v[13];
           }
         }
       }

     }
   }

 }
v[15]=V("AvIncome");
v[30]=(v[2]+v[3])/v[21]; // average income per class
v[16]=V("TotIndividuals");
v[17]=pow(v[21],2);
v[18]=2*v[17]*v[30];
v[19]=v[14]/v[18];
}
else
 v[19]=0;

RESULT(v[19] )


EQUATION("GdpNominal")
/*
Nominal GDP, at varying prices
*/

v[4]=0;
CYCLE(cur, "Supply")
 {
  CYCLES(cur, cur1, "Firm")
   {
    v[1]=VS(cur1,"UnitSales");
    v[2]=VS(cur1,"price");
    v[3]=v[1]*v[2];
    v[4]+=v[3];
   }

 }
CYCLE(cur, "Machinery")
 {
  CYCLES(cur, cur1, "KFirm")
   {
    v[5]=VS(cur1,"KProductionFlow");
    v[6]=VS(cur1,"KPrice");
    v[7]=v[5]*v[6];
    v[4]+=v[7];
   }

 }

RESULT(v[4] )


EQUATION("GdpConstant")
/*
GDP at constant prices
*/

v[1]=V("IndexYear");
v[2]=(double)t;
if(v[1]==v[2])
 {
  CYCLE(cur, "Supply")
   {
    CYCLES(cur, cur1, "Firm")
     {
      v[3]=VS(cur1,"price");
      WRITES(cur1,"ConstPrice",v[3]);
     }

   }
  CYCLE(cur, "Machinery")
   {
    CYCLES(cur, cur1, "KFirm")
     {
      v[4]=VS(cur1,"KPrice");
      WRITES(cur1,"KConstPrice",v[4]);
     }

   }


 }
if(v[1]<v[2])
 {
  v[8]=0;
  CYCLE(cur, "Supply")
   {
    CYCLES(cur, cur1, "Firm")
     {
      v[5]=VS(cur1,"UnitSales");
      v[6]=VS(cur1,"ConstPrice");
      v[7]=v[5]*v[6];
      v[8]+=v[7];
     }

   }
  CYCLE(cur, "Machinery")
   {
    CYCLES(cur, cur1, "KFirm")
     {
      v[9]=VS(cur1,"KProductionFlow");
      v[10]=VS(cur1,"KConstPrice");
      v[11]=v[9]*v[10];
      v[8]+=v[11];
     }

   }


 }
else
 v[8]=V("GdpNominal"); 

RESULT(v[8] )


EQUATION("GdpConstantF")
/*
GDP at constant prices in the final sector
*/

v[1]=V("IndexYear");
v[2]=(double)t;
V("GdpConstant");
if(v[1]<v[2])
 {
  v[8]=0;
  CYCLE(cur, "Supply")
   {
    CYCLES(cur, cur1, "Firm")
     {
      v[5]=VS(cur1,"UnitSales");
      v[6]=VS(cur1,"ConstPrice");
      v[7]=v[5]*v[6];
      v[8]+=v[7];
     }

   }

 }
else
 v[8]=V("GdpNominal"); 

RESULT(v[8] )


EQUATION("GdpConstantK")
/*
GDP at constant prices in the capital sector
*/

v[1]=V("IndexYear");
v[2]=(double)t;
if(v[1]<v[2])
 {
  v[8]=0;
  CYCLE(cur, "Machinery")
   {
    CYCLES(cur, cur1, "KFirm")
     {
      v[9]=VS(cur1,"KProductionFlow");
      v[10]=VS(cur1,"KConstPrice");
      v[11]=v[9]*v[10];
      v[8]+=v[11];
     }

   }

 }
else
 v[8]=V("GdpNominal"); 

RESULT(v[8] )


EQUATION("GdpNGrowth")
/*
Growth rate of the nominal GDP
*/

v[4]=(double)t;
if(v[4]>2)
 {
  v[1]=VL("GdpNominal",1);
  v[2]=V("GdpNominal");
  v[3]=(v[2]/v[1])-1;
 }
else
 v[3]=0;

RESULT(v[3] )


EQUATION("GdpCGrowth")
/*
Growth rate of the GDP at constant prices
*/

v[4]=(double)t;
if(v[4]>2)
 {
  v[1]=VL("GdpConstant",1);
  v[2]=V("GdpConstant");
  v[3]=(v[2]/v[1])-1;
 }
else
 v[3]=0;

RESULT(v[3] )


EQUATION("AvProfit")
/*
Average profits across firms
*/

v[2]=v[3]=0;
CYCLE(cur, "Supply")
 {
  CYCLES(cur, cur1, "Firm")
   {
    v[1]=VS(cur1,"Profit");
    v[2]+=v[1];
    v[3]++;
   }

 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("AvKProfit")
/*
Average of the capital firms profits
*/

v[2]=v[3]=0;
CYCLE(cur, "Machinery")
 {
  CYCLES(cur, cur1, "KFirm")
   {
    v[1]=VS(cur1,"KProfit");
    v[2]+=v[1];
    v[3]++;
   }

 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("AvKCumProfit")
/*
Average of the capital firms cumulated profits
*/

v[2]=v[3]=0;
CYCLE(cur, "Machinery")
 {
  CYCLES(cur, cur1, "KFirm")
   {
    v[1]=VS(cur1,"KCumProfit");
    v[2]+=v[1];
    v[3]++;
   }

 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("AvCumProfit")
/*
Average of firms cumulated profits
*/

v[2]=v[3]=0;
CYCLE(cur, "Supply")
 {
  CYCLES(cur, cur1, "Firm")
   {
    v[1]=VS(cur1,"CumProfit");
    v[2]+=v[1];
    v[3]++;
   }

 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Avx")
/*
Average value of the quality characteristic (non price characteristic that is not price)
*/

v[3]=v[4]=0;
CYCLE(cur, "Supply")
 {
  CYCLES(cur, cur1, "Firm")
   {
    CYCLES(cur1, cur2, "PNeed")
     {
      CYCLES(cur2, cur3, "Ch")
       {
        v[1]=VS(cur3,"IdCh");
        if(v[1]>1)
         { // to make it simple i assume that the first charateristic is price, this should be generalised
          v[2]=VS(cur3,"x");
          v[3]+=v[2];
          v[4]++;
         }
       }

     }

   }

 }
v[5]=v[3]/v[4];

RESULT(v[5] )


EQUATION("Sdx")
/*
Standard deviation of the quality characteristics across firms
*/

v[3]=V("Avx");
v[6]=v[7]=0;
CYCLE(cur, "Supply")
 {
  CYCLES(cur, cur1, "Firm")
   {
    CYCLES(cur1, cur2, "PNeed")
     {
      CYCLES(cur2, cur3, "Ch")
       {
        v[1]=VS(cur3,"IdCh");
        if(v[1]>1)
         {
          v[2]=VS(cur3,"x");
          v[4]=v[2]-v[3];
          v[5]=pow(v[4],2);
          v[6]+=v[5];
          v[7]++;
         }
       }

     }

   }

 }
v[8]=v[6]/v[7];
v[9]=sqrt(v[8]);

RESULT(v[9] )


EQUATION("SdPrice")
/*
Standard deviation of price across firms in the final market
*/

v[1]=V("AvPrice");
v[5]=v[6]=0;
CYCLE(cur, "Supply")
 {
  CYCLES(cur, cur1, "Firm")
   {
    v[2]=VS(cur1,"price");
    v[3]=v[2]-v[1];
    v[4]=pow(v[3],2);
    v[5]+=v[4];
    v[6]++;
   }

 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("AvtauQ")
/*
Average value of the tolerance level	toward quality characteristics
*/

v[3]=v[4]=0;
CYCLE(cur, "Demand")
 {
  CYCLES(cur, cur1, "Class")
   {
    CYCLES(cur1, cur2, "Need")
     {
      CYCLES(cur2, cur3, "DCh")
       {
        v[1]=VS(cur3,"NegativeQuality");
        if(v[1]>0)
         {
          v[2]=VS(cur3,"tau");
          v[3]+=v[2];
          v[4]++;
         }
       }

     }

   }

 }
v[5]=v[3]/v[4];

RESULT(v[5] )


EQUATION("AvtauP")
/*
Average value of the tolerance level toward price characteristic
*/

v[3]=v[4]=0;
CYCLE(cur, "Demand")
 {
  CYCLES(cur, cur1, "Class")
   {
    CYCLES(cur1, cur2, "Need")
     {
      CYCLES(cur2, cur3, "DCh")
       {
        v[1]=VS(cur3,"NegativeQuality");
        if(v[1]<0)
         {
          v[2]=VS(cur3,"tau");
          v[3]+=v[2];
          v[4]++;
         }
       }

     }

   }

 }
v[5]=v[3]/v[4];

RESULT(v[5] )


EQUATION("SdtauQ")
/*
Standard deviaiton of the tolerance level toward the quality characteristic across classes
*/

v[1]=V("AvtauQ");
v[6]=v[7]=0;
CYCLE(cur, "Demand")
 {
  CYCLES(cur, cur1, "Class")
   {
    CYCLES(cur1, cur2, "Need")
     {
      CYCLES(cur2, cur3, "DCh")
       {
        v[2]=VS(cur3,"NegativeQuality");
        if(v[2]>0)
         {
          v[3]=VS(cur3,"tau");
          v[4]=v[3]-v[1];
          v[5]=pow(v[4],2);
          v[6]+=v[5];
          v[7]++;
         }
       }

     }

   }

 }
v[8]=v[6]/v[7];
v[9]=sqrt(v[8]);

RESULT(v[9] )


EQUATION("SdtauP")
/*
Standard deviaiton of the tolerance level toward the price characteristic across classes
*/

v[1]=V("AvtauP");
v[6]=v[7]=0;
CYCLE(cur, "Demand")
 {
  CYCLES(cur, cur1, "Class")
   {
    CYCLES(cur1, cur2, "Need")
     {
      CYCLES(cur2, cur3, "DCh")
       {
        v[2]=VS(cur3,"NegativeQuality");
        if(v[2]<0)
         {
          v[3]=VS(cur3,"tau");
          v[4]=v[3]-v[1];
          v[5]=pow(v[4],2);
          v[6]+=v[5];
          v[7]++;
         }
       }

     }

   }

 }
v[8]=v[6]/v[7];
v[9]=sqrt(v[8]);

RESULT(v[9] )


EQUATION("Dtau")
/*
Difference between the minimum and the maximum asymptoti level of tau: gives the theoretical heterogeneity in consuemrs preferces, while the standard deviaiton depends on how many tiers develop through time (as it is affected by the speed of change)
*/

v[1]=V("tauMax");
v[2]=V("tauMin");
v[3]=v[1]-v[2];

RESULT(v[3] )


EQUATION("SSales")
/*
Total monetary sales of a sector
*/

V("Trade");
v[1]=V("IdGood");
v[3]=0;
CYCLES(p->up, cur, "Supply")
 {
  CYCLES(cur, cur1, "Firm")
   { // cycle through all firms
    v[2]=VS(cur1,"product");
    if(v[2]==v[1])
     {
      v[4]=VS(cur1,"MonetarySales");
      v[3]+=v[4];
     }
   }

 }

RESULT(v[3] )


EQUATION("MovAvSSales")
/*
Exponential Moving Average of the sector's sales
*/

v[1]=VL("MovAvSSales",1);
v[2]=VL("SSales",1);
v[3]=V("aSSales");
v[4]=v[3]*v[2]+(1-v[3])*v[1];

RESULT(v[4] )


EQUATION("NFirmsS")
/*
Number of firms per sector
*/

v[3]=0;
v[1]=V("IdGood");
CYCLES(p->up, cur, "Supply")
 {
  CYCLES(cur, cur1, "Firm")
   {
    v[2]=VS(cur1,"product");
    if(v[2]==v[1])
     v[3]++;
   }

 }

RESULT(v[3] )


EQUATION("HerfIndexS")
/*
Herfindahl indexin each sector
*/

v[4]=v[8]=0;
v[1]=V("IdGood");
CYCLES(p->up, cur, "Supply")
 {
  CYCLES(cur, cur1, "Firm")
   {
    v[2]=VS(cur1,"product");
    if(v[2]==v[1])
     {
      v[3]=VS(cur1,"UnitSales");
      v[4]+=v[3];
     }
   }

 }
WRITE("TotSalesS",v[4]);
CYCLES(p->up, cur, "Supply")
 {
  CYCLES(cur, cur1, "Firm")
   {
    v[5]=VS(cur1,"product");
    if(v[5]==v[1])
     {
      v[6]=VS(cur1,"UnitSales");
      v[7]=v[6]/v[4];
      WRITES(cur1,"MsSector",v[7]);
      v[8]+=(v[7]*v[7]);
     }
   }

 }

RESULT(v[8] )


EQUATION("AvxS")
/*
Averge quality of the good within a sector, weighted by firms market share
*/

V("HerfIndexS");
v[1]=V("IdGood");
v[2]=v[7]=0;
CYCLES(p->up, cur, "Supply")
 {
  CYCLES(cur, cur1, "Firm")
   {
    v[3]=VS(cur1,"product");
    if(v[3]==v[1])
     { // only for the firms that produce the product of this sector
      CYCLES(cur1, cur2, "PNeed")
       { 
        v[4]=VS(cur2,"IdPNeed");
        if(v[4]==0)
         {// only for the product currently produced (excluing the prototypes)
          CYCLES(cur2, cur3, "Ch")
           {
            v[5]=VS(cur3,"IdCh");
            if(v[5]==2)
             {// only for the quality characteristic
              v[6]=VS(cur3,"x"); // check the value of the quality
              v[7]=VS(cur1,"MsSector");
              v[2]+=v[6]*v[7];
             }
           }

         }
       }

     }
   }

 }

RESULT(v[2])


EQUATION("AvpS")
/*
Averge price of the good within a sector, weighted by firms market share
*/

V("HerfIndexS");
v[1]=V("IdGood");
v[2]=v[7]=0;
CYCLES(p->up, cur, "Supply")
 {
  CYCLES(cur, cur1, "Firm")
   {
    v[3]=VS(cur1,"product");
    if(v[3]==v[1])
     { // only for the firms that produce the product of this sector
      CYCLES(cur1, cur2, "PNeed")
       { 
        v[4]=VS(cur2,"IdPNeed");
        if(v[4]==0)
         {// only for the product currently produced (excluing the prototypes)
          CYCLES(cur2, cur3, "Ch")
           {
            v[5]=VS(cur3,"IdCh");
            if(v[5]==1)
             {// only for the price characteristic
              v[6]=VS(cur3,"x"); // check the price
              v[7]=VS(cur1,"MsSector");
              v[2]+=v[6]*v[7];
             }
           }

         }
       }

     }
   }

 }

RESULT(v[2])


EQUATION("NSectors")
/*
Number of sectors (needs) active 
*/

v[2]=0;
CYCLE(cur, "Sectors")
 {
  v[1]=VS(cur,"SSales");
  if(v[1]>0)
   v[2]++;
 }


RESULT(v[2] )


EQUATION("ShareDiff")
/*
Spread of the distribution of consumption share, measured as the difference between the minimum and the maximum share os the asymptotic distribution used 
Other statistics as the skewness or the kurtosis capture only partially the difference between a uniform distribution and a skewed one
*/

v[4]=0;
v[5]=1;
CYCLE(cur, "Demand")
 {
  CYCLES(cur, cur1, "Class")
   {
    v[1]=VS(cur1,"NumClass");
    if(v[1]==2)
     { // use th value of the first tier class
      CYCLES(cur1, cur2, "Need")
       {
        v[2]=VS(cur2,"endExpShare");
        if(v[2]>v[4])
         v[4]=v[2]; // record the maximum value of share across the needs
        if(v[2]<v[5])
         v[5]=v[2]; // record the minimum value of share across needs
       }

     }
   }

 }
v[6]=v[4]-v[5];

PARAMETER;
RESULT(v[6] )




/********************************************************************************************

STATISTICS OVER THE RANDOM RUNS

********************************************************************************************/

EQUATION("Init_Init")
/*
Requires first to initialise all the simulation runs
*/
CYCLE(cur, "Country")
 {
  VS(cur,"Init");
 }
PARAMETER
RESULT(1 )


// AVERAGES


EQUATION("Av_InvHerfIndex")
/*
Average of firms inverse Herfindahl index, across runs
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  cur1=SEARCHS(cur,"Supply");
  v[1]=VS(cur1,"InvHerfIndex");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_Computations")
/*
Activate the equation for income distribution HI, Lorenz curve
*/

CYCLE(cur, "Country")
 {
  cur1=SEARCHS(cur,"Demand");
  VS(cur1,"IncomeDistribution");
  VS(cur,"Andre");
 }

RESULT(1 )


EQUATION("Av_HerfTotalIncome")
/*
Average of the Herfindahl Index for classes income across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  cur1=SEARCHS(cur,"Demand");
  v[1]=VS(cur1,"HerfTotalIncome");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_HerfWageIncome")
/*
Average of the Herfindahl Index for classes wage across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  cur1=SEARCHS(cur,"Demand");
  v[1]=VS(cur1,"HerfWageIncome");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_HerfNonWageIncome")
/*
Average of the Herfindahl Index for classes non wage income across simulations
*/
v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  cur1=SEARCHS(cur,"Demand");
  v[1]=VS(cur1,"HerfNonWageIncome");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvIncome")
/*
Average across runs of the average income across classes
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"AvIncome");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_Atkinson")
/*
Average across runs of the Atkinson inequality index
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"Atkinson");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_Gini")
/*
Average across runs of the Gini inequality index
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"Gini");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_GdpNominal")
/*
Average across runs of the GDP at variable prices
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"GdpNominal");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_GdpConstant")
/*
Average across runs of the GDP at constant prices
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"GdpConstant");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_GdpNGrowth")
/*
Average across runs of the GDP growth at variable prices
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"GdpNGrowth");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_GdpCGrowth")
/*
Average across runs of the GDP at constant prices
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"GdpCGrowth");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_MinWage")
/*
Average across runs of the minimum wage
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"MinWage");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AggProductivity")
/*
Average across runs of the aggregate productivity
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"AggProductivity");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvPrice")
/*
Average across runs of the average price across firms 
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"AvPrice");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_MovAvNbrWork")
/*
Average across runs of the moving average of total number of workers (final and kapital sector)
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"MovAvNbrWork");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_MovAvTotVac")
/*
Average across runs of the moving average of total vacancies offered by firms
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"MovAvTotVac");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_MovAvUnemp")
/*
Average across runs of the moving average of total unemployment (computed with the beveridge curve)
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"MovAvUnemp2");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvProfit")
/*
Average across runs of the average profits across firms 
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"AvProfit");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvKPrice")
/*
Average across runs of the average price across capital firms 
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  cur1=SEARCHS(cur,"Machinery");
  v[1]=VS(cur1,"AvKPrice");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvKProfit")
/*
Average across runs of the average profits across capital firms 
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"AvKProfit");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvKCumProfit")
/*
Average across runs of the average cumulated profits across capital firms 
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"AvKCumProfit");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvCumProfit")
/*
Average across runs of the average cumulated profits across firms 
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"AvCumProfit");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_Avx")
/*
Average across runs of the average level of quality characteristics
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"Avx");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_Sdx")
/*
Average across runs of the standard deviation of quality characteristics across firms
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"Sdx");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_SdPrice")
/*
Average across runs of the standard deviation of price across firms
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"SdPrice");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvtauQ")
/*
Average across runs of the average level of tolerance toward quality charactristics
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"AvtauQ");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvtauP")
/*
Average across runs of the average level of tolerance toward price charactristic
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"AvtauP");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_SdtauQ")
/*
Average across runs of the standard deviation of tolerance toward quality charactristics
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"SdtauQ");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_SdtauP")
/*
Average across runs of the standard deviation of tolerance toward price charactristic
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"SdtauP");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_WageIncome")
/*
Average of the total incom from wage
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"WageIncome");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_NonWageIncome")
/*
Average of the total incom from wage
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"NonWageIncome");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_HerfInd1")
/*
Average herfindhal index in the first sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==1)
     {
      v[1]=VS(cur1,"HerfIndexS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_HerfInd2")
/*
Average herfindhal index in the second sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==2)
     {
      v[1]=VS(cur1,"HerfIndexS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_HerfInd3")
/*
Average herfindhal index in the third sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==3)
     {
      v[1]=VS(cur1,"HerfIndexS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_HerfInd4")
/*
Average herfindhal index in the fourth sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==4)
     {
      v[1]=VS(cur1,"HerfIndexS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_HerfInd5")
/*
Average herfindhal index in the fifth sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==5)
     {
      v[1]=VS(cur1,"HerfIndexS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_HerfInd6")
/*
Average herfindhal index in the sixth sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==6)
     {
      v[1]=VS(cur1,"HerfIndexS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_HerfInd7")
/*
Average herfindhal index in the seventh sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==7)
     {
      v[1]=VS(cur1,"HerfIndexS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_HerfInd8")
/*
Average herfindhal index in the eight sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==8)
     {
      v[1]=VS(cur1,"HerfIndexS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_HerfInd9")
/*
Average herfindhal index in the ninth sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==9)
     {
      v[1]=VS(cur1,"HerfIndexS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_HerfInd10")
/*
Average herfindhal index in the tenth sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==10)
     {
      v[1]=VS(cur1,"HerfIndexS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvxS1")
/*
Average quality in the first sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==1)
     {
      v[1]=VS(cur1,"AvxS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvxS2")
/*
Average quality in the second sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==2)
     {
      v[1]=VS(cur1,"AvxS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvxS3")
/*
Average quality in the third sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==3)
     {
      v[1]=VS(cur1,"AvxS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvxS4")
/*
Average quality in the fourth sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==4)
     {
      v[1]=VS(cur1,"AvxS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvxS5")
/*
Average quality in the fifth sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==5)
     {
      v[1]=VS(cur1,"AvxS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvxS6")
/*
Average quality in the sixth sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==6)
     {
      v[1]=VS(cur1,"AvxS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvxS7")
/*
Average quality in the seventh sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==7)
     {
      v[1]=VS(cur1,"AvxS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvxS8")
/*
Average quality in the eight sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==8)
     {
      v[1]=VS(cur1,"AvxS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvxS9")
/*
Average quality in the ninth sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==9)
     {
      v[1]=VS(cur1,"AvxS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvxS10")
/*
Average quality in the tenth sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==10)
     {
      v[1]=VS(cur1,"AvxS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvpS1")
/*
Average price in the first sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==1)
     {
      v[1]=VS(cur1,"AvpS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvpS2")
/*
Average price in the second sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==2)
     {
      v[1]=VS(cur1,"AvpS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvpS3")
/*
Average price in the third sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==3)
     {
      v[1]=VS(cur1,"AvpS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvpS4")
/*
Average price in the fourth sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==4)
     {
      v[1]=VS(cur1,"AvpS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvpS5")
/*
Average price in the fifth sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==5)
     {
      v[1]=VS(cur1,"AvpS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvpS6")
/*
Average price in the sixth sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==6)
     {
      v[1]=VS(cur1,"AvpS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvpS7")
/*
Average price in the seventh sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==7)
     {
      v[1]=VS(cur1,"AvpS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvpS8")
/*
Average price in the eight sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==8)
     {
      v[1]=VS(cur1,"AvpS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvpS9")
/*
Average price in the ninth sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==9)
     {
      v[1]=VS(cur1,"AvpS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_AvpS10")
/*
Average price in the tenth sector, across simulations
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[5]=VS(cur1,"IdGood");
    if(v[5]==10)
     {
      v[1]=VS(cur1,"AvpS");
      v[2]+=v[1];
      v[3]++;
     }
   }
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


EQUATION("Av_NSectors")
/*
Average number of active sectors in the market
*/

v[2]=v[3]=0;
CYCLE(cur, "Country")
 {
  v[1]=VS(cur,"NSectors");
  v[2]+=v[1];
  v[3]++;
 }
v[4]=v[2]/v[3];

RESULT(v[4] )


// STANDARD DEVIATIONS


EQUATION("Sd_InvHerfIndex")
/*
Standard deviation of the average Herfindahl index across simulations
*/

v[1]=V("Av_InvHerfIndex");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  cur1=SEARCHS(cur,"Supply");
  v[2]=VS(cur1,"InvHerfIndex");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_HerfTotalIncome")
/*
Standard deviation of the average Inverse Herfindahl index for classes total income across simulations
*/

v[1]=V("Av_HerfTotalIncome");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  cur1=SEARCHS(cur,"Demand");
  v[2]=VS(cur1,"HerfTotalIncome");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_HerfWageIncome")
/*
Standard deviation of the average Inverse Herfindahl index for classes wage across simulations
*/

v[1]=V("Av_HerfWageIncome");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  cur1=SEARCHS(cur,"Demand");
  v[2]=VS(cur1,"HerfWageIncome");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_HerfNonWageIncome")
/*
Standard deviation of the average Inverse Herfindahl index for classes non wage income across simulations
*/

v[1]=V("Av_HerfNonWageIncome");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  cur1=SEARCHS(cur,"Demand");
  v[2]=VS(cur1,"HerfNonWageIncome");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_AvIncome")
/*
Standard deviation of average income across classes
*/

v[1]=V("Av_AvIncome");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"AvIncome");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_Atkinson")
/*
Standard deviation of the Atkinson index across runs
*/

v[1]=V("Av_Atkinson");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"Atkinson");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_Gini")
/*
Standard deviation of the Gini index across runs
*/

v[1]=V("Av_Gini");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"Gini");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_GdpNominal")
/*
Standard deviation of the Gdp at variable prices across runs
*/

v[1]=V("Av_GdpNominal");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"GdpNominal");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_GdpConstant")
/*
Standard deviation of the Gdp at constant prices across runs
*/

v[1]=V("Av_GdpConstant");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"GdpConstant");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_GdpNGrowth")
/*
Standard deviation of the Gdp growth at variable prices across runs
*/

v[1]=V("Av_GdpNGrowth");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"GdpNGrowth");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_GdpCGrowth")
/*
Standard deviation of the Gdp growth at constant prices across runs
*/

v[1]=V("Av_GdpCGrowth");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"GdpCGrowth");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_MinWage")
/*
Standard deviation of the minimum wage across runs
*/

v[1]=V("Av_MinWage");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"MinWage");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_AggProductivity")
/*
Standard deviation of the aggregate productivity across runs
*/

v[1]=V("Av_AggProductivity");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"AggProductivity");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_AvPrice")
/*
Standard deviation of the average price across runs
*/

v[1]=V("Av_AvPrice");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"AvPrice");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_MovAvNbrWork")
/*
Standard deviation of the moving average of total number of workers across runs
*/

v[1]=V("Av_MovAvNbrWork");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"MovAvNbrWork");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_MovAvTotVac")
/*
Standard deviation of the moving average of total vacancies offered by firms, across runs
*/

v[1]=V("Av_MovAvTotVac");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"MovAvTotVac");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_MovAvUnemp")
/*
Standard deviation of the moving average of unemployment across runs
*/

v[1]=V("Av_MovAvUnemp");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"MovAvUnemp2");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_AvProfit")
/*
Standard deviation of the average profits acrss firms, across runs
*/

v[1]=V("Av_AvProfit");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"AvProfit");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_AvKPrice")
/*
Standard deviation of the average price across capital firms, across runs
*/

v[1]=V("Av_AvKPrice");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  cur1=SEARCHS(cur,"Machinery");
  v[2]=VS(cur1,"AvKPrice");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_AvKProfit")
/*
Standard deviation of the average profits across capital firms, across runs
*/

v[1]=V("Av_AvKProfit");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"AvKProfit");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_AvKCumProfit")
/*
Standard deviation of the average cumulated profits across capital firms, across runs
*/

v[1]=V("Av_AvKCumProfit");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"AvKCumProfit");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_AvCumProfit")
/*
Standard deviation of the average cumulated profits across firms, across runs
*/

v[1]=V("Av_AvCumProfit");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"AvCumProfit");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_Avx")
/*
Standard deviation of the average quality characteristics across firms
*/

v[1]=V("Av_Avx");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"Avx");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_Sdx")
/*
Standard deviation of the standard deviation of quality characteristics across firms
*/

v[1]=V("Av_Sdx");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"Sdx");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_SdPrice")
/*
Standard deviation of the standard devaiton of price across firms
*/

v[1]=V("Av_SdPrice");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"SdPrice");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_AvtauQ")
/*
Standard deviation of the average value of tolerance toward quality characteristics
*/

v[1]=V("Av_AvtauQ");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"AvtauQ");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_AvtauP")
/*
Standard deviation of the average value of tolerance toward price characteristic
*/

v[1]=V("Av_AvtauP");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"AvtauP");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_SdtauQ")
/*
Standard deviation of the standard devaition of tolerance toward quality characteristics
*/
v[1]=V("Av_SdtauQ");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"SdtauQ");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_SdtauP")
/*
Standard deviation of the average value of tolerance toward quality characteristics
*/

v[1]=V("Av_SdtauP");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"SdtauP");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )



EQUATION("Sd_HerfInd1")
/*
Standard deviation of the average value of Herfindal index in sector 1
*/

v[1]=V("Av_HerfInd1");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[9]=VS(cur1,"IdGood");
    if(v[9]==1)
     {
      v[2]=VS(cur1,"HerfIndexS");
      v[3]=v[2]-v[1];
      v[4]=pow(v[3],2);
      v[5]+=v[4];
      v[6]++;
     }    
   }


 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_HerfInd2")
/*
Standard deviation of the average value of Herfindal index in sector 2
*/

v[1]=V("Av_HerfInd2");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[9]=VS(cur1,"IdGood");
    if(v[9]==2)
     {
      v[2]=VS(cur1,"HerfIndexS");
      v[3]=v[2]-v[1];
      v[4]=pow(v[3],2);
      v[5]+=v[4];
      v[6]++;
     }    
   }


 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_HerfInd3")
/*
Standard deviation of the average value of Herfindal index in sector 3
*/

v[1]=V("Av_HerfInd3");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[9]=VS(cur1,"IdGood");
    if(v[9]==3)
     {
      v[2]=VS(cur1,"HerfIndexS");
      v[3]=v[2]-v[1];
      v[4]=pow(v[3],2);
      v[5]+=v[4];
      v[6]++;
     }    
   }


 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_HerfInd4")
/*
Standard deviation of the average value of Herfindal index in sector 4
*/

v[1]=V("Av_HerfInd4");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[9]=VS(cur1,"IdGood");
    if(v[9]==4)
     {
      v[2]=VS(cur1,"HerfIndexS");
      v[3]=v[2]-v[1];
      v[4]=pow(v[3],2);
      v[5]+=v[4];
      v[6]++;
     }    
   }


 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_HerfInd5")
/*
Standard deviation of the average value of Herfindal index in sector 5
*/

v[1]=V("Av_HerfInd5");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[9]=VS(cur1,"IdGood");
    if(v[9]==5)
     {
      v[2]=VS(cur1,"HerfIndexS");
      v[3]=v[2]-v[1];
      v[4]=pow(v[3],2);
      v[5]+=v[4];
      v[6]++;
     }    
   }


 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_HerfInd6")
/*
Standard deviation of the average value of Herfindal index in sector 6
*/

v[1]=V("Av_HerfInd6");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[9]=VS(cur1,"IdGood");
    if(v[9]==6)
     {
      v[2]=VS(cur1,"HerfIndexS");
      v[3]=v[2]-v[1];
      v[4]=pow(v[3],2);
      v[5]+=v[4];
      v[6]++;
     }    
   }


 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_HerfInd7")
/*
Standard deviation of the average value of Herfindal index in sector 7
*/

v[1]=V("Av_HerfInd7");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[9]=VS(cur1,"IdGood");
    if(v[9]==7)
     {
      v[2]=VS(cur1,"HerfIndexS");
      v[3]=v[2]-v[1];
      v[4]=pow(v[3],2);
      v[5]+=v[4];
      v[6]++;
     }    
   }


 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_HerfInd8")
/*
Standard deviation of the average value of Herfindal index in sector 8
*/

v[1]=V("Av_HerfInd8");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[9]=VS(cur1,"IdGood");
    if(v[9]==8)
     {
      v[2]=VS(cur1,"HerfIndexS");
      v[3]=v[2]-v[1];
      v[4]=pow(v[3],2);
      v[5]+=v[4];
      v[6]++;
     }    
   }


 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_HerfInd9")
/*
Standard deviation of the average value of Herfindal index in sector 9
*/

v[1]=V("Av_HerfInd9");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[9]=VS(cur1,"IdGood");
    if(v[9]==9)
     {
      v[2]=VS(cur1,"HerfIndexS");
      v[3]=v[2]-v[1];
      v[4]=pow(v[3],2);
      v[5]+=v[4];
      v[6]++;
     }    
   }


 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_HerfInd10")
/*
Standard deviation of the average value of Herfindal index in sector 10
*/

v[1]=V("Av_HerfInd10");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  CYCLES(cur, cur1, "Sectors")
   {
    v[9]=VS(cur1,"IdGood");
    if(v[9]==10)
     {
      v[2]=VS(cur1,"HerfIndexS");
      v[3]=v[2]-v[1];
      v[4]=pow(v[3],2);
      v[5]+=v[4];
      v[6]++;
     }    
   }


 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )


EQUATION("Sd_NSectors")
/*
Standard deviation of the average number of active setors (marketed needs) across simulations
*/

v[1]=V("Av_NSectors");
v[5]=v[6]=0;
CYCLE(cur, "Country")
 {
  v[2]=VS(cur,"NSectors");
  v[3]=v[2]-v[1];
  v[4]=pow(v[3],2);
  v[5]+=v[4];
  v[6]++;
 }
v[7]=v[5]/v[6];
v[8]=sqrt(v[7]);

RESULT(v[8] )



MODELEND



void close_sim(void)
{

}




/*

Notes Tum

- Why does the order of preference make the differences the way in whch the code is now? firms below the threshold are thrown cycling through any of the carachteristic. Got it, we actually need to define a truncation of the characteristics browsing to allow for the choice of specific goods.

- We need to insert a random order of characteritic in some INIT equation

- In the TTB we do not allow for a distribution of needs in a class: acounted for by the 'share' parameter, exogenous
*/


/*

Notes, just to not forget:

- ProdCost should be smoothed in order to avoid sudden movements of prices

- Among the characteristics of product should be included: visibility (i.e. smoothed market shares+marketing) and price (to be inverted, probably quite a mess).



*/





