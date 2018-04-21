data one; infile 'c:\megon\docs\npp\dbhall.dat' stopover dlm='09'x;
  input tran$ plot$ numb$ species$ status$ dia87 dia88 dia89;

array biomass{3} bio87 bio88 bio89;

/*use only trees which showed no net increase in dia or
  were missing in either year*/

if dbh88>dbh87

/*convert diameters from cm to inches for some equations*/
if species ne 'FRPE' and species ne 'FRSP' and species ne 'PITA' and species ne
   'SANI' and species ne 'TADI' then DO;
  dbh87=dia87/2.54;
  dbh88=dia88/2.54;
  dbh89=dia89/2.54; end;
else DO;
  dbh87=dia87; dbh88=dia88; dbh89=dia89; end;

if status ne 'D';

/*for each year calculate biomass according to the appropriate equation and
store in the biomass array*/

do i=1 to 3;

if i=1 then dbh=dbh87; else if i=2 then dbh=dbh88; else dbh=dbh89;
if dbh le 0 then go to BOTTOM;

     if species='CAAQ' or species='CATO' or species='CASP' then
          biomass{i}=1.62114*((dbh**2)**1.33298);
else if species='CELA' or species='CEOC' then
          biomass{i}=EXP(0.566+(1.25157*LOG((dbh**2))));
else if species='FRPE' or species='FRSP' then
          biomass{i}=10**((-0.98+(2.45*LOG10(dbh))));
else if species='LIST' then
          biomass{i}=1.68032*((dbh**2)**1.27729);
else if species='LITU' then
          biomass{i}=2.24272*((dbh**2)**1.19469);
else if species='NYSY' or species='NYAQ' then
          biomass{i}=1.30697*((dbh**2)**1.29943);
else if species='PITA' then
          biomass{i}=(10**(1.56+(2.59*LOG10(dbh))))+
                     (10**(1.57+(2.01*LOG10(dbh))));
else if species='QUAL' then
          biomass{i}=1.56965*((dbh**2)**1.34028);
else if species='QULA' then
          biomass{i}=10.22597*((dbh**2)**0.94962);
else if species='QULY' then
          biomass{i}=EXP(0.486+(1.25829*(LOG(dbh**2))));
else if species='QUNI' then
          biomass{i}=5.99898*((dbh**2)**1.08527);
else if species='QUNU' then
          biomass{i}=2.83658*(dbh**2.38225);
else if species='ACRU' then
          biomass{i}=1.69855*((dbh**2)**1.26161);
else if species='QUPA' or species='QUPH' or species='QUSP' or species='QUVE'
     or species='QUMI' then
          biomass{i}=2.89492*((dbh**2)**1.22006);
else if species='SANI' then
          biomass{i}=10**(-1.50+(2.78*LOG10(dbh)));
else if species='TADI' then
          biomass{i}=10**(-0.97+(2.34*LOG10(dbh)));
else biomass{i}=1.80526*((dbh**2)**1.27313);

BOTTOM: end;
/*the difference is production*/
if dia87 lt 10 or dia88 lt 10 or dia88 le dia87 then prod87=0;
     else prod87=bio88-bio87;
if dia88 lt 10 or dia89 lt 10 or dia89 le dia88 then prod88=0;
     else prod88=bio89-bio88;

/*convert pounds to kilograms*/
if species ne 'FRPE' and species ne 'FRSP' and species  ne 'SANI' and species ne
'TADI' and species ne 'PITA' then DO;
     prod87=prod87/2.204623;
     prod88=prod88/2.204623; end;

if species eq 'PITA' then DO;
     prod87=prod87/1000;
     prod88=prod88/1000; end;

/*Multiply to convert each tree to g/m2 (1000 g/Kg divided by 500 m2 plot)*/
/*The Verret plots are 1000 m2*/
if tran ne 'VERRET' then DO; prod87=prod87*2; prod88=prod88*2; end;
run;

data two; set;
proc sort; by tran plot;
proc tabulate;
     title 'Summary of SRP-LSU wood production estimates by plot (g/m2)';
     class tran plot species;
     by tran plot;
     var prod87;
     table species all, prod87*(n*f=3.0 sum*f=6.3 pctsum<species all>);

proc tabulate;
     title 'Summary of SRP-LSU wood production estimates by plot (g/m2)';
     class tran plot species;
     by tran plot;
     var prod88;
     table species all, prod88*(n*f=3.0 sum*f=6.3 pctsum<species all>);

run;
