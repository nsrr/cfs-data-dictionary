********************************************************;
* Title: prep_cfs_data_for_nsrr_step1.sas
* Purpose: QC and prepare clean dataset for phenotypes
*           for CFS subjects based on last visit.
********************************************************;

********************************************************;
* Establish CFS options and libraries
********************************************************;

%include "\\rfa01\bwh-sleepepi-home\projects\cohorts\Family\SAS\Family options and libnames.sas";

libname nsrrdata "&newfamilypath\nsrr-prep\_datasets";

%let release = 0.1.0.beta1;

********************************************************;
* Import CFS data
********************************************************;

*previously combined data;

data combined_rectypes /*nsrrdata.combined_rectypes_original*/;
  set family.cfs_combined_rectypes_07102014;
run;

data original_rectype5;
  set family.lab;
run;

********************************************************;
* Use PROC CONTENTS to identify RecType5 variables
********************************************************;

proc contents data = combined_rectypes out = combined_rectypes_contents noprint;
run;

proc contents data = original_rectype5 out = original_rectype5_contents  noprint;
run;

proc sort data = combined_rectypes_contents;
by name;
run;

proc sort data = original_rectype5_contents;
by name;
run;

data rectype5vars;
  merge original_rectype5_contents (in = a) combined_rectypes_contents (in = b);
  by name;
  if a and b;
run;

/*
proc sort data = rectype5vars;
  by varnum;
run;

proc sql;
  select varnum, name, label
  from rectype5vars;
quit;
*/

********************************************************;
* NULL Variables where it's obvious and known that zeros
*       were sometimes mistakenly used in lieu of NULL
********************************************************;

proc sql noprint;
  select name into :known_rec5vars_inneedof_NULLing separated by ' '
  from rectype5vars
  where (600 le varnum le 607) or (640 le varnum le 642) or (1010 le varnum le 1012) or (varnum in(363,1014,1015,1018,1019,1020,1023,1024)) or (1130 le varnum le 1137)
          or (varnum in(1139,1141,1142,1143,1858,1859)) or (1928 le varnum le 1956) or (1989 le varnum le 2020) or (varnum in(1983,1984)) or (2330 le varnum le 2335)
          or (2350 le varnum le 2358);
quit;

%put &known_rec5vars_inneedof_NULLing;

*sort by personi and rectype;
proc sort data=combined_rectypes;
  by PERSONI RECTYPE;
run;

data combined_rectypes_NULL_err0;
  set combined_rectypes;

  array set0negs_tonull[*] &known_rec5vars_inneedof_NULLing;

  do i = 1 to dim(set0negs_tonull);
    if set0negs_tonull[i] le 0 then set0negs_tonull[i] = .;
  end;
  drop i;

run;

/*
data nsrrdata.combined_rectypes_pass1_20140716;
  set combined_rectypes_NULL_err0;

	attrib _all_ label = "";
  format _all_;
run;
*/

********************************************************;
* Restrict dataset to RecType5 Visits /
*   Drop variables that were not collected in RecType5
********************************************************;

data rectype5_NULL_err0;
  set combined_rectypes_NULL_err0;
  if rectype = 5;
run;

*drop RecType5 Variables with no Observed Values;
data _null_;
  set Rectype5_null_err0 (obs=1);
  array num_vars[*] _NUMERIC_;
  array char_vars[*] _CHARACTER_;
  call symputx('num_qty', dim(num_vars));
  call symputx('char_qty', dim(char_vars));
run;

data _null_;
  set Rectype5_null_err0 end=finished;

  /* Use the reserved word _NUMERIC_ to load all numeric variables  */
  /* into the NUM_VARS array.  Use the reserved word _CHARACTER_ to */
  /* to load all character variables into the CHAR_VARS array.      */
  array num_vars[*] _NUMERIC_;
  array char_vars[*] _CHARACTER_;

  /* Create 'flag' arrays for the variables in NUM_VARS and CHAR_VARS. */
  /* Initialize their values to 'missing'.  Values initialized on an   */
  /* array statement are retained.                                     */
  array num_miss [&num_qty] $ (&num_qty * 'missing');
  array char_miss [&char_qty] $ (&char_qty * 'missing');

  /* LIST will contain the list of variables to be dropped.  Ensure  */
  /* it's length is sufficient.                                      */
  length list $ 5000;

  /* Check for non-missing values.  Reassign the corresponding 'flag' */
  /* value accordingly.                                               */
  do i=1 to dim(num_vars);
    if num_vars(i) ne . then num_miss(i)='non-miss';
  end;
  do i=1 to dim(char_vars);
    if char_vars(i) ne '' then char_miss(i)='non-miss';
  end;

  /* On the last observation of the data set, if a 'flag' value is still */
  /* 'missing', the variable needs to be dropped.  Concatenate the       */
  /* variable's name onto LIST to build the values of a DROP statement   */
  /* to be executed in another step.                                     */
  if finished then do;
  do i= 1 to dim(num_vars);
    if num_miss(i) = 'missing' then list=trim(list)||' '||trim(vname(num_vars(i)));
  end;
  do i= 1 to dim(char_vars);
    if char_miss(i) = 'missing' then list=trim(list)||' '||trim(vname(char_vars(i)));
  end;
  call symput('mlist',list);
  end;
run;

%put &mlist;

data rectype5_narrowedvars;
  set rectype5_NULL_err0;

  drop &mlist;
run;

data nsrrdata.rectype5_pass1;
  set rectype5_narrowedvars;

	attrib _all_ label = "";
  format _all_;

  drop inall incatecholamine incrp incrpsnp incystatinc incytokine inddimer inddimerstar infibrinogen inflmed inghrelin inicam inil6 inil6snp inleptin inmaster inmicroalb inoxldl inpai1 inpanela insolil6 intnfa invermontdna invisfatin lab_datercvd microalb_date ddimerstaram_rundate ddimer_transmit ddimer_datercvd cystatinc_date zipcode state city cellphon midinit cdlabel f2r barcode;
run;

proc export data=nsrrdata.rectype5_pass1 outfile="\\rfa01\bwh-sleepepi-home\projects\cohorts\Family\nsrr-prep\_releases\&release\cfs-rectype5-dataset-&release..csv" dbms=csv replace; run;

/*Consider running checks below, besides just histogram and classic NSRR checks

********************************************************;
* Values to check
********************************************************;

proc sgplot data = added_variablesQC;
  REG X=bmi Y=waist2height;
  LOESS X=bmi Y=waist2height / nomarkers;
  title "Scatter Plot with Fit Models for BMI (X-axis) and Waist to Height ratio (Y-Axis)";
  title;
run;

*ods pdf startpage = no;

proc sgplot data = added_variablesQC;
  REG X=bmi Y=waist2hip;
  LOESS X=bmi Y=waist2hip / nomarkers;
  title "Scatter Plot with Fit Models for BMI (X-axis) and Waist to Hip ratio (Y-Axis)";
  title;
run;


proc sort data = lastrectype_mergephenocare_pcs2; by age; run;
proc sql;
  title "Instances Where BMI > 60 or BMI < 15";
  select personi, rectype, age, BMI, height, weight, waistcm
  from lastrectype_mergephenocare_pcs2
  where  BMI > 60 or (BMI < 15 and BMI ne .);
  title;
run;
proc sort data = lastrectype_mergephenocare_pcs2; by personi; run;

proc sql;
  title "Instances Where Avg SaO2 < Min SaO2";
  select personi, rectype, rdi3p_allrec, avgo2_allrec, lowo2_allrec
  from cfs_lastrectypefinal
  where lowo2_allrec ne . and avgo2_allrec ne . and avgo2_allrec < lowo2_allrec;
  title;
quit;

proc sql;
  title "Instances Where Min SaO2 < 90 and '% SaO2 < 90' = 0";
  select personi, rectype, lowo2_allrec, per90_allrec
  from cfs_lastrectypefinal
  where lowo2_allrec ne . and per90_allrec ne . and lowo2_allrec < 90 and per90_allrec = 0;
  title;
quit;


proc sql;
  title "Instances Where '% SaO2 < 90' = 100 and Min SaO2 > 90 or Avg SaO2 > 90";
  select personi, rectype, per90_allrec, lowo2_allrec, avgo2_allrec
  from cfs_lastrectypefinal
  where lowo2_allrec ne . and avgo2_allrec ne . and per90_allrec = 100 and (lowo2_allrec > 90 or avgo2_allrec > 90);
  title;
quit;
