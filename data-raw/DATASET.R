### Prepare the Interstroke Education Dataset
library(dplyr)
library(labelled)
library(sjlabelled)
library(encryptr)
library(tidyr)
library(tidyverse)

rm(list = ls())

#Import the interstroke database
decrypt_file("data-raw/allstroke_with_supplementray.csv.encryptr.bin", file_name = "data-raw/allstroke_with_supplementray.csv", private_key_path = "data-raw/id_rsa")
interstroke_data <- rio::import("data-raw/allstroke_with_supplementray.csv")
unlink("data-raw/allstroke_with_supplementray.csv")
interstroke_ich_location <- rio::import("data-raw/ich_location.csv")

interstroke_ich_location <- interstroke_ich_location %>%
  mutate(second_or_third_site = ifelse(!is.na(ich_location_secondary_site)|!is.na(ich_location_third_site),"Yes","No")) %>%
  mutate(extension_or_second_or_third_site = ifelse(!is.na(ich_location_extension)|!is.na(ich_location_secondary_site)|!is.na(ich_location_third_site),"Yes","No"))

#Join ICH location data with intertsroke data
interstroke_data <- interstroke_data %>%
  left_join(interstroke_ich_location,by="id")

#Replace 2 with 0 for controls
interstroke_data <- interstroke_data %>%
  mutate(case_num = ifelse(case==2, 0, case))

#Set categorial variables
interstroke_data$case <- factor(interstroke_data$case)
interstroke_data$case <- recode(interstroke_data$case,
                                "1"="Case","2"="Control")

#Set categorial variables
interstroke_data$ich_location_primary <- factor(interstroke_data$ich_location_primary)
interstroke_data$ich_location_primary <- recode(interstroke_data$ich_location_primary,
                                                "1"="Lobar",
                                                "2"="Cerebullum",
                                                "3"="Basal ganglia-internal capsule (BGIC)",
                                                "4"="Thalamus",
                                                "5"="Brainstem",
                                                "6"="Intraventricular",
                                                "99"="Missing")

interstroke_data <- interstroke_data %>%
  mutate(lobar = ifelse(case == "Control", "Control", ifelse(ich_location_primary == "Lobar", "Lobar", "Non-Lobar")))

interstroke_data$ich_location_extension <- factor(interstroke_data$ich_location_extension)
interstroke_data$ich_location_extension <- recode(interstroke_data$ich_location_extension,
                                                  "1"="Lobar",
                                                  "2"="Cerebullum",
                                                  "3"="Basal ganglia-internal capsule (BGIC)",
                                                  "4"="Thalamus",
                                                  "5"="Brainstem",
                                                  "6"="Intraventricular",
                                                  "99"="Missing")

interstroke_data$ich_location_secondary_site <- factor(interstroke_data$ich_location_secondary_site)
interstroke_data$ich_location_secondary_site <- recode(interstroke_data$ich_location_secondary_site,
                                                       "1"="Lobar",
                                                       "2"="Cerebullum",
                                                       "3"="Basal ganglia-internal capsule (BGIC)",
                                                       "4"="Thalamus",
                                                       "5"="Brainstem",
                                                       "6"="Intraventricular",
                                                       "99"="Missing")

interstroke_data$ich_location_third_site <- factor(interstroke_data$ich_location_third_site)
interstroke_data$ich_location_third_site <- recode(interstroke_data$ich_location_third_site,
                                                   "1"="Lobar",
                                                   "2"="Cerebullum",
                                                   "3"="Basal ganglia-internal capsule (BGIC)",
                                                   "4"="Thalamus",
                                                   "5"="Brainstem",
                                                   "6"="Intraventricular",
                                                   "10"="Transformation",
                                                   "99"="Missing")


#For cases, make caseid equal to id
interstroke_data <- interstroke_data %>%
  mutate(caseid = ifelse(case=="Case", id, caseid))

#Set categorial variables
interstroke_data$esex <- factor(interstroke_data$esex)
interstroke_data$esex <- recode(interstroke_data$esex,
                                "1"="Female","2"="Male")

interstroke_data$subeduc <- factor(interstroke_data$subeduc)
interstroke_data$subeduc <- recode(interstroke_data$subeduc,
                                   "1"="None","2"="1-8", "3"="9-12", "4"="Trade School", "5"="College/University")

interstroke_data$moteduc <- factor(interstroke_data$moteduc)
interstroke_data$moteduc <- recode(interstroke_data$moteduc,
                                   "1"="None","2"="1-8", "3"="9-12", "4"="Trade School", "5"="College/University", "6"="Unknown")

interstroke_data$fatduc <- factor(interstroke_data$fatduc)
interstroke_data$fatduc <- recode(interstroke_data$fatduc,
                                  "1"="None","2"="1-8", "3"="9-12", "4"="Trade School", "5"="College/University", "6"="Unknown")

interstroke_data$regionnn7 <- factor(interstroke_data$regionnn7)
interstroke_data$regionnn7 <- recode(interstroke_data$regionnn7,
                                     "1"="Western Europe/North America/Australasia",
                                     "2"="Eastern/Central Europe/Middle East",
                                     "3"="Africa",
                                     "4"="South Asia",
                                     "5"="China",
                                     "6"="South East Asia",
                                     "7"="South America")

interstroke_data$ethnicity <- factor(interstroke_data$ethnicity)
interstroke_data$ethnicity <- recode(interstroke_data$ethnicity,
                                     "1"="European",
                                     "2"="Chinese",
                                     "3"="South Asian",
                                     "4"="Other Asian",
                                     "5"="Arab",
                                     "6"="Latin American",
                                     "7"="Black African",
                                     "8"="Coloured African",
                                     "9"="Other")

interstroke_data$ethnic <- factor(interstroke_data$ethnic)
interstroke_data$ethnic <- recode(interstroke_data$ethnic,
                                  "1"="South/Other Asian",
                                  "2"="Chinese",
                                  "3"="Japanese",
                                  "4"="Malays",
                                  "5"= "Other Asian",
                                  "6"="Persian",
                                  "7"="Arab",
                                  "8"="Blackafrican/Bantu/Hemitic/Nilotic",
                                  "9"="Coloured African",
                                  "10"="European",
                                  "11"="Nativeamerican/Aus Aborigine",
                                  "12"="Latin American",
                                  "13"="Bantu/Semi Bantu",
                                  "14"="Hemitic/Semi Hemitic",
                                  "15"="Nilotic/Hausa",
                                  "16"="Pygmie",
                                  "17"="Swahili",
                                  "18"="other")


interstroke_data$incomectry <- factor(interstroke_data$incomectry)
interstroke_data$incomectry <- recode(interstroke_data$incomectry,
                                      "1"="HIC",
                                      "2"="MIC",
                                      "3"="LIC")

interstroke_data$occupat <- factor(interstroke_data$occupat)
interstroke_data$occupat <- recode(interstroke_data$occupat,
                                   "1"="Professional",
                                   "2"="Skilled Labour",
                                   "3"="General Labour",
                                   "4"="Housewife",
                                   "5"="Farmer",
                                   "6"="Police/Military",
                                   "7"="Business",
                                   "8"="Clerical",
                                   "9"="Disability/Social Security",
                                   "10"="Other")

interstroke_data$smoking <- factor(interstroke_data$smoking)
interstroke_data$smoking <- recode(interstroke_data$smoking,
                                   "1"="Never",
                                   "2"="Former",
                                   "3"="Current 1-19 per day",
                                   "4"="Current 20+ per day")

interstroke_data$subdm <- factor(interstroke_data$subdm)
interstroke_data$subdm <- recode(interstroke_data$subdm,
                                 "1"="No",
                                 "2"="Yes")

interstroke_data$marital <- factor(interstroke_data$marital)
interstroke_data$marital <- recode(interstroke_data$marital,
                                   "1"="Never married",
                                   "2"="Currently",
                                   "3"="Common law/living with Partner",
                                   "4"="Widowed",
                                   "5"="Separated",
                                   "6"="Divorced")

interstroke_data$alcohfreqcat <- factor(interstroke_data$alcohfreqcat)
interstroke_data$alcohfreqcat <- recode(interstroke_data$alcohfreqcat,
                                        "0"="Never",
                                        "1"="Former",
                                        "2"="Current+(0-15/mo)",
                                        "3"="Current+(16-30/mo)",
                                        "4"="Current+(>30/mo)")

interstroke_data$afibflut <- factor(interstroke_data$afibflut)
interstroke_data$afibflut <- recode(interstroke_data$afibflut,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$statipre <- factor(interstroke_data$statipre)
interstroke_data$statipre <- recode(interstroke_data$statipre,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$antihypertpre <- factor(interstroke_data$antihypertpre)
interstroke_data$antihypertpre <- recode(interstroke_data$antihypertpre,
                                         "0"="No",
                                         "1"="Yes")

interstroke_data$tosleep <- factor(interstroke_data$tosleep)
interstroke_data$tosleep <- recode(interstroke_data$tosleep,
                                   "1"="No",
                                   "2"="Yes")

interstroke_data$polluteh <- factor(interstroke_data$polluteh)
interstroke_data$polluteh <- recode(interstroke_data$polluteh,
                                    "1"="No pollution",
                                    "2"="Very mild pollution",
                                    "3"="Mild pollution",
                                    "4"="Moderate pollution",
                                    "5"="Moderately severe pollution",
                                    "6"="Severe pollution")

interstroke_data$pollutew <- factor(interstroke_data$pollutew)
interstroke_data$pollutew <- recode(interstroke_data$pollutew,
                                    "1"="No pollution",
                                    "2"="Very mild pollution",
                                    "3"="Mild pollution",
                                    "4"="Moderate pollution",
                                    "5"="Moderately severe pollution",
                                    "6"="Severe pollution")

interstroke_data$injuhead <- factor(interstroke_data$injuhead)
interstroke_data$injuhead <- recode(interstroke_data$injuhead,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$injuneck <- factor(interstroke_data$injuneck)
interstroke_data$injuneck <- recode(interstroke_data$injuneck,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$painteet <- factor(interstroke_data$painteet)
interstroke_data$painteet <- recode(interstroke_data$painteet,
                                    "0"="No Painful Teeth",
                                    "1"="Painful Teeth")

interstroke_data$paingum <- factor(interstroke_data$paingum)
interstroke_data$paingum <- recode(interstroke_data$paingum,
                                   "0"="No Painful Gums",
                                   "1"="Painful Gums")

interstroke_data$lostteet <- factor(interstroke_data$lostteet)
interstroke_data$lostteet <- recode(interstroke_data$lostteet,
                                    "0"="No Lost Teeth",
                                    "1"="Lost Teeth")

interstroke_data$nooral <- factor(interstroke_data$nooral)
interstroke_data$nooral <- recode(interstroke_data$nooral,
                                  "0"="Blank",
                                  "1"="Checked")

interstroke_data$cstooth <- factor(interstroke_data$cstooth)
interstroke_data$cstooth <- recode(interstroke_data$cstooth,
                                   "0"="No Tooth Infection",
                                   "1"="Tooth Infection")

interstroke_data$cacbpre <- factor(interstroke_data$cacbpre)
interstroke_data$cacbpre <- recode(interstroke_data$cacbpre,
                                   "1"="No",
                                   "2"="Yes")

interstroke_data$subhtn <- factor(interstroke_data$subhtn)
interstroke_data$subhtn <- recode(interstroke_data$subhtn,
                                  "1"="No",
                                  "2"="Yes")

interstroke_data$subrfev <- factor(interstroke_data$subrfev)
interstroke_data$subrfev <- recode(interstroke_data$subrfev,
                                   "1"="No",
                                   "2"="Yes")

interstroke_data$subrhd <- factor(interstroke_data$subrhd)
interstroke_data$subrhd <- recode(interstroke_data$subrhd,
                                  "1"="No",
                                  "2"="Yes")

interstroke_data$afi4wks <- factor(interstroke_data$afi4wks)
interstroke_data$afi4wks <- recode(interstroke_data$afi4wks,
                                   "1"="No",
                                   "2"="Yes")

interstroke_data$meckmlwk <- factor(interstroke_data$meckmlwk)
interstroke_data$meckmlwk <- recode(interstroke_data$meckmlwk,
                                    "1"="No",
                                    "2"="Yes")


interstroke_data$aheitert <- factor(interstroke_data$aheitert)
interstroke_data$aheitert <- recode(interstroke_data$aheitert,
                                    "1"="Tertile 1",
                                    "2"="Tertile 2",
                                    "3"="Tertile 3")


interstroke_data$wrkactv <- factor(interstroke_data$wrkactv)
interstroke_data$wrkactv <- recode(interstroke_data$wrkactv,
                                   "1"="Mainly sedentary",
                                   "2"="Predominately walking on one level",
                                   "3"="Mainly walking",
                                   "4"="Heavy physical labour",
                                   "5"="Subject does not work")

interstroke_data$dissection <- factor(interstroke_data$dissection)
interstroke_data$dissection <- recode(interstroke_data$dissection,
                                      "0"="No",
                                      "1"="Yes")

interstroke_data$migraine <- factor(interstroke_data$migraine)
interstroke_data$migraine <- recode(interstroke_data$migraine,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$ischocsp <- factor(interstroke_data$ischocsp)
interstroke_data$ischocsp <- recode(interstroke_data$ischocsp,
                                    "1"="TACI",
                                    "2"="PACI",
                                    "3"="POCI",
                                    "4"="LACI",
                                    "5"="Other")

interstroke_data$fischocsp <- factor(interstroke_data$fischocsp)
interstroke_data$fischocsp <- recode(interstroke_data$fischocsp,
                                     "1"="TACI",
                                     "2"="PACI",
                                     "3"="POCI",
                                     "4"="LACI",
                                     "5"="Other")

interstroke_data$csdiffsl <- factor(interstroke_data$csdiffsl)
interstroke_data$csdiffsl <- recode(interstroke_data$csdiffsl,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$csrate <- factor(interstroke_data$csrate)
interstroke_data$csrate <- recode(interstroke_data$csrate,
                                  "1"="Very Good",
                                  "2"="Good",
                                  "3"="Fair",
                                  "4"="Bad",
                                  "5"="Very Bad")

interstroke_data$cssleepd <- factor(interstroke_data$cssleepd)
interstroke_data$cssleepd <- recode(interstroke_data$cssleepd,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$diagosa <- factor(interstroke_data$diagosa)
interstroke_data$diagosa <- recode(interstroke_data$diagosa,
                                   "1"="No",
                                   "2"="Yes")

interstroke_data$antidpre <- factor(interstroke_data$antidpre)
interstroke_data$antidpre <- recode(interstroke_data$antidpre,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$antidhos <- factor(interstroke_data$antidhos)
interstroke_data$antidhos <- recode(interstroke_data$antidhos,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$aspirpre <- factor(interstroke_data$aspirpre)
interstroke_data$aspirpre <- recode(interstroke_data$aspirpre,
                                    "1"="No",
                                    "2"="Yes")
interstroke_data$clopdpre <- factor(interstroke_data$clopdpre)
interstroke_data$clopdpre <- recode(interstroke_data$clopdpre,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$dipyrpre <- factor(interstroke_data$dipyrpre)
interstroke_data$dipyrpre <- recode(interstroke_data$dipyrpre,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$orantpre <- factor(interstroke_data$orantpre)
interstroke_data$orantpre <- recode(interstroke_data$orantpre,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$fibrapre<- factor(interstroke_data$fibrapre)
interstroke_data$fibrapre <- recode(interstroke_data$fibrapre,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$ocholpre <- factor(interstroke_data$ocholpre)
interstroke_data$ocholpre <- recode(interstroke_data$ocholpre,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$acepre <- factor(interstroke_data$acepre)
interstroke_data$acepre <- recode(interstroke_data$acepre,
                                  "1"="No",
                                  "2"="Yes")

interstroke_data$arbpre <- factor(interstroke_data$arbpre)
interstroke_data$arbpre <- recode(interstroke_data$arbpre,
                                  "1"="No",
                                  "2"="Yes")

interstroke_data$betabpre <- factor(interstroke_data$betabpre)
interstroke_data$betabpre <- recode(interstroke_data$betabpre,
                                    "1"="No",
                                    "2"="Yes")


interstroke_data$alphbpre <- factor(interstroke_data$alphbpre)
interstroke_data$alphbpre <- recode(interstroke_data$alphbpre,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$cacbpre <- factor(interstroke_data$cacbpre)
interstroke_data$cacbpre <- recode(interstroke_data$cacbpre,
                                   "1"="No",
                                   "2"="Yes")

interstroke_data$diurepre <- factor(interstroke_data$diurepre)
interstroke_data$diurepre <- recode(interstroke_data$diurepre,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$antippre <- factor(interstroke_data$antippre)
interstroke_data$antippre <- recode(interstroke_data$antippre,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$antiphos <- factor(interstroke_data$antiphos)
interstroke_data$antiphos <- recode(interstroke_data$antiphos,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$analgpre <- factor(interstroke_data$analgpre)
interstroke_data$analgpre <- recode(interstroke_data$analgpre,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$nsaidpre <- factor(interstroke_data$nsaidpre)
interstroke_data$nsaidpre <- recode(interstroke_data$nsaidpre,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$coxzipre <- factor(interstroke_data$coxzipre)
interstroke_data$coxzipre <- recode(interstroke_data$coxzipre,
                                    "1"="No",
                                    "2"="Yes")
##here
#medication number calulation

interstroke_data<-interstroke_data %>%
  mutate(medication_number_pread =
           as.numeric(aspirpre=="Yes")+
           as.numeric(clopdpre=="Yes")+
           as.numeric(dipyrpre=="Yes")+
           as.numeric(orantpre=="Yes")+
           as.numeric(statipre=="Yes")+
           as.numeric(antidpre=="Yes")+
           as.numeric(fibrapre=="Yes")+
           as.numeric(ocholpre=="Yes")+
           as.numeric(acepre=="Yes")+
           as.numeric(arbpre=="Yes")+
           as.numeric(betabpre=="Yes")+
           as.numeric(alphbpre=="Yes")+
           as.numeric(cacbpre=="Yes")+
           as.numeric(diurepre=="Yes")+
           as.numeric(antippre=="Yes")+
           as.numeric(analgpre=="Yes")+
           as.numeric(nsaidpre=="Yes")+
           as.numeric(coxzipre=="Yes"))

#comorbidity number calculation
interstroke_data <- interstroke_data %>%
  mutate(comorbidity_number_pread =
           as.numeric(subami=="Yes")+
           as.numeric(subpad=="Yes")+
           as.numeric(subvthr=="Yes")+
           as.numeric(subaf=="Yes")+
           as.numeric(subhiv=="Yes")+
           as.numeric(subdm=="Yes")+
           as.numeric(subhtn=="Yes")+
           as.numeric(subrhd=="Yes")+
           as.numeric(substrk=="Yes")+
           as.numeric(subcb=="Yes")+
           as.numeric(cscanc=="Yes")+
           as.numeric(cshf=="Yes"))

#comorbidity number calculation without HTN and DM
interstroke_data <- interstroke_data %>%
  mutate(comorbidity_number_pread2 =
           as.numeric(subami=="Yes")+
           as.numeric(subpad=="Yes")+
           as.numeric(subvthr=="Yes")+
           as.numeric(subaf=="Yes")+
           as.numeric(subhiv=="Yes")+
           as.numeric(subrhd=="Yes")+
           as.numeric(substrk=="Yes")+
           as.numeric(subcb=="Yes")+
           as.numeric(cscanc=="Yes")+
           as.numeric(cshf=="Yes"))

#surrogate frailty marker in cases and controls as MRS
interstroke_data <- interstroke_data %>%
  mutate(mrs_case_control_frailty_proxy = ifelse(case=="Case",mrscoreb,mrscore))

###
#Interstroke - Proxy
###
interstroke_data$respondt <- factor(interstroke_data$respondt)
interstroke_data$respondt <- recode(interstroke_data$respondt,
                                    "1"="Case",
                                    "2"="Proxy",
                                    "3"= "Both")

interstroke_data$mrscoren <- factor(interstroke_data$mrscoren)
interstroke_data$mrscoren <- recode(interstroke_data$mrscoren,
                                    "0"="0",
                                    "1"="1",
                                    "2"="2",
                                    "3"="3",
                                    "4"="4",
                                    "5"="5",
                                    "6"="6")

interstroke_data$mrscofef <- factor(interstroke_data$mrscofef)
interstroke_data$mrscofef <- recode(interstroke_data$mrscofef,
                                    "0"="0",
                                    "1"="1",
                                    "2"="2",
                                    "3"="3",
                                    "4"="4",
                                    "5"="5",
                                    "6"="6")

interstroke_data$mrscoreb <- factor(interstroke_data$mrscoreb)
interstroke_data$mrscoreb <- recode(interstroke_data$mrscoreb,
                                    "0"="0",
                                    "1"="1",
                                    "2"="2",
                                    "3"="3",
                                    "4"="4",
                                    "5"="5",
                                    "6"="6")

interstroke_data$mrscorefcat <- factor(interstroke_data$mrscorefcat)
interstroke_data$mrscorefcat <- recode(interstroke_data$mrscorefcat,
                                       "0" = "MRS 0-2",
                                       "1" = "MRS 3-6")

interstroke_data$mrscorec <- factor(interstroke_data$mrscorec)
interstroke_data$mrscorec <- recode(interstroke_data$mrscorec,
                                    "0"="0",
                                    "1"="1",
                                    "2"="2",
                                    "3"="3",
                                    "4"="4",
                                    "5"="5",
                                    "6"="6")

interstroke_data$fatlstrk <- factor(interstroke_data$fatlstrk)
interstroke_data$fatlstrk <- recode(interstroke_data$fatlstrk,
                                    "0" = "No",
                                    "1" = "Yes")

interstroke_data$discharge <- factor (interstroke_data$discharge)
interstroke_data$discharge <- recode(interstroke_data$discharge,
                                     "1"="No",
                                     "2"="Yes",
                                     "3"="Still in Hospital")

interstroke_data$liveplace <- factor (interstroke_data$liveplace)
interstroke_data$liveplace <- recode(interstroke_data$liveplace,
                                     "0"="0",
                                     "1"="Home",
                                     "2"="Rehab",
                                     "3"="Institutional Care",
                                     "4"="Other")

interstroke_data$sympapha <- factor (interstroke_data$sympapha)
interstroke_data$sympapha <- recode(interstroke_data$sympapha,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$rpstrktp <- factor (interstroke_data$rpstrktp)
interstroke_data$rpstrktp <- recode(interstroke_data$rpstrktp,
                                    "1"="No",
                                    "2"="Yes")

###
#Interstroke ALcohol
###

interstroke_data$alcb1 <- factor(interstroke_data$alcb1)
interstroke_data$alcb1 <- recode(interstroke_data$alcb1,
                                 "1"="Never",
                                 "2"="Former",
                                 "3"="Current")

interstroke_data$alcb <- factor(interstroke_data$alcb)
interstroke_data$alcb <- recode(interstroke_data$alcb,
                                "1"="Never or Former",
                                "2"="Current")

interstroke_data$alcq <- factor(interstroke_data$alcq)
interstroke_data$alcq <- recode(interstroke_data$alcq,
                                "1"="Never",
                                "2"="Former",
                                "3"="Moderate User",
                                "4"="Binge User")

interstroke_data$alcq3 <- factor(interstroke_data$alcq3)
interstroke_data$alcq3 <- recode(interstroke_data$alcq3,
                                 "1"="Never or Former",
                                 "2"="Moderate User",
                                 "3"="Binge User")

interstroke_data$alcqn <- factor(interstroke_data$alcqn)
interstroke_data$alcqn <- recode(interstroke_data$alcqn,
                                 "1"="Never or Ligher User",
                                 "2"="Moderate User",
                                 "3"="Binge User")

interstroke_data$alcbingetyp <- factor(interstroke_data$alcbingetyp)
interstroke_data$alcbingetyp <- recode(interstroke_data$alcbingetyp,
                                       "0"="Never or Former or Moderate",
                                       "1"="Binge with Wine >50%",
                                       "2"="Binge with Beer >50%",
                                       "3"="Binge with Spirits >50%")

interstroke_data$alcohhis <- factor(interstroke_data$alcohhis)
interstroke_data$alcohhis <- recode(interstroke_data$alcohhis,
                                    "1"="Formerly Used Alcohol",
                                    "2"="Currently Uses Alcohol",
                                    "3"="Never Used Alcohol Products")

interstroke_data$alcohol <- factor(interstroke_data$alcohol)
interstroke_data$alcohol <- recode(interstroke_data$alcohol,
                                   "1"="No",
                                   "2"="Yes")

interstroke_data$spiritf <- factor(interstroke_data$spiritf)
interstroke_data$spiritf <- recode(interstroke_data$spiritf,
                                   "1"="Daily",
                                   "2"="Weekly",
                                   "3"="Monthly")

interstroke_data$winefreq <- factor(interstroke_data$winefreq)
interstroke_data$winefreq <- recode(interstroke_data$winefreq,
                                    "1"="Daily",
                                    "2"="Weekly",
                                    "3"="Monthly")

interstroke_data$beerfreq <- factor(interstroke_data$beerfreq)
interstroke_data$beerfreq <- recode(interstroke_data$beerfreq,
                                    "1"="Daily",
                                    "2"="Weekly",
                                    "3"="Monthly")

interstroke_data$arrackf <- factor(interstroke_data$arrackf)
interstroke_data$arrackf <- recode(interstroke_data$arrackf,
                                   "1"="Daily",
                                   "2"="Weekly",
                                   "3"="Monthly")

interstroke_data$oalcfreq <- factor(interstroke_data$oalcfreq)
interstroke_data$oalcfreq <- recode(interstroke_data$oalcfreq,
                                    "1"="Daily",
                                    "2"="Weekly",
                                    "3"="Monthly")

interstroke_data$tobahis <- factor(interstroke_data$tobahis)
interstroke_data$tobahis <- recode(interstroke_data$tobahis,
                                   "1"="Formerly Used Tobacco",
                                   "2"="Currently Uses Tobacco",
                                   "3"="Never Used Tobacco")

interstroke_data$scurformnev <- factor(interstroke_data$scurformnev)
interstroke_data$scurformnev <- recode(interstroke_data$scurformnev,
                                       "1"="Never Smoker",
                                       "2"="Former Smoker",
                                       "3"="Current Smoker")

interstroke_data$nevfcur <- factor(interstroke_data$nevfcur)
interstroke_data$nevfcur <- recode(interstroke_data$nevfcur,
                                   "1"="Never or Former Smoker",
                                   "2"="Current Smoker")

interstroke_data$phys <- factor(interstroke_data$phys)
interstroke_data$phys <- recode(interstroke_data$phys,
                                "1"="Mainly Inactive",
                                "2"="Mainly Active")

interstroke_data$whrs2tert <- factor(interstroke_data$whrs2tert)
interstroke_data$whrs2tert <- recode(interstroke_data$whrs2tert,
                                     "1"="Tertile 1",
                                     "2"="Tertile 2",
                                     "3"="Tertile 3")

interstroke_data$ahei4tert <- factor(interstroke_data$ahei4tert)
interstroke_data$ahei4tert <- recode(interstroke_data$ahei4tert,
                                     "1"="Tertile 1",
                                     "2"="Tertile 2",
                                     "3"="Tertile 3")

interstroke_data$htnmbp2 <- factor(interstroke_data$htnmbp2)
interstroke_data$htnmbp2 <- recode(interstroke_data$htnmbp2,
                                   "0"="No",
                                   "1"="Yes")

interstroke_data$dmhba1c2 <- factor(interstroke_data$dmhba1c2)
interstroke_data$dmhba1c2 <- recode(interstroke_data$dmhba1c2,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$cardiacrfcat <- factor(interstroke_data$cardiacrfcat)
interstroke_data$cardiacrfcat <- recode(interstroke_data$cardiacrfcat,
                                        "1"="No",
                                        "2"="Yes")

interstroke_data$apob_apoatert <- factor(interstroke_data$apob_apoatert)
interstroke_data$apob_apoatert <- recode(interstroke_data$apob_apoatert,
                                         "1"="Tertile 1",
                                         "2"="Tertile 2",
                                         "3"="Tertile 3")

interstroke_data$subami <- factor(interstroke_data$subami)
interstroke_data$subami <- recode(interstroke_data$subami,
                                  "1"="No",
                                  "2"="Yes")

interstroke_data$subangin <- factor(interstroke_data$subangin)
interstroke_data$subangin <- recode(interstroke_data$subangin,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$subtia <- factor(interstroke_data$subtia)
interstroke_data$subtia <- recode(interstroke_data$subtia,
                                  "1"="No",
                                  "2"="Yes")

interstroke_data$substrk <- factor(interstroke_data$substrk)
interstroke_data$substrk <- recode(interstroke_data$substrk,
                                   "1"="No",
                                   "2"="Yes")

interstroke_data$subcb <- factor(interstroke_data$subcb)
interstroke_data$subcb <- recode(interstroke_data$subcb,
                                 "1"="No",
                                 "2"="Yes")

interstroke_data$subhvr <- factor(interstroke_data$subhvr)
interstroke_data$subhvr <- recode(interstroke_data$subhvr,
                                  "1"="No",
                                  "2"="Yes")

interstroke_data$subpad <- factor(interstroke_data$subpad)
interstroke_data$subpad <- recode(interstroke_data$subpad,
                                  "1"="No",
                                  "2"="Yes")

interstroke_data$subhchol <- factor(interstroke_data$subhchol)
interstroke_data$subhchol <- recode(interstroke_data$subhchol,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$cshf <- factor(interstroke_data$cshf)
interstroke_data$cshf <- recode(interstroke_data$cshf,
                                "1"="No",
                                "2"="Yes")

interstroke_data$subvthr <- factor(interstroke_data$subvthr)
interstroke_data$subvthr <- recode(interstroke_data$subvthr,
                                   "1"="No",
                                   "2"="Yes")

interstroke_data$subbbled <- factor(interstroke_data$subbbled)
interstroke_data$subbbled <- recode(interstroke_data$subbbled,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$subaf <- factor(interstroke_data$subaf)
interstroke_data$subaf <- recode(interstroke_data$subaf,
                                 "1"="No",
                                 "2"="Yes")

##New Diagnosis AF
interstroke_data <- interstroke_data %>%
  mutate(newly_diagnosed_af = ifelse(subaf=="No" & afibflut == "Yes", "Yes", "No"))
interstroke_data$newly_diagnosed_af <- factor(interstroke_data$newly_diagnosed_af)

interstroke_data$antiplate <- factor(interstroke_data$antiplate)
interstroke_data$antiplate <- recode(interstroke_data$antiplate,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$orantpre <- factor(interstroke_data$orantpre)
interstroke_data$orantpre <- recode(interstroke_data$orantpre,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$fibrapre <- factor(interstroke_data$fibrapre)
interstroke_data$fibrapre <- recode(interstroke_data$fibrapre,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$acepre <- factor(interstroke_data$acepre)
interstroke_data$acepre <- recode(interstroke_data$acepre,
                                  "1"="No",
                                  "2"="Yes")

interstroke_data$arbpre <- factor(interstroke_data$arbpre)
interstroke_data$arbpre <- recode(interstroke_data$arbpre,
                                  "1"="No",
                                  "2"="Yes")

interstroke_data$betabpre <- factor(interstroke_data$betabpre)
interstroke_data$betabpre <- recode(interstroke_data$betabpre,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$alphbpre <- factor(interstroke_data$alphbpre)
interstroke_data$alphbpre <- recode(interstroke_data$alphbpre,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$diurepre <- factor(interstroke_data$diurepre)
interstroke_data$diurepre <- recode(interstroke_data$diurepre,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$education <- factor(interstroke_data$education)
interstroke_data$education <- recode(interstroke_data$education,
                                     "0"="None",
                                     "1"="1-12 Years",
                                     "2"="Trade School or University")

interstroke_data$iscstroke <- factor(interstroke_data$iscstroke)
interstroke_data$iscstroke <- recode(interstroke_data$iscstroke,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$ichemstroke <- factor(interstroke_data$ichemstroke)
interstroke_data$ichemstroke <- recode(interstroke_data$ichemstroke,
                                       "1"="No",
                                       "2"="Yes")

interstroke_data$htnadmbp <- factor(interstroke_data$htnadmbp)
interstroke_data$htnadmbp <- recode(interstroke_data$htnadmbp,
                                    "0"="No",
                                    "1"="Yes")

interstroke_data$fstress <- factor(interstroke_data$fstress)
interstroke_data$fstress <- recode(interstroke_data$fstress,
                                   "1"="Little or None",
                                   "2"="Moderate or Severe")

interstroke_data$locquarthi <- factor(interstroke_data$locquarthi, levels=c("2","1"))
interstroke_data$locquarthi <- recode(interstroke_data$locquarthi,
                                      "1"="High Control",
                                      "2"="No or Less Control")

interstroke_data$depr <- factor(interstroke_data$depr)
interstroke_data$depr <- recode(interstroke_data$depr,
                                "1"="Not Sad in Last 2 Weeks",
                                "2"="Sad in Last 2 Weeks")

interstroke_data$global_stress2 <- factor(interstroke_data$global_stress2)
interstroke_data$global_stress2 <- recode(interstroke_data$global_stress2,
                                          "1"="None or Some Periods",
                                          "2"="Several Periods or Permanent")

interstroke_data$workstrs <- factor(interstroke_data$workstrs)
interstroke_data$workstrs <- recode(interstroke_data$workstrs,
                                    "1"="Never",
                                    "2"="Some of the time",
                                    "3"="Several Periods",
                                    "4"="Permanent")

interstroke_data$homestrs <- factor(interstroke_data$homestrs)
interstroke_data$homestrs <- recode(interstroke_data$homestrs,
                                    "1"="Never",
                                    "2"="Some of the time",
                                    "3"="Several Periods",
                                    "4"="Permanent")

interstroke_data$global_stress <- factor(interstroke_data$global_stress)
interstroke_data$global_stress <- recode(interstroke_data$global_stress,
                                         "1"="Never",
                                         "2"="Some of the time",
                                         "3"="Several Periods",
                                         "4"="Permanent")

interstroke_data$finastrs <- factor (interstroke_data$finastrs)
interstroke_data$finastrs <- recode (interstroke_data$finastrs,
                                     "1"="Little/none",
                                     "2"="Moderate",
                                     "3"="High/Severe")

interstroke_data$strclass <- factor (interstroke_data$strclass)
interstroke_data$strclass <- recode (interstroke_data$strclass,
                                     "1"="None",
                                     "2"="1 Event",
                                     "3"=">=2 Events")

interstroke_data$locquart <- factor (interstroke_data$locquart)
interstroke_data$locquart <- recode (interstroke_data$locquart,
                                     "1"="Q1:No Control",
                                     "2"="Q2",
                                     "3"="Q3",
                                     "4"="Q4:High Control")

interstroke_data$quartloc <- factor (interstroke_data$quartloc)
interstroke_data$quartloc <- recode (interstroke_data$quartloc,
                                     "1"="Q1:High Control",
                                     "2"="Q2:Less Control",
                                     "3"="Q3:Lesser Control",
                                     "4"="Q4:No Control")

interstroke_data$sad2wks <- factor(interstroke_data$sad2wks)
interstroke_data$sad2wks <- recode(interstroke_data$sad2wks,
                                   "1"="No",
                                   "2"="Yes")

interstroke_data$depdose <- factor (interstroke_data$depdose)
interstroke_data$depdose <- recode (interstroke_data$depdose,
                                    "1"="Not depressed",
                                    "2"="0-1 Items",
                                    "3"="2-4 Items",
                                    "4"=">4 Items")

interstroke_data$depdosedef3 <- factor (interstroke_data$depdosedef3)
interstroke_data$depdosedef3 <- recode (interstroke_data$depdosedef3,
                                        "0"="Not depressed",
                                        "1"="0-2 Items",
                                        "2"="3-4 Items",
                                        "3"=">4 Items")

interstroke_data$fstrktype <- factor (interstroke_data$fstrktype)
interstroke_data$fstrktype <- recode (interstroke_data$fstrktype,
                                      "1"="Ischemic",
                                      "2"="ICH",
                                      "3"="SAH",
                                      "Controls"="Controls")

interstroke_data$strktype <- factor (interstroke_data$strktype)
interstroke_data$strktype <- recode (interstroke_data$strktype,
                                     "1"="Ischemic",
                                     "2"="ICH",
                                     "3"="SAH",
                                     "Controls"="Controls")


interstroke_data$ctrltype <- factor (interstroke_data$ctrltype)
interstroke_data$ctrltype <- recode (interstroke_data$ctrltype,
                                     "1"="Community Based",
                                     "2"="Relative of a patient",
                                     "3"="Unrelated visitor",
                                     "4"="Patients or outpatient")

##
#Interstroke - Cancer
##

interstroke_data$cscancts <- factor(interstroke_data$cscancts)
interstroke_data$cscancts <- recode(interstroke_data$cscancts,
                                    "0"="No",
                                    "1"="Yes")

interstroke_data$cscanctc <- factor(interstroke_data$cscanctc)
interstroke_data$cscanctc <- recode(interstroke_data$cscanctc,
                                    "0"="No",
                                    "1"="Yes")

interstroke_data$cscanctr <- factor(interstroke_data$cscanctr)
interstroke_data$cscanctr <- recode(interstroke_data$cscanctr,
                                    "0"="No",
                                    "1"="Yes")

interstroke_data$cscanc <- factor(interstroke_data$cscanc)
interstroke_data$cscanc <- recode(interstroke_data$cscanc,
                                  "1"="No",
                                  "2"="Yes")

interstroke_data$owncar <- factor(interstroke_data$owncar)
interstroke_data$owncar <- recode(interstroke_data$owncar,
                                  "0"="Non car owner",
                                  "1"="Car owner")

interstroke_data$ownbike <- factor(interstroke_data$ownbike)
interstroke_data$ownbike <- recode(interstroke_data$ownbike,
                                   "0"="Non car owner",
                                   "1"="Car owner")

interstroke_data$subhiv <- factor(interstroke_data$subhiv)
interstroke_data$subhiv <- recode(interstroke_data$subhiv,
                                  "1"="No",
                                  "2"="Yes")

interstroke_data$smkexpos <- factor(interstroke_data$smkexpos)
interstroke_data$smkexpos <- recode(interstroke_data$smkexpos,
                                    "1"="Never",
                                    "2"="Less once per week",
                                    "3"="1-2 times per week",
                                    "4"="3-6 times per week",
                                    "5"="Everyday")

interstroke_data$othstrs <- factor(interstroke_data$othstrs)
interstroke_data$othstrs <- recode(interstroke_data$othstrs,
                                   "1"="No",
                                   "2"="Yes")

interstroke_data$vegetari <- factor(interstroke_data$vegetari)
interstroke_data$vegetari <- recode(interstroke_data$vegetari,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$meat <- factor(interstroke_data$meat)
interstroke_data$meat <- recode(interstroke_data$meat,
                                "1"="<1 per month - never",
                                "2"="monthly",
                                "3"="weekly",
                                "4"="daily")

interstroke_data$fruits <- factor(interstroke_data$fruits)
interstroke_data$fruits <- recode(interstroke_data$fruits,
                                  "1"="<1 per month - never",
                                  "2"="monthly",
                                  "3"="weekly",
                                  "4"="daily")

interstroke_data$bcpills <- factor(interstroke_data$bcpills)
interstroke_data$bcpills <- recode(interstroke_data$bcpills,
                                   "1"="No",
                                   "2"="Yes",
                                   "3"="Unknown")

interstroke_data$fertused <- factor(interstroke_data$fertused)
interstroke_data$fertused <- recode(interstroke_data$fertused,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$hormrepl <- factor(interstroke_data$hormrepl)
interstroke_data$hormrepl <- recode(interstroke_data$hormrepl,
                                    "1"="No",
                                    "2"="Yes",
                                    "3"="Unknown")

interstroke_data$polustrk <- factor(interstroke_data$polustrk)
interstroke_data$polustrk <- recode(interstroke_data$polustrk,
                                    "0"="Unchecked",
                                    "1"="Checked")

interstroke_data$rppbtumour <- factor(interstroke_data$rppbtumour)
interstroke_data$rppbtumour <- recode(interstroke_data$rppbtumour,
                                      "0"="Unchecked",
                                      "1"="Checked")

interstroke_data$rpsbtumour <- factor(interstroke_data$rpsbtumour)
interstroke_data$rpsbtumour <- recode(interstroke_data$rpsbtumour,
                                      "0"="Unchecked",
                                      "1"="Checked")

interstroke_data$m1seiz <- factor(interstroke_data$m1seiz)
interstroke_data$m1seiz <- recode(interstroke_data$m1seiz,
                                  "0"="Unchecked",
                                  "1"="Checked")

interstroke_data$m1depr <- factor(interstroke_data$m1depr)
interstroke_data$m1depr <- recode(interstroke_data$m1depr,
                                  "0"="Unchecked",
                                  "1"="Checked")

###
# Interstroke Sleep
###

interstroke_data$cslsnor <- factor(interstroke_data$cslsnor)
interstroke_data$cslsnor <- recode(interstroke_data$cslsnor,
                                   "1"="Never",
                                   "2"="Rarely",
                                   "3"="1-2 times per week",
                                   "4"="3-4 times per week",
                                   "5"="5-7 times per week",
                                   "6"="Don't know" )

interstroke_data$cssnort <- factor(interstroke_data$cssnort)
interstroke_data$cssnort <- recode(interstroke_data$cssnort,
                                   "1"="Never",
                                   "2"="Rarely",
                                   "3"="1-2 times per week",
                                   "4"="3-4 times per week",
                                   "5"="5-7 times per week",
                                   "6"="Don't know" )

interstroke_data$csbreat <- factor(interstroke_data$csbreat)
interstroke_data$csbreat <- recode(interstroke_data$csbreat,
                                   "1"="Never",
                                   "2"="Rarely",
                                   "3"="1-2 times per week",
                                   "4"="3-4 times per week",
                                   "5"="5-7 times per week",
                                   "6"="Don't know" )

interstroke_data$cssdplan <- factor(interstroke_data$cssdplan)
interstroke_data$cssdplan <- recode(interstroke_data$cssdplan,
                                    "0"="No Nap",
                                    "1"="Unplanned Nap",
                                    "2"="Planned Nap")

interstroke_data$cswake_factor <- cut(interstroke_data$cswake, c(-1,1,30))
interstroke_data$cswake_factor <- recode(interstroke_data$cswake_factor,
                                         "(-1,1]" = "Waking once or less",
                                         "(1,30]" = "Waking more than once")

interstroke_data$cssleeph_factor <- cut(interstroke_data$cssleeph, c(-1,4,5,6,7,8,9,14))
interstroke_data$cssleeph_factor <- recode(interstroke_data$cssleeph_factor,
                                           "(-1,4]" = "<5",
                                           "(4,5]" = "5",
                                           "(5,6]" = "6",
                                           "(6,7]" = "7",
                                           "(7,8]" = "8",
                                           "(8,9]" = "9",
                                           "(9,14]" = ">9")
#new 24 hour sleep variable

interstroke_data <- interstroke_data %>%
  mutate(cssleeph24 = ifelse(!is.na(cssleeph), ifelse(!is.na(csslepth), cssleeph + csslepth, cssleeph), NA))

interstroke_data$cssleeph24_factor <- cut(interstroke_data$cssleeph24, c(-1,4,5,6,7,8,9,14))
interstroke_data$cssleeph24_factor <- recode(interstroke_data$cssleeph24_factor,
                                             "(-1,4]" = "<5",
                                             "(4,5]" = "5",
                                             "(5,6]" = "6",
                                             "(6,7]" = "7",
                                             "(7,8]" = "8",
                                             "(8,9]" = "9",
                                             "(9,14]" = ">9")

interstroke_data$imagetp <- factor(interstroke_data$imagetp)
interstroke_data$imagetp <- recode(interstroke_data$imagetp,
                                   "1"="CT",
                                   "2"="MRI",
                                   "3"="Both")

interstroke_data$leisactv <- factor(interstroke_data$leisactv)
interstroke_data$leisactv <- recode(interstroke_data$leisactv,
                                    "1"="Mainly sedentary",
                                    "2"="Mild exercise",
                                    "3"="Moderate exercise",
                                    "4"="Strenuous exercise")

interstroke_data$symphh <- factor (interstroke_data$symphh)
interstroke_data$symphh <- recode(interstroke_data$symphh,
                                  "0"="No",
                                  "1"="No",
                                  "2"="Yes")

interstroke_data$subvthr <- factor (interstroke_data$subvthr)
interstroke_data$subvthr <- recode(interstroke_data$subvthr,
                                   "1"="No",
                                   "2"="Yes")

interstroke_data$teeth <- factor (interstroke_data$teeth)
interstroke_data$teeth <- recode(interstroke_data$teeth,
                                 "1"="No",
                                 "2"="Yes")

interstroke_data$paw <- factor (interstroke_data$paw)
interstroke_data$paw <- recode(interstroke_data$paw,
                               "1"="mainly_sedentary",
                               "2"="predominantly_walking_on_one_level",
                               "3"="mainly walking or heavy physical labour")

interstroke_data$alcohfreqwk <- factor (interstroke_data$alcohfreqwk)
interstroke_data$alcohfreqwk <- recode(interstroke_data$alcohfreqwk,
                                       "1"="never/former",
                                       "2"="low/moderate",
                                       "3"="high intake/binge")

interstroke_data$depr <- factor (interstroke_data$depr)
interstroke_data$depr <- recode(interstroke_data$depr,
                                "1"="not sad 2 weeks",
                                "2"="sad 2 weeks")

interstroke_data$ahei3tert <- factor (interstroke_data$ahei3tert)
interstroke_data$ahei3tert <- recode(interstroke_data$ahei3tert,
                                     "1"="tertile 1",
                                     "2"="tertile 2",
                                     "3"="tertile3")

interstroke_data$ischocsp <- factor (interstroke_data$ischocsp)
interstroke_data$ischocsp <- recode(interstroke_data$ischocsp,
                                    "1"="TACI",
                                    "2"="PACI",
                                    "3"="POCI",
                                    "4"="LACI",
                                    "5"="Other")


interstroke_data$vascimag <- factor (interstroke_data$vascimag)
interstroke_data$vascimag <- recode (interstroke_data$vascimag,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$mravasc <- factor (interstroke_data$mravasc)
interstroke_data$mravasc <- recode (interstroke_data$mravasc,
                                    "0"="No",
                                    "1"="Yes")

interstroke_data$ctavasc <- factor (interstroke_data$ctavasc)
interstroke_data$ctavasc <- recode (interstroke_data$ctavasc,
                                    "0"="No",
                                    "1"="Yes")

interstroke_data$physwork <- factor (interstroke_data$physwork)
interstroke_data$physwork <- recode (interstroke_data$physwork,
                                     "1"="Strenuous",
                                     "2"="Moderate",
                                     "3"="Mild",
                                     "4"="MainlySedentary")

interstroke_data$gstress <- factor (interstroke_data$gstress)
interstroke_data$gstress <- recode (interstroke_data$gstress,
                                    "1"="never",
                                    "2"="some_home/wrk")

interstroke_data$fstress <- factor (interstroke_data$fstress)
interstroke_data$fstress <- recode (interstroke_data$fstress,
                                    "1"="Little/none",
                                    "2"="Moderate/Severe")

interstroke_data$depr <- factor (interstroke_data$depr)
interstroke_data$depr <- recode (interstroke_data$depr,
                                 "1"="Not_Sad_2wks",
                                 "2"="sad_2wks")

interstroke_data$depdosedef2 <- factor (interstroke_data$depdosedef2)
interstroke_data$depdosedef2 <- recode (interstroke_data$depdosedef2,
                                        "0"="NotDepressed",
                                        "1"="<4Items",
                                        "2"=">=4Items")

interstroke_data$cscanct <- factor (interstroke_data$cscanct)
interstroke_data$cscanct <- recode (interstroke_data$cscanct,
                                    "1"="no",
                                    "2"="yes")

interstroke_data$cookphouse <- factor (interstroke_data$cookphouse)
interstroke_data$cookphouse <- recode (interstroke_data$cookphouse,
                                       "0"="Outside House",
                                       "1"="Both",
                                       "2"="Inside House")

interstroke_data$kerofuel <- factor (interstroke_data$kerofuel)
interstroke_data$kerofuel <- recode (interstroke_data$kerofuel,
                                     "0"="No",
                                     "1"="Yes")

interstroke_data$charfuel <- factor (interstroke_data$charfuel)
interstroke_data$charfuel <- recode (interstroke_data$charfuel,
                                     "0"="No",
                                     "1"="Yes")

interstroke_data$coalfuel <- factor (interstroke_data$coalfuel)
interstroke_data$coalfuel <- recode (interstroke_data$coalfuel,
                                     "0"="No",
                                     "1"="Yes")

interstroke_data$woodfuel <- factor (interstroke_data$woodfuel)
interstroke_data$woodfuel <- recode (interstroke_data$woodfuel,
                                     "0"="No",
                                     "1"="Yes")

interstroke_data$gobarfuel <- factor (interstroke_data$gobarfuel)
interstroke_data$gobarfuel <- recode (interstroke_data$gobarfuel,
                                      "0"="No",
                                      "1"="Yes")

interstroke_data$cropfuel <- factor (interstroke_data$cropfuel)
interstroke_data$cropfuel <- recode (interstroke_data$cropfuel,
                                     "0"="No",
                                     "1"="Yes")

interstroke_data$elecfuel <- factor (interstroke_data$elecfuel)
interstroke_data$elecfuel <- recode (interstroke_data$elecfuel,
                                     "0"="No",
                                     "1"="Yes")

interstroke_data$dungfuel <- factor (interstroke_data$dungfuel)
interstroke_data$dungfuel <- recode (interstroke_data$dungfuel,
                                     "0"="No",
                                     "1"="Yes")

interstroke_data$gasfuel <- factor (interstroke_data$gasfuel)
interstroke_data$gasfuel <- recode (interstroke_data$gasfuel,
                                    "0"="No",
                                    "1"="Yes")

interstroke_data$shrubfuel <- factor (interstroke_data$shrubfuel)
interstroke_data$shrubfuel <- recode (interstroke_data$shrubfuel,
                                      "0"="No",
                                      "1"="Yes")

interstroke_data$othfuel <- factor (interstroke_data$othfuel)
interstroke_data$othfuel <- recode (interstroke_data$othfuel,
                                    "0"="No",
                                    "1"="Yes")

interstroke_data$cookplace <- factor (interstroke_data$cookplace)
interstroke_data$cookplace <- recode (interstroke_data$cookplace,
                                      "1"="Inside the house",
                                      "2"="Outside the house",
                                      "3"="Both")

interstroke_data$nolgwork <- factor (interstroke_data$nolgwork)
interstroke_data$nolgwork <- recode (interstroke_data$nolgwork,
                                     "0"="No",
                                     "1"="Yes")

interstroke_data$gowkcar <- factor (interstroke_data$gowkcar)
interstroke_data$gowkcar <- recode (interstroke_data$gowkcar,
                                    "0"="No",
                                    "1"="Yes")

interstroke_data$gowkbus <- factor (interstroke_data$gowkbus)
interstroke_data$gowkbus <- recode (interstroke_data$gowkbus,
                                    "0"="No",
                                    "1"="Yes")

interstroke_data$gowkbike <- factor (interstroke_data$gowkbike)
interstroke_data$gowkbike <- recode (interstroke_data$gowkbike,
                                     "0"="No",
                                     "1"="Yes")

interstroke_data$gowktran <- factor (interstroke_data$gowktran)
interstroke_data$gowktran <- recode (interstroke_data$gowktran,
                                     "0"="No",
                                     "1"="Yes")

interstroke_data$gowkwalk <- factor (interstroke_data$gowkwalk)
interstroke_data$gowkwalk <- recode (interstroke_data$gowkwalk,
                                     "0"="No",
                                     "1"="Yes")

interstroke_data$gowkoth <- factor (interstroke_data$gowkoth)
interstroke_data$gowkoth <- recode (interstroke_data$gowkoth,
                                    "0"="No",
                                    "1"="Yes")

interstroke_data$workrv <- factor (interstroke_data$workrv)
interstroke_data$workrv <- recode (interstroke_data$workrv,
                                   "1"="Yes",
                                   "2"="No")

interstroke_data$workrv <- factor (interstroke_data$workrv)
interstroke_data$workrv <- recode (interstroke_data$workrv,
                                   "1"="Yes",
                                   "2"="No")

interstroke_data$thromhos <- factor (interstroke_data$thromhos)
interstroke_data$thromhos <- recode (interstroke_data$thromhos,
                                     "1"="No thrombolysis in hospital",
                                     "2"="Had thrombolysis in hospital")


#Re-level regionnn7
interstroke_data$regionnn7_cat <- factor(interstroke_data$regionnn7)
interstroke_data$regionnn7_cat=recode(interstroke_data$regionnn7_cat,
                                      "Western Europe/North America/Australasia"="High Income Countries",
                                      "Eastern/Central Europe/Middle East"="Middle Income Countries",
                                      "Africa"="Low Income Countries",
                                      "South Asia"="Low Income Countries",
                                      "China"="Middle Income Countries",
                                      "South East Asia"="Low Income Countries",
                                      "South America"="Middle Income Countries")

#Set factor levels for cancer type
interstroke_data$cscancl <- factor(interstroke_data$cscancl)
interstroke_data$cscancl <- recode(interstroke_data$cscancl,
                                   "1"="Brain",
                                   "2"="Bone",
                                   "3"="Blood",
                                   "4"="Bowel",
                                   "5"="Breast",
                                   "6"= "Cervical/Uterine(womb)/Ovarian",
                                   "7"="Esophagus",
                                   "8"="Head and Neck",
                                   "9"="Liver",
                                   "10"="Lung",
                                   "11"="Mouth/Tongue",
                                   "12"="Pancreas",
                                   "13"="Prostate",
                                   "14"="Skin",
                                   "15"="Stomach",
                                   "16"="Urinary",
                                   "17"="Multi-site",
                                   "18"="Others")

#new variable for cooking fuel

interstroke_data <- interstroke_data %>%
  mutate(count = (kerofuel=="Yes") + (charfuel=="Yes") + (coalfuel=="Yes") + (woodfuel=="Yes") +
           (gobarfuel=="Yes") + (cropfuel=="Yes") +(elecfuel=="Yes") + (dungfuel=="Yes") +
           (gasfuel=="Yes") + (shrubfuel=="Yes") +(othfuel=="Yes")) %>%
  mutate(cooking_fuel = ifelse(count>1,"Combined",NA)) %>%
  mutate(cooking_fuel = ifelse(is.na(cooking_fuel),
                               ifelse(kerofuel=="Yes","Kerosene",
                                      ifelse(charfuel=="Yes","Charcoal",
                                             ifelse(coalfuel=="Yes","Coal",
                                                    ifelse(woodfuel=="Yes","Wood",
                                                           ifelse(gobarfuel=="Yes","Gobar",
                                                                  ifelse(cropfuel=="Yes","Crop",
                                                                         ifelse(elecfuel=="Yes","Electricity",
                                                                                ifelse(dungfuel=="Yes","Dung",
                                                                                       ifelse(gasfuel=="Yes","Gas",
                                                                                              ifelse(shrubfuel=="Yes","Shrub",
                                                                                                     ifelse(othfuel=="Yes","Other",NA))))))))))),cooking_fuel)) %>%
  mutate(cooking_fuel_3_level = ifelse(cooking_fuel=="Kerosene","Kerosene",
                                       ifelse(cooking_fuel=="Charcoal","Solid fuel",
                                              ifelse(cooking_fuel=="Coal","Solid fuel",
                                                     ifelse(cooking_fuel=="Wood","Solid fuel",
                                                            ifelse(cooking_fuel=="Gobar","Solid fuel",
                                                                   ifelse(cooking_fuel=="Crop","Solid fuel",
                                                                          ifelse(cooking_fuel=="Electricity","Clean fuel",
                                                                                 ifelse(cooking_fuel=="Dung","Solid fuel",
                                                                                        ifelse(cooking_fuel=="Gas","Clean fuel",
                                                                                               ifelse(cooking_fuel=="Shrub","Solid fuel",
                                                                                                      ifelse(cooking_fuel=="Other",NA,
                                                                                                             ifelse(cooking_fuel=="Combined","Combined",NA))))))))))))) %>%
  mutate(cooking_fuel_3_level = ifelse(cooking_fuel_3_level=="Combined" & (elecfuel!="Yes" & gasfuel!="Yes"),"Solid fuel",cooking_fuel_3_level)) %>%
  mutate(cooking_fuel_3_level = ifelse(cooking_fuel_3_level=="Combined" & (kerofuel!="Yes" &
                                                                             charfuel!="Yes" &
                                                                             coalfuel!="Yes" &
                                                                             woodfuel!="Yes" &
                                                                             gobarfuel!="Yes" &
                                                                             cropfuel!="Yes" &
                                                                             dungfuel!="Yes" &
                                                                             shrubfuel!="Yes" &
                                                                             othfuel!="Yes" &
                                                                             dungfuel!="Yes"),"Clean fuel",cooking_fuel_3_level)) %>%
  select(-count)

#new variable for transport

interstroke_data <- interstroke_data %>%
  mutate(count = (gowkcar=="Yes") + (gowkbus=="Yes") + (gowkmoto=="Yes") + (gowkbike=="Yes") +
           (gowktran=="Yes") + (gowkwalk=="Yes") +(gowkoth=="Yes"))
interstroke_data <- interstroke_data %>% mutate(transport_wk = ifelse(gowkcar=="Yes","Car",
                               ifelse(gowkbus=="Yes","Bus",
                                      ifelse(gowkmoto=="Yes","Motorcycle/Scooter",
                                             ifelse(gowkbike=="Yes","Bicycle",
                                                    ifelse(gowktran=="Yes","Train",
                                                           ifelse(gowkwalk=="Yes","Walk",
                                                                  ifelse(gowkoth=="Yes","Other",NA))))))),transport_wk)


interstroke_data$cooking_fuel <- factor(interstroke_data$cooking_fuel)
interstroke_data$cooking_fuel <- relevel(interstroke_data$cooking_fuel, ref = "Electricity")

interstroke_data$cooking_fuel_3_level <- factor(interstroke_data$cooking_fuel_3_level)
interstroke_data$cooking_fuel_3_level <- relevel(interstroke_data$cooking_fuel_3_level, ref = "Clean fuel")

interstroke_data$transport_wk <- factor(interstroke_data$transport_wk)
interstroke_data$transport_wk <- relevel(interstroke_data$transport_wk, ref = "Walk")

interstroke_data$thromall <- factor (interstroke_data$thromall)
interstroke_data$thromall <- recode (interstroke_data$thromall,
                                     "0"="No",
                                     "1"="Yes")

interstroke_data$levconsc <- factor (interstroke_data$levconsc)
interstroke_data$levconsc <- recode (interstroke_data$levconsc,
                                     "1"="Alert",
                                     "2"="Drowsy",
                                     "3"="Unconcious")

interstroke_data$sepadivo <- factor (interstroke_data$sepadivo)
interstroke_data$sepadivo <- recode (interstroke_data$sepadivo,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$lossjob <- factor (interstroke_data$lossjob)
interstroke_data$lossjob <- recode (interstroke_data$lossjob,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$lossjob <- factor (interstroke_data$lossjob)
interstroke_data$lossjob <- recode (interstroke_data$lossjob,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$busfail <- factor (interstroke_data$busfail)
interstroke_data$busfail <- recode (interstroke_data$busfail,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$violence <- factor (interstroke_data$violence)
interstroke_data$violence <- recode (interstroke_data$violence,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$famcnfl <- factor (interstroke_data$famcnfl)
interstroke_data$famcnfl <- recode (interstroke_data$famcnfl,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$injury <- factor (interstroke_data$injury)
interstroke_data$injury <- recode (interstroke_data$injury,
                                   "1"="No",
                                   "2"="Yes")

interstroke_data$deathfam <- factor (interstroke_data$deathfam)
interstroke_data$deathfam <- recode (interstroke_data$deathfam,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$deathspo <- factor (interstroke_data$deathspo)
interstroke_data$deathspo <- recode (interstroke_data$deathspo,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$stopwork <- factor (interstroke_data$stopwork)
interstroke_data$stopwork <- recode (interstroke_data$stopwork,
                                     "1"="Blank",
                                     "2"="Checked")

interstroke_data$autonomy <- factor (interstroke_data$autonomy)
interstroke_data$autonomy <- recode (interstroke_data$autonomy,
                                     "1"="None",
                                     "2"="Little",
                                     "3"="Moderate",
                                     "4"="Substantial",
                                     "5"="Complete",
                                     "6"="Not applicable")

interstroke_data$wrkcntrl <- factor (interstroke_data$wrkcntrl)
interstroke_data$wrkcntrl <- recode (interstroke_data$wrkcntrl,
                                     "1"="Strongly disagree",
                                     "2"="Disagree",
                                     "3"="Neutral",
                                     "4"="Agree",
                                     "5"="Strongly agree")

interstroke_data$lifcntrl <- factor (interstroke_data$lifcntrl)
interstroke_data$lifcntrl <- recode (interstroke_data$lifcntrl,
                                     "1"="Strongly disagree",
                                     "2"="Disagree",
                                     "3"="Neutral",
                                     "4"="Agree",
                                     "5"="Strongly agree")

interstroke_data$futurexp <- factor (interstroke_data$futurexp)
interstroke_data$futurexp <- recode (interstroke_data$futurexp,
                                     "1"="Strongly disagree",
                                     "2"="Disagree",
                                     "3"="Neutral",
                                     "4"="Agree",
                                     "5"="Strongly agree")

interstroke_data$treated <- factor (interstroke_data$treated)
interstroke_data$treated <- recode (interstroke_data$treated,
                                    "1"="Strongly disagree",
                                    "2"="Disagree",
                                    "3"="Neutral",
                                    "4"="Agree",
                                    "5"="Strongly agree")

interstroke_data$changes <- factor (interstroke_data$changes)
interstroke_data$changes <- recode (interstroke_data$changes,
                                    "1"="Strongly disagree",
                                    "2"="Disagree",
                                    "3"="Neutral",
                                    "4"="Agree",
                                    "5"="Strongly agree")

interstroke_data$gaveup <- factor (interstroke_data$gaveup)
interstroke_data$gaveup <- recode (interstroke_data$gaveup,
                                   "1"="Strongly disagree",
                                   "2"="Disagree",
                                   "3"="Neutral",
                                   "4"="Agree",
                                   "5"="Strongly agree")

interstroke_data$interest <- factor (interstroke_data$interest)
interstroke_data$interest <- recode (interstroke_data$interest,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$tired <- factor (interstroke_data$tired)
interstroke_data$tired <- recode (interstroke_data$tired,
                                  "1"="No",
                                  "2"="Yes")

interstroke_data$glweight <- factor (interstroke_data$glweight)
interstroke_data$glweight <- recode (interstroke_data$glweight,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$tosleep <- factor (interstroke_data$tosleep)
interstroke_data$tosleep <- recode (interstroke_data$tosleep,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$focus <- factor (interstroke_data$focus)
interstroke_data$focus <- recode (interstroke_data$focus,
                                  "1"="No",
                                  "2"="Yes")

interstroke_data$thkdeath <- factor (interstroke_data$thkdeath)
interstroke_data$thkdeath <- recode (interstroke_data$thkdeath,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$feeldown <- factor (interstroke_data$feeldown)
interstroke_data$feeldown <- recode (interstroke_data$feeldown,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$othstrs <- factor (interstroke_data$othstrs)
interstroke_data$othstrs <- recode (interstroke_data$othstrs,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$ntconscan <- factor (interstroke_data$ntconscan)
interstroke_data$ntconscan <- recode (interstroke_data$ntconscan,
                                      "0"="No",
                                      "1"="Yes")

interstroke_data$antihypert3 <- factor (interstroke_data$antihypert3)
interstroke_data$antihypert3 <- recode (interstroke_data$antihypert3,
                                        "0"="None",
                                        "1"="1 medication",
                                        "2"=">=2 medications",
                                        "5" = ">=2 medications",
                                        "6" = ">=2 medications")

interstroke_data$aspirpre <- factor (interstroke_data$aspirpre)
interstroke_data$aspirpre <- recode (interstroke_data$aspirpre,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$ctrltype <- factor (interstroke_data$ctrltype)
interstroke_data$ctrltype <- recode (interstroke_data$ctrltype,
                                     "1"="Community Based",
                                     "2"="Relative of a patient",
                                     "3"="Unrelated visitor",
                                     "4"= "Patients or Outpatient")

interstroke_data$exrhrcs <- factor (interstroke_data$exrhrcs)
interstroke_data$exrhrcs <- recode (interstroke_data$exrhrcs,
                                    "1"="No/Unknown",
                                    "2"="Yes")

interstroke_data$exrdaycs <- factor (interstroke_data$exrdaycs)
interstroke_data$exrdaycs <- recode (interstroke_data$exrdaycs,
                                     "1"="No/Unknown",
                                     "2"="Yes")

interstroke_data$symwoke <- factor (interstroke_data$symwoke)
interstroke_data$symwoke <- recode (interstroke_data$symwoke,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$wakesym <- factor (interstroke_data$wakesym)
interstroke_data$wakesym <- recode (interstroke_data$wakesym,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$dcct65 <- factor (interstroke_data$dcct65)
interstroke_data$dcct65 <- recode (interstroke_data$dcct65,
                                   "1"="No",
                                   "2"="Yes")

interstroke_data$work<- factor (interstroke_data$work)
interstroke_data$work <- recode (interstroke_data$work,
                                 "1"="No",
                                 "2"="Yes")

interstroke_data$marijhis <- factor (interstroke_data$marijhis)
interstroke_data$marijhis <- recode (interstroke_data$marijhis,
                                     "1"="Never",
                                     "2"="Formerly",
                                     "3"= "Currently")

interstroke_data$cocaihis <- factor (interstroke_data$cocaihis)
interstroke_data$cocaihis <- recode (interstroke_data$cocaihis,
                                     "1"="Never",
                                     "2"="Formerly",
                                     "3"= "Currently")

interstroke_data$opiathis <- factor (interstroke_data$opiathis)
interstroke_data$opiathis <- recode (interstroke_data$opiathis,
                                     "1"="Never",
                                     "2"="Formerly",
                                     "3"= "Currently")

interstroke_data$smokexpose <- factor (interstroke_data$smokexpose)
interstroke_data$smokexpose <- recode (interstroke_data$smokexpose,
                                       "1"="No Exposure",
                                       "2"="<1/week",
                                       "3"= "1-6times/week",
                                       "4"= "Everyday")

interstroke_data$ls6menst <- factor (interstroke_data$ls6menst)
interstroke_data$ls6menst <- recode (interstroke_data$ls6menst,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$failpreg <- factor (interstroke_data$failpreg)
interstroke_data$failpreg <- recode (interstroke_data$failpreg,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$pregnant <- factor (interstroke_data$pregnant)
interstroke_data$pregnant <- recode (interstroke_data$pregnant,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$preghtn <- factor (interstroke_data$preghtn)
interstroke_data$preghtn <- recode (interstroke_data$preghtn,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$pregpuri <- factor (interstroke_data$pregpuri)
interstroke_data$pregpuri <- recode (interstroke_data$pregpuri,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$preglegs <- factor (interstroke_data$preglegs)
interstroke_data$preglegs <- recode (interstroke_data$preglegs,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$pregseiz <- factor (interstroke_data$pregseiz)
interstroke_data$pregseiz <- recode (interstroke_data$pregseiz,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$pregdm <- factor (interstroke_data$pregdm)
interstroke_data$pregdm <- recode (interstroke_data$pregdm,
                                   "1"="No",
                                   "2"="Yes")

interstroke_data$miscarrig <- factor (interstroke_data$miscarrig)
interstroke_data$miscarrig <- recode (interstroke_data$miscarrig,
                                      "1"="No",
                                      "2"="Yes")

interstroke_data$menstop <- factor (interstroke_data$menstop)
interstroke_data$menstop <- recode (interstroke_data$menstop,
                                    "1"="No",
                                    "2"="Yes",
                                    "3"= "Unknown")

interstroke_data$pregstk <- factor (interstroke_data$pregstk)
interstroke_data$pregstk <- recode (interstroke_data$pregstk,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$ftoast1 <- factor (interstroke_data$ftoast1)
interstroke_data$ftoast1 <- recode (interstroke_data$ftoast1,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$clopdhos <- factor (interstroke_data$clopdhos)
interstroke_data$clopdhos <- recode (interstroke_data$clopdhos,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$dipyrhos <- factor (interstroke_data$dipyrhos)
interstroke_data$dipyrhos <- recode (interstroke_data$dipyrhos,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$asdiphos <- factor (interstroke_data$asdiphos)
interstroke_data$asdiphos <- recode (interstroke_data$asdiphos,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$motbbled <- factor (interstroke_data$motbbled)
interstroke_data$motbbled <- recode (interstroke_data$motbbled,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$fatbbled <- factor (interstroke_data$fatbbled)
interstroke_data$fatbbled <- recode (interstroke_data$fatbbled,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$ononstrk <- factor (interstroke_data$ononstrk)
interstroke_data$ononstrk <- recode (interstroke_data$ononstrk,
                                     "0"="No",
                                     "1"="Yes")

interstroke_data$csbldc <- factor (interstroke_data$csbldc)
interstroke_data$csbldc <- recode (interstroke_data$csbldc,
                                   "1"="No",
                                   "2"="Yes")

interstroke_data$ceperfom <- factor (interstroke_data$ceperfom)
interstroke_data$ceperfom <- recode (interstroke_data$ceperfom,
                                     "1"="No",
                                     "2"="Yes")

interstroke_data$m1physio <- factor (interstroke_data$m1physio)
interstroke_data$m1physio <- recode (interstroke_data$m1physio,
                                     "1"="No",
                                     "2"="Yes",
                                     "3"= "Unsure")

interstroke_data$m1occupa <- factor (interstroke_data$m1occupa)
interstroke_data$m1occupa <- recode (interstroke_data$m1occupa,
                                     "1"="No",
                                     "2"="Yes",
                                     "3"= "Unsure")

interstroke_data$m1speech <- factor (interstroke_data$m1speech)
interstroke_data$m1speech <- recode (interstroke_data$m1speech,
                                     "1"="No",
                                     "2"="Yes",
                                     "3"= "Unsure")

interstroke_data$m1antipl <- factor (interstroke_data$m1antipl)
interstroke_data$m1antipl <- recode (interstroke_data$m1antipl,
                                     "1"="No",
                                     "2"="Yes",
                                     "3"= "Unsure")

interstroke_data$m1antico <- factor (interstroke_data$m1antico)
interstroke_data$m1antico <- recode (interstroke_data$m1antico,
                                     "1"="No",
                                     "2"="Yes",
                                     "3"= "Unsure")

interstroke_data$m1bldpld <- factor (interstroke_data$m1bldpld)
interstroke_data$m1bldpld <- recode (interstroke_data$m1bldpld,
                                     "1"="No",
                                     "2"="Yes",
                                     "3"= "Unsure")

interstroke_data$m1choles <- factor (interstroke_data$m1choles)
interstroke_data$m1choles <- recode (interstroke_data$m1choles,
                                     "1"="No",
                                     "2"="Yes",
                                     "3"= "Unsure")

interstroke_data$fatstrk <- factor (interstroke_data$fatstrk)
interstroke_data$fatstrk <- recode (interstroke_data$fatstrk,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$motstrk <- factor (interstroke_data$motstrk)
interstroke_data$motstrk <- recode (interstroke_data$motstrk,
                                    "1"="No",
                                    "2"="Yes")

interstroke_data$antithromb <- factor (interstroke_data$antithromb)
interstroke_data$antithromb <- recode (interstroke_data$antithromb,
                                       "0" = "No Antiplatelets",
                                       "1"="Antiplatelets",
                                       "2"="Oral Anticoagulation")

interstroke_data$bmi5grp <- factor (interstroke_data$bmi5grp)
interstroke_data$bmi5grp <- recode (interstroke_data$bmi5grp,
                                    "1"="0< BMI <=22",
                                    "2"="22< BMI <=25",
                                    "3"="25< BMI <=27",
                                    "4"="27< BMI <=30",
                                    "5"=" BMI >30")

interstroke_data$bmi_quintiles <- ntile(interstroke_data$bmi,5)
interstroke_data$bmi_quintiles <- factor(interstroke_data$bmi_quintiles)

interstroke_data$homestrs_3 <- recode(interstroke_data$homestrs,
                                      "Never"="Never",
                                      "Some of the time"="Some of the time",
                                      "Several Periods"="Several Periods/Permanent",
                                      "Permanent"="Several Periods/Permanent")

interstroke_data$workstrs_3 <- recode(interstroke_data$workstrs,
                                      "Never"="Never",
                                      "Some of the time"="Some of the time",
                                      "Several Periods"="Several Periods/Permanent",
                                      "Permanent"="Several Periods/Permanent")

interstroke_data$global_stress_3 <- recode(interstroke_data$global_stress,
                                           "Never"="Never",
                                           "Some of the time"="Some of the time",
                                           "Several Periods"="Several Periods/Permanent",
                                           "Permanent"="Several Periods/Permanent")

interstroke_data <- interstroke_data %>%
  mutate(bmi_3_level = ifelse(bmi<25,"BMI <25",
                              ifelse(bmi<30,"BMI 25-29.9","BMI >=30")))

interstroke_data$bmi_3_level <- factor(interstroke_data$bmi_3_level,
                                       levels=c("BMI <25","BMI 25-29.9","BMI >=30"))

interstroke_data$highwhr <- factor (interstroke_data$highwhr)
interstroke_data$highwhr <- recode (interstroke_data$highwhr,
                                    "1"="Low",
                                    "2"="Moderate",
                                    "3"="High")

interstroke_data$cscancm1 <- factor (interstroke_data$cscancm1)
interstroke_data$cscancm1 <- recode (interstroke_data$cscancm1,
                                     "1"="Brain",
                                     "2"="Bone",
                                     "3"="Blood",
                                     "4"="Bowel",
                                     "5"="Breast",
                                     "6"= "Cervical/Uterine(womb)/Ovarian",
                                     "7"="Esophagus",
                                     "8"="Head and Neck",
                                     "9"="Liver",
                                     "10"="Lung",
                                     "11"="Mouth/Tongue",
                                     "12"="Pancreas",
                                     "13"="Prostate",
                                     "14"="Skin",
                                     "15"="Stomach",
                                     "16"="Urinary",
                                     "17"="Multi-site",
                                     "18"="Others")

interstroke_data$cscancm1_cat<-recode(interstroke_data$cscancm1,
                                      "Brain"="Brain Cancer",
                                      "Bone"="Bone Cancer",
                                      "Blood"="Blood Cancer",
                                      "Bowel"="GI Cancer",
                                      "Breast"="Gonadal Cancer",
                                      "Cervical/Uterine(womb)/Ovarian"= "Gonadal Cancer",
                                      "Esophagus"="GI Cancer",
                                      "Head and Neck"="Head and Neck Cancer",
                                      "Liver"="GI Cancer",
                                      "Lung"="Lung Cancer",
                                      "Mouth/Tongue"="Head and Neck Cancer",
                                      "Pancreas"="GI Cancer",
                                      "Prostate"="Gonadal Cancer",
                                      "Skin"="Skin Cancer",
                                      "Stomach"="GI Cancer",
                                      "Urinary"="Urinary Cancer",
                                      "Multi-site"="Multi-site Cancer",
                                      "Others"="Other Cancer")

interstroke_data$cscancl_cat<-recode(interstroke_data$cscancl,
                                     "Brain"="Brain Cancer",
                                     "Bone"="Bone Cancer",
                                     "Blood"="Blood Cancer",
                                     "Bowel"="GI Cancer",
                                     "Breast"="Gonadal Cancer",
                                     "Cervical/Uterine(womb)/Ovarian"= "Gonadal Cancer",
                                     "Esophagus"="GI Cancer",
                                     "Head and Neck"="Head and Neck Cancer",
                                     "Liver"="GI Cancer",
                                     "Lung"="Lung Cancer",
                                     "Mouth/Tongue"="Head and Neck Cancer",
                                     "Pancreas"="GI Cancer",
                                     "Prostate"="Gonadal Cancer",
                                     "Skin"="Skin Cancer",
                                     "Stomach"="GI Cancer",
                                     "Urinary"="Urinary Cancer",
                                     "Multi-site"="Multi-site Cancer",
                                     "Others"="Other Cancer")


#Years since diagnosis of cancer to stroke calculation
interstroke_data <- interstroke_data %>%
  mutate(years_since_diagnosis_cancer = eage-cscanca)%>%
  mutate(years_since_diagnosis_cancer_cat=ifelse(years_since_diagnosis_cancer <1,"<1 year",
                                                 ifelse (years_since_diagnosis_cancer <2, "<2 years",
                                                         ifelse(years_since_diagnosis_cancer >=2, ">=2 years", "error"))))

interstroke_data$years_since_diagnosis_cancer_cat <- factor (interstroke_data$years_since_diagnosis_cancer_cat,
                                                             levels=c(">=2 years", "<1 year", "<2 years"))

## WORK METS
# Create new Variable for Work Hours
# Confine work hours to 0-20 hours range
interstroke_data <- interstroke_data %>%
  mutate(workhrs_1 = ifelse(is.na(workhrs), 0, ifelse(workhrs <0 | workhrs > 20, 0, workhrs))) %>%
  mutate(workhrs_week = workhrs*5)

# Calculate work METS
#METS - Work
interstroke_data <- interstroke_data %>%
  mutate(wacthrs_1 = wacthrs)

interstroke_data <- interstroke_data %>%
  mutate(wactmins_1 = wactmins)

interstroke_data$wacthrs_1 =  interstroke_data$wacthrs_1*300
interstroke_data$wactmins_1 =  interstroke_data$wactmins_1*5

interstroke_data <- interstroke_data %>%
  mutate(workmins_A = interstroke_data$wacthrs_1 + interstroke_data$wactmins_1)

# Confine work hours to 0-1080 mins range
interstroke_data <- interstroke_data %>% mutate(workmins_A = ifelse(workmins_A <0 | workmins_A > 5400, 0, workmins_A))

# Replace NA with 0
interstroke_data$workmins_A[is.na(interstroke_data$workmins_A)] = 0

#Multiply hours worked by MET level (physwork)
interstroke_data <- interstroke_data %>%
  mutate(mets_workalone =  ifelse(physwork == "Sedentary", workhrs_1*1.25,
                                  ifelse(physwork == "Mild", workhrs_1*2,
                                         ifelse(physwork == "Moderate", workhrs_1*4,
                                                ifelse(physwork == "Strenuous", workhrs_1*6,NA)))))

# Confine METS 0 to <15000
interstroke_data <- interstroke_data %>%
  mutate(mets_workalone = ifelse(mets_workalone <0 | mets_workalone > 15000, NA, mets_workalone))

#Wealth Index Tertile
interstroke_data$windextert <- factor (interstroke_data$windextert)
interstroke_data$windextert <- recode (interstroke_data$windextert,
                                       "1"="Tertile 1",
                                       "2"="Tertile 2",
                                       "3" = "Tertile 3")

#Hospital Oral Anticoagulant Use
interstroke_data$oranthos <- factor (interstroke_data$oranthos)
interstroke_data$oranthos <- recode (interstroke_data$oranthos,
                                     "1"="No",
                                     "2"="Yes")

#Hospital Aspirin Use
interstroke_data$aspirhos <- factor (interstroke_data$aspirhos)
interstroke_data$aspirhos <- recode (interstroke_data$aspirhos,
                                     "1"="No",
                                     "2"="Yes")

#Hospital Statin Use
interstroke_data$statihos <- factor (interstroke_data$statihos)
interstroke_data$statihos <- recode (interstroke_data$statihos,
                                     "1"="No",
                                     "2"="Yes")

#Hospital Beta blocker Use
interstroke_data$betabhos <- factor (interstroke_data$betabhos)
interstroke_data$betabhos <- recode (interstroke_data$betabhos,
                                     "1"="No",
                                     "2"="Yes")

#Hospital ace i Use
interstroke_data$acehos <- factor (interstroke_data$acehos)
interstroke_data$acehos <- recode (interstroke_data$acehos,
                                     "1"="No",
                                     "2"="Yes")

#Hospital arb Use
interstroke_data$arbhos <- factor (interstroke_data$arbhos)
interstroke_data$arbhos <- recode (interstroke_data$arbhos,
                                   "1"="No",
                                   "2"="Yes")

#Hospital alpha blocker use
interstroke_data$alphbhos <- factor (interstroke_data$alphbhos)
interstroke_data$alphbhos <- recode (interstroke_data$alphbhos,
                                   "1"="No",
                                   "2"="Yes")

#Hospital CCB Use
interstroke_data$cacbhos<- factor (interstroke_data$cacbhos)
interstroke_data$cacbhos <- recode (interstroke_data$cacbhos,
                                     "1"="No",
                                     "2"="Yes")

# Hospital diuretic use
interstroke_data$diurehos<- factor (interstroke_data$diurehos)
interstroke_data$diurehos <- recode (interstroke_data$diurehos,
                                    "1"="No",
                                    "2"="Yes")

#Cardiac RF with Meds
interstroke_data$cardiacmeds <- factor (interstroke_data$cardiacmeds)
interstroke_data$cardiacmeds <- recode (interstroke_data$cardiacmeds,
                                        "1"="No",
                                        "2"="Cardiac RF + Meds",
                                        "3"="Cardiac RF + No Meds")

#OCSP
interstroke_data$ischocspfinal <- factor (interstroke_data$ischocspfinal)
interstroke_data$ischocspfinal <- recode (interstroke_data$ischocspfinal,
                                          "1"="TACI",
                                          "2"="PACI",
                                          "3"="PCOI",
                                          "4" = "LACI")

#OCSP Lacunar
interstroke_data$oscplaci <- factor (interstroke_data$oscplaci)
interstroke_data$oscplaci <- recode (interstroke_data$oscplaci,
                                     "0"="OSCP Non-Lacunar",
                                     "1"="OCSP Lacunar")

#TOAST Undetermined
interstroke_data$undetermined <- factor (interstroke_data$undetermined)
interstroke_data$undetermined <- recode (interstroke_data$undetermined,
                                         "0"="No",
                                         "1"="Yes")

#TOAST Cardioembolic
interstroke_data$cardioembolic <- factor (interstroke_data$cardioembolic)
interstroke_data$cardioembolic <- recode (interstroke_data$cardioembolic,
                                          "0"="No",
                                          "1"="Yes")

#TOAST Large Vessel
interstroke_data$largevessel <- factor (interstroke_data$largevessel)
interstroke_data$largevessel <- recode (interstroke_data$largevessel,
                                        "0"="No",
                                        "1"="Yes")

#TOAST smallvessel
interstroke_data$smallvessel <- factor (interstroke_data$smallvessel)
interstroke_data$smallvessel <- recode (interstroke_data$smallvessel,
                                        "0"="No",
                                        "1"="Yes")

#TOAST Other
interstroke_data$othertoast <- factor (interstroke_data$othertoast)
interstroke_data$othertoast <- recode (interstroke_data$othertoast,
                                       "0"="No",
                                       "1"="Yes")

##TOAST
interstroke_data <- interstroke_data %>%
  mutate(TOAST = ifelse(cardioembolic == "Yes","TOAST Cardioembolic",
                        ifelse(largevessel == "Yes" ,"TOAST Large Vessel",
                               ifelse(smallvessel == "Yes", "TOAST Small Vessel",
                                      ifelse(undetermined == "Yes", "TOAST Undetermined",
                                             ifelse(othertoast == "Yes", "Other TOAST Classification", "Unchecked"))))))
interstroke_data$TOAST <- factor(interstroke_data$TOAST, levels = c("TOAST Cardioembolic", "TOAST Large Vessel", "TOAST Small Vessel", "TOAST Undetermined", "Other TOAST Classification" ))

##Age Group Categories
interstroke_data$agecat <- recode(interstroke_data$agecat,
                                  "1" = "<=45 yrs",
                                  "2" = "46-55 yrs",
                                  "3" = "56-65 yrs",
                                  "4" = "66-70 yrs",
                                  "5" = ">70 yrs")
interstroke_data$agecat <- factor(interstroke_data$agecat, levels = c("<=45 yrs", "46-55 yrs", "56-65 yrs", "66-70 yrs", ">70 yrs" ))


##Age group decade
interstroke_data <- interstroke_data %>%
  mutate(age_group_decade=ifelse(eage>=18&eage<40,"<40",
                                 ifelse(eage>=40&eage<50,"40-49",
                                        ifelse(eage>=50&eage<60,"50-59",
                                               ifelse(eage>=60&eage<70,"60-69",
                                                      ifelse(eage>=70&eage<79,"70-79",
                                                             ifelse(eage>=80&eage<120,">80","invalid")))))))
interstroke_data$age_group_decade <- factor(interstroke_data$age_group_decade, levels = c("<40", "40-49", "50-59", "60-69","70-79", ">80"))


#AF on ECG
interstroke_data$afecg <- factor (interstroke_data$afecg)
interstroke_data$afecg <- recode (interstroke_data$afecg,
                                  "0"="Blank",
                                  "1"="Checked")

#Echo Performed
interstroke_data$lvefechol <- factor (interstroke_data$lvefechol)
interstroke_data$echo <- recode (interstroke_data$lvefechol,
                                 "0"="No",
                                 "1" = "Yes",
                                 "2"="Yes",
                                 "3" = "Yes")

##EF
interstroke_data$lvefechol <- recode (interstroke_data$lvefechol,
                                      "0"="Not done",
                                      "1" = "EF < 35%",
                                      "2"="EF 35-50%",
                                      "3" = "EF > 50%")

#ECG Performed by 1 Month
interstroke_data$cardecg <- factor (interstroke_data$cardecg)
interstroke_data$cardecg <- recode (interstroke_data$cardecg,
                                    "0"="Blank",
                                    "1"="Checked")

#Holter
interstroke_data$cardholt <- factor (interstroke_data$cardholt)
interstroke_data$cardholt <- recode (interstroke_data$cardholt,
                                     "0"="Blank",
                                     "1"="Checked")

#Echo
interstroke_data$cardtte <- factor (interstroke_data$cardtte)
interstroke_data$cardtte <- recode (interstroke_data$cardtte,
                                    "0"="Blank",
                                    "1"="Checked")

#Echo
interstroke_data$cardnone <- factor (interstroke_data$cardnone)
interstroke_data$cardnone <- recode (interstroke_data$cardnone,
                                     "0"="Blank",
                                     "1"="Checked")

#Baseline AF
interstroke_data$afbefore <- factor (interstroke_data$afbefore)
interstroke_data$afbefore <- recode (interstroke_data$afbefore,
                                     "0" = "No",
                                     "1"="No",
                                     "2"="Yes")

#CHAGAS
interstroke_data$cschag <- factor (interstroke_data$cschag)
interstroke_data$cschag <- recode (interstroke_data$cschag,
                                   # "0" = "No",
                                   "1"="No",
                                   "2"="Yes")

#Symptom of Weakness/Paralysis
interstroke_data$sympweak <- factor (interstroke_data$sympweak)
interstroke_data$sympweak <- recode (interstroke_data$sympweak,
                                     "0" = "No",
                                     "1"="No",
                                     "2"="Yes")

#Facial Weakness
interstroke_data$faceweak <- factor (interstroke_data$faceweak)
interstroke_data$faceweak <- recode (interstroke_data$faceweak,
                                     "0" = "No",
                                     "1"="Yes")

#Arm Weakness
interstroke_data$armweak <- factor (interstroke_data$armweak)
interstroke_data$armweak <- recode (interstroke_data$armweak,
                                    "0" = "No",
                                    "1"="Yes")

#Leg Weakness
interstroke_data$legweak <- factor (interstroke_data$legweak)
interstroke_data$legweak <- recode (interstroke_data$legweak,
                                    "0" = "No",
                                    "1"="Yes")

#Symptom of numbness
interstroke_data$sympnumb <- factor (interstroke_data$sympnumb)
interstroke_data$sympnumb <- recode (interstroke_data$sympnumb,
                                     "0" = "No",
                                     "1"="No",
                                     "2"="Yes")

#Facial numbness
interstroke_data$facenumb <- factor (interstroke_data$facenumb)
interstroke_data$facenumb <- recode (interstroke_data$facenumb,
                                     "0" = "No",
                                     "1"="Yes")

#Arm numbness
interstroke_data$armnumb <- factor (interstroke_data$armnumb)
interstroke_data$armnumb <- recode (interstroke_data$armnumb,
                                    "0" = "No",
                                    "1"="Yes")

#Leg numbness
interstroke_data$legnumb <- factor (interstroke_data$legnumb)
interstroke_data$legnumb <- recode (interstroke_data$legnumb,
                                    "0" = "No",
                                    "1"="Yes")

#Aphasia
interstroke_data$aphasia <- factor (interstroke_data$sympapha)
interstroke_data$aphasia <- recode (interstroke_data$aphasia,
                                    "1" = "No",
                                    "2"="Yes")

#Unsteady Gait
interstroke_data$sympgait <- factor (interstroke_data$sympgait)
interstroke_data$sympgait <- recode (interstroke_data$sympgait,
                                     "1" = "No",
                                     "2"="Yes")

#Vertigo
interstroke_data$sympvert <- factor (interstroke_data$sympvert)
interstroke_data$sympvert <- recode (interstroke_data$sympvert,
                                     "1" = "No",
                                     "2"="Yes")

#Homonymous Hemianopia
interstroke_data$symphh <- factor (interstroke_data$symphh)
interstroke_data$symphh <- recode (interstroke_data$symphh,
                                   "0" = "No",
                                   "1" = "No",
                                   "2"="Yes")

##Any Alcoholic Drinks Day Before Stroke
interstroke_data <- interstroke_data %>%
  mutate(acute_alcohol = ifelse (csalcoh == 0, "No", "Yes"))

#Antithrombotics Pre-Hospital
interstroke_data$antithrompre <- factor (interstroke_data$antithrompre)
interstroke_data$antithrompre <- recode (interstroke_data$antithrompre,
                                         "0" = "No",
                                         "1"="Yes")

#Angry or Emotional
interstroke_data$ang24hr <- factor (interstroke_data$ang24hr)
interstroke_data$ang24hr <- recode (interstroke_data$ang24hr,
                                    "1" = "No",
                                    "2"="Yes")

#AF ECG
interstroke_data$afecg <- factor(interstroke_data$afecg)
interstroke_data$afecg <- recode(interstroke_data$afecg,
                                 "0" = "Not Present",
                                 "1" = "Present")
#Recent STEMI
interstroke_data$stemiecg <- factor(interstroke_data$stemiecg)
interstroke_data$stemiecg <- recode(interstroke_data$stemiecg,
                                    "0" = "Not Present",
                                    "1" = "Present")

##LVH ECG
interstroke_data$lvhecg <- factor(interstroke_data$lvhecg)
interstroke_data$lvhecg <- recode(interstroke_data$lvhecg,
                                  "0" = "Not Present",
                                  "1" = "Present")

#PMitrale ECG
interstroke_data$pmecg <- factor(interstroke_data$pmecg)
interstroke_data$pmecg <- recode(interstroke_data$pmecg,
                                 "0" = "Not Present",
                                 "1" = "Present")

#PPulmonale ECG
interstroke_data$ppecg <- factor(interstroke_data$ppecg)
interstroke_data$ppecg <- recode(interstroke_data$ppecg,
                                 "0" = "Not Present",
                                 "1" = "Present")

##RVH ECG
interstroke_data$rvhecg <- factor(interstroke_data$rvhecg)
interstroke_data$rvhecg <- recode(interstroke_data$rvhecg,
                                  "0" = "Not Present",
                                  "1" = "Present")

##GNI
interstroke_data <- interstroke_data %>%
  mutate(gni=ifelse(country== 6 |country == 31 |country == 8 |country == 26 |country == 1 |country == 7 |country == 34 |country == 30 |country == 32 |country == 36,"HIC",
                    ifelse(country == 15 |country == 13 |country == 21 |country == 33 |country == 17 |country == 14 |country == 22 |country == 27,"UMIC-1",
                           ifelse(country == 16 |country == 3 |country == 9 |country == 28 |country == 29 |country == 25 |country == 5,"UMIC-2",
                                  ifelse(country == 19 |country == 24 |country == 12 |country == 4 |country == 20 |country == 11 |country == 10,"LMIC","invalid")))))
interstroke_data$gni <- factor(interstroke_data$gni, levels = c("HIC", "UMIC-1", "UMIC-2", "LMIC"))

interstroke_data$sbpcadmcat8 <- factor(interstroke_data$sbpcadmcat8)
interstroke_data$sbpcadmcat8 <- recode(interstroke_data$sbpcadmcat8,
                                       "1"="SBP <120",
                                       "2"="SBP 120-130",
                                       "3"="SBP 131-140",
                                       "4"="SBP 141-150",
                                       "5"="SBP 151-160",
                                       "6"="SBP 161-170",
                                       "7"="SBP 171-180",
                                       "8"="SBP >180")

#employed
interstroke_data$employed <- factor(interstroke_data$employed)
interstroke_data$employed <- recode(interstroke_data$employed,
                                    "0"="Employed",
                                    "1"="Not Employed")

#employed4catagories
interstroke_data$employ4 <- factor(interstroke_data$employ4)
interstroke_data$employ4 <- recode(interstroke_data$employ4,
                                   "0"="Employed",
                                   "1"="Unemployed",
                                   "2"="Retired",
                                   "3"="Disability")


#Calculate urine sodium to creatatine ratio
interstroke_data$una_cr_ratio <- interstroke_data$una/interstroke_data$ucr

#Divide into 4 quantiles
interstroke_data$una_cr_ratio_q <-  ntile(interstroke_data$una_cr_ratio, 4)

source("data-raw/urine_na_formulas.R")
#Calculate 24hr Sodium Excretion (Tanaka)
interstroke_data$na_24h_tanaka = na_24h_tanaka(interstroke_data$una,interstroke_data$ucr,interstroke_data$sex,interstroke_data$age,interstroke_data$weight,interstroke_data$height)

interstroke_data$na_24h_tanaka_g = interstroke_data$na_24h_tanaka/1000
#Divide into 4 quantiles
interstroke_data$na_24h_tanaka_q = ntile(interstroke_data$na_24h_tanaka, 4)

interstroke_data$na_24h_tanaka_q <- factor (interstroke_data$na_24h_tanaka_q, levels=c(2,1,3,4))

interstroke_data$na_24h_tanaka_q <- recode (interstroke_data$na_24h_tanaka_q,
                                            "1"="<2.8 g/day",
                                            "2"="2.8–3.5 g/day",
                                            "3"="3.5–4.26 g/day",
                                            "4"=">4.26 g/day")

#Calculate 24hr Potassium Excretion (Tanaka)
interstroke_data$k_24h_tanaka = k_24h_tanaka(interstroke_data$uk,interstroke_data$ucr,interstroke_data$sex,interstroke_data$age,interstroke_data$weight,interstroke_data$height)

interstroke_data$k_24h_tanaka_g = interstroke_data$k_24h_tanaka/1000
#Divide into 4 quantiles
interstroke_data$k_24h_tanaka_q = ntile(interstroke_data$k_24h_tanaka, 4)

interstroke_data$k_24h_tanaka_q <- factor (interstroke_data$k_24h_tanaka_q, levels=c(1,2,3,4))

interstroke_data$k_24h_tanaka_q <- recode (interstroke_data$k_24h_tanaka_q,
                                           "1"="<1.34 g/day",
                                           "2"="1.34–1.58 g/day",
                                           "3"="1.58–1.86 g/day",
                                           "4"=">1.86 g/day")
#Days From Symptom Onset to Interview
interstroke_data <- interstroke_data %>%
  mutate(days_to_interview = difftime(datedint, symdate, units = "days"))

##Vegetarian
interstroke_data$vegetari <- factor(interstroke_data$vegetari)
interstroke_data$vegetari <- recode(interstroke_data$vegetari,
                                    "1" = "No",
                                    "2" = "Yes")

##Vegetarian/Milk Consumption
interstroke_data$vegmilk <- factor(interstroke_data$vegmilk)
interstroke_data$vegmilk <- recode(interstroke_data$vegmilk,
                                   "1" = "No",
                                   "2" = "Yes")

##Vegetarian/Egg Consumption
interstroke_data$vegeggs <- factor(interstroke_data$vegeggs)
interstroke_data$vegeggs <- recode(interstroke_data$vegeggs,
                                   "1" = "No",
                                   "2" = "Yes")

##Vegetarian/Fish Consumption
interstroke_data$vegfish <- factor(interstroke_data$vegfish)
interstroke_data$vegfish <- recode(interstroke_data$vegfish,
                                   "1" = "No",
                                   "2" = "Yes")

##Vegetarian/Chick Product Consumption
interstroke_data$vegchick <- factor(interstroke_data$vegchick)
interstroke_data$vegchick <- recode(interstroke_data$vegchick,
                                    "1" = "No",
                                    "2" = "Yes")

##Whole Grains/Bread/Cereals
interstroke_data$wgbread <- factor(interstroke_data$wgbread)
interstroke_data$wgbread <- recode(interstroke_data$wgbread,
                                   "1"="<1 per month - never",
                                   "2"="Monthly",
                                   "3"="Weekly",
                                   "4"="Daily")

##Refined Breads/Rice/Pasta
interstroke_data$rfbread <- factor(interstroke_data$rfbread)
interstroke_data$rfbread <- recode(interstroke_data$rfbread,
                                   "1"="<1 per month - never",
                                   "2"="Monthly",
                                   "3"="Weekly",
                                   "4"="Daily")

##Dairy Products
interstroke_data$dairy <- factor(interstroke_data$dairy)
interstroke_data$dairy <- recode(interstroke_data$dairy,
                                 "1"="<1 per month - never",
                                 "2"="Monthly",
                                 "3"="Weekly",
                                 "4"="Daily")

##Meat
interstroke_data$meat <- factor(interstroke_data$meat)
interstroke_data$meat <- recode(interstroke_data$meat,
                                "1"="<1 per month - never",
                                "2"="Monthly",
                                "3"="Weekly",
                                "4"="Daily")

##Organ Meat
interstroke_data$orgameat <- factor(interstroke_data$orgameat)
interstroke_data$orgameat <- recode(interstroke_data$orgameat,
                                    "1"="<1 per month - never",
                                    "2"="Monthly",
                                    "3"="Weekly",
                                    "4"="Daily")

##Poultry
interstroke_data$poultry <- factor(interstroke_data$poultry)
interstroke_data$poultry <- recode(interstroke_data$poultry,
                                   "1"="<1 per month - never",
                                   "2"="Monthly",
                                   "3"="Weekly",
                                   "4"="Daily")

##Eggs
interstroke_data$eggs <- factor(interstroke_data$eggs)
interstroke_data$eggs <- recode(interstroke_data$eggs,
                                "1"="<1 per month - never",
                                "2"="Monthly",
                                "3"="Weekly",
                                "4"="Daily")

##Fish and Seafood
interstroke_data$seafood <- factor(interstroke_data$seafood)
interstroke_data$seafood <- recode(interstroke_data$seafood,
                                   "1"="<1 per month - never",
                                   "2"="Monthly",
                                   "3"="Weekly",
                                   "4"="Daily")

##Pizza
interstroke_data$pizza <- factor(interstroke_data$pizza)
interstroke_data$pizza <- recode(interstroke_data$pizza,
                                 "1"="<1 per month - never",
                                 "2"="Monthly",
                                 "3"="Weekly",
                                 "4"="Daily")

##Green Leafy Vegetables
interstroke_data$greenveg <- factor(interstroke_data$greenveg)
interstroke_data$greenveg <- recode(interstroke_data$greenveg,
                                    "1"="<1 per month - never",
                                    "2"="Monthly",
                                    "3"="Weekly",
                                    "4"="Daily")

##Raw Veg
interstroke_data$rawveg <- factor(interstroke_data$rawveg)
interstroke_data$rawveg <- recode(interstroke_data$rawveg,
                                  "1"="<1 per month - never",
                                  "2"="Monthly",
                                  "3"="Weekly",
                                  "4"="Daily")

##Cooked Veg
interstroke_data$cookveg <- factor(interstroke_data$cookveg)
interstroke_data$cookveg <- recode(interstroke_data$cookveg,
                                   "1"="<1 per month - never",
                                   "2"="Monthly",
                                   "3"="Weekly",
                                   "4"="Daily")

##Legumes/Nuts/Seeds
interstroke_data$nuts <- factor(interstroke_data$nuts)
interstroke_data$nuts <- recode(interstroke_data$nuts,
                                "1"="<1 per month - never",
                                "2"="Monthly",
                                "3"="Weekly",
                                "4"="Daily")

##Potatoes
interstroke_data$bpotat <- factor(interstroke_data$bpotat)
interstroke_data$bpotat <- recode(interstroke_data$bpotat,
                                  "1"="<1 per month - never",
                                  "2"="Monthly",
                                  "3"="Weekly",
                                  "4"="Daily")

##Pickled Food
interstroke_data$pickled <- factor(interstroke_data$pickled)
interstroke_data$pickled <- recode(interstroke_data$pickled,
                                   "1"="<1 per month - never",
                                   "2"="Monthly",
                                   "3"="Weekly",
                                   "4"="Daily")

##Deep Fried Food
interstroke_data$fryfood <- factor(interstroke_data$fryfood)
interstroke_data$fryfood <- recode(interstroke_data$fryfood,
                                   "1"="<1 per month - never",
                                   "2"="Monthly",
                                   "3"="Weekly",
                                   "4"="Daily")

##Salty Snacks
interstroke_data$salty <- factor(interstroke_data$salty)
interstroke_data$salty <- recode(interstroke_data$salty,
                                 "1"="<1 per month - never",
                                 "2"="Monthly",
                                 "3"="Weekly",
                                 "4"="Daily")

##Fruits
interstroke_data$fruits <- factor(interstroke_data$fruits)
interstroke_data$fruits <- recode(interstroke_data$fruits,
                                  "1"="<1 per month - never",
                                  "2"="Monthly",
                                  "3"="Weekly",
                                  "4"="Daily")

##Ice-cream/Pudding
interstroke_data$icecream <- factor(interstroke_data$icecream)
interstroke_data$icecream <- recode(interstroke_data$icecream,
                                    "1"="<1 per month - never",
                                    "2"="Monthly",
                                    "3"="Weekly",
                                    "4"="Daily")

##Sweets - dessert/confectionary
interstroke_data$sweets <- factor(interstroke_data$sweets)
interstroke_data$sweets <- recode(interstroke_data$sweets,
                                  "1"="<1 per month - never",
                                  "2"="Monthly",
                                  "3"="Weekly",
                                  "4"="Daily")

#Confectionary,sugar,syrup
interstroke_data$sugar <- factor(interstroke_data$sugar)
interstroke_data$sugar <- recode(interstroke_data$sugar,
                                 "1"="<1 per month - never",
                                 "2"="Monthly",
                                 "3"="Weekly",
                                 "4"="Daily")

#Fruit Juice & Drinks
interstroke_data$fruitdrk <- factor(interstroke_data$fruitdrk)
interstroke_data$fruitdrk <- recode(interstroke_data$fruitdrk,
                                    "1"="<1 per month - never",
                                    "2"="Monthly",
                                    "3"="Weekly",
                                    "4"="Daily")

#Carbonated Beverages
interstroke_data$carbddrk <- factor(interstroke_data$carbddrk)
interstroke_data$carbddrk <- recode(interstroke_data$carbddrk,
                                    "1"="<1 per month - never",
                                    "2"="Monthly",
                                    "3"="Weekly",
                                    "4"="Daily")

#oliveoil = "Olive Oil in Cooking"
interstroke_data$oliveoil <- factor(interstroke_data$oliveoil)
interstroke_data$oliveoil <- recode(interstroke_data$oliveoil,
                                    "0"="No",
                                    "1"="Yes")

#palmoil = "Palm/Coconut Oil in Cooking"
interstroke_data$palmoil <- factor(interstroke_data$palmoil)
interstroke_data$palmoil <- recode(interstroke_data$palmoil,
                                   "0"="No",
                                   "1"="Yes")

#canolaoil = "Canola Oil in Cooking"
interstroke_data$canolaoil <- factor(interstroke_data$canolaoil)
interstroke_data$canolaoil <- recode(interstroke_data$canolaoil,
                                     "0"="No",
                                     "1"="Yes")

#cornoil = "Corn Oil in Cooking"
interstroke_data$cornoil <- factor(interstroke_data$cornoil)
interstroke_data$cornoil <- recode(interstroke_data$cornoil,
                                   "0"="No",
                                   "1"="Yes")

#gheeoil = "Ghee/Butter in Cooking"
interstroke_data$gheeoil <- factor(interstroke_data$gheeoil)
interstroke_data$gheeoil <- recode(interstroke_data$gheeoil,
                                   "0"="No",
                                   "1"="Yes")
#lardoil = "Lard in Cooking"
interstroke_data$lardoil <- factor(interstroke_data$lardoil)
interstroke_data$lardoil <- recode(interstroke_data$lardoil,
                                   "0"="No",
                                   "1"="Yes")

#margall = "Margarine in Cooking"
interstroke_data$margall <- factor(interstroke_data$margall)
interstroke_data$margall <- recode(interstroke_data$margall,
                                   "0"="No",
                                   "1"="Yes")

#Set the labels for the table
var_label(interstroke_data) <- list(days_to_interview = "Days to Interview",
                                    sympweak = "Paralysis or Weakness",
                                    faceweak = "Facial Weakness",
                                    armweak = "Arm Weakness",
                                    legweak = "Leg Weakness",
                                    sympnumb = "Sensory Loss or Numbness",
                                    facenumb = "Facial Numbness",
                                    armnumb = "Arm Numbness",
                                    legnumb = "Leg Numbness",
                                    aphasia = "Aphasia",
                                    sympgait = "Unsteady Gait",
                                    sympvert = "Vertigo",
                                    symphh = "Homonymous Hemianopia",
                                    csalcoh = "Alcoholic Drinks 24 hrs before Stroke",
                                    acute_alcohol = "Acute Alcohol Intake",
                                    ang24hr = "Recent Emotional Upset",
                                    antithrompre = "Pre-Hospital Antithrombotic Rx",
                                    eage = 'Age, yrs',
                                    esex = "Sex",
                                    subeduc = "Education",
                                    moteduc = "Mother education",
                                    fatduc = "Father education",
                                    regionnn7 = "Region",
                                    ethnicity = "Ethnicity",
                                    hhincome = "Household income",
                                    employed = "Employed",
                                    employ4 = "Employed Categories",
                                    incomectry = "Income Region",
                                    marital = "Marital Status",
                                    occupat = "Occupation",
                                    smoking = "Smoking",
                                    alcohfreqcat = "Alcohol History",
                                    afibflut = "History of Atrial Fibrillation/Flutter",
                                    statipre = "Statin Pre Admission",
                                    antihypertpre = "Antihypertensives Pre Admission",
                                    polluteh = "Pollution at Home",
                                    pollutew = "Pollution at Work",
                                    injuneck = "Injury Neck",
                                    injuhead = "Injury Head",
                                    painteet = "Painful Teeth",
                                    cstooth = "Tooth Infection in Last 4 Weeks",
                                    paingum = "Painful Gums",
                                    lostteet = "Lost Teeth",
                                    sysbp = "Systolic Blood Pressure (mmHg)",
                                    aheiscore = "Total AHEI Score",
                                    aheitert = "AHEI Tertile",
                                    bmi = "Body Mass Index (kg/m2)",
                                    nooral = "No painful/lost teeth or gums",
                                    subdm = "Diabetes",
                                    cacbpre= "Calcium Channel Blocker Use Pre-admission",
                                    subhtn = "Hypertension History",
                                    subrfev = "Rheumatic Fever History",
                                    subrhd = "Rheumatic Heart Disease",
                                    afi4wks = "Acute Febrile Illness Within 4 Weeks",
                                    meckmlwk = "Neck Manipulation within the last week",
                                    wrkactv = "Physical Activity at Work",
                                    dissection = "Dissection",
                                    migraine = "Migraine",
                                    ischocsp = "OCSP classification",
                                    fischocsp = "Final OCSP Classification",
                                    cssleeph = "Subjective Sleep Duration in Hours",
                                    cssleeph_factor = "Subjective Sleep Duration in Hours",
                                    cssleeph24 = "Subjective Sleep Duration/24 Hours",
                                    cssleeph24_factor = "Subjective Sleep Duration/24 Hours",
                                    csdiffsl = "Difficulty Falling Asleep",
                                    cswake = "Frequency of Nocturnal Awakening",
                                    cswake_factor = "Frequency of Nocturnal Awakening",
                                    csrate = "Self-Rated Overall Sleep Quality",
                                    cssleepd = "Napping During the Day",
                                    diagosa = "Diagnosis of Obstructive Sleep Apnoea",
                                    antidpre = "Taking an Antidepressant Pre-admission",
                                    antidhos = "Taking an Antidepressant In Hospital",
                                    antippre = "Taking an Antipsychotic Pre-admission",
                                    antiphos = "Taking an Antipsychotic In Hospital",
                                    alcb1 = "Alcohol Use",
                                    alcb1 = "Alcohol Use",
                                    alcq = "Alcohol Use",
                                    alcq3 = "Alcohol Use",
                                    alcqn = "Alcohol Use",
                                    alcbingetyp = "Alcohol - Binge Types",
                                    alcohhis = "Alcohol History",
                                    alcohage = "Age Started Drinking Alcohol",
                                    alcohol = ">5 Drinks per day",
                                    beerfwk = "Number of Beers per Week",
                                    spiritfwk = "Number of Spirits per Week",
                                    arrackfwk = "Number of Arracks per Week",
                                    oalcfwk = "Number of Other Alcoholic Drinks per Week",
                                    alcallday = "Total Number of Drinks per Day",
                                    alcallwk = "Total Number of Drinks per Week",
                                    alcallmth = "Total Number of Drinks per Month",
                                    freqalc = "Number of Times per Month",
                                    numalcoh = "Average Number of Drinks",
                                    spirita = "Spirits Average Number of Drinks",
                                    spiritd = "Duration of Spirits",
                                    spirqtag = "Year Stopped Drinking Spirits",
                                    spiritf = "Spirits Frequency",
                                    winefreq = "Wine Frequency",
                                    wineamt = "Wine Average Number of Drinks",
                                    winedur = "Duration of Wine",
                                    wineqtag = "Year Stopped Drinking Wine",
                                    ethnic = "Ethnicity (18 levels)",
                                    mrscoreb = "Modified Rankin Before Stroke",
                                    mrscorec = "Modified Rankin Current",
                                    mrscofef = "Modified Rankin at one month follow-up",
                                    mrscorefcat = "Modified Rankin Category at one month follow-up",
                                    fatlstrk = "Fatal Stroke",
                                    beerfreq = "Beer Frequency",
                                    beeramt = "Beer Average Number of Drinks",
                                    beerdur = "Duration of Beer",
                                    beerqtag = "Year Stopped Drinking Beer",
                                    arrackf = "Arrack Frequency",
                                    arracka = "Arrack Average Number of Drinks",
                                    arrackd = "Duration of Arrack",
                                    arraqtag = "Year Stopped Drinking Arrack",
                                    oalcfreq = "Other Alcohol Frequency",
                                    oalcamt = "Beer Average Number of Drinks",
                                    oalcdur = "Duration of Beer",
                                    oalcqtag = "Year Stopped Drinking Beer",
                                    tobahis = "History of Tobacco Use",
                                    scurformnev = "Smoking History",
                                    nevfcur = "Smoking History",
                                    phys = "Leisure Physical Activity",
                                    whrs2tert = "Tertile of Waist-Hip-Ratio",
                                    ahei4tert = "Tertile of AHEI",
                                    htnmbp2 = "Self Reported Hypertension or High Morning BP>140/90",
                                    dmhba1c2 = "History of Diabetes or HbA1c>=6.5%",
                                    cardiacrfcat = "History of Cardiac Risk Factors",
                                    apob_apoatert = "Tertile of ApoB:ApoA",
                                    subami = "Baseline History of MI",
                                    subangin = "Baseline History of Angina",
                                    subtia = "Baseline History of TIA",
                                    substrk = "Baseline History of Stroke",
                                    subcb = "Baseline History of Chronic Bronchitis",
                                    subhvr = "Baseline History of Valve Replacement",
                                    antihypert3 = "Number of antihypertensives pre admission",
                                    subpad = "Baseline History of PAD",
                                    subhchol = "Baseline History of High Cholesterol",
                                    subvthr = "Baseline History of Venous Thromboembolism",
                                    subaf = "Baseline History of Atrial Fibrillation or Flutter",
                                    newly_diagnosed_af = "Newly Diagnosed AF",
                                    cshf = "Previous history of Heart Failure",
                                    antiplate = "Antiplatelet Use Pre-Admission",
                                    orantpre = "Oral Anticoagulant Use Pre-Admission",
                                    fibrapre = "Fibrate Use Pre-Admission",
                                    acepre = "ACE Inhibitor Use Pre-Admission",
                                    arbpre = "ARB Use Pre-Admission",
                                    betabpre = "Beta Blocker Use Pre-Admission",
                                    alphbpre = "Alpha Blocker Use Pre-Admission",
                                    diurepre = "Diuretic Use Pre-Admission",
                                    education = "Education",
                                    iscstroke = "Ischemic Stroke",
                                    ichemstroke = "ICH Stroke",
                                    strataid = "Match ID",
                                    htnadmbp = "History of Hypertension or Adjusted BP>140/90 at admission",
                                    fstress = "Financial Stress",
                                    locquarthi = "Locus of Control",
                                    depr = "Depression",
                                    global_stress2 = "Global Stress",
                                    constrktp = "Initial Brain Image - Stroke Type",
                                    mrscoren = "Modified-Rankin -1 Month Follow-up",
                                    owncar= "Owns Car/Automobile",
                                    ownbike= "Own Bicycle",
                                    subhiv= "HIV at baseline",
                                    tobahis= "History of tobacco use",
                                    smkexpos= "Exposed to other peoples smoke",
                                    othstrs= "Other stress",
                                    vegetari= "Vegetarian",
                                    rawvegf= "Vegetables (raw) number times",
                                    meat= "In the last 12 months how often do you eat meat",
                                    fruits= "In the last 12 months how often do you eat fruits",
                                    bcpills= "Birth control",
                                    fertused= "Used fertility drugs",
                                    hormrepl= "Female hormone replacements",
                                    rppbtumour= "Primary brain tumour",
                                    rpsbtumour= "Secondary brain tumour",
                                    m1seiz= "Seizure",
                                    sympapha="Aphasia at time of stroke",
                                    subbbled= "History of previous ICH",
                                    m1depr= "Depression",
                                    cslsnor= "Told that they have loud snoring during sleep",
                                    cssnort= "Told that they have snorting or gasping during sleep",
                                    csbreat= "Told that their breathing stops, they choke or struggle for breath during sleep",
                                    symphh ="Homonymous Hemianopsia",
                                    leisactv="Level Of Physical Activity During Leisure Time",
                                    subvthr="Venous Thromboembolism",
                                    teeth="Combined Painful teeth, Lost teeth or Painful gum",
                                    paw="Work-Physical Activity",
                                    alcohfreqwk="Alcohol History and Frequency",
                                    depr="Depression",
                                    ahei3tert="Tertile of AHEI Score",
                                    ischocsp="OCSP",
                                    liveplace="Discharged place",
                                    vascimag="Vascular Imagine",
                                    ctavasc="CTA performed",
                                    mravasc="MRA performed",
                                    chol="Cholesterol",
                                    hdlc="HDL Cholesterol",
                                    ldlc="LDL Cholesterol",
                                    nonhdl="Non HDL Cholesterol",
                                    apoa1="APO A1",
                                    apob="APO B",
                                    apob_apoa="APO A/APO B",
                                    physwork="Physical Activity-Work",
                                    gstress="Global Stress",
                                    fstress="Financial Stress",
                                    depdose="Depression Group",
                                    depscore="Depression Score",
                                    depr="Depression",
                                    depdosedef2="Depression2",
                                    cscanct= "Cancer-Receiving Treatment",
                                    cscanctc= "Cancer-Receiving Chemotherapy",
                                    cscancl= "Cancer Location-Primary",
                                    cscanca = "Cancer - Age at diagnosis",
                                    cscancts = "Cancer-Receiving Surgery",
                                    cscanctr = "Cancer-Receiving Radiotherapy",
                                    facthmkm = "Distance from home to factory (km)",
                                    factwpkm = "Distance from work to factory (km)",
                                    trafhmkm = "Distance from home to traffic (km)",
                                    trafwpkm = "Distance from work to traffic (km)",
                                    facthmm = "Distance from home to factory (m)",
                                    factwpm = "Distance from work to factory (m)",
                                    trafhmm = "Distance from home to traffic (m)",
                                    trafwpm = "Distance from work to traffic (m)",
                                    gowkhrs = "Time spent travelling to work (hours)",
                                    gowkmins = "Time spent travelling to work (mins)",
                                    nolgwork = "No longer working",
                                    gowkcar = "Car",
                                    gowkbus = "Bus",
                                    gowkmoto = "Motorcycle/Scooter",
                                    gowkbike = "Bicycle",
                                    gowktran = "Train",
                                    gowkwalk = "Walk",
                                    gowkoth = "Other",
                                    transport_wk = "Transport to work",
                                    cookphouse = "Cooking Place",
                                    kerofuel = "Kerosene",
                                    charfuel = "Charcoal",
                                    coalfuel = "Coal",
                                    woodfuel = "Wood",
                                    gobarfuel = "Gobar gas",
                                    cropfuel = "Agriculture/crop",
                                    elecfuel = "Electricity",
                                    dungfuel = "Animal dung",
                                    gasfuel = "Gas",
                                    shrubfuel = "Shrub/grass",
                                    othfuel = "Other fuel",
                                    cookplace = "Where is the cooking performed",
                                    cooking_fuel = "Cooking Fuel",
                                    cooking_fuel_3_level = "Cooking Fuel Type",
                                    cscancm1= "Cancer Location-Metastatic No.1",
                                    cssdplan= "Napping, planned or unplanned",
                                    csslepth= "Duration of nap in hours",
                                    thromall= "Received Thrombolysis",
                                    workstrs="Stress at work",
                                    homestrs="Stress at home",
                                    global_stress="General Stress",
                                    finastrs="Financial Stress",
                                    strclass="Stressful Life Events",
                                    locquart="Locus of Control",
                                    quartloc="Locus of Control",
                                    sad2wks="Feeling Depressed",
                                    depdosedef3="Depression Score",
                                    levconsc="Level of Consciousness at time of stroke",
                                    workrv= "Employed",
                                    fstrktype="Stroke Types",
                                    whr= "Waist-to-Hip Ratio",
                                    waist = "Waist (cm)",
                                    height = "Height (cm)",
                                    hip = "Hip (cm)",
                                    weight = "Weight (kg)",
                                    spoucig="Cigarettes (Number per day)",
                                    sysadmc="Adjusted SBP at time of admission",
                                    sysadm="Time of admission SBP",
                                    diasadm="Time of admission DBP",
                                    sysmorn="Morning after admission SBP",
                                    diasmorn="Morning after admission DBP",
                                    sysint="Time of interview SBP",
                                    diasint="Time of interview DBP",
                                    medication_number_pread= "Number of medications preadmission",
                                    comorbidity_number_pread = "Number of comorbidities preadmission",
                                    comorbidity_number_pread2 = "Number of comorbidities preadmission",
                                    mrs_case_control_frailty_proxy = "Modified Rankin Scale prior to stroke/at baseline for controls",
                                    aspirpre="Aspirin Pre-Hospital Admission",
                                    clopdpre="Clopidogrel Pre-Hospital Admission",
                                    orantpre="Oral anticoagulants Pre-Hospital Admission",
                                    sepadivo="Marital separation/divorce",
                                    lossjob="Loss of job/retirement",
                                    busfail="Loss of crop/business failure",
                                    violence="Violence",
                                    famcnfl="Major intra-family conflict",
                                    injury="Major personal injury or illness",
                                    deathfam="Death/major illness of a family member",
                                    deathspo="Death of a spouse",
                                    deathdt="Date of Death",
                                    stopwork="No longer working",
                                    autonomy="Autonomy organising events of your work day",
                                    wrkcntrl="Control over what happens at work",
                                    lifcntrl="Control over what happens in life",
                                    futurexp="More positive than negative about the future",
                                    treated="Treated unfairly",
                                    changes="Life full of changes",
                                    gaveup="Gave up trying to make improvements",
                                    interest="Lose interest",
                                    tired="Feel tired",
                                    glweight="Gain or lose weight",
                                    tosleep="Trouble sleeping",
                                    focus="Trouble concentrating",
                                    thkdeath="Think a lot about death",
                                    feeldown="Feel down",
                                    othstrs="Other stress",
                                    regionnn7_cat="3 Level Income Regions",
                                    cscancl_cat="Cancer-Systems Based",
                                    years_since_diagnosis_cancer="Years Since Diagnosis of Cancer",
                                    years_since_diagnosis_cancer_cat="Years Since Diagnosis of Cancer-levels",
                                    ctrltype="Source of control",
                                    thromhos="Thrombolysis in Hospital",
                                    cscancm1_cat= "Cancer, site of metastastes No. 1- Systems bases",
                                    dcct65= "HbA1C>= 6.5%",
                                    hba1c= "HbA1c (fraction)",
                                    exrhrcs="Physical exertion in the hour prior",
                                    exrdaycs="Physical exertion during the same hour in the previous day",
                                    wakesym="Wake up because of symptoms?",
                                    symwoke="Symptoms when woke up?",
                                    highwhr="Waist-to-Hip Ratio Categories",
                                    bmi5grp="Body Mass Index Categories",
                                    bmi_3_level="Body Mass Index Groups",
                                    bmi_quintiles="Body Mass Index Quintiles",
                                    ntconscan = "Evidence of previous stroke on CT which is not consistent with current presentation",
                                    workhrs = "Hours/day do physical activity at work",
                                    workhrs_week = "Hours/week do physical activity at work",
                                    mets_workalone = "METS per day (Work)",
                                    workmins_A = "Mins per week, Work",
                                    work = "Currently Employed",
                                    marijhis = "Marijuana Use History",
                                    cocaihis = "Cocaine Use History",
                                    opiathis = "Opiate Use History",
                                    smokexpose = "Smoke Exposure",
                                    ls6menst= "6 or less Menstrual Cycles per year",
                                    failpreg = "Failed Pregnancy",
                                    pregnant= "Ever Pregnant",
                                    preghtn= "Hypertension During Pregnancy",
                                    pregpuri= "Proteinuria During Pregnancy",
                                    preglegs= "Leg Swelling During Pregnancy",
                                    pregseiz= "Seizure During Pregnancy",
                                    pregdm= "Diabetes Suring Pregancy",
                                    miscarrig = "Miscarriage During Pregnancy",
                                    menstop = "Menopause",
                                    antithromb = "Use of Antithrombotics",
                                    imagetp = "Imaging Method at Presentation",
                                    ctrltype = "Source of Control",
                                    symhour = "Hour of the day that stroke symptoms began",
                                    windextert = "Wealth Index Tertile",
                                    oranthos = "Hospital Anticoagulant Use",
                                    aspirhos = "Hospital Aspirin Use",
                                    statihos = "Hospital Statin Use",
                                    betabhos = "Hospital Beta-blocker Use",
                                    acehos = "Hospital ACE Inhibitor Use",
                                    arbhos = "Hospital ARB Use",
                                    alphbhos = "Hospital Alpha Blocker Use",
                                    cacbhos = "Hospital CCB Use",
                                    diurehos = "Hospital Diuretic Use",
                                    cardiacmeds = "Cardiac RF with Meds",
                                    ischocspfinal = "Ischemic OCSP Type",
                                    oscplaci = "OSCP Lacunar",
                                    undetermined = "TOAST undetermined",
                                    cardioembolic = "TOAST cardioembolic",
                                    largevessel = "TOAST large vessel",
                                    smallvessel = "TOAST small vessel",
                                    othertoast = "Other TOAST classification",
                                    TOAST = "TOAST Classification",
                                    afecg = "ECG AF",
                                    echo = "Echo performed",
                                    lvefechol = "Echo EF",
                                    cardecg = "ECG 1/12 FU",
                                    cardholt = "Holter 1/12 FU",
                                    cardtte = "Echo at 1/12 FU",
                                    cardnone = "No Cardiac Investigations",
                                    afbefore = "AF at Baseline",
                                    age_group_decade = "Age Group Decade",
                                    agecat = "Age Group Categories",
                                    ftoast1 = "Cardioembolic Stroke",
                                    cschag = "Hx Chagas Disease",
                                    global_stress_3 = "General Stress",
                                    homestrs_3 = "Home Stress",
                                    workstrs_3 = "Work Stress",
                                    inradm = "INR Admission",
                                    hrtrtadm = "Heart rate on admission",
                                    gni = "Gross National Income Region",
                                    afecg = "AF ECG",
                                    stemiecg = "Recent STEMI ECG",
                                    lvhecg = "LVH ECG",
                                    pmecg = "P Mitrale ECG",
                                    ppecg = "P Pulmonale ECG",
                                    rvhecg = "RVH ECG",
                                    datedint = "Date of interview",
                                    arrdate = "Date patient arrived in hopsital",
                                    uk = "Urinary potassium",
                                    una = "Urinary sodium",
                                    k = "Serum potassium",
                                    na = "Serum sodium",
                                    uk_ucrea = "Urinary potassium creatinine ratio",
                                    ucr = "Urinary creatinine",
                                    una_ucrea = "Urinary sodium creatinine ratio",
                                    sysbpadmcat8 = "Admission SBP category",
                                    na_24h_tanaka = "Estimated sodium excretion (Tanaka)",
                                    na_24h_tanaka_g = "Estimated sodium excretion (Tanaka) g/day",
                                    na_24h_tanaka_q = "Estimated sodium excretion (Tanaka)",
                                    una_cr_ratio = "Urinary Sodium to Creatitine Ratio - Quintiles",
                                    una_cr_ratio_q = "Urinary Sodium to Creatitine Ratio - Quintiles",
                                    k_24h_tanaka = "Estimated sodium excretion (Tanaka)",
                                    k_24h_tanaka_q = "Estimated sodium excretion (Tanaka) - Quintiles",
                                    k_24h_tanaka_g = "Estimated sodium excretion (Tanaka) g/day",
                                    symdate = "Symptoms begin date",
                                    symhour = "Symptoms begin time hour",
                                    symmins = "Symptoms begin time min",
                                    arrdate = "Patient arrive date",
                                    arrhour = "Patient arrive time hour",
                                    arrmins = "Patient arrive time mins",
                                    clopdhos = "Clopidogrel in Hospital",
                                    dipyrhos = "Dipryridamole in hospital",
                                    asdiphos = "Aspirin and dipyridamole in hospital",
                                    motbbled = "Mother bleed in the brain",
                                    fatbbled = "Father bleed in the brain",
                                    scandate = "Date of first scan",
                                    scanhr = "Time of scan hour",
                                    scanmin = "Time of scan min",
                                    ononstrk = "Non-stroke cause",
                                    csbldc = "Blood pressure checked before stroke",
                                    ceperfom = "CEA performed",
                                    m1physio = "Physio since stroke",
                                    m1occupa = "Occupational therapy since stroke",
                                    m1speech = "Speech therapy since stroke",
                                    m1antipl = "Antiplatelets since stroke",
                                    m1antico = "Anticoagulants sine stroke",
                                    m1bldpld = "Blood pressure lowering drug since stroke",
                                    m1choles = "Cholesterol lowering drug since stroke",
                                    motstrk = "Mother had stroke",
                                    fatstrk = "Father had stroke",
                                    vegetari = "Vegetarian",
                                    vegmilk = "Vegetarian/Milk Consumption",
                                    vegeggs = "Vegetarian/Egg Consumption",
                                    vegfish = "Vegetarian/Fish Consumption",
                                    vegchick = "Vegetarian/Chick Product Consumption",
                                    wgbread = "Whole Grain Bread/Cereal",
                                    rfbread = "Refined Bread/Pasta/Rice",
                                    dairy = "Dairy Products",
                                    meat = "Meat",
                                    orgameat = "Organ Meats",
                                    poultry = "Poultry",
                                    eggs = "Eggs",
                                    seafood = "Seafood",
                                    pizza = "Pizza",
                                    greenveg = "Green Leafy Vegetables",
                                    rawveg = "Raw Vegetables",
                                    cookveg = "Cooked Vegetables",
                                    nuts = "Legumes/Nuts/Seeds",
                                    bpotat = "Potatoes",
                                    pickled = "Pickled Food",
                                    fryfood = "Deep Fried Food",
                                    salty = "Salty Snacks",
                                    fruits = "Fruits",
                                    icecream = "Ice-cream/Pudding",
                                    sweets = "Sweets/Icecream/dessert/confectionary",
                                    sugar = "Confectionary/Sugar/Syrups",
                                    fruitdrk = "Fruit Juice & Drinks",
                                    carbddrk = "Carbonated Beverages",
                                    wholewk = "Whole Grains Weekly",
                                    refwk = "Refined Breads/Rice Weekly",
                                    bpotawk = "Potatoes boiled Weekly",
                                    pizzawk = "Pizza Weekly",
                                    meatwk = "Meat Weekly",
                                    poultrywk = "Poultry Weekly",
                                    orgameatwk = "Organ Meats Weekly",
                                    fishwk = "Seafood Weekly",
                                    eggswk = "Eggs Weekly",
                                    dairwk = "Dairy Products Weekly",
                                    nutswk = "Legumes/Nuts/Seeds Weekly",
                                    frywk = "Deep Fried Food Weekly",
                                    saltwk = "Salty Snacks Weekly",
                                    pickwk = "Pickled Food Weekly",
                                    sweetwk = "Desserts/Snacks Weekly",
                                    sugarwk = "Confectionary Weekly",
                                    icewk = "Icecream Weekly",
                                    carbodrkwk = "Carbonated Beverages Weekly",
                                    cookwk = "Cooked Veg Weekly",
                                    wholeday = "Whole Grain Breads Daily",
                                    refday = "Refined Breads/rice Daily",
                                    bpotaday = "Potatoes Boiled Daily",
                                    pizzaday = "Pizza Daily",
                                    meatday = "Meat Daily",
                                    poultryday = "Poultry Daily",
                                    orgameatday = "Organ Meats Daily",
                                    fishday = "Seafood Daily",
                                    eggsday = "Eggs Daily",
                                    dairday = "Dairy Products Daily",
                                    nutsday = "Legumes/Nuts/Seeds Daily",
                                    fryday = "Fried Food Daily",
                                    saltday = "Salty Snacks Daily",
                                    pickday = "Pickled Food Daily",
                                    sweetday = "Desserts/Snacks Daily",
                                    sugarday = 'Confectionary Daily',
                                    iceday = "Ice-cream Daily",
                                    carbodrkday = "Carbonated Beverages Daily",
                                    fruitdrkday = "Fruit Drinks Daily",
                                    fruitsday = "Fruits Daily",
                                    gvegday = "Green Veg Daily",
                                    rawday = "Raw Veg Daily",
                                    cookday = "Cooked Veg Daily",
                                    allvegahei = "Veg AHEI Score",
                                    fruitahei = "Fruits AHEI Score",
                                    nutsahei = "Nuts AHEI Score",
                                    white_meatahei = "White/Red Meat AHEI Score",
                                    wholeahei = "Whole Grains AHEI Score",
                                    friedahei = "Fried, Pizza AHEI Score",
                                    friedsahei = "Fried, Pizza, Salt AHEI Score",
                                    alcahei = "Alcohol AHEI Score",
                                    aheiscore = "Total AHEI Score",
                                    oliveoil = "Olive Oil in Cooking",
                                    palmoil = "Palm/Coconut Oil in Cooking",
                                    canolaoil = "Canola Oil in Cooking",
                                    cornoil = "Corn Oil in Cooking",
                                    gheeoil = "Ghee/Butter in Cooking",
                                    lardoil = "Lard in Cooking",
                                    margall = "Margarine in Cooking")

#Interstroke Education
#Select only the variables you need
interstroke_education <- interstroke_data %>%
  select(subeduc,case_num,case,caseid,id,eage,moteduc,fatduc,regionnn7,ethnicity,hhincome,incomectry,marital,occupat,esex,smoking,subdm,
         htnadmbp,dmhba1c2,whrs2tert,fstrktype,
         sysmorn,sysadm,sysint,sysbp,
         diasadm,diasmorn,diasint,diasbp,
         aheiscore,alcohfreqcat,afibflut,bmi,phys,statipre,antihypertpre,nevfcur,cardiacrfcat)

usethis::use_data(interstroke_education, overwrite = TRUE)


#Interstroke ICH
#Select only the variables you need
interstroke_ich <- interstroke_data %>%
  select(lobar,idcenter,subeduc,case_num,case,caseid,id,eage,education,regionnn7,cardiacrfcat,nevfcur,aheitert,apob_apoatert,
         ethnicity,hhincome,incomectry,marital,occupat,esex,smoking,subdm,htnadmbp,dmhba1c2,whrs2tert,subhtn,htnmbp2,alcohfreqwk,
         antiplate,orantpre,statipre,strktype,fstrktype,ldlc,hdlc,spoucig,whr,
         sysadm,diasadm,sysmorn,diasmorn,sysint,diasint,scurformnev,country,
         mrscofef,mrscorec,antihypert3,sysadmc,
         dcct65,hba1c,global_stress,global_stress2,exrhrcs,exrdaycs,wakesym,symwoke,
         sysbp,aheiscore,alcohfreqcat,afibflut,bmi,phys,antihypertpre,ich_location_primary,
         ich_location_extension,ich_location_secondary_site,ich_location_third_site,extension_or_second_or_third_site,second_or_third_site)

if (1) {
##############
#TEMPORARY Create 1:3 controls for ICH
#############
  atest <- interstroke_ich %>%
    filter(fstrktype=="ICH") %>%
    mutate(strktype_equal_fstrktype = ifelse(as.character(strktype)==as.character(fstrktype),"Yes","No"))%>%
    filter(strktype_equal_fstrktype=="Yes")

is_ich_cases <- interstroke_ich %>%
  filter(case=="Case") %>%
  mutate(strktype_equal_fstrktype = ifelse(as.character(strktype)==as.character(fstrktype),"Yes","No")) %>%
  filter(strktype_equal_fstrktype=="Yes") %>%
  filter(strktype=="ICH")

#Get the controls
is_controls <- interstroke_ich %>%
  filter(case=="Control")

`%not_in%` <- purrr::negate(`%in%`)

#Store all ICH controls
is_ich_controls <- is_controls[is_controls$caseid %in% is_ich_cases$id,]

#Store all ischemic/SAH controls
is_ischemic_controls <- is_controls[is_controls$caseid %not_in% is_ich_cases$id,]
is_ischemic_controls$caseid <- as.integer(is_ischemic_controls$caseid)

set.seed(1234)
#Create a 1:3 case:control match
for(i in 1:3) {
  for(ich_id in is_ich_cases$id) {
    ich_idcenter <- is_ich_cases[which(is_ich_cases$id==ich_id),"idcenter"]
    ich_esex <- is_ich_cases[which(is_ich_cases$id==ich_id),"esex"]
    ich_eage <- is_ich_cases[which(is_ich_cases$id==ich_id),"eage"]

    #Find a suitable control in the ischemic controls
    #Match by idcenter, sex, age +/- 5,
    is_ischemic_control_match <- is_ischemic_controls %>%
      filter(idcenter==ich_idcenter) %>%
      filter(esex==ich_esex) %>%
      filter(eage<=ich_eage+5 & eage>=ich_eage-5) %>%
      mutate(eage_difference=abs(ich_eage-eage)) %>%
      filter(is.na(caseid)) %>%
      slice_min(eage_difference,n=1) %>%
      slice_sample(n=1) %>%
      mutate(caseid=ich_id) %>%
      select(-eage_difference)

    is_ischemic_controls <- is_ischemic_controls %>%
      rows_update(is_ischemic_control_match, by = "id")

  }
}

  is_ischemic_controls <- is_ischemic_controls %>%
    filter(!is.na(caseid))

  interstroke_ich <- bind_rows(is_ich_cases,is_ich_controls,is_ischemic_controls)

  interstroke_ich_labelled <- interstroke_ich %>%
    sjlabelled::copy_labels(is_ich_cases)

  interstroke_ich <- interstroke_ich_labelled

  #######
  #Needs to be confirmed
  #######

  ##############
  #TEMPORARY Create Case:Case match
  ##############

  is_ich_cases_lobar <- interstroke_ich %>%
    filter(lobar=="Lobar") %>%
    mutate(lobarcaseid = id) %>%
    select(id,lobarcaseid,idcenter)

  is_ich_cases_nonlobar <- interstroke_ich %>%
    filter(lobar=="Non-Lobar") %>%
    mutate(lobarcaseid = as.integer(NA)) %>%
    select(id,lobarcaseid,idcenter)

  set.seed(1234)
  for(ich_id in is_ich_cases_lobar$id) {
    ich_idcenter <- is_ich_cases_lobar[which(is_ich_cases_lobar$id==ich_id),"idcenter"]

    is_nonlobar_case_match <- is_ich_cases_nonlobar %>%
      filter(idcenter==ich_idcenter) %>%
      filter(is.na(lobarcaseid)) %>%
      slice_sample(n=1) %>%
      mutate(lobarcaseid=ich_id)

    is_ich_cases_nonlobar <- is_ich_cases_nonlobar %>%
      rows_update(is_nonlobar_case_match, by = "id")

  }

  is_ich_cases_nonlobar <- is_ich_cases_nonlobar %>%
    filter(!is.na(lobarcaseid))

  is_ich_cases_lobar_matched_nonlobar <- rbind(is_ich_cases_lobar,is_ich_cases_nonlobar)

  is_ich_cases_lobar_matched_nonlobar <- is_ich_cases_lobar_matched_nonlobar %>%
    select(-idcenter)

  interstroke_ich <- interstroke_ich %>%
    full_join(is_ich_cases_lobar_matched_nonlobar, by ="id")

  interstroke_ich_labelled <- interstroke_ich %>%
    sjlabelled::copy_labels(is_ich_cases)

  interstroke_ich <- interstroke_ich_labelled

}


#######
#Needs to be confirmed
#######

usethis::use_data(interstroke_ich, overwrite = TRUE)

#Interstroke Sleep
#Select only the variables you need
interstroke_sleep <- interstroke_data %>%
  select(ewakeyest,ewaketod,ewakenoctc,nocsleep,tosleep,cardiacrfcat,fstrktype,
         case_num,case,caseid,id,eage,moteduc,fatduc,regionnn7,ethnicity,hhincome,
         incomectry,marital,occupat,esex,smoking,subdm,
         htnadmbp,dmhba1c2,whrs2tert,
         sysbp,aheiscore,alcohfreqcat,afibflut,bmi,nevfcur,statipre,antihypertpre,
         cssleeph,csdiffsl,cswake,cssleepd,csrate,cswake_factor,cssleeph_factor,cssleeph24_factor,cssleeph24,
         cslsnor,cssnort,csbreat,diagosa,phys,wrkactv,global_stress2,depr,subhtn,
         subhchol,respondt,cssdplan,csslepth,subeduc,mrscoreb,alcohfreqwk,
         ahei4tert,medication_number_pread,
         comorbidity_number_pread,mrs_case_control_frailty_proxy,comorbidity_number_pread2,ntconscan,mrscorec,mrscofef,afi4wks,imagetp,whr,ctrltype,
         wakesym,symwoke,symhour,symmins,wakmins1,wakhrs1,lobar,datedint,arrdate,thromhos,thrompre,TOAST,sysmorn, scandate, scanhr, scanmin, arrdate, arrhour, arrmins, aspirhos,
         clopdhos, dipyrhos,asdiphos, oranthos, statihos, betabhos,acehos,arbhos,diurehos, alphbhos, cacbhos,
         sympweak, sympnumb,sympapha, sympgait, sympvert, symphh)


usethis::use_data(interstroke_sleep, overwrite = TRUE)

#Interstroke Pollution
interstroke_data$facthmkm <- interstroke_data$facthmkm + interstroke_data$facthmm/1000
interstroke_data$factwpkm <- interstroke_data$factwpkm + interstroke_data$factwpm/1000
interstroke_data$trafhmkm <- interstroke_data$trafhmkm + interstroke_data$trafhmm/1000
interstroke_data$trafwpkm <- interstroke_data$trafwpkm + interstroke_data$trafwpm/1000

#Select only the variables you need
interstroke_pollution <- interstroke_data %>%
  select(polluteh,pollutew,cardiacrfcat,htnadmbp,dmhba1c2,whrs2tert,htnmbp2,scurformnev,fstrktype,
         subhtn,aheitert,cookphouse,education,country,incomectry,cooking_fuel,cooking_fuel_3_level,
         facthmkm, factwpkm, trafhmkm, trafwpkm, facthmm, factwpm, trafhmm, trafwpm, gowkhrs, gowkmins,
         kerofuel, charfuel, coalfuel, woodfuel, gobarfuel, cropfuel, elecfuel, dungfuel, gasfuel, shrubfuel, othfuel, cookplace,
         case_num,case,caseid,id,eage,moteduc,fatduc,regionnn7,ethnicity,nevfcur,hhincome,incomectry,marital,occupat,esex,smoking,subdm,
         sysbp,aheiscore,alcohfreqcat,afibflut,bmi,phys,statipre,antihypertpre,employed,employ4,gowkcar,gowkbus,gowkmoto,
         gowkbike,gowktran,gowkwalk,gowkoth,transport_wk)

usethis::use_data(interstroke_pollution, overwrite = TRUE)

#Interstroke Head_Neck
#Select only the variables you need
interstroke_head_neck <- interstroke_data %>%
  select(injuneck,injuhead,meckmlwk,cardiacrfcat,htnadmbp,dmhba1c2,whrs2tert,fstrktype,
         case_num,case,caseid,id,eage,subeduc,regionnn7,ethnicity,hhincome,incomectry,marital,occupat,esex,subdm,nevfcur,phys,
         sysbp,aheiscore,alcohfreqcat,afibflut,bmi,statipre,antihypertpre,dissection,migraine,ischocsp,subhtn,htnmbp2,vascimag,ctavasc,mravasc,scurformnev,hhincome,cardiacrfcat,education,country,incomectry)

usethis::use_data(interstroke_head_neck, overwrite = TRUE)

#Interstroke Periodontitis
#Select only the variables you need
interstroke_periodontitis <- interstroke_data %>%
  select(painteet,paingum,lostteet,nooral,whrs2tert,fstrktype,bmi_3_level,
         case_num,case,caseid,id,eage,moteduc,fatduc,regionnn7,ethnicity,htnmbp2,education,scurformnev,
         hhincome,incomectry,marital,occupat,esex,smoking,subdm,nevfcur,phys,country,
         sysbp,aheiscore,alcohfreqcat,afibflut,bmi,statipre,polluteh,pollutew,
         antihypertpre,cacbpre,subhtn,subrfev,subrhd,afi4wks,subeduc,
         aheitert,cstooth,cardiacrfcat,htnadmbp,dmhba1c2,teeth,ctrltype,gni,windextert)

usethis::use_data(interstroke_periodontitis, overwrite = TRUE)

#Interstroke Proxy
#Select only the variables you need
interstroke_proxy <- interstroke_data %>%
  select(case_num,case,caseid,id,eage,alcohhis,constrktp,discharge,ethnic,leisactv,levconsc,liveplace,marital,mrscofef,mrscoreb,
         mrscorec,occupat,othstsp,regionnn7,esex,subaf,subangin,subbbled,subdm,subeduc,subhchol,whrs2tert,fstrktype,
         discharge,mrscoreb,mrscorec,respondt,mrscofef,phys,cardiacrfcat,htnadmbp,nevfcur,dmhba1c2,thromall,levconsc,phys,depdose, depscore,depr,depdosedef2,depdosedef3,
         global_stress2,symphh,alcohfreqwk,subbbled,ahei4tert,occupat,incomectry,fischocsp,afibflut,global_stress,
         bmi5grp,highwhr,ahei4tert,
         subhtn,subpad,subtia,subvthr,sympapha,tobahis,wrkactv,datedint)

usethis::use_data(interstroke_proxy, overwrite = TRUE)

#Interstroke Alcohol
#Select only the variables you need
interstroke_alcohol <- interstroke_data %>%
  select(case_num,case,caseid,id,eage,esex,alcb1,alcb,alcq,alcq3,alcqn,alcbingetyp,alcohhis,alcohage,alcohol,beerfwk,spiritf,spiritfwk,arrackfwk,oalcfwk,alcallwk,
         freqalc,numalcoh,spirita,spiritd,spirqtag,winefreq,wineamt,winedur,wineqtag,beerfreq,beeramt,beerdur,beerqtag,
         fstrktype,
         arrackf,arracka,arraqtag,oalcfreq,oalcamt,oalcdur,oalcqtag,tobahis,regionnn7,ethnicity,occupat,scurformnev,
         nevfcur,phys,whrs2tert,ahei4tert,htnmbp2,dmhba1c2,cardiacrfcat,apob_apoatert,subami,subangin,subtia,subpad,subhchol,
         subvthr,subaf,antiplate,orantpre,statipre,fibrapre,acepre,arbpre,betabpre,alphbpre,cacbpre,diurepre,subeduc,education,
         windextert,case,iscstroke,ichemstroke,strataid,htnadmbp,fstress,locquarthi,depr,global_stress2)

usethis::use_data(interstroke_alcohol, overwrite = TRUE)

#Interstroke Severity
#Select only the variables you need
interstroke_severity <- interstroke_data %>%
  select(case_num,case,caseid,id,eage,esex,sysadm,diasadm,waist1,hip1,height,weight,liveplace,marital,mrscoren,mrscofef,mrscoreb,
         htnadmbp,dmhba1c2,whrs2tert,fstrktype,bmi,
         leisactv,highwhr,statipre,global_stress2,hhincome,
         cardiacrfcat,ahei4tert,phys,antiplate,antihypert3,orantpre,alcohfreqwk,depr,substrk,
         mrscorec,occupat,othstsp,regionnn7,respondt,subaf,subangin,subbbled,subdm,subeduc,subhchol,nevfcur,phys,cardiacrfcat,
         subhtn,subpad,subtia,subvthr,sympapha,tobahis,wrkactv)

usethis::use_data(interstroke_severity, overwrite = TRUE)

#Select only the variables you need
interstroke_womenshealth <- interstroke_data %>%
  select(case_num, case, caseid, id, eage, esex, thromhos, regionnn7, subeduc, marital, work, occupat, hhincome, htnadmbp, subhchol, dmhba1c2, subangin
         , substrk, subbbled, subtia, subpad, subvthr, subaf, subami, diagosa, migraine, phys, alcohfreqwk, nevfcur, marijhis, cocaihis,
         opiathis, smokexpose, depr, bcpills, ls6menst, failpreg, fertused, pregnant, preghtn, pregpuri,
         preglegs, pregseiz, pregdm, miscarrig, menstop, hormrepl, pregstk, fstrktype, whrs2tert, totpollute,
         cardiacrfcat, antithromb )

usethis::use_data(interstroke_womenshealth, overwrite = TRUE)

#Interstroke Psychological_stress
interstroke_stress <- interstroke_data %>%
  select(case_num,case,caseid,id,
         eage,bmi,whr,whrs2tert,sysadm,diasadm,cardiacrfcat,education,
         chol,ldlc,hdlc,nonhdl,apoa1,apob,apob_apoa,fstrktype,
         workstrs,homestrs,global_stress,finastrs,strclass,locquart,quartloc,sad2wks,depdosedef3,
         physwork,gstress,fstress,locquarthi,incomectry,mrscoreb,mrscorec,mrscofef,mrscorefcat,mrscoren,fatlstrk,
         depdose,depscore,depr,depdosedef2,antidpre,antippre,antidhos,antiphos,
         moteduc,fatduc,regionnn7,ethnicity,hhincome,incomectry,marital,occupat,esex,smoking,subdm,
         htnadmbp,dmhba1c2,country,sysbp,diasbp,aheiscore,alcohfreqcat,afibflut,nevfcur,statipre,antihypertpre,
         phys,wrkactv,global_stress2,depr,subhtn,subhchol,respondt,subeduc,workrv,feeldown,thkdeath,focus,
         tosleep,glweight,tired,interest,gaveup,changes,treated,futurexp,lifcntrl,wrkcntrl,autonomy,stopwork,othstrs,
         deathspo,deathfam,injury,famcnfl,violence,busfail,lossjob,sepadivo,htnmbp2,scurformnev,aheitert,antihypertpre,
         idcenter,ctrltype,windex,windextert,fstrktype,global_stress_3,homestrs_3,workstrs_3,days_to_interview)

usethis::use_data(interstroke_stress, overwrite = TRUE)


#Interstroke Physical Activity
#Select only the variables you need
interstroke_pa <- interstroke_data %>%
  select(workhrs,workhrs_week,mets_workalone,
         subeduc,case_num,case,caseid,id,eage,moteduc,fatduc,regionnn7,ethnicity,hhincome,incomectry,marital,occupat,esex,smoking,subdm,
         htnadmbp,dmhba1c2,whrs2tert,fstrktype,
         sysmorn,sysadm,sysint,sysbp,
         diasadm,diasmorn,diasint,diasbp,
         aheiscore,alcohfreqcat,afibflut,bmi,phys,statipre,antihypertpre,nevfcur,cardiacrfcat,workmins_A)

usethis::use_data(interstroke_pa, overwrite = TRUE)


#Interstroke Body Mass Index
#Select only the variables you need
interstroke_bmi <- interstroke_data %>%
  select(workhrs,workhrs_week,mets_workalone,bmi_3_level,
         subeduc,case_num,case,caseid,id,eage,moteduc,fatduc,regionnn7,ethnicity,hhincome,incomectry,marital,occupat,esex,smoking,subdm,
         htnadmbp,dmhba1c2,whrs2tert,fstrktype,
         sysmorn,sysadm,sysint,sysbp,whr,
         diasadm,diasmorn,diasint,diasbp,
         bmi_quintiles,
         aheiscore,alcohfreqcat,afibflut,bmi,phys,statipre,antihypertpre,nevfcur,cardiacrfcat,workmins_A)

usethis::use_data(interstroke_bmi, overwrite = TRUE)

#Interstroke AF
interstroke_af <- interstroke_data %>%
  select(case_num,case,caseid,id,idcenter,
         eage,height,weight,bmi,bmi_3_level,whr,waist,hip,whr,whrs2tert,sysadm,diasadm,cardiacrfcat,education,
         chol,ldlc,hdlc,nonhdl,apoa1,apob,apob_apoa,fstrktype,
         workstrs,homestrs,quartloc,locquarthi,gstress,fstress,global_stress2,sad2wks,
         phys,physwork,incomectry,mrscoreb,mrscorec,mrscofef,mrscorefcat,mrscoren,fatlstrk,
         moteduc,fatduc,regionnn7,ethnicity,hhincome,windextert,marital,occupat,esex,scurformnev,nevfcur,
         htnadmbp,dmhba1c2,country,sysbp,diasbp,aheiscore,alcohfreqcat,alcb1,
         subaf,afibflut,statipre,antihypertpre,antihypert3,
         subhtn,subhchol,subdm,subangin,subami,subpad,subvthr,subhiv,subrhd,substrk,subtia,subcb,cscanc,cshf,subhvr,
         respondt,subeduc,workrv,gaveup,htnmbp2,aheitert,ctrltype,orantpre,oranthos,aspirpre,aspirhos,antithromb,
         cardiacmeds,ischocspfinal,oscplaci,undetermined,cardioembolic,largevessel,smallvessel,othertoast,TOAST,
         afecg,echo,lvefechol,cardecg,cardholt,cardtte,cardnone,afbefore,newly_diagnosed_af,agecat,
         age_group_decade, cschag, inradm, alcallwk, alcallday, alcallmth, afecg, stemiecg, lvhecg, pmecg, ppecg, rvhecg,gni)

usethis::use_data(interstroke_af, overwrite = TRUE)

#Interstroke Diet
interstroke_diet <- interstroke_data %>%
  select(case_num,case,caseid,id,idcenter,regionnn7,ethnicity,gni,
         eage,agecat,age_group_decade,height,weight,bmi,bmi_3_level,whr,waist,hip,whr,whrs2tert,sysadm,diasadm,education,
         chol,ldlc,hdlc,nonhdl,apoa1,apob,apob_apoa,fstrktype,workstrs,homestrs,quartloc,locquarthi,gstress,fstress,global_stress2,sad2wks,
         phys,physwork,incomectry,mrscoreb,mrscorec,mrscofef,mrscorefcat,mrscoren,fatlstrk,
         moteduc,fatduc,hhincome,windextert,marital,occupat,esex,scurformnev,nevfcur,
         htnadmbp,dmhba1c2,country,sysbp,diasbp,aheiscore,alcohfreqcat,alcb1,afibflut,respondt,subeduc,ctrltype,
         vegetari,vegmilk,vegeggs,vegfish,vegchick,wgbread,rfbread,dairy,meat,orgameat,poultry,eggs,
         seafood,pizza,greenveg,rawveg,cookveg,nuts,bpotat,pickled,fryfood,salty,fruits,
         icecream,sweets,sugar,fruitdrk,carbddrk,wholewk,refwk,bpotawk,pizzawk,meatwk,poultrywk,
         orgameatwk,fishwk,eggswk,dairwk,nutswk,frywk,saltwk,pickwk,sweetwk,sugarwk,icewk,carbodrkwk,
         cookwk,wholeday,refday,bpotaday,pizzaday,meatday,poultryday,orgameatday,fishday,eggsday,
         dairday,nutsday,fryday,saltday,pickday,sweetday,sugarday,iceday,carbodrkday,fruitdrkday,
         fruitsday,gvegday,rawday,cookday,allvegahei,fruitahei,nutsahei,white_meatahei,wholeahei,
         friedahei,friedsahei,alcahei,aheiscore,aheitert,
         oliveoil,palmoil,canolaoil,cornoil,gheeoil,lardoil,margall)

usethis::use_data(interstroke_diet, overwrite = TRUE)

#Interstroke Stroke Subtype
interstroke_stroke_subtype <- interstroke_data %>%
  select(case_num,case,caseid,id,idcenter,
         eage,height,weight,bmi,bmi_3_level,whr,waist,hip,whr,whrs2tert,sysadm,diasadm,cardiacrfcat,education,
         chol,ldlc,hdlc,nonhdl,apoa1,apob,apob_apoa,fstrktype,
         workstrs,homestrs,quartloc,locquarthi,gstress,fstress,global_stress2,sad2wks,
         phys,physwork,incomectry,mrscoreb,mrscorec,mrscofef,mrscorefcat,mrscoren,fatlstrk,
         moteduc,fatduc,regionnn7,ethnicity,hhincome,windextert,marital,occupat,esex,scurformnev,nevfcur,
         htnadmbp,dmhba1c2,country,sysbp,diasbp,aheiscore,alcb1,
         subaf,afibflut,statipre,antihypertpre,antihypert3,
         subhtn,subhchol,subdm,subangin,subami,subpad,subvthr,subhiv,subrhd,substrk,subtia,subcb,cscanc,cshf,subhvr,
         respondt,subeduc,workrv,gaveup,htnmbp2,aheitert,ctrltype,orantpre,oranthos,aspirpre,aspirhos,antithromb,
         cardiacmeds,ischocspfinal,oscplaci,undetermined,cardioembolic,largevessel,smallvessel,othertoast,TOAST,
         afecg,echo,lvefechol,cardecg,cardholt,cardtte,cardnone,afbefore,newly_diagnosed_af,agecat, age_group_decade, cschag, inradm,
         sympweak,faceweak,armweak,legweak,sympnumb,facenumb,armnumb,legnumb,aphasia,sympgait,sympvert,symphh,
         alcohhis,alcohfreqcat,acute_alcohol,csalcoh,ang24hr,mrscoreb,mrscorec,mrscofef,mrscorefcat,mrscoren,hrtrtadm,antithrompre)

usethis::use_data(interstroke_stroke_subtype, overwrite = TRUE)

#Interstroke K_bp
interstroke_k_bp <- interstroke_data %>%
  select(case_num,case,caseid,id,
         eage,bmi,whr,whrs2tert,weight,height,ucr,sysadm,diasadm,cardiacrfcat,education,
         chol,ldlc,hdlc,nonhdl,apoa1,apob,apob_apoa,fstrktype,
         workstrs,homestrs,global_stress,finastrs,strclass,locquart,quartloc,sad2wks,depdosedef3,
         physwork,gstress,locquarthi,incomectry,mrscoreb,mrscorec,mrscofef,mrscorefcat,mrscoren,fatlstrk,
         depr,moteduc,fatduc,regionnn7,ethnicity,hhincome,incomectry,marital,occupat,esex,smoking,subdm,
         htnadmbp,dmhba1c2,country,sysbp,diasbp,aheiscore,alcohfreqcat,afibflut,nevfcur,statipre,antihypertpre,
         phys,wrkactv,global_stress2,depr,subhtn,subhchol,respondt,subeduc,workrv,
         htnmbp2,scurformnev,aheitert,antihypertpre,idcenter,ctrltype,windex,fstrktype,global_stress_3,sysbpintcat8,sysbpmorncat8,sysbpcat,diasbp,sysbp,diasint,sysint,diasmorn,sysmorn,diasadm,sysadm,
         na,k,urinedt,urinemin,urinehr,una_ucrea,una,uk,uk_ucrea,na)

usethis::use_data(interstroke_k_bp, overwrite = TRUE)


#Interstroke Urinary Na and K
interstroke_urinary_na_k <- interstroke_data %>%
  select(case_num,case,caseid,id,
         ucr,dob,sex,regionnn7,idcenter,chol,ldlc,hdlc,subdm,diabstrk,strktype,mrscorec,htn,htnbp,
         htnmbp,nevfcur,eage,sex,sysadm,diasadm,hrtrtadm,weight,height,crea,na,una,uk,symdate,symhour,
         symmins,urinedt,urinehr,urinemin,ethnicity,subdm,mrscofef,diurepre,diurehos,scorefatert,
         arrdate,arrhour,arrmins,nevfcur,scurformnev,alcb1,alcb,alcb1,whr,phys,aheiscore,subhtn,
         sysbp,diasbp,afibflut,subeduc,ethnicn,alcohfreqcat4,bmi,subdm,substrk,subtia,subaf,subami,
         scurformnev,sysbpcat6,diasbpcat6,aheiscore,leiswork,betabpre,acepre,arbpre,diurepre,toast1,
         toast2,toast3,toast4,country,k,na,betabhos,acehos,arbhos,diurehos,sysmorn,diasmorn,sysint,
         diasint,ctrltype,apoa1,apob,blooddt,bloodhr,bloodmin,paw,physwork,pa,leiswork,pawl,workhrs,
         leishrs,workmins,leismins,nocsleep,na_24h_tanaka_q,na_24h_tanaka_g,k_24h_tanaka_g,k_24h_tanaka_q)

usethis::use_data(interstroke_urinary_na_k, overwrite = TRUE)

#Interstroke Gender
interstroke_gender <- interstroke_data %>%
  select(case_num,case,caseid,id,eage,esex,regionnn7,symdate,symhour,incomectry, symmins,arrdate,arrhour,arrmins,aspirhos,
         clopdhos,dipyrhos,asdiphos,oranthos,thromhos,ethnicity,subeduc,marital,work,occupat,hhincome,
         htnadmbp,apob_apoatert,dmhba1c2, idcenter, subangin,substrk,subbbled,cardiacrfcat,motbbled,fatbbled,motstrk,fatstrk,
         subtia,subpad,subvthr,subaf,migraine,alcohfreqwk,nevfcur,depr,mrscorec,mrscoreb, scandate,scanhr,scanmin,onstrknm,fstrktype,
         csbldc,liveplace,ceperfom,m1physio,m1occupa,m1speech,m1antipl,m1antico,m1bldpld,m1choles,ls6menst,
         bcpills,failpreg,fertused,pregnant, subaf, orantpre, fischocsp, ftoast1, deathdt, preghtn,pregpuri,preglegs,pregseiz,pregdm,miscarrig,menstop,hormrepl,
         pregstk, cardioembolic, whrs2tert, afecg,afbefore )

usethis::use_data(interstroke_gender, overwrite = TRUE)

