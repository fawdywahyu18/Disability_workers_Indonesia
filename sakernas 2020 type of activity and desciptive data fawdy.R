library(dplyr)
library(rlist)
library(haven)
library(lazyeval)
library(writexl)
library(foreign)
library(stargazer)

setwd("")
data_raw <- read_dta(file="sakernas_20208_dta.dta")

# Creating household ID
data_raw <- data_raw %>%
  mutate(hhid=paste(kode_prov, kode_kab, klasifikas, id_nks, urutan, no_dsrt, sep=""))
data_raw$hhid <- as.numeric(data_raw$hhid)

# Check duplicates in hhid
id_occur <- data.frame(table(data_raw$hhid))
nrow(id_occur[id_occur$Freq > 1,]) # the number of obs which contains duplicated id

# Ensuring the individuals are 15 or older
data_1 <- data_raw %>%
  rename(age=k6) %>%
  rename(fw=final_weig) %>%
  rename(urban=klasifikas)
data_1 <- data_1 %>%
  filter(age>=15)

#============================================Type of Acitivty================================================#

# Working individuals
data_1 <- data_1 %>%
  mutate(employed=ifelse(r9a==1 | r9b==1 | r9c==1 | r10a==1,1,0)) %>% # Employed individuals
  mutate(caker1=ifelse(r22a==1,1,0)) %>% # Looking for work
  mutate(caker2=ifelse(r22b==1,1,0)) %>% # Establishing a new business/firm
  mutate(caker3=ifelse(r25a==3,1,0)) %>% # Hopeless of job
  mutate(caker4=ifelse(r25a == 1 | r25a == 2,1,0)) # Have a job in future start

data_2 <- data_1 %>%
  mutate(dlabforce = ifelse(employed==1,1, 
                            ifelse(caker1==1,2,
                                   ifelse(caker2==1,3,
                                          ifelse(caker3==1,4,
                                                 ifelse(caker4==1,5,
                                                        ifelse(r31a==3,6,
                                                               ifelse(r31b==1,7,8))))))))
# 1 "Working" 2 "Looking" 3 "Establishing new business" 4 "Discouraged" 
# 5 "Future job arranged" 6 "Student" 7 "Housekeeping" 8 "Others"
data_2$dlabforce <- labelled(data_2$dlabforce, c(working=1, looking=2,
                                                 establishing_new_business=3, discouraged=4,
                                                 future_job_arranged=5, student=6,
                                                 housekeeping=7, others=8))

# core labor force
# based on core definition, labor force includes those who are employed and those who are unemployed
# but actively looking for work
data_2 <- data_2 %>%
  mutate(labforce_core=ifelse(dlabforce>=1 & dlabforce<=2,1,0))

# Economically active vs not economically active population
data_2 <- data_2 %>%
  mutate(active=ifelse(dlabforce>=1 & dlabforce<=5,1,0))
# 1 "Economically Active" 0 "Not Economically Active"
# this definition of economically activity is different with the definition from ILO

# Employement
data_2 <- data_2 %>%
  mutate(employed=replace(employed, labforce_core==0, NA)) %>%
  mutate(employed=replace(employed, dlabforce>=2 & dlabforce<=5, 0))

# broad labor force
# based on broad definition, labor force includes those who are employed and those who are unemployed but
# actively looking for work, establishing new business, discouraged, and with future job arrangement
data_2 <- data_2 %>%
  mutate(labforce_broad=ifelse(is.na(employed)==0,1,0))

data_test <- data_2 %>%
  filter(employed==1) %>%
  mutate(number_employed=sum(fw))
mean(data_test$number_employed)
# should be equal to 128 454 18 (hasil backasting)

# Core unemployment
data_2 <- data_2 %>%
  mutate(unemployed=ifelse(employed==0 & is.na(employed)!=TRUE,1,0)) %>%
  mutate(unemployed_core=unemployed)

data_test <- data_2 %>%
  filter(unemployed_core==1) %>%
  mutate(number_unemployed=sum(fw))
mean(data_test$number_unemployed)
# should be equal to 9 767 75 (hasil backasting)

# Broad unemployment and ever worked
data_2 <- data_2 %>%
  mutate(unemployment_broad=unemployed) %>%
  mutate(everworked=ifelse(r28==1,1,0))

# student, housekeeping, and others category among not economically active
data_2 <- data_2 %>%
  mutate(student=ifelse(dlabforce==6 & labforce_broad==0, 1, 0)) %>%
  mutate(housekeeping=ifelse(dlabforce==7 & labforce_broad==0, 1, 0)) %>%
  mutate(others=ifelse(dlabforce==8 & labforce_broad==0, 1, 0))

# total number of working hours of all jobs during the previous week
data_2 <- data_2 %>%
  mutate(hour=ifelse(employed==1, r16a, NA))

# Under employment
data_2 <- data_2 %>%
  mutate(underemp=ifelse(hour<35,1,NA)) %>%
  mutate(underemp=replace(underemp, hour==0 | (hour>=35 & is.na(hour)!=TRUE),0))


# Disability people
data_3 <- data_2 %>%
  mutate(disability1 = ifelse(r8a>=2 & r8a<=3,1,0)) %>%
  mutate(disability2 = ifelse(r8b>=2 & r8b<=3,1,0)) %>%
  mutate(disability3 = ifelse(r8c>=2 & r8c<=3,1,0)) %>%
  mutate(disability4 = ifelse(r8d>=2 & r8d<=3,1,0)) %>%
  mutate(disability5 = ifelse(r8e>=2 & r8e<=3,1,0)) %>%
  mutate(disability6 = ifelse(r8f>=2 & r8f<=3,1,0))

data_3 <- data_3 %>%
  mutate(disability=ifelse(disability1==1,1,
                           ifelse(disability2==1,2,
                                  ifelse(disability3==1,3,
                                         ifelse(disability4==1,4,
                                                ifelse(disability5==1,5,
                                                       ifelse(disability6==1,6,0)))))))
data_3$disability <- labelled(data_3$disability, c(no_disability=0, vision=1, hearing=2,
                                                   mobility=3, move_hands_finger=4,
                                                   communication=5, others=6))
# 1 "vision" 2 "hearing" 3 "mobility" 4 "move-hands-finger" 
# 5 "communication" 6 "others" 0 "non-disability"

# Self estimate disability ability
data_3 <- data_3 %>%
  mutate(disability_ability=ifelse(r8a==2 | r8b==2 | r8c==2 | r8d==2 | r8e==2 | r8f==2, 1, 
                                   ifelse(r8a==3 | r8b==3 | r8c==3 | r8d==3 | r8e==3 | r8f==3, 2, NA)))
# 1 "sedang" 2 "parah"
data_3$disability_ability <- labelled(data_3$disability_ability, c(sedang=1, parah=2))

# Disability worker
data_3 <- data_3 %>%
  mutate(disability_worker=ifelse((disability>=1 & disability<=6) & employed==1,1,0))
# 0 "non_disability_worker" 1 "disability_worker"

# Disability people in labor force
data_3 <- data_3 %>%
  mutate(disability_lf=ifelse((disability>=1 & disability<=6) & labforce_broad==1,
                              1,0))
# 0 "non disability people" 1 "disability people" in broad labor force


#============================================HH Member Char================================================#

# Gender
data_3 <- data_3 %>%
  mutate(gender=ifelse(k4==1,1,0))
# 0 "female" 1 "male"
data_3$gender <- labelled(data_3$gender, c(female=0, male=1))

# Education level
data_3 <- data_3 %>%
  mutate(educ_level=ifelse(r6a==2,1,
                           ifelse(r6a==3,2,
                                  ifelse(r6a>=4 & r6a<=5,3,
                                         ifelse(r6a>=6 & r6a<=8,4,0)))))
# 0 "tidak punya ijazah SD" 1 "SD" 2 "SMP" 
# 3 "SMA" 4 "Pendidikan Tinggi"
data_3$educ_level <- labelled(data_3$educ_level, c(no_diploma=0, primary_school=1,
                                                   junior_hs=2, high_school=3,
                                                   higher_education=4))

# Training/Kursus
data_3 <- data_3 %>%
  mutate(train=ifelse(r6d==1 | r6e==1,1,0))
# 0 "Not Trained" 1 "Trained"

# Islands/Provinces
data_3 <- data_3 %>%
  mutate(d_prov=ifelse(kode_prov>=11 & kode_prov<=21,0,
                       ifelse(kode_prov>=31 & kode_prov<=36,1,
                              ifelse(kode_prov>=51 & kode_prov<=53,2,
                                     ifelse(kode_prov>=61 & kode_prov<=65,3,
                                            ifelse(kode_prov>=71 & kode_prov<=76,4,5))))))
# 0 "Sumatra" 1 "Jawa-Bali" 2 "Nusa Tenggara" 
# 3 "Kalimantan" 4 "Sulawesi" 5 "Maluku-Papua"

# Household size
data_3 <- data_3 %>%
  mutate(hhsize=ifelse(is.na(jlh_art)==0, jlh_art, NA)) %>% #HH Size all members
  mutate(hhsize5=ifelse(is.na(jlh_art5)==0, jlh_art5, NA))

#=====================================Descriptive Data (Requests)=========================================#

# Creating function for descriptive table
descriptive_table = function(data, group_col1, group_col2, filter1) {
  data %>%
    filter_(.dots = lazy(filter1)) %>%
    mutate(total_all=sum(fw)) %>%
    group_by_(.dots = lazy(group_col1)) %>%
    mutate(total_g1=sum(fw)) %>%
    ungroup() %>%
    group_by_(.dots = lazy(group_col1), lazy(group_col2)) %>%
    summarize(number_of_people=sum(fw),
              share_prctg=round(number_of_people*100/mean(total_all), 3),
              share_prctg_g1=round(number_of_people*100/mean(total_g1), 3)) %>%
    ungroup() %>%
    group_by_(.dots = lazy(group_col1)) %>%
    mutate(check2=sum(share_prctg_g1))
}

descriptive_table2 = function(data, group_col1, group_col2, group_col3, filter1) {
  data %>%
    filter_(.dots = lazy(filter1)) %>%
    mutate(total_all=sum(fw)) %>%
    group_by_(.dots = lazy(group_col1), lazy(group_col2), lazy(group_col3)) %>%
    summarize(number_of_people=sum(fw),
              share_prctg=round(number_of_people*100/mean(total_all), 3))
}

data_4 = data_3

# Disability based on hh member
disability_hhmember = data_4 %>%
  filter(is.na(hhsize)==0) %>%
  mutate(mean_hhsize_all=weighted.mean(hhsize, fw)) %>%
  group_by(disability) %>%
  summarise(mean_hhsize=round(weighted.mean(hhsize, fw), 0),
            mean_hhsize_all=round(mean(mean_hhsize_all)), 0)
hhmember = disability_hhmember

# Disability based on urban/rural
disability_urban = data_4 %>%
  filter(labforce_broad==1) %>%
  mutate(total_all=sum(fw)) %>%
  group_by(disability) %>%
  mutate(total_g1=sum(fw)) %>%
  ungroup() %>%
  group_by(disability, urban) %>%
  summarize(number_of_people=sum(fw),
            share_prctg=round(number_of_people*100/mean(total_all), 3),
            share_prctg_g1=round(number_of_people*100/mean(total_g1), 3)) %>%
  ungroup() %>%
  group_by(disability) %>%
  mutate(check2=sum(share_prctg_g1))
urban = disability_urban

# Disability based on gender
disability_gender = data_4 %>%
  filter(labforce_broad==1) %>%
  mutate(total_all=sum(fw)) %>%
  group_by(disability) %>%
  mutate(total_g1=sum(fw)) %>%
  ungroup() %>%
  group_by(disability, gender) %>%
  summarize(number_of_people=sum(fw),
            share_prctg=round(number_of_people*100/mean(total_all), 3),
            share_prctg_g1=round(number_of_people*100/mean(total_g1), 3)) %>%
  ungroup() %>%
  group_by(disability) %>%
  mutate(check2=sum(share_prctg_g1))
gender = disability_gender

# Disability based on provinces
disability_prov = descriptive_table(data_4, disability, kode_prov, labforce_broad==1)
kode_provinsi = disability_prov

# Disability based on islands or regions
disability_islands = descriptive_table(data_4, disability, d_prov, labforce_broad==1)
islands = disability_islands

# Educ Level in disability group and gender
disability_el = descriptive_table(data_4, disability, educ_level, labforce_broad==1)
r6a = disability_el
gender_el = descriptive_table(data_4, k4, educ_level, labforce_broad==1)

# age in disability group
disability_age <- data_4 %>%
  filter(labforce_broad==1) %>%
  mutate(mean_age_all=weighted.mean(age, fw)) %>%
  group_by(disability) %>%
  summarise(mean_age=weighted.mean(age, fw),
            mean_age_all=mean(mean_age_all))
k6 = disability_age

# Training per disability group
disability_train <- descriptive_table(data_4, disability, train, labforce_broad==1)
r6d_e = disability_train

# Employment per disability group
disability_employed = descriptive_table(data_4, disability, employed, is.na(employed)==0)
employed_people = disability_employed

# Disability based on their ability
disability_ability = data_4 %>%
  filter(labforce_broad==1) %>%
  mutate(total_all=sum(fw)) %>%
  group_by(disability) %>%
  mutate(total_g1=sum(fw)) %>%
  ungroup() %>%
  group_by(disability, disability_ability) %>%
  summarize(number_of_people=sum(fw),
            share_prctg=round(number_of_people*100/mean(total_all), 3),
            share_prctg_g1=round(number_of_people*100/mean(total_g1), 3)) %>%
  ungroup() %>%
  group_by(disability) %>%
  mutate(check2=sum(share_prctg_g1))

# Disability workers per lapangan usaha
disability_lu <- descriptive_table(data_4, disability, r13a_kateg, employed==1)
lapangan_usaha = disability_lu

# Reason for NOT working
data_4 = data_4 %>%
  mutate(reas_not_working=ifelse(r10a==1 & labforce_broad==1, r10b, NA),
         reas_not_working=replace(reas_not_working, r10b==0 | r10b==9, NA),
         reas_nw_covid=ifelse(reas_not_working==7 | reas_not_working==8 | r10c==1, 1, 0))
  
disability_rnw = descriptive_table(data_4, disability, reas_not_working, is.na(reas_not_working)==0)
r10b = disability_rnw
disability_rnw_c = descriptive_table(data_4, disability, reas_nw_covid, is.na(reas_not_working)==0)
r10c = disability_rnw_c

# Looking for job or establishing a new business
data_4 = data_4 %>%
  mutate(lj_eb=ifelse(caker1==1 | caker2==1,1,0),
         lj_eb=replace(lj_eb, labforce_broad==0, NA))
disability_lj_eb = descriptive_table(data_4, disability, lj_eb, is.na(lj_eb)==0)
r22a_b = disability_lj_eb

# Duration for preparing business/looking job (in days)
data_4 <- data_4 %>%
  mutate(durationy_lj_eb=ifelse(lj_eb==1 & labforce_broad==1, r23a*12, NA),
         durationm_lj_eb=ifelse(is.na(durationy_lj_eb)==0, r23a, NA),
         duration_lj_eb=ifelse(is.na(durationm_lj_eb)==0, durationy_lj_eb+durationm_lj_eb, NA))
disability_dlj <- data_4 %>%
  filter(is.na(duration_lj_eb)==FALSE) %>%
  mutate(mean_all=weighted.mean(duration_lj_eb,fw)) %>%
  group_by(disability) %>%
  summarise(mean_duration=weighted.mean(duration_lj_eb, fw)*30,
            mean_duration_all=mean(mean_all)*30)
r23a = disability_dlj

# Effort for looking a job or establishing a business (1 week or 1 month ago)
data_4 <- data_4 %>%
  mutate(effort_lj1=ifelse(r24a==1,1,0)) %>%
  mutate(effort_lj2=ifelse(r24b==1,1,0)) %>%
  mutate(effort_lj3=ifelse(r24c==1,1,0)) %>%
  mutate(effort_lj4=ifelse(r24d==1,1,0)) %>%
  mutate(effort_lj5=ifelse(r24e==1,1,0)) %>%
  mutate(effort_lj6=ifelse(r24f==1,1,0))

data_4 <- data_4 %>%
  mutate(effort_lj=ifelse(effort_lj1==1,1,
                          ifelse(effort_lj2==1,2,
                                 ifelse(effort_lj3==1,3,
                                        ifelse(effort_lj4==1,4,
                                               ifelse(effort_lj5==1,5,
                                                      ifelse(effort_lj6==1,6,NA)))))))%>%
  mutate(effort_lj=replace(effort_lj, labforce_broad==0, NA))

data_4$effort_lj <- labelled(data_4$effort_lj, c(bursa=1, kantor=2, iklan_mandiri=3, kenalan=4,
                                                 modal_siup=5, others=6))
disability_ef_lj <- descriptive_table(data_4, disability, effort_lj, is.na(effort_lj)==0)
r24a_f = disability_ef_lj
provinces_ef_lj <- descriptive_table(data_4, kode_prov, effort_lj, is.na(effort_lj)==0)
r24a_f_prov = provinces_ef_lj

# Reason for NOT looking for job or preparing business
data_4 <- data_4 %>%
  mutate(reasNot_lj_eb=ifelse(labforce_broad==1, r25a, NA)) %>%
  mutate(reasNot_lj_eb=replace(reasNot_lj_eb, r25a==0, NA))
disability_reasNot <- descriptive_table(data_4, disability, reasNot_lj_eb, is.na(reasNot_lj_eb)==0)
r25a = disability_reasNot
# Reason for NOT looking for job or preparing business (ONLY COVID related reason)
data_4 <- data_4 %>%
  mutate(reas_nljeb_covid=ifelse(reasNot_lj_eb==7 | reasNot_lj_eb==8 | r25b==1, 1, 0),
         reas_nljeb_covid=replace(reas_nljeb_covid, labforce_broad==0, NA))
disability_reasNot_covid <- descriptive_table(data_4, disability, reas_nljeb_covid, is.na(reas_nljeb_covid)==0)
r25b = disability_reasNot_covid

# Willing to accept job offer
data_4 <- data_4 %>%
  mutate(will_aj=ifelse(r26==1,1,0),
         will_aj=replace(will_aj, labforce_broad==0, NA))
disability_waj <- descriptive_table(data_4, disability, will_aj, is.na(will_aj)==0)
r26 = disability_waj

# Prakerja Training 1
know_prakerja = descriptive_table(data_4, disability, r27a, labforce_broad==1)
sign_up_prakerja = descriptive_table(data_4, disability, r27b, labforce_broad==1)

# Prakerja Training
data_4 <- data_4 %>%
  mutate(reas_sign_prakerja=ifelse(r27a==1 & r27b==1 & labforce_broad==1, r27c, NA),
         reas_sign_prakerja=replace(reas_sign_prakerja, r27c==0,NA),
         increase_skill_prakerja=ifelse(r27d==1 & r27e==1 & labforce_broad==1, r27f, NA),
         increase_skill_prakerja=replace(increase_skill_prakerja, r27f==2, 0),
         money_prakerja=ifelse(r27g==1 & labforce_broad==1 & r27h1==1, 1, 
                                ifelse(r27g==1 & labforce_broad==1 & r27h2==1, 2, 
                                       ifelse(r27g==1 & labforce_broad==1 & r27h3==1, 3,
                                              ifelse(r27g==1 & labforce_broad==1 & r27h4==1, 4,
                                                     ifelse(r27g==1 & labforce_broad==1 & r27h5==1, 5, NA))))))
data_4$money_prakerja <- labelled(data_4$money_prakerja, c(sehari2=1, modal_usaha=2, bayar_utang=3,
                                                           nabung=4, lainnya=5))

disability_signup_prakerja = descriptive_table(data_4, disability, reas_sign_prakerja, is.na(reas_sign_prakerja)==0)
disability_increaseSkill_prakerja = descriptive_table(data_4, disability, increase_skill_prakerja, is.na(increase_skill_prakerja)==0)
disability_money_prakerja = descriptive_table(data_4, disability, money_prakerja, is.na(money_prakerja)==0)
r27c = disability_signup_prakerja
r27f = disability_increaseSkill_prakerja
r27h = disability_money_prakerja

provinces_signup_prakerja = descriptive_table(data_4, kode_prov, reas_sign_prakerja, is.na(reas_sign_prakerja)==0)
provinces_increaseSkill_prakerja = descriptive_table(data_4, kode_prov, increase_skill_prakerja, is.na(increase_skill_prakerja)==0)
provinces_money_prakerja = descriptive_table(data_4, kode_prov, money_prakerja, is.na(money_prakerja)==0)
r27c_prov = provinces_signup_prakerja
r27f_prov = provinces_increaseSkill_prakerja
r27h_prov = provinces_money_prakerja

# Primary Working status
data_4 <- data_4 %>%
  mutate(status_pw=ifelse(r12a!=0 & employed==1, r12a, NA))
data_4$status_pw <- labelled(data_4$status_pw, c(berusaha_sendiri=1,
                                                 berusaha_dibantu_btt=2,
                                                 berusaha_dibantu_bt=3,
                                                 buruh_karyawan_pegawai=4,
                                                 pb_pertanian=5,
                                                 pb_nonpertanian=6,
                                                 pk_td=7))
provinces_ws <- descriptive_table(data_4, kode_prov, status_pw, is.na(status_pw)==FALSE)
disability_ws <- descriptive_table(data_4, disability, status_pw, is.na(status_pw)==FALSE)
r12a = disability_ws
r12a_prov = provinces_ws
gender_ws <- descriptive_table(data_4, k4, status_pw, is.na(status_pw)==FALSE)

# Wage/Salary (in money 1 month ago)
data_4 <- data_4 %>%
  mutate(wage=ifelse(employed==1, r14a1+r14a2, NA))

disability_wage <- data_4 %>%
  filter(is.na(wage)==FALSE) %>%
  mutate(mean_all=weighted.mean(wage, fw)) %>%
  group_by(disability) %>%
  summarise(mean_wage=weighted.mean(wage, fw),
            mean_all=mean(mean_all))
r14a1_2 = disability_wage

provinces_wage <- data_4 %>%
  filter(is.na(wage)==FALSE) %>%
  mutate(mean_all=weighted.mean(wage, fw)) %>%
  group_by(kode_prov) %>%
  summarise(mean_wage=weighted.mean(wage, fw),
            mean_all=mean(mean_all))
r14a1_2_prov = provinces_wage

gender_wage <- data_4 %>%
  filter(is.na(wage)==FALSE) %>%
  mutate(mean_all=weighted.mean(wage, fw)) %>%
  group_by(gender) %>%
  summarise(mean_wage=weighted.mean(wage, fw),
            mean_all=mean(mean_all))

# Covid 19 effect on wage (self estimate)
data_4 <- data_4 %>%
  mutate(wage_covid=ifelse(r14b!=0 & employed==1, r14b, NA))
data_4$wage_covid = labelled(data_4$wage_covid, c(tambah=1, kurang=2, tetap=3, beda_pekerjaan=4))
disability_wc <- descriptive_table(data_4, disability, wage_covid, is.na(wage_covid)==0)
provinces_wc <- descriptive_table(data_4, kode_prov, wage_covid, is.na(wage_covid)==0)
r14b = disability_wc
r14b_prov = provinces_wc

# Working hour(total, 1 week ago)
disability_wh <- data_4 %>%
  filter(is.na(hour)==FALSE) %>%
  mutate(mean_all=weighted.mean(hour, fw)) %>%
  group_by(disability) %>%
  summarise(mean_wage=weighted.mean(hour, fw),
            mean_all=mean(mean_all))
r16a = disability_wh

# Covid 19 effect on primary job working hour (self estimate)
data_4 <- data_4 %>%
  mutate(hour_covid=ifelse(r16b!=0 & employed==1, r16b, NA),
         reas_hour_covid=ifelse(r16c!=0 & employed==1, r16c, NA),
         reas_hour_ce=ifelse(reas_hour_covid==6 | reas_hour_covid==7, 1, 0),
         reas_hour_ce=replace(reas_hour_ce, r16d==1, 1),
         reas_hour_ce=replace(reas_hour_ce, employed==0, NA))
disability_hour_covid = descriptive_table(data_4, disability, hour_covid, is.na(hour_covid)==0)
disability_reas_hour_covid = descriptive_table(data_4, disability, reas_hour_covid, is.na(reas_hour_covid)==0)
disability_reas_hour_ce = descriptive_table(data_4, disability, reas_hour_ce, is.na(reas_hour_ce)==0)
r16b = disability_hour_covid
r16c = disability_reas_hour_covid
r16d = disability_reas_hour_ce

# Working using internet
data_4 <- data_4 %>%
  mutate(internet_working=ifelse(r17a==1,1,0),
         internet_working=replace(internet_working, employed==0, NA))
disability_iw <- descriptive_table(data_4, disability, internet_working, is.na(internet_working)==FALSE)
r17a = disability_iw

# Working Location
data_4 <- data_4 %>%
  mutate(working_location=ifelse(r18d1==1, 1,
                                 ifelse(r18d2==1, 2,
                                        ifelse(r18d3==1, 3,
                                               ifelse(r18d4==1, 4,
                                                      ifelse(r18d5==1, 5,
                                                             ifelse(r18d6==1, 6,
                                                                    ifelse(r18d7==1, 7, NA))))))),
         working_location=replace(working_location, employed==0 | working_location==0, NA))

data_4$working_location = labelled(data_4$working_location, c(rumah=1, pasar=2, bioskop=3, mall_ruko=4, terminal=5,
                              pinggir_jalan=6, lainnya=7))
disability_wl = descriptive_table(data_4, disability, working_location, is.na(working_location)==0)
r18d1_7 = disability_wl

# Working Location (area)
data_4 <- data_4 %>%
  mutate(area_wl=ifelse(r19b!=0, r19b, NA),
         area_wl=replace(area_wl, employed==0 | r19a==1, NA),
         commute_wl=ifelse(r19c!=0, r19c, NA),
         commute_wl=replace(commute_wl, employed==0 | r19a==1, NA),
         mode_wl=ifelse(r19d!=0, r19d, NA),
         mode_wl=replace(mode_wl, employed==0 | r19a==1, NA))
disability_awl = descriptive_table(data_4, disability, area_wl, is.na(area_wl)==0)
disability_cwl = descriptive_table(data_4, disability, commute_wl, is.na(commute_wl)==0)
disability_mwl = descriptive_table(data_4, disability, mode_wl, is.na(mode_wl)==0)
r19a_b = disability_awl
r19a_c = disability_cwl
r19a_d = disability_mwl

# More than 1 job
data_4 <- data_4 %>%
  mutate(more_1job=ifelse(employed==1, r20a, NA),
         more_1job=replace(more_1job, r20a==2, 0))
disability_m1j = descriptive_table(data_4, disability, more_1job, is.na(more_1job)==0)
provinces_m1j = descriptive_table(data_4, kode_prov, more_1job, is.na(more_1job)==0)
r20a = disability_m1j
r20a_prov = provinces_m1j

disability_excel_2020 = list(hhmember=hhmember, urban=urban, disability_ability=disability_ability, 
                             employed_people=employed_people, gender=gender, lapangan_usaha=lapangan_usaha, 
                             know_prakerja=know_prakerja, sign_up_prakerja=sign_up_prakerja, k6=k6,
                             r6a=r6a, r6d_e=r6d_e, r10b=r10b, r10c=r10c, r12a=r12a, r14a1_2=r14a1_2, r14b=r14b, r16a=r16a, r16b=r16b,
                             r16c=r16c, r16d=r16d, r17a=r17a, r18d1_7=r18d1_7, r19a_b=r19a_b, r19a_c=r19a_c,
                             r19a_d=r19a_d, r20a=r20a, r22a_b=r22a_b, r23a=r23a, r24a_f=r24a_f, r25a=r25a,
                             r25b=r25b, r26=r26, r27c=r27c, r27f=r27f, r27h=r27h)
write_xlsx(disability_excel_2020, "disability_excel_2020.xlsx")

provinces_excel_2020 = list(kode_prvinsi=kode_provinsi, islands=islands, r12a_prov=r12a_prov, r14a1_2_prov=r14a1_2_prov, r14b_prov=r14b_prov,
                       r20a_prov=r20a_prov, r24a_f_prov=r24a_f_prov, r27c_prov=r27c_prov,
                       r27f_prov=r27f_prov, r27h_prov=r27h_prov)
write_xlsx(provinces_excel_2020, "provinces_excel_2020.xlsx")

gender_excel = list(gender_el, gender_wage, gender_ws)
write_xlsx(gender_excel, "gender_excel_2020.xlsx")

# Analyze the data
# Within disability
# Independent variables: gender, educ_level, train, d_prov, hhsize, urban, disability_ability, age, b5_r20_kat (lapangan usaha)
# dependet variables: employed, wage

data_reg = data_4
data_reg = data_reg %>%
  filter(labforce_broad==1,
         educ_level>0) %>%
  mutate(gender=factor(gender),
         disability_type=factor(ifelse(disability>0, disability, NA)),
         educ_level=factor(educ_level),
         train=factor(train),
         d_prov=factor(d_prov),
         urban=factor(urban),
         disability_ability=factor(disability_ability),
         lapangan_usaha=factor(r13a_kateg),
         disability = factor(disability),
         employed = factor(employed),
         working_status = factor(status_pw))

# Logit regression for employed opportunity
require(broom) # for tidy()
require(knitr) # for kable()
logit_controls <- glm(employed ~ disability_type + gender + educ_level + train + urban + 
                        disability_ability + age + d_prov,
                      data=data_reg, family = "binomial")
logit_wc <- glm(employed ~ disability_type,
                data=data_reg, family = "binomial")

summary(logit_wc)
summary(logit_controls)

stargazer(logit_wc, logit_controls,
          title = "Hasil Regresi Logit 2020",
          out = "logit regression output 2020.htm")

# Weighted least square for wages
employed_only = data_reg %>%
  filter(employed==1)
wls_wc <- lm(wage ~ disability_type, data = employed_only, weights=fw)
summary(wls_wc)
wls_controls <- lm(wage ~ disability_type + gender + educ_level + train + urban + 
                     disability_ability + age + d_prov,# + lapangan_usaha,
                   data = employed_only, weights = fw)
summary(wls_controls)

stargazer(wls_wc, wls_controls,
          title = "Hasil Regresi WLS 2020",
          out = "WLS regression output 2020.htm")



