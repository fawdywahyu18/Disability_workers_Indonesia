library(dplyr)
library(rlist)
library(haven)
library(lazyeval)
library(writexl)
library(foreign)
library(stargazer)

setwd("E:/Kerja/Project Prakasa/2021/Sakernas")
data_raw <- read_dta(file="sakernas_20198_dta.dta")

# Creating household ID
data_raw <- data_raw %>%
  mutate(hhid=paste(kode_prov, kode_kab, klasifikas, id_nks, urutan, no_dsrt, sep=""))
data_raw$hhid <- as.numeric(data_raw$hhid)

# Check duplicates in hhid
id_occur <- data.frame(table(data_raw$hhid))
nrow(id_occur[id_occur$Freq > 1,]) # the number of obs which contains duplicated id

# Ensuring the individuals are 15 or older
data_1 <- data_raw %>%
  rename(age=b4_k8) %>%
  rename(fw=final_weig) %>%
  rename(urban=klasifikas)
data_1 <- data_1 %>%
  filter(age>=15)

#============================================Type of Acitivty================================================#

# Working individuals
data_1 <- data_1 %>%
  mutate(employed=ifelse(b5_r5a1==1 | b5_r6==1,1,0)) %>% # Employed individuals
  # mutate(employed=replace(employed, b5_r11==2, 0)) %>%
  mutate(caker1=ifelse(b5_r12a==1,1,0)) %>% # Looking for work
  mutate(caker2=ifelse(b5_r12b==1,1,0)) %>% # Establishing a new business/firm
  mutate(caker3=ifelse(b5_r17a==3,1,0)) %>% # Hopeless of job
  mutate(caker4=ifelse(b5_r17a == 1 | b5_r17a == 2,1,0)) # Have a job in future start

data_2 <- data_1 %>%
  mutate(dlabforce = ifelse(employed==1,1, 
                            ifelse(caker1==1,2,
                                   ifelse(caker2==1,3,
                                          ifelse(caker3==1,4,
                                                 ifelse(caker4==1,5,
                                                        ifelse(b5_r5a2==3,6,
                                                               ifelse(b5_r5a3==1,7,8))))))))
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
# should be equal to 128 755 27 (hasil backasting)
# check at the https://www.bps.go.id/indicator/6/1953/2/jumlah-dan-persentase-penduduk-bekerja-dan-pengangguran.html

# Core unemployment
data_2 <- data_2 %>%
  mutate(unemployed=ifelse(employed==0 & is.na(employed)!=TRUE,1,0)) %>%
  mutate(unemployed_core=unemployed)

data_test <- data_2 %>%
  filter(unemployed==1) %>%
  mutate(number_unemployed=sum(fw))
mean(data_test$number_unemployed)
# should be equal to 7 104 42 (hasil backasting)

# Broad unemployment and ever worked
data_2 <- data_2 %>%
  mutate(unemployment_broad=unemployed) %>%
  mutate(everworked=ifelse(b5_r47==1,1,0))

# student, housekeeping, and others category among not economically active
data_2 <- data_2 %>%
  mutate(student=ifelse(dlabforce==6 & labforce_broad==0, 1, 0)) %>%
  mutate(housekeeping=ifelse(dlabforce==7 & labforce_broad==0, 1, 0)) %>%
  mutate(others=ifelse(dlabforce==8 & labforce_broad==0, 1, 0))

# total number of working hours of all jobs during the previous week
data_2 <- data_2 %>%
  mutate(hour=ifelse(employed==1, b5_r44a, NA))

# Under employment
data_2 <- data_2 %>%
  mutate(underemp=ifelse(hour<35,1,NA)) %>%
  mutate(underemp=replace(underemp, hour==0 | (hour>=35 & is.na(hour)!=TRUE),0))


# Disability people
data_3 <- data_2 %>%
  mutate(disability1 = ifelse(b5_r4a>=2 & b5_r4a<=3,1,0)) %>%
  mutate(disability2 = ifelse(b5_r4b>=5 & b5_r4b<=6,1,0)) %>%
  mutate(disability3 = ifelse(b5_r4c>=2 & b5_r4c<=3,1,0)) %>%
  mutate(disability4 = ifelse(b5_r4d>=6 & b5_r4d<=6,1,0)) %>%
  mutate(disability5 = ifelse(b5_r4e>=2 & b5_r4e<=3,1,0)) %>%
  mutate(disability6 = ifelse(b5_r4f>=5 & b5_r4f<=6,1,0))

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
  mutate(disability_ability=ifelse(b5_r4a==2 | b5_r4b==5 | b5_r4c==2 | b5_r4d==5 | b5_r4e==2 | b5_r4f==5, 1, 
                                   ifelse(b5_r4a==3 | b5_r4b==6 | b5_r4c==3 | b5_r4d==6 | b5_r4e==3 | b5_r4f==6, 2, NA)))
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


#=============================================HH Member Char==================================================#

# Gender
data_3 <- data_3 %>%
  mutate(gender=ifelse(b4_k6==1,1,0))
# 0 "female" 1 "male"
data_3$gender <- labelled(data_3$gender, c(female=0, male=1))

# Education level
data_3 <- data_3 %>%
  mutate(educ_level=ifelse(b5_r1a>=2 & b5_r1a<=4,1,
                           ifelse(b5_r1a>=5 & b5_r1a<=7,2,
                                  ifelse(b5_r1a>=8 & b5_r1a<=11,3,
                                         ifelse(b5_r1a>=12 & b5_r1a<=16,4,0)))))
# 0 "tidak punya ijazah SD" 1 "SD" 2 "SMP" 
# 3 "SMA" 4 "Pendidikan Tinggi"
data_3$educ_level <- labelled(data_3$educ_level, c(no_diploma=0, primary_school=1,
                                        junior_hs=2, high_school=3,
                                        higher_education=4))

# Training/Kursus
data_3 <- data_3 %>%
  mutate(train=ifelse(b5_r1d==1 | b5_r1f==1,1,0)) %>%
  mutate(train=replace(train, b5_r1f==2, 0))
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

# Household Size (all)
data_3 <- data_3 %>%
  mutate(hhsize=ifelse(is.na(b2_r1)==0, b2_r1, NA), # Household size (all)
         hhsize5=ifelse(is.na(b2_r2)==0, b2_r2, NA)) # Household size > 5 years old

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
disability_gender = descriptive_table(data_4, disability, gender, labforce_broad==1)
k6 = disability_gender

# Disability based on provinces
disability_prov = descriptive_table(data_4, disability, kode_prov, labforce_broad==1)
kode_provinsi = disability_prov

# Disability based on islands or regions
disability_islands = descriptive_table(data_4, disability, d_prov, labforce_broad==1)
islands = disability_islands

# Disability based on their ability
disability_ability = descriptive_table(data_4, disability, disability_ability, labforce_broad==1)

# Disability per regions (only laborforce)
disability_regions <- data_4 %>%
  filter(labforce_broad==1) %>%
  group_by(kode_prov, kode_kab, disability) %>%
  summarise(number_of_people=sum(fw))
r3a_b = disability_regions
r3a_b

# age in disability group
disability_age <- data_4 %>%
  filter(labforce_broad==1) %>%
  mutate(mean_age_all=weighted.mean(age, fw)) %>%
  group_by(disability) %>%
  summarise(mean_age=weighted.mean(age, fw),
            mean_age_all=mean(mean_age_all))
k8 = disability_age

# Education level per disability group
disability_el <- descriptive_table(data_4, disability, educ_level, labforce_broad==1)
k9 = disability_el

# Education level per gender group
gender_el <- descriptive_table(data_4, gender, educ_level, labforce_broad==1)

# Disability workers per lapangan usaha
disability_lu <- descriptive_table(data_4, disability, b5_r20_kat, employed==1)
lapangan_usaha = disability_lu

# Employment per disability group
disability_employed = descriptive_table(data_4, disability, employed, is.na(employed)==0)
employed_people = disability_employed

# Duration of unemployed
data_4 <- data_3 %>%
  mutate(unemployed_duration=ifelse(b5_r10!=0 & unemployed==1,1,2))
data_4$unemployed_duration <- labelled(data_4$unemployed_duration, 
                                       c(zero_three_months=1, more_three_months=2))
disability_und <- descriptive_table(data_4, disability, unemployed_duration, labforce_broad==1)
r10 = disability_und

# Guarantee back to work
data_4 <- data_4 %>%
  mutate(backtowork_g=ifelse(b5_r11==1,1,0))
disability_bg <- descriptive_table(data_4, disability, backtowork_g, labforce_broad==1)
r11 = disability_bg

# Looking for job (1 week ago)
data_4 <- data_4 %>%
  mutate(looking_job_d=ifelse(dlabforce==2,1,0))
disability_lj <- descriptive_table(data_4, disability, looking_job_d, labforce_broad==1)
r12a = disability_lj

# Preparing for business (1 week ago)
data_4 <- data_4 %>%
  mutate(est_nb_d=ifelse(dlabforce==3,1,0))
disability_eb <- descriptive_table(data_4, disability, est_nb_d, labforce_broad==1)
r12b = disability_eb

# Duration for preparing business/looking job (in days)
data_4 <- data_4 %>%
  mutate(duration_lj_eb=ifelse(is.na(b5_r14)==FALSE & labforce_broad==1, b5_r14, NA))
disability_dlj <- data_4 %>%
    filter(labforce_broad==1 & is.na(duration_lj_eb)==FALSE) %>%
    mutate(mean_all=weighted.mean(duration_lj_eb,fw)) %>%
    group_by(disability) %>%
    summarise(mean_duration=weighted.mean(duration_lj_eb, fw)*30,
              mean_duration_all=mean(mean_all)*30)
r14 = disability_dlj

# Reason looking for job or preparing business
data_4 <- data_4 %>%
  mutate(reas_lj_eb=ifelse(labforce_broad==1 & is.na(duration_lj_eb)==0, b5_r15, NA)) %>%
  mutate(reas_lj_eb=replace(reas_lj_eb, b5_r15==0, NA))
disability_reas_lj_eb <- descriptive_table2(data_4, disability, backtowork_g, reas_lj_eb, is.na(reas_lj_eb)==0)
r15 = disability_reas_lj_eb

# Effort for looking a job or establishing a business (1 week or 1 month ago)
data_4 <- data_4 %>%
  mutate(effort_lj1=ifelse(b5_r16a==1,1,0)) %>%
  mutate(effort_lj2=ifelse(b5_r16b==3,1,0)) %>%
  mutate(effort_lj3=ifelse(b5_r16c==1,1,0)) %>%
  mutate(effort_lj4=ifelse(b5_r16d==3,1,0)) %>%
  mutate(effort_lj5=ifelse(b5_r16e==1,1,0)) %>%
  mutate(effort_lj6=ifelse(b5_r16f==3,1,0)) %>%
  mutate(effort_lj7=ifelse(b5_r16g==1,1,0)) %>%
  mutate(effort_lj8=ifelse(b5_r16h==3,1,0)) %>%
  mutate(effort_lj9=ifelse(b5_r16i==1,1,0))

data_4 <- data_4 %>%
  mutate(effort_lj=ifelse(effort_lj1==1,1,
                          ifelse(effort_lj2==1,2,
                                 ifelse(effort_lj3==1,3,
                                        ifelse(effort_lj4==1,4,
                                               ifelse(effort_lj5==1,5,
                                                      ifelse(effort_lj6==1,6,
                                                             ifelse(effort_lj7==1,7,
                                                                    ifelse(effort_lj8==1,8,
                                                                           ifelse(effort_lj9==1,9,NA)))))))))) %>%
  mutate(effort_lj=replace(effort_lj, labforce_broad==0, NA))

data_4$effort_lj <- labelled(data_4$effort_lj, c(bursa=1, kantor=2, iklan=3, iklan_mandiri=4,
                                                 kenalan=5, modal=6, tempat_usaha=7, siup=8,
                                                 others=9))
disability_ef_lj <- descriptive_table(data_4, disability, effort_lj, is.na(effort_lj)==0)
r16a_i = disability_ef_lj

# Reason for NOT looking for job or preparing business (1 week ago)
data_4 <- data_4 %>%
  mutate(reasNot_lj_eb=ifelse(looking_job_d==0 & est_nb_d==0, b5_r17a, NA)) %>%
  mutate(reasNot_lj_eb=replace(reasNot_lj_eb, b5_r17a==0, NA))
disability_reasNot <- descriptive_table(data_4, disability, reasNot_lj_eb, is.na(reasNot_lj_eb)==0)
r17a = disability_reasNot

# Willing to accept job offer
data_4 <- data_4 %>%
  mutate(will_aj=ifelse(b5_r18a==1,1,0))
disability_waj <- descriptive_table(data_4, disability, will_aj, labforce_broad==1)
r18a = disability_waj

# Training per disability group
disability_train <- descriptive_table(data_4, disability, train, labforce_broad==1)
r1d_f = disability_train

# Duration for preparing business/looking job as a main job (in days)
data_4 <- data_4 %>%
  mutate(durationm_lj_eb=ifelse(is.na(b5_r22b)==FALSE & labforce_broad==1, b5_r22b, NA))
disability_dljm <- data_4 %>%
  filter(labforce_broad==1 & is.na(durationm_lj_eb)==FALSE) %>%
  mutate(mean_all=weighted.mean(durationm_lj_eb,fw)) %>%
  group_by(disability) %>%
  summarise(mean_duration=weighted.mean(durationm_lj_eb, fw)*30,
            mean_duration_all=mean(mean_all)*30)
r22b = disability_dljm

# Working hour in a week
disability_wh <- data_4 %>%
  filter(is.na(hour)==FALSE) %>%
  mutate(mean_all=weighted.mean(hour, fw)) %>%
  group_by(disability) %>%
  summarise(mean_wh=weighted.mean(hour, fw),
            mean_wh_all=mean(mean_all))
r44a = disability_wh

# Primary Working status
data_4 <- data_4 %>%
  mutate(status_pw=ifelse(b5_r24a!=0 & employed==1, b5_r24a, NA))
data_4$status_pw <- labelled(data_4$status_pw, c(berusaha_sendiri=1,
                                                 berusaha_dibantu_btt=2,
                                                 berusaha_dibantu_bt=3,
                                                 buruh_karyawan_pegawai=4,
                                                 pb_pertanian=5,
                                                 pb_nonpertanian=6,
                                                 pk_td=7))
disability_ws <- descriptive_table(data_4, disability, status_pw, is.na(status_pw)==FALSE)
r24a = disability_ws
gender_ws <- descriptive_table(data_4, gender, status_pw, is.na(status_pw)==FALSE)

# Working using internet
data_4 <- data_4 %>%
  mutate(internet_working=ifelse(b5_r25b==1 & employed==1,1,0))
disability_iw <- descriptive_table(data_4, disability, internet_working, is.na(internet_working)==FALSE)
r25b = disability_iw

# How the internet is being used (selling things trough email/sosmed/website/apps)
data_4 <- data_4 %>%
  mutate(sell_em=ifelse(employed==1 & internet_working==1 & (b5_r25c3==1 | b5_r25c4==3),1,0))
disability_si <- descriptive_table(data_4, disability, sell_em, is.na(sell_em)==FALSE)
r25c3_4 = disability_si

# Wage and salary for primary work (a month ago)
data_4 <- data_4 %>%
  mutate(wage=ifelse(employed==1 & (b5_r24a==1 | b5_r24a==5 | b5_r24a==6),b5_r28b1,NA)) %>%
  mutate(wage=replace(wage, employed==1 & b5_r24a==4 & is.na(wage)==FALSE, b5_r28c1))
disability_wage <- data_4 %>%
  filter(is.na(wage)==FALSE) %>%
  mutate(mean_all=weighted.mean(wage, fw)) %>%
  group_by(disability) %>%
  summarise(mean_wage=weighted.mean(wage, fw),
            mean_all=mean(mean_all))
r28b_c = disability_wage

# Wage and salary for primary work (1 month ago) per gender group
gender_wage <- data_4 %>%
  filter(is.na(wage)==FALSE) %>%
  mutate(mean_all=weighted.mean(wage, fw)) %>%
  group_by(gender) %>%
  summarise(mean_wage=weighted.mean(wage, fw),
            mean_all=mean(mean_all))

# Contract status
data_4 <- data_4 %>%
  mutate(contract_status=ifelse(employed==1 & (b5_r24a==4 | b5_r24a==5 | b5_r24a==6), b5_r31, NA))
disability_cs <- descriptive_table(data_4, disability, contract_status, is.na(contract_status)==0)
r31 = disability_cs

# Workers Union
data_4 <- data_4 %>%
  mutate(wu=ifelse(is.na(contract_status)==0 & b5_r32!=3, b5_r32, NA)) %>%
  mutate(wu=replace(wu, b5_r32==2,0))
disability_wu <- descriptive_table2(data_4, disability, wu, gender, is.na(wu)==0)
r32 = disability_wu

# Institution Name
data_4 <- data_4 %>%
  mutate(inst_name=ifelse(b5_r34==0 & employed==0, NA, b5_r34))
disability_in <- descriptive_table(data_4, disability, inst_name, is.na(inst_name)==0)
r34 = disability_in

# Constanly commuting
data_4 <- data_4 %>%
  mutate(commute=ifelse(b5_r35==3 & employed==1 &(b4_k3==9 | b4_k3==10), NA, b5_r36b)) %>%
  mutate(commute_dist=ifelse(commute==1, b5_r36c, NA)) %>%
  mutate(commute_time=ifelse(commute==1, b5_r36d, NA)) %>%
  mutate(commute_mode=ifelse(commute==1, b5_r36e, NA))

disability_com <- descriptive_table(data_4, disability, commute, is.na(commute)==0)
disability_comdist <- descriptive_table(data_4, disability, commute_dist, is.na(commute_dist)==0)
disability_comtime <- descriptive_table(data_4, disability, commute_time, is.na(commute_time)==0)
disability_commode <- descriptive_table(data_4, disability, commute_mode, is.na(commute_mode)==0)
r36b = disability_com
r36c = disability_comdist
r36d = disability_comtime
r36e = disability_commode

# Underemployment (less than 40 hours working hour in a week)
data_4 <- data_4 %>%
  mutate(under_reas=ifelse(employed==1 & hour<40 & b5_r44b>=40 & b5_r46!=0, b5_r46, NA))
disability_reasund <- descriptive_table(data_4, disability, under_reas, is.na(under_reas)==0)
r46 = disability_reasund

# Ever worked
disability_ew <- descriptive_table(data_4, disability, everworked, is.na(everworked)==0)
r47 = disability_ew

# Ever stopped
data_4 <- data_4 %>%
  mutate(everstopped=ifelse(b5_r48!=0, b5_r48, NA))
disability_es <- descriptive_table(data_4, disability, everstopped, is.na(everstopped)==0)
r48 = disability_es

# Reason to stop working
data_4 <- data_4 %>%
  mutate(reas_stop=ifelse(b5_r49==0, NA, b5_r49)) %>%
  mutate(reas_stop=replace(reas_stop, labforce_broad==0, NA))
disability_rs <- descriptive_table(data_4, disability, reas_stop, is.na(reas_stop)==0)
r49 = disability_rs


# Export to excel
disability_excel = list(hhmember=hhmember, kode_provinsi=kode_provinsi, islands=islands, urban=urban, 
                        disability_ability=disability_ability, employed_people=employed_people, lapangan_usaha=lapangan_usaha,
                        k6=k6, k8=k8, k9=k9, r10=r10, r11=r11, 
                        r12a=r12a, r12b=r12b, r14=r14, r15=r15, r16a_i=r16a_i, r17a=r17a, r18a=r18a,
                        r1d_f=r1d_f, r22b=r22b, r24a=r24a, r25b=r25b, r25c3_4=r25c3_4, r28b_c=r28b_c, 
                        r31=r31, r32=r32, r34=r34, r36b=r36b, r36c=r36c, r36d=r36d, r36e=r36e, r3a_b=r3a_b, 
                        r44a=r44a, r46=r46, r47=r47, r48=r48, r49=r49)

write_xlsx(disability_excel, "disability_excel_2019.xlsx")

gender_excel = list(gender_el, gender_wage, gender_ws)
write_xlsx(gender_excel, "gender_excel_2019.xlsx")

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
         lapangan_usaha=factor(b5_r20_kat),
         disability = factor(disability),
         employed = factor(employed),
         working_status = factor(status_pw))

# Logit regression for employed opportunity
logit_controls <- glm(employed ~ disability_type + gender + educ_level + train + urban + 
                      disability_ability + age + d_prov,
                    data=data_reg, family = "binomial")
logit_wc <- glm(employed ~ disability_type,
                data=data_reg, family = "binomial")

summary(logit_wc)
summary(logit_controls)

stargazer(logit_wc, logit_controls,
          title = "Hasil Regresi Logit 2019",
          out = "logit regression output 2019.htm")

# Weighted least square for wages
employed_only = data_reg %>%
  filter(employed==1)
wls_wc <- lm(wage ~ disability_type, data = employed_only, weights=fw)
summary(wls_wc)
wls_controls <- lm(wage ~ disability_type + gender + educ_level + train + urban + 
                     disability_ability + age + d_prov + lapangan_usaha,
                   data = employed_only, weights = fw)
summary(wls_controls)

stargazer(wls_wc, wls_controls,
          title = "Hasil Regresi WLS 2019",
          out = "WLS regression output 2019.htm")



