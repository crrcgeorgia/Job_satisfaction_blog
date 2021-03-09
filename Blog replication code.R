library(haven)
library(ggplot2)
library(margins)
library(survey)
library(sjPlot)
library(ggeffects)
library(questionr)
library(ggpubr)
library(MASS)
library(stats)
library(wCorr)
library(EdSurvey)
library(WGCNA)

setwd("D:/Makhare Files/Makhare/Blogs/job satisfaction")



cb19<-read_dta("CB2019_Georgia_response_30Jan2020 (1).dta")
names(cb19)


table(cb19$STRATUM)
#STRATUM
cb19$STRATUM_new<-cb19$STRATUM
cb19$STRATUM_new<-factor(cb19$STRATUM_new, levels = c(1,2,3) , labels = c("Capital", "Urban", "Rural"))
table(cb19$STRATUM_new)

#religion

cb19$RELGION_new<-cb19$RELGION
cb19$RELGION_new[cb19$RELGION_new!=3]<-1
cb19$RELGION_new[cb19$RELGION_new==3]<-0

cb19$RELGION_new<-factor(cb19$RELGION_new, levels = c(0,1) , labels = c("Orthodox", "NonOrthodox"))
table(cb19$RELGION_new)


#sex

table(cb19$RESPSEX)
cb19$RESPSEX_new<-cb19$RESPSEX
cb19$RESPSEX_new<-factor(cb19$RESPSEX_new, levels = c(1,2), labels = c("Male", "Female"))
table(cb19$RESPSEX_new)

##agegroup

table(cb19$RESPAGE)
cb19$RESPAGE_new<-cb19$RESPAGE
cb19$RESPAGE_new[cb19$RESPAGE_new>55]<-3
cb19$RESPAGE_new[cb19$RESPAGE_new>35]<-2
cb19$RESPAGE_new[cb19$RESPAGE_new>17]<-1
cb19$RESPAGE_new<-factor(cb19$RESPAGE_new, levels = c(1,2,3), labels = c("18-35", "36-55", "56+"))
table(cb19$RESPAGE_new)

##Education
table(cb19$RESPEDU)
cb19$RESPEDU_new<-cb19$RESPEDU
cb19$RESPEDU_new[cb19$RESPEDU_new<0]<-NA
cb19$RESPEDU_new[cb19$RESPEDU_new<5]<-1
cb19$RESPEDU_new[cb19$RESPEDU_new==5]<-2
cb19$RESPEDU_new[cb19$RESPEDU_new>5]<-3
cb19$RESPEDU_new<-factor(cb19$RESPEDU_new, levels = c(1,2,3), labels = c("Secondary or lower", "Technical", "Tertiary"))

table(cb19$RESPEDU_new)


#employment

table(cb19$RESPEMP)
cb19$RESPEMP_new<-cb19$RESPEMP
cb19$RESPEMP_new[cb19$RESPEMP_new<0]<-NA
cb19$RESPEMP_new<-factor(cb19$RESPEMP_new, levels = c(0,1), labels = c("Not employed", "Employed"))
table(cb19$RESPEMP_new)



#ETHNICITY

cb19$ETHNIC_new<-cb19$ETHNIC
cb19$ETHNIC_new[cb19$ETHNIC_new<0]<-NA
cb19$ETHNIC_new[cb19$ETHNIC_new>3]<-4
cb19$ETHNIC_new<-factor(cb19$ETHNIC_new, levels = c(1,2,3,4), labels = c("Armenian", "Azerbaijani", "Georgian", "Other ethnic"))

table(cb19$ETHNIC_new)
#
cb19$ETHNIC_new2<-cb19$ETHNIC
cb19$ETHNIC_new2[cb19$ETHNIC_new2<0]<-NA
cb19$ETHNIC_new2[cb19$ETHNIC_new2>3]<-1
cb19$ETHNIC_new2[cb19$ETHNIC_new2==2]<-1
cb19$ETHNIC_new2[cb19$ETHNIC_new2==3]<-2

cb19$ETHNIC_new2<-factor(cb19$ETHNIC_new2, levels = c(1,2), labels = c("NonGeorgian", "Georgian"))

table(cb19$ETHNIC_new2)


### knowledge of computer and languages
table(cb19$COMPABL)

cb19$COMPABL_new<-cb19$COMPABL
cb19$COMPABL_new[cb19$COMPABL_new<0]<-NA
cb19$COMPABL_new<-factor(cb19$COMPABL_new, levels = c(1,2,3,4), labels = c("CompNo basic", "CompBeginner", "CompIntermediate", "CompAdvanced"))

table(cb19$COMPABL_new)

#numeric version
cb19$COMPABL_new_num<-cb19$COMPABL
cb19$COMPABL_new_num[cb19$COMPABL_new_num<0]<-NA

table(cb19$COMPABL_new_num)

# knowledge of english

table(cb19$KNOWENG)

cb19$KNOWENG_new<-cb19$KNOWENG
cb19$KNOWENG_new[cb19$KNOWENG_new<0]<-NA
cb19$KNOWENG_new<-factor(cb19$KNOWENG_new, levels = c(1,2,3,4), labels = c("EngNo basic", "EngBeginner", "EngIntermediate", "EngAdvanced"))

table(cb19$KNOWENG_new)

cb19$KNOWENG_new_num<-cb19$KNOWENG #numeric version
cb19$KNOWENG_new_num[cb19$KNOWENG_new_num<0]<-NA
table(cb19$KNOWENG_new_num)




## job satisfaction
table(cb19$JOBSARFN)
cb19$JOBSARFN_new<-cb19$JOBSARFN
cb19$JOBSARFN_new[cb19$JOBSARFN_new<=-1]<-NA
cb19$JOBSARFN_new[cb19$JOBSARFN_new<=2]<-0
cb19$JOBSARFN_new[cb19$JOBSARFN_new==3]<-1
cb19$JOBSARFN_new[cb19$JOBSARFN_new>2]<-2
table(cb19$JOBSARFN_new)

cb19$JOBSARFN_new_fact<-cb19$JOBSARFN_new
cb19$JOBSARFN_new_fact<-factor(cb19$JOBSARFN_new_fact, levels = c(0,1,2), labels = c("Dissatisfied", "Average satisfaction", "Satisfied"))

prop.table(xtabs(INDWT~JOBSARFN_new_fact, data=cb19, na.action = "na.omit"))




#job satisfaction table without -7
cb19$JOBSARFN_missing<-cb19$JOBSARFN
cb19$JOBSARFN_missing[cb19$JOBSARFN_missing<=-1]<-NA
cb19$JOBSARFN_missing<-factor(cb19$JOBSARFN_missing, levels = c(1,2,3,4,5), labels = c("Very dissatisfied", "Somewhat dissatisfied", "Average satisfaction", "Somewhat satisfied", "Very satisfied"))
prop.table(xtabs(INDWT~JOBSARFN_missing, data=cb19, na.action = "na.omit"))


summary(cb19$WORKYRS)
cb19$WORKYRS_r<-cb19$WORKYRS
cb19$WORKYRS_r[cb19$WORKYRS_r<=-1]<-NA

table(cb19$WORKTYP)
cb19$WORKTYP_r<-cb19$WORKTYP
cb19$WORKTYP_r[cb19$WORKTYP_r<=-1]<-NA
cb19$WORKTYP_r[cb19$WORKTYP_r==8]<-NA
cb19$WORKTYP_r[cb19$WORKTYP_r<=4]<-0
cb19$WORKTYP_r[cb19$WORKTYP_r>=5]<-1
cb19$WORKTYP_r<-factor(cb19$WORKTYP_r, levels = c(0,1), labels = c("Works in the private sector", "Does not work in the private sector"))
table(cb19$WORKTYP_r)

#numeric version:
cb19$WORKTYP_r_num<-cb19$WORKTYP
cb19$WORKTYP_r_num[cb19$WORKTYP_r_num<=-1]<-NA
cb19$WORKTYP_r_num[cb19$WORKTYP_r_num==8]<-NA
cb19$WORKTYP_r_num[cb19$WORKTYP_r_num<=4]<-0
cb19$WORKTYP_r_num[cb19$WORKTYP_r_num>=5]<-1
table(cb19$WORKTYP_r_num)




table(cb19$RESPEMP)

table(cb19$PERSINC)
cb19$PERSINC_r<-cb19$PERSINC
cb19$PERSINC_r[cb19$PERSINC_r==2]<-1
cb19$PERSINC_r[cb19$PERSINC_r==3]<-1
cb19$PERSINC_r[cb19$PERSINC_r==4]<-1
cb19$PERSINC_r[cb19$PERSINC_r<0]<-NA
cb19$PERSINC_r[cb19$PERSINC_r==1]<-99
cb19$PERSINC_r[cb19$PERSINC_r==9]<-0
cb19$PERSINC_r[cb19$PERSINC_r==8]<-1
cb19$PERSINC_r[cb19$PERSINC_r==7]<-2
cb19$PERSINC_r[cb19$PERSINC_r==6]<-3
cb19$PERSINC_r[cb19$PERSINC_r==5]<-4
cb19$PERSINC_r[cb19$PERSINC_r==99]<-5
cb19$PERSINC_r[cb19$PERSINC_r==0]<-1



cb19$PERSINC_r<-factor(cb19$PERSINC_r, levels = c(1,2,3,4,5), labels = c("From 0 to 50 USD", "From USD 51 to 100", "From USD 101 to 250", "From USD 251 to 400", "More than 401 USD"))
table(cb19$PERSINC_r)


#Variable numeric version
cb19$PERSINC_r_num<-cb19$PERSINC
cb19$PERSINC_r_num[cb19$PERSINC_r_num==2]<-1
cb19$PERSINC_r_num[cb19$PERSINC_r_num==3]<-1
cb19$PERSINC_r_num[cb19$PERSINC_r_num==4]<-1
cb19$PERSINC_r_num[cb19$PERSINC_r_num<0]<-NA
cb19$PERSINC_r_num[cb19$PERSINC_r_num==1]<-99
cb19$PERSINC_r_num[cb19$PERSINC_r_num==9]<-0
cb19$PERSINC_r_num[cb19$PERSINC_r_num==8]<-1
cb19$PERSINC_r_num[cb19$PERSINC_r_num==7]<-2
cb19$PERSINC_r_num[cb19$PERSINC_r_num==6]<-3
cb19$PERSINC_r_num[cb19$PERSINC_r_num==5]<-4
cb19$PERSINC_r_num[cb19$PERSINC_r_num==99]<-5
cb19$PERSINC_r_num[cb19$PERSINC_r_num==0]<-1
table(cb19$PERSINC_r_num)


#check 1+2 categories together

cb19$PERSINC_dkra<-cb19$PERSINC
cb19$PERSINC_dkra[cb19$PERSINC_dkra==2]<-1
cb19$PERSINC_dkra[cb19$PERSINC_dkra==3]<-1
cb19$PERSINC_dkra[cb19$PERSINC_dkra==4]<-1

cb19$PERSINC_dkra[cb19$PERSINC_dkra==1]<-99
cb19$PERSINC_dkra[cb19$PERSINC_dkra==9]<-0
cb19$PERSINC_dkra[cb19$PERSINC_dkra==8]<-1
cb19$PERSINC_dkra[cb19$PERSINC_dkra==7]<-2
cb19$PERSINC_dkra[cb19$PERSINC_dkra==6]<-3
cb19$PERSINC_dkra[cb19$PERSINC_dkra==5]<-4
cb19$PERSINC_dkra[cb19$PERSINC_dkra==99]<-5
cb19$PERSINC_dkra[cb19$PERSINC_dkra==0]<-1
cb19$PERSINC_dkra[cb19$PERSINC_dkra<0]<-0
cb19$PERSINC_dkra<-factor(cb19$PERSINC_dkra, levels = c(0,1,2,3,4,5), labels = c("RA/DK","From 0 to 50 USD", "From USD 51 to 100", "From USD 101 to 250", "From USD 251 to 400", "More than 401 USD"))
table(cb19$PERSINC_dkra)











#Wealth (proxied through an index of ownership of different durable goods);
table(cb19$OWNCOTV)
cb19$OWNCOTV_r<-cb19$OWNCOTV
cb19$OWNCOTV_r[cb19$OWNCOTV_r<=-1]<-NA
cb19$OWNDIGC_r<-cb19$OWNDIGC
cb19$OWNDIGC_r[cb19$OWNDIGC_r<=-1]<-NA
cb19$OWNWASH_r<-cb19$OWNWASH
cb19$OWNWASH_r[cb19$OWNWASH_r<=-1]<-NA
cb19$OWNFRDG_r<-cb19$OWNFRDG
cb19$OWNFRDG_r[cb19$OWNFRDG_r<=-1]<-NA
cb19$OWNAIRC_r<-cb19$OWNAIRC
cb19$OWNAIRC_r[cb19$OWNAIRC_r<=-1]<-NA
cb19$OWNCARS_r<-cb19$OWNCARS
cb19$OWNCARS_r[cb19$OWNCARS_r<=-1]<-NA
cb19$OWNLNDP_r<-cb19$OWNLNDP
cb19$OWNLNDP_r[cb19$OWNLNDP_r<=-1]<-NA
cb19$OWNCELL_r<-cb19$OWNCELL
cb19$OWNCELL_r[cb19$OWNCELL_r<=-1]<-NA


cb19$wealth<-(cb19$OWNCOTV_r+
                cb19$OWNDIGC_r+
                cb19$OWNWASH_r+
                cb19$OWNFRDG_r+
                cb19$OWNAIRC_r+
                cb19$OWNCARS_r+
                cb19$OWNLNDP_r+
                cb19$OWNCELL_r)
table(cb19$wealth)



# attendence on the religious services
table(cb19$RELSERV)
cb19$RELSERV_new<-cb19$RELSERV

table(cb19$RELSERV_new)
cb19$RELSERV_new[cb19$RELSERV_new<0]<-NA
cb19$RELSERV_new[cb19$RELSERV_new<4]<-1
cb19$RELSERV_new[cb19$RELSERV_new==4]<-2
cb19$RELSERV_new[cb19$RELSERV_new==5]<-3
cb19$RELSERV_new[cb19$RELSERV_new==6]<-4
cb19$RELSERV_new[cb19$RELSERV_new==7]<-5
cb19$RELSERV_new<-factor(cb19$RELSERV_new, levels = c(1,2,3,4,5), labels = c("Once a week or more often" ,"At least once a month" ,"Only on special religious holidays" ,"Less often" ,"Never"))

# marital status
table(cb19$RESPMAR)
cb19$RESPMAR_new<-cb19$RESPMAR
table(cb19$RESPMAR_new)
cb19$RESPMAR_new[cb19$RESPMAR_new<0]<-NA
cb19$RESPMAR_new[cb19$RESPMAR_new==3 | cb19$RESPMAR_new==4 | cb19$RESPMAR_new==5]<-2
cb19$RESPMAR_new[cb19$RESPMAR_new>2]<-3
cb19$RESPMAR_new<-factor(cb19$RESPMAR_new, levels = c(1,2,3), labels = c("Never Married", "Married or cohabiting", "No longer married"))
prop.table(xtabs(INDWT~RESPMAR_new, data=cb19, na.action = "na.omit"))

cb19$RESPMAR_new_2<-cb19$RESPMAR
table(cb19$RESPMAR_new_2)
cb19$RESPMAR_new_2[cb19$RESPMAR_new_2<0]<-NA
cb19$RESPMAR_new_2[cb19$RESPMAR_new_2==3 | cb19$RESPMAR_new_2==4 | cb19$RESPMAR_new_2==5]<-2
cb19$RESPMAR_new_2[cb19$RESPMAR_new_2>2]<-1
cb19$RESPMAR_new_2<-factor(cb19$RESPMAR_new_2, levels = c(1,2), labels = c("Not in marriage", "Married or cohabiting"))
prop.table(xtabs(INDWT~RESPMAR_new_2, data=cb19, na.action = "na.omit"))

##_________________________________________________________MODEL________________________________________________________----
cb_svy<-svydesign(~PSU, strata = cb19$SUBSTRATUM, weights = cb19$INDWT, data = cb19)

#_____________________________________Demographic variables______________________________________
# OLR regression - SVYOLR
# survey OLR method for check statistical significance
dem_model_olr<-svyolr(JOBSARFN_new_fact~RESPEDU_new+
                        STRATUM_new+
                        ETHNIC_new2+
                        RESPAGE_new+
                        RESPSEX_new+
                        RESPMAR_new, design = cb_svy)
summary(dem_model_olr)
# svyolr method showed  that there are statistical significance (t value > |1.96| ) in SEX and EDUCATION variables.


## OLR regression - POLR
# POLR method for calculate estimations
cb_svy_1<-cb_svy$variables
cb_svy_1$normalized<-(cb_svy_1$INDWT/((sum(cb_svy_1$INDWT)/length(cb_svy_1$INDWT))))
dem_model_olr_estim<-polr(JOBSARFN_new_fact~RESPEDU_new+
                            STRATUM_new+
                            ETHNIC_new2+
                            RESPAGE_new+
                            RESPSEX_new+
                            RESPMAR_new, weights=normalized, data=cb_svy_1)

summary(dem_model_olr_estim)
edu_olr_plot<-plot(ggemmeans(dem_model_olr_estim, terms = c("RESPEDU_new")))
sex_olr_plot<-plot(ggemmeans(dem_model_olr_estim, terms = c("RESPSEX_new")))
mar_olr_plot<-plot(ggemmeans(dem_model_olr_estim, terms = c("RESPMAR_new")))

print(ggemmeans(dem_model_olr_estim,"RESPEDU_new"))
print(ggemmeans(dem_model_olr_estim, "RESPSEX_new"))
print(ggemmeans(dem_model_olr_estim, "RESPMAR_new"))
print(ggemmeans(dem_model_olr_estim, terms = c("RESPMAR_new", "RESPSEX_new")))
plot(ggemmeans(dem_model_olr_estim, terms = c("RESPSEX_new", "RESPMAR_new_2")))

ggarrange(edu_olr_plot, sex_olr_plot, mar_olr_plot,
          ncol = 1, nrow = 3)


#________________________________________Non-demographic variables_________________________

#Together social & economical variables with demographics. Check significance
check<-svyolr(JOBSARFN_new_fact~WORKTYP_r+
              COMPABL_new+
              KNOWENG_new+
              wealth+ RELSERV_new+
              PERSINC_r+RESPEDU_new+
                STRATUM_new+
                ETHNIC_new2+
                RESPSEX_new+
                RESPAGE_new+RESPMAR_new, design=cb_svy)
summary(check)
# Work type, Knowledge of English and personal income make sense.
### _____________________________________These 3 factors individually POLR
wtyp<-polr(JOBSARFN_new_fact~WORKTYP_r+
      RESPEDU_new+
      STRATUM_new+
      ETHNIC_new2+
      RESPSEX_new+
      RESPAGE_new+RESPMAR_new, weights=normalized, data=cb_svy_1)

summary(wtyp)


wtyp_olr_plot<-plot(ggemmeans(wtyp, terms = c("WORKTYP_r")), show.y.title = FALSE, show.x.title = FALSE)
print(ggemmeans(wtyp, "WORKTYP_r"))


eng<-polr(JOBSARFN_new_fact~KNOWENG_new+
             RESPEDU_new+
             STRATUM_new+
             ETHNIC_new2+
             RESPSEX_new+
             RESPAGE_new+RESPMAR_new, weights=normalized, data=cb_svy_1)

summary(eng)

eng_olr_plot<-plot(ggemmeans(eng, terms = c("KNOWENG_new")), show.y.title = FALSE, show.x.title = FALSE)
print(ggemmeans(eng, "KNOWENG_new"))

inc<-polr(JOBSARFN_new_fact~PERSINC_r+
      RESPEDU_new+
      STRATUM_new+
      ETHNIC_new2+
      RESPSEX_new+
      RESPAGE_new+RESPMAR_new, weights=normalized, data=cb_svy_1)
summary(inc)
inc_olr_plot<-plot(ggemmeans(inc, terms = c("PERSINC_r")), show.y.title = FALSE, show.x.title = FALSE)
print(ggemmeans(inc, "PERSINC_r"))

ggarrange(wtyp_olr_plot, eng_olr_plot, inc_olr_plot,
          ncol = 1, nrow = 3)





#Check correlation between income and English knowledge / work sector
cor.test(cb_svy_1$PERSINC_r_num, cb_svy_1$KNOWENG_new_num,  method = "spearman", use = "complete.obs")
cor.test(cb_svy_1$PERSINC_r_num, cb_svy_1$WORKTYP_r_num,  method = "spearman", use = "complete.obs")
cor.test(cb_svy_1$PERSINC_r_num, cb_svy_1$COMPABL_new_num,  method = "spearman", use = "complete.obs")
cor.test(cb_svy_1$KNOWENG_new_num, cb_svy_1$COMPABL_new_num,  method = "spearman", use = "complete.obs")



