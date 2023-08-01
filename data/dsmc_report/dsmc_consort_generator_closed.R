#' @section Development:
#' 7.20.22 Initial build was about 7/10/22. Decided to split into two consort diagrams, which was accomplished
#' today. JC \cr
#' 


library(gt)
library(gtsummary)
library(ggpmisc)


##### Screening Selection #####
screened <- comet_scienctist_markdown %>% filter(!is.na(scrn_date))
not_screened <- anti_join(comet_scienctist_markdown, screened, by = "record_id")

##### Screening Status ######
#All screened participants are filtered into statuses
#A dataframe is kept (remaining) that stores participants
#Participants are removed from the remaining df as they are given a screening status
#Any participants left in the remaining df are likely errors and need to be looked into
screen_failed <- screened %>% filter(scrn_elig == 1)
remaining <- anti_join(screened, screen_failed, by = "record_id")

eligible <- remaining %>% filter(scrn_elig == 0)
remaining <- anti_join(remaining, eligible, by = "record_id")

on_hold <- remaining %>% filter(scrn_elig == 2 )
remaining <- anti_join(remaining, on_hold, by = "record_id")

still_screening <- remaining %>% filter(!is.na(scrn_date) & is.na(scrn_elig))
remaining <- anti_join(remaining, still_screening, by = "record_id")


#### Screen Fail Reasons ####
screen_fail_reason <- screen_failed %>%
  mutate(age_inelig = case_when(scrn_age >= 81 ~ 1,
                                TRUE ~ 0)) %>%
  mutate(tics_inelig = case_when(scrn_tics_score <= 25 ~ 1,
                                 TRUE ~ 0)) %>%
  mutate(scrn_reschedule = case_when(scrn_reschedule == 1 ~ 0,
                                     scrn_reschedule == 2 ~ 1)) %>%
  mutate(scrn_drop_reason = as.numeric(scrn_drop_reason)) %>%
  mutate(`Reason` = case_when(age_inelig == 1 ~ "Age",
                              tics_inelig == 1 ~ "TICS < 25",
                              scrn_reschedule == 1 ~ "Not Interested - No Reason",
                              scrn_drop_reason == 1 ~ "Too Busy",
                              scrn_drop_reason == 2 ~ "Distance to Study Activities",
                              scrn_drop_reason == 3 ~ "Discomfort with Procedures",
                              scrn_drop_reason == 4 ~ "Disinterest in Study Premise",
                              scrn_drop_reason == 6 | scrn_drop_reason == 8 ~ "Health Concerns - Personal",
                              scrn_drop_reason == 7 ~ "Health Concerns - Family",
                              scrn_drop_reason == 9 ~ "COVID Concerns",
                              scrn_drop_reason == 10 ~ "Lost to follow-up",
                              scrn_drop_reason == 5 ~ "Other",
                              scrn_en_fluence == 1 ~ "Not English Speaking",
                              scrn_out == 1 ~ "Travel more than 4 weeks",
                              scrn_transport == 1 ~ "Reliable Transport",
                              scrn_joint_pain == 1 ~ "Joint Pain",
                              scrn_phy_move_free == 1 ~ "Assistive Walking Device",
                              scrn_overhead == 1 ~ "Joint Pain",
                              scrn_mri_attempt == 1 ~ "MRI Attempt",
                              scrn_mri_implant == 1 ~ "MRI Eligibility",
                              scrn_mri_metal == 1 ~ "MRI Eligibility",
                              scrn_med_subst == 1 ~ "Substance Abuse",
                              scrn_cancer_treat == 1 ~ "Cancer",
                              scrn_med_diab == 1 ~ "Insulin",
                              scrn_heart_treat == 1 ~ "Cardiac Issue",
                              scrn_med_neuro == 1 ~ "Neurological Issue",
                              scrn_htn_treat_ch == 1 ~ "Blood Pressure Meds",
                              scrn_rapamod == 1 ~ "Too Active",
                              scrn_rapavig == 1 ~ "Too Active",
                              scrn_rapa_st == 1 ~ "Too Active",
                              scrn_pcp_clear_response == 1 ~ "No Physician Clearance")) %>%
  select(Reason)

phone_screen_mistakes <- screen_fail_reason %>%
  filter(is.na(Reason))



screen_fail_gt <- gtsummary::tbl_summary(screen_fail_reason) 
screen_fail_reason_table <- screen_fail_gt$table_body %>%
  select(label, stat_0) %>%
  filter(label != "Reason") %>%
  #rename(Reason = "label", "n (%)" = stat_0) %>%
  unite(reason, c("label","stat_0"), sep = " - " ) 



string <- ""
for(i in 1:nrow(screen_fail_reason_table)) {
  temp <- paste0(as.character(screen_fail_reason_table[i,]), "\n")
  string <- paste0(string,temp)
}

#### Second part of consort Diagram #####
consented <- eligible %>% filter(!is.na(consent_1_date))
remaining_2 <- anti_join(eligible, consented, by = "record_id")

scheduled <- remaining_2 %>% filter(is.na(consent_1_date) & !is.na(comet_baselinecogsched))
remaining_2 <- anti_join(remaining_2, scheduled, by = "record_id")

to_be_scheduled <- remaining_2 %>% filter((is.na(scrn_elig) | scrn_elig ==  0) & !is.na(scrn_date) & is.na(comet_baselinecogsched))
remaining_2 <- anti_join(remaining_2, to_be_scheduled, by = "record_id")


##### Third part of consort diagram
randomized <- consented %>% filter(!is.na(group))
remaining_3 <- anti_join(consented, randomized, by = "record_id")

screen_failed_after_consent <- remaining_3 %>% filter(baseline_status == 3)
remaining_3 <- anti_join(remaining_3, screen_failed_after_consent, by = "record_id")

in_baseline <- remaining_3 %>% filter(!is.na(consent_1_date) & is.na(group))
remaining_3 <- anti_join(remaining_3, in_baseline, by = "record_id")


screen_fail_after_consent_reason <- screen_failed_after_consent %>%
  mutate(`Reason` = case_when(baseline_scrnfail_ie == 1 ~ "Age",
                              baseline_scrnfail_ie == 2 ~ "Not able to speak and read English",
                              baseline_scrnfail_ie == 3 ~ "Not cleared by PCP or healthcare provider",
                              baseline_scrnfail_ie == 4 ~ "No reliable means of transport",
                              baseline_scrnfail_ie == 5 ~ "Adjudicated not eligible",
                              baseline_scrnfail_ie == 6 ~ "Travel more than 4 weeks",
                              baseline_scrnfail_ie == 7 ~ "Use of assistive device for ambulation",
                              baseline_scrnfail_ie == 8 ~ "Joint pain",
                              baseline_scrnfail_ie == 9 ~ "MRI contraindication",
                              baseline_scrnfail_ie == 10 ~ "Substance abuse",
                              baseline_scrnfail_ie == 11 ~ "Cancer",
                              baseline_scrnfail_ie == 12 ~ "Insulin",
                              baseline_scrnfail_ie == 13 ~ "Heart disease",
                              baseline_scrnfail_ie == 14 ~ "Neurological condition",
                              baseline_scrnfail_ie == 15 ~ "Hx of head injury",
                              baseline_scrnfail_ie == 16 ~ "Change in blood pressure meds",
                              baseline_scrnfail_ie == 17 ~ "Too active",
                              baseline_scrnfail_ie == 18 ~ "Unsafe condition as determined \n by medical monitor",
                              baseline_scrnfail_nocont == 1 ~ "Too busy",
                              baseline_scrnfail_nocont == 2 ~ "Distance to study activities",
                              baseline_scrnfail_nocont == 3 ~ "Discomfort with procedures",
                              baseline_scrnfail_nocont == 4 ~ "Disinterest in study premise",
                              baseline_scrnfail_nocont == 5 ~ "Other",
                              baseline_scrnfail_nocont == 7 ~ "Health concerns - family",
                              baseline_scrnfail_nocont == 8 ~ "Health concerns - personal",
                              baseline_scrnfail_nocont == 9 ~ "COVID concerns"
  )) %>%
  select(Reason)

screen_fail_gt_2 <- gtsummary::tbl_summary(screen_fail_after_consent_reason) 
screen_fail_reason_table_2 <- screen_fail_gt_2$table_body %>%
  select(label, stat_0) %>%
  filter(label != "Reason") %>%
  #rename(Reason = "label", "n (%)" = stat_0) %>%
  unite(reason, c("label","stat_0"), sep = " - " ) 



string_2 <- ""
for(i in 1:nrow(screen_fail_reason_table_2)) {
  temp <- paste0(as.character(screen_fail_reason_table_2[i,]), "\n")
  string_2 <- paste0(string_2,temp)
}

##### Fourth part of consort diagram  #####
toning <- randomized %>% filter(group == 2)
remaining_4 <- anti_join(randomized, toning, by = "record_id")

endurance <- remaining_4 %>% filter(group == 3)
remaining_4 <- anti_join(remaining_4, endurance, by = "record_id")

resistance <- remaining_4 %>% filter(group == 4)
remaining_4 <- anti_join(remaining_4, resistance, by = "record_id")

combo <- remaining_4 %>% filter(group == 5)
remaining_4 <- anti_join(remaining_4, combo, by = "record_id")


##### Consort diagram language ######
screened_language <- paste0("Screened (n = ",nrow(screened),")")
eligible_language <- paste0("Eligible for Consent (n = ",nrow(eligible),")")
still_screening_language <- paste0("Still Screening (n = ",nrow(on_hold) + nrow(still_screening),")")
screen_fail_language <- paste0("Screen Failed (n = ",nrow(screen_failed),")")

consented_language <- paste0("Consented (n = ",nrow(consented),")")
in_process_language <- paste0("Expected to Consent \n(n = ",nrow(scheduled)+nrow(to_be_scheduled),")")

randomized_language <- paste0("Randomized (n = ",nrow(randomized),")")
in_baseline_language <- paste0("In Baseline (n = ",nrow(in_baseline),")")
screen_failed_baseline_language <- paste0("Screen Failed (n = ",nrow(screen_failed_after_consent),")")


toning_language <- toning_language <- paste0("Group ",toning$random_group_letter[1],"\n (n = ",nrow(toning),")")
endurance_language <- paste0("Group ",endurance$random_group_letter[1],"\n (n = ",nrow(endurance),")")
resistance_language <- paste0("Group ",resistance$random_group_letter[1],"\n (n = ",nrow(resistance),")")
combo_language <- paste0("Group ",combo$random_group_letter[1],"\n (n = ",nrow(combo),")")


toning_status_language <- paste0("Active (n = ",sum(toning$intervention_status == 1 | is.na(toning$intervention_status)),")\n",
                                 "Lost to follow-up (n = ",sum(toning$intervention_status == 5, na.rm = T),")\n",
                                 "Withdrawn \n Unwilling to test (n = ",sum(toning$intervention_status == 4, na.rm = T),")\n",
                                 "Willing to test (n = ",sum(toning$intervention_status == 3, na.rm = T),")\n")
endurance_status_language <- paste0("Active (n = ",sum(endurance$intervention_status == 1 | is.na(endurance$intervention_status)),")\n",
                                 "Lost to follow-up (n = ",sum(endurance$intervention_status == 5, na.rm = T),")\n",
                                 "Withdrawn \n Unwilling to test (n = ",sum(endurance$intervention_status == 4, na.rm = T),")\n",
                                 "Willing to test (n = ",sum(endurance$intervention_status == 3, na.rm = T),")\n")
resistance_status_language <- paste0("Active (n = ",sum(resistance$intervention_status == 1 | is.na(resistance$intervention_status)),")\n",
                                    "Lost to follow-up (n = ",sum(resistance$intervention_status == 5, na.rm = T),")\n",
                                    "Withdrawn \n Unwilling to test (n = ",sum(resistance$intervention_status == 4, na.rm = T),")\n",
                                    "Willing to test (n = ",sum(resistance$intervention_status == 3, na.rm = T),")\n")
combo_status_language <- paste0("Active (n = ",sum(combo$intervention_status == 1 | is.na(combo$intervention_status)),")\n",
                                    "Lost to follow-up (n = ",sum(combo$intervention_status == 5, na.rm = T),")\n",
                                    "Withdrawn \n Unwilling to test (n = ",sum(combo$intervention_status == 4, na.rm = T),")\n",
                                    "Willing to test (n = ",sum(combo$intervention_status == 3, na.rm = T),")\n")

toning_completed_language <- paste0("Completed\n (n = ",sum(toning$intervention_status == 2, na.rm = T),")")
endurance_completed_language <- paste0("Completed\n (n = ",sum(endurance$intervention_status == 2, na.rm = T),")")
resistance_completed_language <- paste0("Completed\n (n = ",sum(resistance$intervention_status == 2, na.rm = T),")")
combo_completed_language <- paste0("Completed\n (n = ",sum(combo$intervention_status == 2, na.rm = T),")")
#### ggplot settings ####
font_size <- 3
font_size_smaller <- 2.5
line_size <- .3

#### Beginning Screening ggplot ####
data <- tibble(x= 30:150, y= 30:150)
data %>% 
  ggplot(aes(x, y)) + 
  xlim(-60,180) +
  theme_void() +
  ####### Adding Boxes
  geom_rect(xmin = 35, xmax=85, ymin=147, ymax=153, color='black',  #Screened
            fill='white', size=0.25) +
  geom_rect(xmin = 25, xmax=95, ymin=27, ymax=33, color='black',  #Eligible
            fill='white', size=0.25) +
  geom_rect(xmin = -30, xmax=30, ymin=107, ymax=113, color='black',  #Still Screening
            fill='white', size=0.25) +
  geom_rect(xmin = 90, xmax=170, ymin=40, ymax=145, color='black',  #Screen Failed
            fill='white', size=0.25) -> p
  ###### Writing Lettering
p +
  annotate('text', x= 60, y=150,label= screened_language, size=font_size) +  #Screened
  annotate('text', x= 60, y=30,label= eligible_language, size=font_size) +   #Eligible
  annotate('text', x= 0, y=110,label= still_screening_language, size=font_size) +  #Still Screening
  annotate('text', x=130 , y=140,label= screen_fail_language, size=font_size) + #Screen Failed Number
  annotate('text', x=130 , y=88,label= string, size=font_size_smaller) -> p    #Screen Failed reason
###### Adding arrows
p +
  geom_segment(
    x=60, xend=60, y=147, yend=33.5, 
    size=line_size, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) + #screened to eligible
  geom_segment(
    x=60, xend=30.5, y=110, yend=110, 
    size=line_size, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +  #middle to still screening
  geom_segment(
    x=60, xend=89.5, y=110, yend=110, 
    size=line_size, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) -> p #screened to screen failed
p
ggsave(file.path(data_dir,'dsmc_report','closed_consort_screen.png'), p, scale = 1,width = 6.92, height = 5, units = "in")

#### Beginning Enrolled ggplot####
data <- tibble(x= -50:50, y= -50:50)
data %>%
  ggplot(aes(x, y)) +
  ylim(-60,50) +
  xlim(-60,198) +
  #scale_x_continuous(breaks=seq(-60,225,5)) +
  theme_void() +
  ####### Adding Boxes
  geom_rect(xmin = 25, xmax=95, ymin=47, ymax=53, color='black',  #Eligible
            fill='white', size=0.25) +
  geom_rect(xmin = -22, xmax=22, ymin=32, ymax=44, color='black',  #Scheduled
            fill='white', size=0.25) +
  geom_rect(xmin = 40, xmax=80, ymin=22, ymax=28, color='black',  #Consented
            fill='white', size=0.25) +
  geom_rect(xmin = 38, xmax=82, ymin=-13, ymax=-7, color='black',  #Randomized
            fill='white', size=0.25) +
  geom_rect(xmin = -20, xmax=20, ymin=9, ymax=15, color='black',  #In Baseline
            fill='white', size=0.25) +
  geom_rect(xmin = 88, xmax=153, ymin=-5, ymax=25, color='black',  #Screen Failed Baseline
            fill='white', size=0.25) +
  geom_rect(xmin = -56, xmax=-24, ymin=-17, ymax=-23, color='black',  #Core and fusion group
            fill='white', size=0.25) +
  geom_rect(xmin = 14, xmax=46, ymin=-17, ymax=-23, color='black',  #Endurance group
            fill='white', size=0.25) +
  geom_rect(xmin = 84, xmax=116, ymin=-17, ymax=-23, color='black',  #Resistance group
          fill='white', size=0.25) +
  geom_rect(xmin = 144, xmax=176, ymin=-17, ymax=-23, color='black',  #Combo group
            fill='white', size=0.25) +
  geom_rect(xmin = -35, xmax=5, ymin=-25, ymax=-40, color='black',  #Core and fusion status
            fill='white', size=0.25) + 
  geom_rect(xmin = 35, xmax=75, ymin=-25, ymax=-40, color='black',  #Endurance status
            fill='white', size=0.25) +
  geom_rect(xmin = 105, xmax=145, ymin=-25, ymax=-40, color='black',  #Resistance status
            fill='white', size=0.25) +
  geom_rect(xmin = 165, xmax=205, ymin=-25, ymax=-40, color='black',  #Combo status
            fill='white', size=0.25) +
  geom_rect(xmin = -56, xmax=-24, ymin=-42, ymax=-48, color='black',  #Core and fusion completed
            fill='white', size=0.25) +
  geom_rect(xmin = 14, xmax=46, ymin=-42, ymax=-48, color='black',  #Endurance completed
            fill='white', size=0.25) +
  geom_rect(xmin = 84, xmax=116, ymin=-42, ymax=-48, color='black',  #Resistance completed
            fill='white', size=0.25) +
  geom_rect(xmin = 144, xmax=176, ymin=-42, ymax=-48, color='black',  #Combo completed
            fill='white', size=0.25)-> p
  
###### Writing Lettering
p +
  annotate('text', x= 60, y=50,label= eligible_language, size=font_size) +   #Eligible
  annotate('text', x= 60, y=25,label= consented_language, size=font_size) +    #Consented
  annotate('text', x= 0, y=38,label= in_process_language, size=font_size) +  #Scheduled to consent
  annotate('text', x= 60, y=-10,label= randomized_language, size=font_size) +  #Randomized
  annotate('text', x= 0, y=12,label= in_baseline_language, size=font_size) + #In baseline
  annotate('text', x=120 , y=23, label= screen_failed_baseline_language, size=font_size) + #Screen failed number
  annotate('text', x=120 , y=8,label= string_2, size=font_size_smaller) + #Screen Failed reasons after baseline
  annotate('text', x=-40 , y=-20,label= toning_language, size=font_size) + #Core & Fusion
  annotate('text', x=30 , y=-20,label= endurance_language, size=font_size) + #Endurance Group
  annotate('text', x=100 , y=-20,label= resistance_language, size=font_size) + #Resistance Group
  annotate('text', x=160 , y=-20,label= combo_language, size=font_size) + #Combo Group
  annotate('text', x=-15 , y=-33,label= toning_status_language, size=font_size_smaller) + #Toning Status Box
  annotate('text', x=55 , y=-33,label= endurance_status_language, size=font_size_smaller) + #Endurance status box
  annotate('text', x=125 , y=-33,label= resistance_status_language, size=font_size_smaller) + #Resistance status box
  annotate('text', x=185 , y=-33,label= combo_status_language, size=font_size_smaller) + #Combo status box
  annotate('text', x=-40 , y=-45,label= toning_completed_language, size=font_size) + #Toning Completed
  annotate('text', x=30 , y=-45,label= endurance_completed_language, size=font_size) + #Endurance Completed
  annotate('text', x=100 , y=-45,label= resistance_completed_language, size=font_size) + #Resistance Completed
  annotate('text', x=160 , y=-45,label= combo_completed_language, size=font_size) -> p  #Combo Completed
  
###### Adding arrows
p +
  geom_segment(
    x=60, xend=60, y=47, yend=28.5,
    size=line_size, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) + #eligible for consent to consented
  geom_segment(
    x=60, xend=22.5, y=38, yend=38,
    size=line_size, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) + #eligible for consent to scheduled to consent
  geom_segment(
    x=60, xend=60, y=22, yend=-6.5,
    size=line_size, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +  #consented to randomized
  geom_segment(
    x=60, xend=20.5, y=12, yend=12,
    size=line_size, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) + #Consented to in baseline
  geom_segment(
    x=60, xend=87.5, y=12, yend=12,
    size=line_size, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) + #Consented to Screen failed in baseline
  geom_segment(
    x=60, xend=60, y=-13, yend=-15,
    size=line_size, linejoin = "mitre", lineend = "butt") + #Joiner between randomized and horizontal line
  geom_segment(
    x=-40, xend=160, y=-15, yend=-15,
    size=line_size, linejoin = "mitre", lineend = "butt") + #Horizontal line above statuses
  geom_segment(
    x=-40, xend=-40, y=-15, yend=-17,
    size=line_size, linejoin = "mitre", lineend = "butt") + #Joiner between horizontal line and C&F
  geom_segment(
    x=30, xend=30, y=-15, yend=-17,
    size=line_size, linejoin = "mitre", lineend = "butt") + #Joiner between horizontal line and Endurance
  geom_segment(
    x=100, xend=100, y=-15, yend=-17,
    size=line_size, linejoin = "mitre", lineend = "butt") + #Joiner between horizontal line and Resistance
  geom_segment(
    x=160, xend=160, y=-15, yend=-17,
    size=line_size, linejoin = "mitre", lineend = "butt") +  #Joiner between  horizontal line and Combo
  geom_segment(
    x=-40, xend=-40, y=-23, yend=-41,
    size=line_size, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=30, xend=30, y=-23, yend=-41,
    size=line_size, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=100, xend=100, y=-23, yend=-41,
    size=line_size, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=160, xend=160, y=-23, yend=-41,
    size=line_size, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) -> p
  
p
ggsave(file.path(data_dir,'dsmc_report','closed_consort_enrolled.png'), p, scale = 1.1,width = 6.92, height = 6.92, units = "in")




