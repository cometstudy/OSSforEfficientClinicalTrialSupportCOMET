#' @author JC and EDV
#' 
#' @name vo2_aggregation
#' 
#' @title Summarize and aggregate VO2 Data
#' 
#' @section Copyright: 
#' Copyright © 2022 University of Kansas
#' 
#' @description 
#' This script is heavily adapted from VO2_from_Parvo_files_20190604.R, 
#' a script written by EDV to combine VO2 data across multiple exercise studies. A few points have been added - 
#' time point language and date. Most language has been left the same; therefore it is heavily commented. The primary purpose of this
#' is to aggregate data and easily identify missing data. Analysis files expect to be rewritten at the end of the study.
#' 
#' @section Development
#' 5.27.22 Finished adapting and pushed into comet_nucleus. JC \cr
#' 5.31.22 Implementing on rshiny server. foreach package isn't available on server. JC
#' 5.31.22 Successfully implemented on rshiny server. JC

message("load REDcap data")
#### Format REDCap data to join with vo2 data ####
comet_ids <- comet %>%
  filter(redcap_event_name == "baseline_arm_1") %>%
  select(record_id, comet_study_id, redid)

redcap_vo2 <- physical_assessment_visit %>%
  left_join(., comet_ids, by = "record_id")

message("file Names")
################ Get the files ############
files <- list.files(file.path(data_dir, 'raw', 'vo2_data'),pattern="csv|CSV", full.names=T)
filesshort <- list.files(file.path(data_dir, 'raw', 'vo2_data'),pattern="csv|CSV", full.names=F)
x <- data.frame(str_split(gsub("-|__|_",".",filesshort),"[.]", simplify = T))

message("bind files")
################ Prep List of files to read in ############
files.df2 <- cbind(files,x)

message("prep empty dfs")
################ Prep Empty Dataframes ############

#Prep dataframes
summcolnames<-c("FullID","HSC","RED","ID","TP","Duration","peakVO2L","peakVO2mlkgmin","VCO2atpeak","VEatpeak","peakRER",
                "HRatpeak","peakHR","OUES",'Date')
summary_measures<-data.frame(matrix(data=0, nrow=1,ncol=length(summcolnames)))
colnames(summary_measures)<-summcolnames

#pre and post test data frames
data_colnames<-c("FullID",'HSC','RED','ID', 'TP','MinBin','TIME','VO2_L/m','VO2/kg','VCO2','VE','RER','HR')
time.col<-which(data_colnames=="TIME") #I key off this postion a lot, so if I add more columns to the left,this should keep relative position
pretest_measures<-data.frame(matrix(data=0, nrow=1,ncol=length(data_colnames)))
posttest_measures<-data.frame(matrix(data=0, nrow=1,ncol=length(data_colnames)))


testdata_colnames<-data_colnames[time.col:length(data_colnames)]
data_final<-as.data.frame(NULL)  #initialize the final dataframe

message("start for loop of available vo2 files")
##################### Process Parvo files ########
for(i in 1:nrow(files.df2)){

  #print(paste(files.df2$X3[i], files.df2$X2[i]))
  
  #### Match to REDCap data ###
  current_vo2 <- redcap_vo2 %>%
    filter(str_pad(as.character(comet_study_id), width = 3, side = "left", pad = "0") == files.df2$X3[i])
  
  
  
  data<-NULL #initialize
  
  no_col <- 15 #max(count.fields(files.df2[i,1], sep = ","))
  
  data <- data.frame(read.table(file.path(files.df2[i,1]), header = F, stringsAsFactors = F, sep=",", fill=T, col.names=c(paste0("X",seq(1:no_col))))) #read in data
  
  
  test_date_line <- grep("Metabolic Text Report", data$X1)          
  test_date <- ymd(paste0(data$X2[test_date_line],"-",data$X4[test_date_line],"-",data$X6[test_date_line]))
  
  message(paste("i =",i,"redcap =",current_vo2$comet_study_id[1],"file =",files.df2$X3[i],test_date,as.Date(current_vo2$ex_gxt_1_time[1])))
  
  if(any(!is.na(as.Date(current_vo2$ex_gxt_1_time)))) {
    
    date_dif_array <- array(abs(test_date - as.Date(current_vo2$ex_gxt_1_time)))
    likely_tp <- current_vo2$redcap_event_name[which(date_dif_array == min(date_dif_array, na.rm = T))]
    #message(paste0(current_vo2$comet_study_id[1]," ",likely_tp))
    
  }  else {
    #message(paste0(current_vo2$comet_study_id[1]," ",current_vo2$ex_gxt_1_time))
    likely_tp <- NA
  }
             hsc<-files.df2$X1[i]
             red<-files.df2$X2[i]
             id<-files.df2$X3[i]
             # tp<-case_when(likely_tp == "baseline_arm_1" ~ 1,
             #               likely_tp == "week_26_arm_1" ~ 2,
             #               likely_tp == "week_52_arm_1" ~ 3)
             tp <- likely_tp
             fullid<-paste(hsc,red,id,tp,sep=".")
             
             #message(paste(fullid, current_vo2$ex_gxt_1_time))

             
             #Isolate the columns we need
             
             columnheadline<-grep('^TIME', data[,1])
             
             data[columnheadline, ]<-  data[columnheadline, ] %>% 
               mutate_if(is.character,  str_replace_all, pattern = "[[\"]]", replacement = "") %>%
               mutate_if(is.character,  str_trim)

             
             
             vo2L.col<-grep('^VO2$',data[columnheadline,])
             vo2mL.col<-grep("^VO2/kg$",data[columnheadline,])
             vco2.col<-grep("^VCO2$",data[columnheadline,])
             ve.col<-grep("^VE$",data[columnheadline,])[1]
             rer.col<-grep("^RER$",data[columnheadline,])
             hr.col<-grep("^HR$",data[columnheadline,])[1] #there are two HR columns commonly, use the first one
             
             #Find the start of test data and the post test summary timings near the bottom
             testmarker<-grep('------', data[,1])+1
             
             if( length(grep('Start', data[,2]))==0L){
               print("no Starttime time.")
               readline("Bad starting line. Note id and move on.")
             } else {
               if(grepl("\\:",data[grep('Start', data[,2]),1])){
                 x<-strsplit(data[grep('Start', data[,2]),1],":") #Finds the start time
                 x <- as.numeric(x)
                 starttime<-x[1]+round(x[2]/60,4)
               } else {
                 starttime<-as.numeric(data[grep('Start', data[,2]),1]) #Finds the start time
               }
             }
             
             #' 20230321: Adding endtime for no cool down times
             if( is.numeric(grep('Cool|Stop', data[,2]))==F | is_empty(grep('Cool|Stop', data[,2])) ){
               print("no Cool down time")
               #readline("Bad end line. Note id and move on")
               endtime <- case_when(id == "141" ~ 9.012498)
             } else {
               
               if(grepl("\\:",data[grep('Cool|Stop', data[,2]),1][1])){
                 x<-strsplit(data[grep('Cool|Stop', data[,2]),1][1],":") #Finds the end time
                 x <- as.numeric(x)
                 endtime<-x[1]+round(x[2]/60,4)
                 
               } else {
                 endtime<-as.numeric(data[grep('Cool|Stop', data[,2]),1][1]) #Finds the end time
                 
                 
               }
             } # end is cool down missing
             
             
             if(grepl("\\:",data[testmarker,1])){ #some time fields have decimal time other have conventional time
               
               tempendmarker<-min(which( data[testmarker:nrow(data), 1]==""))   +testmarker-1-1
               data[testmarker:tempendmarker,1]<-sapply(strsplit(data[testmarker:tempendmarker,1],":"),
                                                        function(x) {
                                                          x <- as.numeric(x)
                                                          as.numeric(x[1]+round(x[2]/60,4))
                                                        }
               )
             }
             
             
             #finds the first non-numeric entry in col 1 after the beginning of the test
             #subtract 1 because testmaker is plus 1 and subtract 1 because the which statement identifies
             #the first na, not the last number
             endmarker<-min(which(is.na(as.numeric(data[testmarker:nrow(data), 1]))))+testmarker-1-1  
             
             
             
             
             
             #find the start of the active portion of the test and convert the startmaker back to the larger larger dataset frame of reference
             #Searches first 14 samples for the beginning of the data to accomodate MOVER and ADMIT pre-GXT 3 minute rest data capture
             startline<-testmarker-1+which.min(abs(as.numeric(data[seq(testmarker,(testmarker+14)), 1])-starttime))
             
             #find the start of the active portion of the test and convert the startmaker back to the larger larger dataset frame of reference
             #Searches for the recording that is closest to the reported end time
             endline<-testmarker-1+which.min(abs(as.numeric(data[seq(testmarker,endmarker), 1])-endtime)) 
             
             
             #Isolate just the active test data
             testdata<-data.frame(data[startline:endline,c(1,vo2L.col,vo2mL.col,vco2.col,ve.col,rer.col,hr.col)])
             testdata<-as.data.frame(sapply(testdata, as.numeric))
             colnames(testdata)<-testdata_colnames
             
             
             
             #number of 30s samples to average across
             period<- 2  
             
             
             #determine number of reporting periods
             minquotient<- (endline-startline+1) %/% period 
             minremainder<- (endline-startline+1) %% period
             
             
             ### Commenting out foreach and replacing with for loop
             # foreach(j=1:minquotient) %do%  {
             #   
             #   #j=1
             #   
             #   if(j==1){temp<-matrix(0,minquotient,length(data_colnames))
             #   temp<-as.data.frame(temp)}#initialize
             #   
             #   #For full minutes
             #   temp[j,1]<-fullid
             #   temp[j,2]<-hsc  #hsc number
             #   temp[j,3]<-red  #red number
             #   temp[j,4]<-id   #study id
             #   temp[j,5]<-tp   #timepoint
             #   temp[j,6]<-j*(1/period)  #minute
             #   
             #   lo<-ifelse(j==1,1,j*(period)-1)
             #   hi<-j*(period)
             #   temp[j,time.col:length(data_colnames)]<-as.numeric(colMeans(testdata[lo:hi,]))
             #   
             # } #end foreach quotient
             
             for (j in 1:minquotient){
               
               #j=1
               
               if(j==1){temp<-matrix(0,minquotient,length(data_colnames))
               temp<-as.data.frame(temp)}#initialize
               
               #For full minutes
               temp[j,1]<-fullid
               temp[j,2]<-hsc  #hsc number
               temp[j,3]<-red  #red number
               temp[j,4]<-id   #study id
               temp[j,5]<-tp   #timepoint
               temp[j,6]<-j*(1/period)  #minute
               
               lo<-ifelse(j==1,1,j*(period)-1)
               hi<-j*(period)
               temp[j,time.col:length(data_colnames)]<-as.numeric(colMeans(testdata[lo:hi,]))
               
             } #end for quotient
             
             if(minremainder %% 2 ==1){temp[j+1,1:6]<-c(fullid,hsc, red, id, tp, j/period +.5)    }
             if(minremainder %% 2 ==1){temp[j+1,time.col:length(data_colnames)]<-testdata[hi+1,]    }
             
             
             colnames(temp)<-data_colnames
             temp<-as.data.frame(temp) %>%
               mutate(Date = as.character(test_date))
             
             if(i==1) {
               data_final<-temp
             } else {
               data_final<-rbind(data_final,temp)
             }
             
             
             
             
             ##########################################
             ########## Gather values we want for each test
             
             #ID the peak VO2max
             vo2maxLine<-which.max(testdata$"VO2/kg")
             lastLine<-nrow(testdata)
             
             
             #eventsline<-grep('Events', data[,1])
             #summaryline<-grep('Summary', data[,1])
             
             ##Figure HR at peak
             hratpeak<-testdata$HR[vo2maxLine]
             
             hrmax<-max(testdata$HR)
             
             
             
             
             summary_measures[i,1]<-fullid
             summary_measures[i,2]<-hsc
             summary_measures[i,3]<-red
             summary_measures[i,4]<-id
             summary_measures[i,5]<-tp
             summary_measures[i,6]<-endtime-starttime
             summary_measures[i,7]<-testdata$"VO2_L/m"[vo2maxLine]
             summary_measures[i,8]<-testdata$"VO2/kg"[vo2maxLine]
             summary_measures[i,9]<-testdata$VCO2[vo2maxLine]
             summary_measures[i,10]<-testdata$VE[vo2maxLine]
             summary_measures[i,11]<-testdata$RER[vo2maxLine]
             summary_measures[i,12]<-hratpeak
             summary_measures[i,13]<-hrmax
             summary_measures[i,15]<-as.character(test_date)
             
             
             #calculate OUES from the 15s reports
             #From Hollenberg JACC 2000
             testdata$VO2ml<-testdata$"VO2_L/m"*1000
             testdata$logVE<-log10(testdata$VE)
             
             summary_measures[i,14]<-lm(VO2ml ~ logVE, data=testdata)$coefficients[2] #the slope of the linear regression
             
             
             # This file was adapted from previous ADRC vo2 work
             # These studies 11132 and 140787 are previous studies. This section is not needed for 146904
             # ### if a MOVER or ADMIT with a 3 minute rest period before the GXT. Grab 30s bins before exercise starts
             # if(files.df2$X1[i]=="11132" | files.df2$X1[i]=="140787"){
             #   
             #   #data_colnames<-c("FullID",'HSC','RED','ID', 'TP','MinBin','TIME','VO2_L/m','VO2/kg','VCO2','VE','RER','HR')
             #   #vo2L.col<-grep('^VO2[[:space:]]',as.data.frame(data[columnheadline,]))
             #   #vo2mL.col<-grep("^VO2/kg",as.data.frame(data[columnheadline,]))
             #   #vco2.col<-grep("^VCO2[[:space:]]",as.data.frame(data[columnheadline,]))
             #   #ve.col<-grep("^VE[[:space:]]",as.data.frame(data[columnheadline,]))[1]
             #   #rer.col<-grep("^RER[[:space:]]",as.data.frame(data[columnheadline,]))
             #   #hr.col<-grep("^HR[[:space:]]",as.data.frame(data[columnheadline,]))[1] #th
             #   
             #   foreach(k=1:6) %do%  {
             #     
             #     #k=1
             #     
             #     if(k==1){
             #       temp_b<-as.data.frame(matrix(0,6,length(data_colnames)))
             #     }#initialize
             #     
             #     #For full minutes
             #     temp_b[k,1]<-fullid
             #     temp_b[k,2]<-hsc  #hsc number
             #     temp_b[k,3]<-red  #red number
             #     temp_b[k,4]<-id   #study id
             #     temp_b[k,5]<-tp   #timepoint
             #     temp_b[k,6]<-k*(1/period)  #minute
             #     
             #     lo<-ifelse(k==1,1,k*(period)-1)
             #     hi<-k*(period)
             #     xxx<-data[(testmarker+lo-1):(testmarker+hi-1), c(1,vo2L.col,vo2mL.col,vco2.col,ve.col,rer.col,hr.col)]
             #     xxx<-as.data.frame(sapply(xxx, as.numeric), row.names = F)
             #     temp_b[k,time.col:length(data_colnames)]<-colMeans(xxx)
             #     
             #   } #end foreach quotient
             #   
             #   colnames(temp_b)<-data_colnames
             #   temp_b<-as.data.frame(temp_b)
             #   
             #   if(exists("data_final_b")==F) {
             #     data_final_b<-temp_b
             #   } else {
             #     data_final_b<-rbind(data_final_b,temp_b)
             #   }
             #   
             #   
             #   
             #   
             #   #### bin the post test measures as well
             #   
             #   
             #   foreach(k=1:8) %do%  {
             #     
             #     #k=1
             #     
             #     if(k==1){
             #       temp_c<-as.data.frame(matrix(0,6,length(data_colnames)))
             #     }#initialize
             #     
             #     #For full minutes
             #     temp_c[k,1]<-fullid
             #     temp_c[k,2]<-hsc  #hsc number
             #     temp_c[k,3]<-red  #red number
             #     temp_c[k,4]<-id   #study id
             #     temp_c[k,5]<-tp   #timepoint
             #     temp_c[k,6]<-k*(1/period)  #minute
             #     
             #     lo<-ifelse(k==1,1,k*(period)-1)
             #     hi<-k*(period)
             #     xxx<-data[(endline+lo):(endline+hi),c(1,vo2L.col,vo2mL.col,vco2.col,ve.col,rer.col,hr.col)] 
             #     xxx<-as.data.frame(sapply(xxx, as.numeric))
             #     temp_c[k,time.col:length(data_colnames)]<-colMeans(xxx)
             #     
             #   } #end foreach quotient
             #   
             #   colnames(temp_c)<-data_colnames
             #   temp_C<-as.data.frame(temp_c)
             #   
             #   if(exists("data_final_c")==F) {
             #     data_final_c<-temp_c
             #   } else {
             #     data_final_c<-rbind(data_final_c,temp_c)
             #   }
             #   
             # } #end foreach, on the pretest and post test assessments
             
} #end for i  files



############ write out ###############
#Rename dataframes so that easier to understand study wide

#'2023: Particpant 149 on 4/19/23 was misnamed as 143. This is corrected. JC
vo2_data_final <- data_final %>%
  mutate(ID = case_when(Date == ymd("20230419") & ID == "143" ~ "149",
         T ~ ID))

#'2023: Particpant 149 on 4/19/23 was misnamed as 143. This is corrected. JCm
vo2_summary_measures <- summary_measures %>%
  mutate(ID = case_when(Date == ymd("20230419") & ID == "143" ~ "149",
         T ~ ID))

save(list = c('vo2_data_final','vo2_summary_measures'), file = file.path(clean_data_destination,'comet_vo2_clean.Rdata'))

# if(any(duplicated(summary_measures$FullID))==T ){ break }
# 
# a<- data_final
# a <- a %>% dplyr::select(HSC,RED,ID,TP,MinBin,everything(),-FullID, -TIME) %>% 
#   mutate(MinBin = as.numeric(MinBin)) %>% 
#                   mutate(MinBin = ifelse(MinBin %% 1 == 0.5 & MinBin < 10, paste0("0",MinBin),
#                    ifelse( MinBin %% 1 == 0 & MinBin < 10, paste0("0",MinBin,".0"),
#                            ifelse( MinBin %% 1 == 0 & MinBin >= 10,paste0(MinBin,".0"),MinBin))))
# a.melt <- melt(a, id.vars = c("HSC","RED","ID","TP","MinBin"))
# a.cast<-dcast(a.melt, HSC + RED + ID + TP  ~ variable + MinBin )
# write.csv(a.cast,paste0(basedir,'/IndividualTestData_Wide_',Sys.Date(),'.csv'))
# 
# b<-data_final_b
# b <- b %>% dplyr::select(HSC,RED,ID,TP,MinBin,'VO2_L/m','VO2/kg',RER, VCO2) %>% 
#   mutate(MinBin = as.numeric(MinBin)) %>% 
#   mutate(MinBin = ifelse(MinBin %% 1 == 0.5 & MinBin < 10, paste0("0",MinBin),
#                          ifelse( MinBin %% 1 == 0 & MinBin < 10, paste0("0",MinBin,".0"),
#                                  ifelse( MinBin %% 1 == 0 & MinBin >= 10,paste0(MinBin,".0"),MinBin))))
# b.melt <-melt(b, id.vars = c("HSC","RED","ID","TP","MinBin"))
# b.cast<-dcast(b.melt, HSC + RED + ID + TP  ~ variable + MinBin )
# write.csv(b.cast,paste0(basedir,'/IndividualPreTestData_Wide_',Sys.Date(),'.csv'))
# 
# 
# c<-data_final_c
# c <- c %>% dplyr::select(HSC,RED,ID,TP,MinBin,'VO2_L/m','VO2/kg',RER, VCO2) %>% 
#   mutate(MinBin = as.numeric(MinBin)) %>% 
#   mutate(MinBin = ifelse(MinBin %% 1 == 0.5 & MinBin < 10, paste0("0",MinBin),
#                          ifelse( MinBin %% 1 == 0 & MinBin < 10, paste0("0",MinBin,".0"),
#                                  ifelse( MinBin %% 1 == 0 & MinBin >= 10,paste0(MinBin,".0"),MinBin))))
# c.melt <-melt(c, id.vars = c("HSC","RED","ID","TP","MinBin"))
# c.cast<-dcast(c.melt, HSC + RED + ID + TP  ~ variable + MinBin )
# write.csv(c.cast,paste0(basedir,'/IndividualPostTestData_Wide_',Sys.Date(),'.csv'))
# 
# 
# # A side project for Jill.
# # c<-read.csv('~/Desktop/ADMITformatting3.csv', stringsAsFactors = F, header = T)
# # 
# # 
# # c <- c %>% select(FullID,RED,ID,MinBin,RER) 
# # 
# # c.melt <-melt(c, id.vars = c("FullID","RED","ID","MinBin"))
# # 
# # c.cast<-dcast(c.melt, FullID + RED + ID  ~ variable + MinBin )
# # 
# # write.csv(c.cast,'~/Desktop/ADMIT_AngryJill_Wide_2018-10-09.csv')
# # 
# 
# ########### Plot Out #################
# 
# library(ggplot2)              
# 
# plot.it <-function(df, x){
#   df.melt.x <-df %>% filter(variable==x)
#   df.melt.x$HSC <-factor(df.melt.x$HSC)
#   ggplot(data=df.melt.x, aes(x=MinBin, y=value, group=HSC)) +
#     geom_line(aes(linetype=HSC,color=HSC))+
#     geom_point(aes(shape=HSC,color=HSC)) + ylab(x)
# }
# 
# plot.it(b.melt,"VO2/kg")
# plot.it(b.melt,"RER")
# 
# plot.it(c.melt,"VO2/kg")
# plot.it(c.melt,"RER")
# 
