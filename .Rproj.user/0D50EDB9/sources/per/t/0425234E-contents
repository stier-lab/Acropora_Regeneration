# photosynthesis and respirometry code for Regen 3.0 June 2023
remotes::install_github('colin-olito/LoLinR')
library("ggplot2")
library("segmented")
library("plotrix")
library("gridExtra")
library("LoLinR")
library("lubridate")
library("chron")
library('plyr')
library('dplyr')
library('tidyverse')
library('stringr')
library('Rmisc')
library('janitor')
library('readxl')

#Set the path of all respirometry files
current_dir<- getwd()
path.p <- file.path(current_dir, "runs")
#setwd("../Regeneration/Respiration/Data/initial/runs")

#Bring in the file names.
file.names <- list.files(path = path.p, pattern = "csv$")
file.names.clean <- tools::file_path_sans_ext(file.names)
Print(file.names.clean)

#create respiration data frame - 4 columns and # rows corresponding to # of respo files (120)
Photo.R<- data.frame(matrix(NA, nrow=length(file.names.clean), ncol=4))
colnames(Photo.R) <- c("fragment.ID.full","Intercept", "umol.L.sec","Temp.C")
View(Photo.R)

#load in sample information file 
Treatments<- read.csv(file= "samp_info.csv")
Volume<- read.csv(file= "chamber_vol.csv")
SA<- read.csv(file= "final_surface_areas.csv")%>%
  select(coral_id, CSA_cm2)%>%
  mutate(coral_id = as.character(coral_id))
list_df = list(Treatments, Volume, SA)
Sample.Info<-list_df%>%reduce(left_join, by= 'coral_id')

#load in times as a list the same length as the number of files
starttimes<- read.csv(file= "starttimes.csv")
rtime<-starttimes$rtime #list respiration start times. For respiration measurements, filter as > than this time
ptime<-starttimes$ptime #for photosynthesis, filter as < than this time

#create path to single respo file 
path.p1 <- file.path(current_dir, "runs", "20230605_g1_2_O2.csv")
# Read the respo CSV file using the created path
R <- read.csv(path.p1, skip = 1, header=T)%>%
  select(delta_t, Value, Temp)%>% # select columns 'delta_t', 'Value', and 'Temp'
  mutate(delta_t=as.numeric(delta_t))%>%
  filter(delta_t > 25.0)

  model <- rankLocReg(xall=R$delta_t, yall=R$Value, alpha=0.4, method= "pc", verbose= TRUE)
  
plot(model)

P<-  read.csv(path.p1, skip = 1, header=T)%>%
  select(delta_t, Value, Temp)%>% # select columns 'delta_t', 'Value', and 'Temp'
  mutate(delta_t=as.numeric(delta_t))%>%
  filter(delta_t > 10 & delta_t < 25)

Pmodel <- rankLocReg(xall=P$delta_t, yall=P$Value, alpha=0.4, method= "pc", verbose= TRUE) # 0.4 means it has to encompass at least 40% of the data points
plot(Pmodel)


# LOOP example from arianna #####
# Run loop to extract slopes from respiration data
for(i in 1:length(file.names)) { # for every file in list start at the first and run this following function
  Resp.Data <-read.table(file.path(path.p,file.names[i]), skip = 203, header=T, sep=",", na.string="NA", fill = TRUE, as.is=TRUE, fileEncoding="latin1") #reads in the data files
  Resp.Data$Temp <- Resp.Data[,16] #assigns temp column
  Resp.Data$Time.Min <- seq.int(from = 0, to = ((nrow(Resp.Data) * 0.05) - 0.05), by = 0.05) #set time in min
  Resp.Data <- Resp.Data %>% #filters data by phase (respiration only)
    filter(Time.Min > rtime[i])
  Resp.Data.N <- Resp.Data[,9] #subset desired columns
  
  for(j in 1:(ncol(Resp.Data.N))){
    model <- rankLocReg(
      xall=Resp.Data$Time.Min, yall=as.numeric(Resp.Data.N[, j]), 
      alpha=0.4, method="pc", verbose=TRUE) #extract slopes, percentile rank method with minimum window size of 0.4. This means that in order to fit a slope, it has to encompass at least 40% of available datapoints. 
    
    pdf(paste0("../Regeneration_3/respiration/output/",date[i], "_",run[i],"_",rename[j],"_regression_trunc.pdf")) #generate output file names
    plot(model)
    dev.off()
    
    Resp.Rb[j,1] <- as.character(date[i]) #stores the date
    Resp.Rb[j,2] <- as.character(run[i]) #stores the run number
    Resp.Rb[j,3] <- as.character(samp[j+(i-1)*ncol(Resp.Data.N)]) #stores the sample ID
    Resp.Rb[j,4] <- as.character(rename[j]) #stores the chamber ID
    Resp.Rb[j,5:6] <- model$allRegs[i,c(4,5)] #inserts slope and intercept in the dataframe
    
  }
  Resp.R <- rbind(Resp.R, Resp.Rb) #bind final data frame
}

processFile <- function(file.path, rtime, date, run, rename) {

  Resp.Data <- read.table(file.path(path.p, "20230606_g2_89_O2.csv"), skip = 201, sep = ",", na.string = "NA", fill = TRUE, as.is = TRUE, fileEncoding = "latin1")
  #Resp.Data <- read.table(file.path, skip = 203, header = T, sep = ",", na.string = "NA", fill = TRUE, as.is = TRUE, fileEncoding = "latin1")
  Resp.Data$Temp <- Resp.Data[,16]
  Resp.Data$Time.Min <- seq.int(from = 0, to = ((nrow(Resp.Data) * 0.05) - 0.05), by = 0.05)
  Resp.Data <- Resp.Data %>% filter(Time.Min > rtime)
  Resp.Data.N <- Resp.Data[, 9]
  
  Resp.Rb <- data.frame(Date = character(0), Run = character(0), Sample.ID = character(0),
                        Chamber.ID = character(0), Slope = numeric(0), Intercept = numeric(0))
  
  for (j in 1:(ncol(Resp.Data.N))) {
    model <- rankLocReg(xall = Resp.Data$Time.Min, yall = as.numeric(Resp.Data.N[, j]),
                        alpha = 0.4, method = "pc", verbose = TRUE)
    
    pdf(paste0("../Regeneration_3/respiration/output/", date, "_", run, "_", rename[j], "_regression_trunc.pdf"))
    plot(model)
    dev.off()
    
    Resp.Rb[j, 1] <- as.character(date)
    Resp.Rb[j, 2] <- as.character(run)
    Resp.Rb[j, 3] <- as.character(samp[j + (i - 1) * ncol(Resp.Data.N)])
    Resp.Rb[j, 4] <- as.character(rename[j])
    Resp.Rb[j, 5:6] <- model$allRegs[i, c(4, 5)]
  }
  
  return(Resp.Rb)
}
