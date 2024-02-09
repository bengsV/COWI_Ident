

 
###########################################################################################
# Loading all needed packages and auxiliary files
###########################################################################################



 
library("parallel")  
library(R6)
library('sets')



source("Feedback.R")
source("Performance.R")
source("Auxiliary.R")
source("Learner.R")
source("Pocowist.R")
source("SAVAGE.R")
source("PBR-SSCO.R")

 
###########################################################################################
# Setting some global parameters  #################
	

delta_range 			= c(0.01,0.05,0.1)
num.cores 	      		<- 6 # number of CPU cores
reps 		            = 100


###########################################################################################
# COWI Identification Problems 
###########################################################################################
###########################################################################################
#  Easy setting and number of arms 10
###########################################################################################

set.seed(21) 
num_arms          		= 10


pocowist        <- POCOWIST$new(num_arms = num_arms, delta = delta_range[1], transitive = FALSE)
trapocowist     <- POCOWIST$new(num_arms = num_arms, delta = delta_range[1], transitive = TRUE)
savage          <- SAVAGE$new(num_arms   = num_arms, delta = delta_range[1])
pbr_ssco        <- PBR_SSCO$new(num_arms = num_arms, delta = delta_range[1], k = 1)


diff_vec 	= c("super_easy","easy","medium","hard")
perf 		= Performance$new(learners = list(pocowist,trapocowist,savage,pbr_ssco), diff="easy", deltas = delta_range,  n = num_arms, reps= reps, cores = num.cores )


perf$runSimulations()
perf$getStatistics()
perf$plotSucessRates()
perf$getTables()
 

tagetfile=paste("Easy_Run_COPE_Ident",Sys.Date(),"_N_arms",num_arms,"_Repetitions",reps,"_",".RData",sep="")
save.image(file=tagetfile)




###########################################################################################
# COWI Identification Problems 
###########################################################################################
###########################################################################################
#  Easy setting and number of arms 20
###########################################################################################


set.seed(21)
num_arms          = 20 


pocowist        <- POCOWIST$new(num_arms = num_arms, delta = delta_range[1], transitive = FALSE)
trapocowist     <- POCOWIST$new(num_arms = num_arms, delta = delta_range[1], transitive = TRUE)
savage          <- SAVAGE$new(num_arms   = num_arms, delta = delta_range[1])
pbr_ssco        <- PBR_SSCO$new(num_arms = num_arms, delta = delta_range[1], k = 1)


diff_vec 	= c("super_easy","easy","medium","hard")
perf 		= Performance$new(learners = list(pocowist,trapocowist,savage,pbr_ssco), diff="easy", deltas = delta_range,  n = num_arms, reps= reps, cores = num.cores )


perf$runSimulations()
perf$getStatistics()
perf$plotSucessRates()
perf$getTables()


tagetfile=paste("Easy_Run_COPE_Ident",Sys.Date(),"_N_arms",num_arms,"_Repetitions",reps,"_",".RData",sep="")
save.image(file=tagetfile)

###########################################################################################
# COWI Identification Problems 
###########################################################################################
###########################################################################################
#  Medium setting and number of arms 20
###########################################################################################

 
set.seed(21)



pocowist        <- POCOWIST$new(num_arms = num_arms, delta = delta_range[1], transitive = FALSE)
trapocowist     <- POCOWIST$new(num_arms = num_arms, delta = delta_range[1], transitive = TRUE)
savage          <- SAVAGE$new(num_arms   = num_arms, delta = delta_range[1])
pbr_ssco        <- PBR_SSCO$new(num_arms = num_arms, delta = delta_range[1], k = 1)


diff_vec 	= c("super_easy","easy","medium","hard")
perf 		= Performance$new(learners = list(pocowist,trapocowist,savage,pbr_ssco), diff="medium", deltas = delta_range,  n = num_arms, reps= reps, cores = num.cores )


perf$runSimulations()
perf$getStatistics()
perf$plotSucessRates()
perf$getTables()


tagetfile=paste("Medium_Run_COPE_Ident",Sys.Date(),"_N_arms",num_arms,"_Repetitions",reps,"_",".RData",sep="")
save.image(file=tagetfile)


###########################################################################################
# CW Identification Problems 
###########################################################################################

# load the SOTA

source("SELECT.R")
source("DKWT.R")


###########################################################################################
#  Easy setting and number of arms 20
###########################################################################################


set.seed(21)



pocowist        <- POCOWIST$new(num_arms = num_arms, delta = delta_range[1], transitive = FALSE)
trapocowist     <- POCOWIST$new(num_arms = num_arms, delta = delta_range[1], transitive = TRUE)
select          <- SELECT$new(num_arms   = num_arms, delta = delta_range[1], Delta = 0.1)
dkwt            <- DKWT$new(num_arms     = num_arms, delta = delta_range[1])


diff_vec 	= c("super_easy","easy","medium","hard")
perf 		= Performance$new(learners = list(pocowist,trapocowist,select,dkwt), diff="easy", deltas = delta_range,  n = num_arms, reps= reps, cores = num.cores, CW_Ident = TRUE )


perf$runSimulations()
perf$getStatistics()
perf$plotSucessRates()
perf$getTables()


tagetfile=paste("Easy_Run_CW_Ident",Sys.Date(),"_N_arms",num_arms,"_Repetitions",reps,"_",".RData",sep="")
save.image(file=tagetfile)

###########################################################################################
# CW Identification Problems 
###########################################################################################
###########################################################################################
#  Medium setting and number of arms 20
###########################################################################################


set.seed(21)

pocowist        <- POCOWIST$new(num_arms = num_arms, delta = delta_range[1], transitive = FALSE)
trapocowist     <- POCOWIST$new(num_arms = num_arms, delta = delta_range[1], transitive = TRUE)
select          <- SELECT$new(num_arms   = num_arms, delta = delta_range[1], Delta = 0.05)
dkwt            <- DKWT$new(num_arms     = num_arms, delta = delta_range[1])


diff_vec 	= c("super_easy","easy","medium","hard")
perf 		  = Performance$new(learners = list(pocowist,trapocowist,select,dkwt), diff="medium", deltas = delta_range,  n = num_arms, reps= reps, cores = num.cores, CW_Ident = TRUE )


perf$runSimulations()
perf$getStatistics()
perf$plotSucessRates()
perf$getTables()


tagetfile = paste("Medium_Run_CW_Ident", Sys.Date(), "_N_arms", num_arms, "_Repetitions", reps, "_", ".RData", sep="")
save.image(file = tagetfile)