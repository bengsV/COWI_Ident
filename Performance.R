
#' @title Performance Class
#' 
#'
#' @description
#' Class to run several runs of learners at once.
#'



Performance = R6Class("Performance",
                      public = list(
                        
                        #' @field learners (`numeric()`)\cr
                        #' List of Learner instances to be evaluated.
                        learners = NULL,
                        
                        #' @field problem_inst (`list()`)\cr
                        #' A Feedback instance determining the problem environment.
                        problem_inst = list(),
                        
                        #' @field reps (`integer()`)\cr
                        #' Number of repetitions.
                        reps = NULL,
                        
                        #' @field delta (`numeric()`)\cr
                        #' Stores the seeked error probabilities.
                        delta_range = 0,
                        
                        #' @field cores (`integer()`)\cr
                        #' Number of cores.
                        cores = NULL,
                        
                        #' @field CW_Ident (`Boolean()`)\cr
                        #' Boolean indicating whether the Condorcet winner identification problem should be solved.
                        CW_Ident = NULL,
                        
                        #' @field results (`list()`)\cr
                        #' stores the results.
                        results = list(),
                        
                        #' @field result_statistics (`list()`)\cr
                        #' stores the relevant statistics from the results.
                        result_statistics = list(),
                        
                        #' @description
                        #' Creates a new instance of this [R6][R6::R6Class] class.
                        #'
                        initialize = function(learners, n, diff, deltas = c(0.01,0.05,0.1), reps = 1, cores = 1, CW_Ident = FALSE) {
                          
                          self$learners            = learners
                          self$problem_inst        = list(n_arms = n, diff = diff)
                          self$delta_range         = deltas
                          self$reps                = reps
                          self$cores               = cores
                          self$CW_Ident            = CW_Ident
                          
                        },
                        
                        
                        #' @description
                        #' Printer.
                        #' @param ... (ignored).
                        print = function() { 
                          cat("  diffculty: ", self$problem_inst$diff, "\n", sep = "")
                          cat("  number of arms: ", self$problem_inst$n_arms, "\n", sep = "")
                        },
                        
                        
                        runSimulationStep = function(rep_i) {
                          
                          
                          P           = NULL
                          result_list = list()
                          
                          if(self$CW_Ident){
                            if(self$problem_inst$diff == "super_easy"){
                              P = genRandomCondWinPrefMatrix(self$problem_inst$n_arms,p_low=0.9,p_up=1)
                            }
                            if(self$problem_inst$diff == "easy"){
                              P = genRandomCondWinPrefMatrix(self$problem_inst$n_arms,p_low=0.6,p_up=1)
                            }
                            if(self$problem_inst$diff == "medium"){
                              P = genRandomCondWinPrefMatrix(self$problem_inst$n_arms,p_low=0.55,p_up=0.7)
                            }
                            if(self$problem_inst$diff == "hard"){
                              P = genRandomCondWinPrefMatrix(self$problem_inst$n_arms,p_low=0.51,p_up=0.55)
                            }
                          }
                          else{
                            if(self$problem_inst$diff == "super_easy"){
                              P = genRandomPrefMatrix(self$problem_inst$n_arms,p_low=0.9,p_up=1)
                            }
                            if(self$problem_inst$diff == "easy"){
                              P = genRandomPrefMatrix(self$problem_inst$n_arms,p_low=0.6,p_up=1)
                            }
                            if(self$problem_inst$diff == "medium"){
                              P = genRandomPrefMatrix(self$problem_inst$n_arms,p_low=0.55,p_up=0.7)
                            }
                            if(self$problem_inst$diff == "hard"){
                              P = genRandomPrefMatrix(self$problem_inst$n_arms,p_low=0.51,p_up=0.55)
                            }
                          }
                            

                            
                    
                          problem_env         = Feedback$new(P)
                          CP_set_problem_env  = problem_env$getCopeleandWinners()
                          
                          temp 					      =	 1
                          
                          for (learner in self$learners){
                            for (delta in self$delta_range){
                              learner$clear()
                              learner$setDelta(delta)
                              learner$setDataModel(problem_env)
                              learner$run(problem_env)
                              result_list[[temp]] 	= list(name = class(learner)[1], delta = delta,  samples = learner$samples, cum_times = cumsum(learner$times)[learner$samples], target_arm = learner$target_arm, target_arms = CP_set_problem_env )
                              temp 					        = temp + 1
                            }
                          }
                          
                          print("Step")
                          print(rep_i)
                          return (result_list)
                        },
                        
                        
                        runSimulations = function() {
                          
                          self$results = mclapply(X = 1:self$reps,self$runSimulationStep, mc.cores = self$cores)
                          
                        },
                        
                        
                        getStatistics = function() {
                          
                          for (j in 1:length(self$learners)){
                            for (k in 1:length(self$delta_range)){
                              #print("We are at:")
                              #print(((j-1)*length(self$delta_range)+k))
                              
                              self$result_statistics[[((j-1)*length(self$delta_range)+k)]]               = list(name = "", delta = self$delta_range[k], mean_samples = NULL, mean_cum_time = NULL, sd_samples = NULL, sd_cum_time =NULL, max_samples=NULL, 
                                                                                                      mean_accuracy = NULL, sd_accuracy = NULL)
                              
                              self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$name          = self$results[[1]][[((j-1)*length(self$delta_range)+k)]][1]
                              
                              if((self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$name=="POCOWIST")& j==2 ){
                                self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$name          = paste("TRA",self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$name)
                              }
                              
                              
                              temp_samples                              = self$results[[1]][[((j-1)*length(self$delta_range)+k)]]$samples
                              temp_times                                = self$results[[1]][[((j-1)*length(self$delta_range)+k)]]$cum_times
                              temp_accuracy                             = is.element( self$results[[1]][[((j-1)*length(self$delta_range)+k)]]$target_arm   , self$results[[1]][[((j-1)*length(self$delta_range)+k)]]$target_arms )   
                              self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$max_samples   = self$results[[1]][[((j-1)*length(self$delta_range)+k)]]$samples
                              
                              for (i in 2:length(self$results)){
                                
                                temp_samples                              = rbind(temp_samples,   self$results[[i]][[((j-1)*length(self$delta_range)+k)]]$samples)
                                temp_times                                = rbind(temp_times,     self$results[[i]][[((j-1)*length(self$delta_range)+k)]]$cum_times)
                                temp_accuracy                             = c(temp_accuracy,is.element( self$results[[i]][[((j-1)*length(self$delta_range)+k)]]$target_arm   , self$results[[i]][[((j-1)*length(self$delta_range)+k)]]$target_arms )  )
                                self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$max_samples   = max(self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$max_samples,self$results[[i]][[((j-1)*length(self$delta_range)+k)]]$samples)
                              }
                              
                              self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$mean_samples  = apply(temp_samples,2,mean)
                              self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$mean_cum_time = apply(temp_times,2,mean)
                              self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$sd_samples    = apply(temp_samples,2,sd)
                              self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$sd_cum_time   = apply(temp_times,2,sd)
                              self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$mean_accuracy = mean(temp_accuracy)
                              self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$sd_accuracy   = sd(temp_accuracy)
                              
                            }
                          }
                          
                          
                          
                        },
                        
                        
                        plotSucessRates = function() {
                          
                          temp                = self$getTables()
                          
                          cex_const	  =	2.2
                          
                          temp$Res_mat_samples     = matrix(rep(0,length(self$learners)*length(self$delta_range)), ncol = length(self$delta_range))
                          temp$Res_mat_samples_sd  = matrix(rep(0,length(self$learners)*length(self$delta_range)), ncol = length(self$delta_range))
                          temp$Res_mat_acc         = matrix(rep(0,length(self$learners)*length(self$delta_range)), ncol = length(self$delta_range))
                          
                          for (j in 1:length(self$learners)){
                            for (k in 1:length(self$delta_range)){
                              temp$Res_mat_samples[j,k]      = self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$mean_samples
                              temp$Res_mat_samples_sd[j,k]   = self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$sd_samples
                              temp$Res_mat_acc[j,k]          = self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$mean_accuracy
                              #ylim_min = min(ylim_min,self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$mean_accuracy )
                              #xlim_max = max(xlim_max,self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$max_samples)
                            }
                          }
                          
                          
                          ylim_min		=	min(temp$Res_mat_acc)
                          xlim_max   	= max(temp$Res_mat_samples)
                          
                          
                          
                          legend_vec = c(self$result_statistics[[1]]$name)
                          
                          plot( temp$Res_mat_samples[1,] , temp$Res_mat_acc[1,], type="l",ylab="",xlab="",cex.axis=cex_const, ylim=c(ylim_min-0.1,1), xlim=c(0,xlim_max)	)
                          #lines(temp$Res_mat_samples[1,] + temp$Res_mat_samples_sd[1,], temp$Res_mat_acc[1,]  , col=1, lty=2)
                          #lines(temp$Res_mat_samples[1,] - temp$Res_mat_samples_sd[1,], temp$Res_mat_acc[1,]  , col=1, lty=2)
                          
                          title( main=paste(self$problem_inst$diff,  ", n = ", self$problem_inst$n_arms), xlab="Iterations", ylab="Sucess Rate",cex.lab=cex_const) 
                          
                          for (j in 2:length(self$learners)){
                            
                            lines(temp$Res_mat_samples[j,] , temp$Res_mat_acc[j,] , col=j)
                            #lines(temp$Res_mat_samples[j,] + temp$Res_mat_samples_sd[j,], temp$Res_mat_acc[j,]  , col=j, lty=2)
                            #lines(temp$Res_mat_samples[j,] - temp$Res_mat_samples_sd[j,], temp$Res_mat_acc[j,]  , col=j, lty=2)
                            
                            
                            
                            legend_vec = c(legend_vec,self$result_statistics[[((j-1)*length(self$delta_range)+1)]]$name)
                          }
                          
                          legend("bottomright",legend=legend_vec,col=1:length(self$learners),lty=rep(1,length(self$learners)),cex=1.5)
                          
                          
                          
                          
                        },
                        
                        getTables = function() {
                          
                           
                          
                          Res_mat_samples     = matrix(rep(0,length(self$learners)*length(self$delta_range)), ncol = length(self$delta_range))
                          Res_mat_samples_sd  = matrix(rep(0,length(self$learners)*length(self$delta_range)), ncol = length(self$delta_range))
                          Res_mat_acc         = matrix(rep(0,length(self$learners)*length(self$delta_range)), ncol = length(self$delta_range))
                          
                          for (j in 1:length(self$learners)){
                            for (k in 1:length(self$delta_range)){
                              Res_mat_samples[j,k]      = self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$mean_samples
                              Res_mat_samples_sd[j,k]   = self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$sd_samples
                              Res_mat_acc[j,k]          = self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$mean_accuracy
                              #ylim_min = min(ylim_min,self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$mean_accuracy )
                              #xlim_max = max(xlim_max,self$result_statistics[[((j-1)*length(self$delta_range)+k)]]$max_samples)
                            }
                          }
                          
                          return(list(Res_mat_samples = Res_mat_samples, Res_mat_samples_sd = Res_mat_samples_sd, Res_mat_acc = Res_mat_acc))
                          
                          
                        },
                        
                        
                        reportRuntimes = function() {
                          
                          
                          
                          Time_Steps = self$problem_inst$time_horizon
                          
                          
                          temp = c(self$result_statistics[[1]]$mean_cum_time[Time_Steps], self$result_statistics[[1]]$sd_cum_time[Time_Steps])
                          
                          
                          for (j in 2:length(self$learners)){
                            
                            temp = rbind(temp,  c(self$result_statistics[[j]]$mean_cum_time[Time_Steps],self$result_statistics[[j]]$sd_cum_time[Time_Steps]) )
                            
                            
                          }
                          
                          
                          return(temp)
                          
                          
                        }
                        
                      ),
                      
                      
                      
)





