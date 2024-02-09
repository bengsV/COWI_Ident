#' @title SAVAGE Learner
#'
#' @include Learner.R
#'
#' @description
#' This Learner specializes [Learner] to match the SAVAGE algorithm
#'
SAVAGE = R6Class("SAVAGE", inherit = Learner,
                   public = list(
                     
                     
                     #' @field P (`numeric(n,n)`)\cr
                     #' Stores the estimate of the pairwise preference matrix.
                     P = NULL,
                     
                     #' @field Count (`numeric(n,n)`)\cr
                     #' Stores the counts, i.e., how often a pair was used.
                     Count = NULL,
                     
                     #' @field n (`integer()`)\cr
                     #' Number of arms.
                     n = NULL,
                     
                     #' @field W (`Boolean(n,n)`)\cr
                     #' Stores the explore set.
                     W = NULL,
                     
                     #' @field A (`numeric(n,n)`)\cr
                     #' Stores the lower interval values.
                     A = NULL,
                     
                     #' @field B (`numeric(n,n)`)\cr
                     #' Stores the upper interval values.
                     B = NULL,
                     
                     #' @field opt_cope (`numeric(n)`)\cr
                     #' Stores the optimistic Copeland scores.
                     opt_cope = NULL,
                     
                     #' @field pess_cope (`numeric(n)`)\cr
                     #' Stores the pessimistic Copeland scores.
                     pess_cope = NULL,
                     
                     
                     #' @description
                     #' Creates a new instance of this [R6][R6::R6Class] class.
                     initialize = function(num_arms, delta){
                       super$initialize(delta = delta)
                       
                       self$n            = num_arms 
                       self$P            = matrix(rep(0.5,num_arms*num_arms),ncol=num_arms)
                       self$Count        = matrix(rep(0,num_arms*num_arms),ncol=num_arms)
                       self$A            = matrix(rep(0,num_arms*num_arms),ncol=num_arms)
                       self$B            = matrix(rep(1,num_arms*num_arms),ncol=num_arms)
                       self$W            = matrix(rep(TRUE,num_arms*num_arms),ncol=num_arms)  
                       for (i in 1:num_arms){
                         self$Count[i,i] = 0.000000000001
                         self$W[i,i]     = FALSE
                       }
                       self$pess_cope    = rep(0,num_arms)
                       self$opt_cope     = rep(num_arms-1,num_arms)
                       self$delta        = delta
                        
                       
                     },
                     
                     #' @description
                     #' resets the internal statistics of the learner
                     #' 
                     clear = function(){      
                       super$clear()  
                       self$P            = matrix(rep(0.5,self$n*self$n),ncol=self$n)
                       self$Count        = matrix(rep(0,self$n*self$n),ncol=self$n)
                       self$A            = matrix(rep(0,self$n*self$n),ncol=self$n)
                       self$B            = matrix(rep(1,self$n*self$n),ncol=self$n)
                       self$W            = matrix(rep(TRUE,self$n*self$n),ncol=self$n)  
                       for (i in 1:self$n){
                         self$Count[i,i] = 0.000000000001
                         self$W[i,i]     = FALSE
                       }
                       self$pess_cope    = rep(0,self$n)
                       self$opt_cope     = rep(self$n-1,self$n)
                     },
                     
                     
                     confidence_radius = function(arm_pair){
                       return(sqrt(1/(2*self$Count[arm_pair[1],arm_pair[2]]) * log(  (  pi^2*choose(self$n,2)*  (self$Count[arm_pair[1],arm_pair[2]])^2 )/(3*self$delta)  ) )  )
                     },
                     
                     getCopelandScore = function(i,mat) {
                       score = 0
                       for(j in 1:self$n){
                         if(i==j){
                           next
                         }
                         score = score + 1*(mat[i,j]>0.5)
                       }
                       return (score)
                     },
                     
                     getCopelandScores = function(mat) {
                       return (sapply(1:self$n,self$getCopelandScore,mat=mat))
                     },
                     
                    
                     action = function(data_model) {
                       
                       temp_count         = self$Count 
                       temp_count[!self$W] = Inf
                       
                       return (which(temp_count==min(temp_count), arr.ind = TRUE)[1,])
                       
                     },
                     
                     
                     update = function(chosen_arms,data_model) { 
                       
                       temp 							= 	data_model$getFeedback(c(chosen_arms[1],chosen_arms[2]))
                       
                       self$Count[chosen_arms[1],chosen_arms[2]] = self$Count[chosen_arms[1],chosen_arms[2]] + 1
                       self$Count[chosen_arms[2],chosen_arms[1]] = self$Count[chosen_arms[1],chosen_arms[2]]
                       
                       self$Count[chosen_arms[1],chosen_arms[1]] = self$Count[chosen_arms[1],chosen_arms[1]] + 1
                       self$Count[chosen_arms[2],chosen_arms[2]] = self$Count[chosen_arms[2],chosen_arms[2]] + 1
                       
                       if (temp==1){
                         self$P[chosen_arms[1],chosen_arms[2]] = (1-1/self$Count[chosen_arms[1],chosen_arms[2]])*self$P[chosen_arms[1],chosen_arms[2]] + 1/self$Count[chosen_arms[1],chosen_arms[2]]
                         self$P[chosen_arms[2],chosen_arms[1]] = 1-self$P[chosen_arms[1],chosen_arms[2]]
                       }
                       if(temp==2){
                         # nothing happens as SAVAGE cannot deal with indifference feedback
                       }
                       if(temp==3){                         
                         self$P[chosen_arms[2],chosen_arms[1]] = (1-1/self$Count[chosen_arms[2],chosen_arms[1]])*self$P[chosen_arms[2],chosen_arms[1]] + 1/self$Count[chosen_arms[2],chosen_arms[1]]
                         self$P[chosen_arms[1],chosen_arms[2]] = 1-self$P[chosen_arms[2],chosen_arms[1]]
                       } 
                       
                       temp                                   = self$confidence_radius(chosen_arms)
                       self$A[chosen_arms[1],chosen_arms[2]]  = self$P[chosen_arms[1],chosen_arms[2]] - temp
                       self$B[chosen_arms[1],chosen_arms[2]]  = self$P[chosen_arms[1],chosen_arms[2]] + temp
                       self$A[chosen_arms[2],chosen_arms[1]]  = self$P[chosen_arms[2],chosen_arms[1]] - temp
                       self$B[chosen_arms[2],chosen_arms[1]]  = self$P[chosen_arms[2],chosen_arms[1]] + temp
                       self$pess_cope                         = self$getCopelandScores(self$A)
                       self$opt_cope                          = self$getCopelandScores(self$B)
                       
                       max_pess_cope                          = max(self$pess_cope)
                       
                       for(i in (1:self$n)){
                         for (j in (1:self$n)){
                           if(self$W[i,j]==FALSE){
                             next
                           }
                           
                           if(self$A[i,j] > 0.5 || self$B[i,j] < 0.5 ||
                              (max_pess_cope > self$opt_cope[i] && max_pess_cope > self$opt_cope[j])){
                             self$W[i,j]                          = FALSE
                           }
                         }
                       }

                       
                     },
                     
                     finished = function() { 
                       
                        
                       if(sum(self$W)!=0){
                         return (FALSE)
                       }
                       
                       
                       self$target_arm = (which(self$opt_cope==max(self$opt_cope), arr.ind = TRUE)[1])
                       return (TRUE) 
                       
                     }
                   )
)