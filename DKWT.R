#' @title DKWT Learner
#'
#' @include Learner.R
#'
#' @description
#' This Learner specializes [Learner] to match the DKWT algorithm (restricted to duels, i.e. k=2)
#'
DKWT = R6Class("DKWT", inherit = Learner,
                 public = list(
                    
                   #' @field n (`integer()`)\cr
                   #' Number of arms.
                   n = NULL,
                   
                   #' @field P (`numeric(n,n)`)\cr
                   #' Stores the estimate of the pairwise preference matrix.
                   P = NULL,
                   
                   #' @field Count (`numeric(n,n)`)\cr
                   #' Stores the counts, i.e., how often a pair was used.
                   Count = NULL,
                   
                   #' @field A (`Boolean(n)`)\cr
                   #' Stores the set of active arms.
                   A = NULL,
                   
                   #' @field schedule (`integer()`)\cr
                   #' Stores the schedule of how to duel the next arms.
                   schedule = NULL,
                   
                   #' @field gamma_r (`numeric`)\cr
                   #' Stores the round-wise gamma parameter.
                   gamma_r = NULL,
                   
                   #' @field h_r (`numeric`)\cr
                   #' Stores the round-wise h parameter.
                   h_r = NULL,
                   
                   #' @field T_r (`numeric`)\cr
                   #' Stores the round-wise comparisons.
                   T_r = NULL,
                   
                   #' @field r (`numeric`)\cr
                   #' Stores the round counter.
                   r = NULL,
                   
                   #' @description
                   #' Creates a new instance of this [R6][R6::R6Class] class.
                   initialize = function(num_arms, delta){
                     
                     self$n            = num_arms 
                     super$initialize(delta = delta)
                     self$P            = matrix(rep(0.5,num_arms*num_arms),ncol=num_arms)
                     self$Count        = matrix(rep(0,num_arms*num_arms),ncol=num_arms)
                     self$A            = rep(TRUE,num_arms)  
                     self$schedule     = sample(1:(num_arms),2,replace=FALSE)
                     self$gamma_r      = 6*self$delta/(pi^2)
                     self$h_r          = 1/4
                     self$T_r          = self$TrComputation(self$h_r,self$gamma_r)
                     self$r            = 1
                   },
                   
                   #' @description
                   #' resets the internal statistics of the learner
                   #' 
                   clear = function(){      
                     super$clear()   
                     self$P            = matrix(rep(0.5,self$n*self$n),ncol=self$n)
                     self$Count        = matrix(rep(0,self$n*self$n),ncol=self$n)
                     self$A            = rep(TRUE,self$n)  
                     self$schedule     = sample(1:(self$n),2,replace=FALSE)
                     self$delta        = self$delta/ceiling(self$n)
                     self$gamma_r      = 6*self$delta/(pi^2)
                     self$h_r          = 1/4
                     self$T_r          = self$TrComputation(self$h_r,self$gamma_r)
                     self$r            = 1
                   },
                   
               
                   TrComputation = function(h, gamma){
                     return( ceiling( 8/h^2*log((4/gamma))  )  )
                   },
                   
                   #' @description
                   #' sets the seeked error probability of the learner
                   #' 
                   setDelta = function(delta) {    
                     self$delta        = delta/ceiling(self$n) 
                     self$gamma_r      = 6*self$delta/(pi^2)
                     self$h_r          = 1/4
                     self$T_r          = self$TrComputation(self$h_r,self$gamma_r)
                     self$r            = 1
                   },
                   
          
                   action = function(data_model) {
          
                     return (self$schedule)
                     
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
                       # nothing happens as DKWT cannot deal with indifference feedback
                     }
                     if(temp==3){                         
                       self$P[chosen_arms[2],chosen_arms[1]] = (1-1/self$Count[chosen_arms[2],chosen_arms[1]])*self$P[chosen_arms[2],chosen_arms[1]] + 1/self$Count[chosen_arms[2],chosen_arms[1]]
                       self$P[chosen_arms[1],chosen_arms[2]] = 1-self$P[chosen_arms[2],chosen_arms[1]]
                     }  
                      
                     
                     self$T_r = self$T_r - 1
                     
                     if(self$T_r == 0){
                       
                       
                       if(abs(self$P[chosen_arms[1],chosen_arms[2]]-self$P[chosen_arms[2],chosen_arms[1]])>self$h_r  ){
                         
                         winner   = chosen_arms[1]
                         loser    = chosen_arms[2]
                         
                         if(self$P[chosen_arms[1],chosen_arms[2]] < self$P[chosen_arms[2],chosen_arms[1]]){
                           winner = chosen_arms[2]
                           loser  = chosen_arms[1]
                         }
                         
                         self$A[loser]  = FALSE
                         
                         #print("Active set")
                         #print(self$A)
                         #print("chosen arms")
                         #print(chosen_arms)
                         
                         if(sum(self$A)<=2){
                           self$schedule  = which(self$A==TRUE)
                         }
                         else{
                           self$A[winner] = FALSE
                           self$schedule  = c(winner,sample((1:self$n)[self$A],1,replace=FALSE))
                           self$A[winner] = TRUE
                         }
                         self$r             = 1
                         
                       }
                       else{ # UNSURE case
                         self$r             = self$r + 1
                       }
                       
                       self$gamma_r       = 6*self$delta/(pi^2*self$r^2)
                       self$h_r           = 2^(-self$r-1)
                       self$T_r           = self$TrComputation(self$h_r,self$gamma_r)
                     }
                     
                     
                   },
                   
                   finished = function() { 
                     
                     
                     if(sum(self$A)!=1){
                       return (FALSE)
                     }
                     
                     
                     self$target_arm = (1:self$n)[self$A]
                     return (TRUE) 
                     
                   }
                 )
)