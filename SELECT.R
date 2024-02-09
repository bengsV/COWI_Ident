#' @title SELECT Learner
#'
#' @include Learner.R
#'
#' @description
#' This Learner specializes [Learner] to match the SELECT algorithm
#'
SELECT = R6Class("SELECT", inherit = Learner,
                 public = list(
                   
                   
                   #' @field m (`numeric()`)\cr
                   #' Stores the comparisons made per arm pair.
                   m = 1,
                   
                   
                   #' @field n (`integer()`)\cr
                   #' Number of arms.
                   n = NULL,
                   
                   #' @field Count (`numeric(n,n)`)\cr
                   #' Stores the number wins, i.e., how often an arm has won against another.
                   Count = NULL,
                   
                   
                   #' @field A (`Boolean(n)`)\cr
                   #' Stores the set of active arms.
                   A = NULL,
                   
                   
                   #' @field schedule (`list()`)\cr
                   #' Stores the schedule of how to duel the active arms.
                   schedule = NULL,
                   
                   #' @field Delta (`numeric()`)\cr
                   #' The smallest gap parameter.
                   Delta = 0.01,
                   
                   
                   
                   #' @description
                   #' Creates a new instance of this [R6][R6::R6Class] class.
                   initialize = function(num_arms, delta, Delta){
                     super$initialize(delta = delta)
                     
                     self$n            = num_arms 
                     self$A            = rep(TRUE,num_arms)  
                     self$m            = ceiling(self$delta2mTransformer(num_arms, delta, Delta))
                     self$Delta        = Delta  
                     self$Count        = matrix(rep(0,num_arms*num_arms),ncol=num_arms)
                     self$schedule     = list()
                     for (i in 1:floor(self$n/2)) {
                       self$schedule[[i]] = c(2*(i)-1,2*i)
                     }
                    
                     
                   },
                   
                   #' @description
                   #' resets the internal statistics of the learner
                   #' 
                   clear = function(){      
                     super$clear()   
                     self$A            = rep(TRUE,self$n) 
                     self$Count        = matrix(rep(0,self$n*self$n),ncol=self$n)
                     self$m            = ceiling(self$delta2mTransformer(self$n, self$delta, self$Delta))
                     self$Delta        = self$Delta  
                     self$Count        = matrix(rep(0,self$n*self$n),ncol=self$n)
                     self$schedule     = list()
                     for (i in 1:floor(self$n/2)) {
                       self$schedule[[i]] = c(2*(i)-1,2*i)
                     }
                   },
                   
                   
                   
                   delta2mTransformer = function(n, delta, Delta){
                     return( ceiling( (1+log(1/delta)/log(log(n,2)))*(log(2)/2)*log(log(n,2),2)/Delta^2) )
                   },
                   
                   #' @description
                   #' sets the seeked error probability of the learner
                   #' 
                   setDelta = function(delta) {    
                     super$setDelta(delta)   
                     self$m  = self$delta2mTransformer(self$n, delta, self$Delta)
                   },
                   
                   
                   newSchedule = function(){
                     self$schedule     = list()
                     active_arms       = (1:self$n)[self$A]
                     #print("active")
                     #print(active_arms)
                     for (i in 1:floor(length(active_arms)/2)) {
                       self$schedule[[i]] = c(active_arms[(2*(i)-1)],active_arms[(2*i)])
                     }
                   },
                   
                   
                   action = function(data_model) {
                     return (self$schedule[[1]])
                   },
                   
                   
                   update = function(chosen_arms,data_model) { 
                     
                     temp 							= 	data_model$getFeedback(c(chosen_arms[1],chosen_arms[2]))
                     
                     if (temp==1){
                       
                       self$Count[chosen_arms[1],chosen_arms[2]] = self$Count[chosen_arms[1],chosen_arms[2]] + 1
                       
                       if ( self$Count[chosen_arms[1],chosen_arms[2]]>self$m/2  ){
                         self$A[chosen_arms[2]]  = FALSE
                         self$schedule           = self$schedule[-1]
                         if( (length(self$schedule)==0) && (sum(self$A)>1)){
                           self$newSchedule()
                         }
                         
                       }
                     }
                     if(temp==2){
                       # nothing happens as SELECT cannot deal with indifference feedback
                     }
                     if(temp==3){                         
                       
                       self$Count[chosen_arms[2],chosen_arms[1]] = self$Count[chosen_arms[2],chosen_arms[1]] + 1
                       
                       if ( self$Count[chosen_arms[2],chosen_arms[1]]>self$m/2  ){
                         self$A[chosen_arms[1]]  = FALSE
                         self$schedule           = self$schedule[-1]
                         if( (length(self$schedule)==0)&& (sum(self$A)>1)){
                           self$newSchedule()
                         }
                         
                       }
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