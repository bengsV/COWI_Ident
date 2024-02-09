#' @title PBR-SSCO Learner
#'
#' @include Learner.R
#'
#' @description
#' This Learner specializes [Learner] to match the PBR-SSCO algorithm
#'
PBR_SSCO = R6Class("PBR-SSCO", inherit = Learner,
                 public = list(
                   
                   
                   #' @field Y_hat (`numeric(n,n)`)\cr
                   #' Stores the estimate of the pairwise preference matrix.
                   Y_hat = NULL,
                   
                   #' @field Count (`numeric(n,n)`)\cr
                   #' Stores the counts, i.e., how often a pair was used.
                   Count = NULL,
                   
                   #' @field n (`integer()`)\cr
                   #' Number of arms.
                   n = NULL,
                   
                   #' @field A (`Boolean(n,n)`)\cr
                   #' Stores the set of all pairs of options still racing.
                   A = NULL,
                   
                   #' @field B (`numeric(n)`)\cr
                   #' Stores the set of selected arms.
                   B = NULL,
                   
                   #' @field B (`numeric(n)`)\cr
                   #' Stores the set of selected arms.
                   C = NULL,
                   
                   #' @field D (`numeric(n)`)\cr
                   #' Stores the set of discarded arms.
                   D = NULL,
                   
                   #' @field U (`numeric(n,n)`)\cr
                   #' Stores the upper confidence estimates  of the pairwise preference matrix.
                   U = NULL,
                   
                   #' @field L (`numeric(n,n)`)\cr
                   #' Stores the lower confidence estimates  of the pairwise preference matrix.
                   L = NULL,
                   
                   #' @field w (`integer()`)\cr
                   #' Stores for each arm the cardinality of the set of superior arms.
                   w = NULL,
                   
                   #' @field z (`integer()`)\cr
                   #' Stores for each arm the cardinality of the  set of defeated arms.
                   z = NULL,
                   
                   #' @field k (`integer`)\cr
                   #' Seeked number of top-k options.
                   k = 1,
                   
                   
                   #' @description
                   #' Creates a new instance of this [R6][R6::R6Class] class.
                   initialize = function(num_arms, delta, k = 1){
                     super$initialize(delta = delta)
                     
                     self$n            = num_arms 
                     self$Y_hat        = matrix(rep(0.5,num_arms*num_arms), ncol = num_arms)
                     self$Count        = matrix(rep(0,num_arms*num_arms), ncol = num_arms)
                     self$A            = matrix(rep(TRUE,num_arms*num_arms), ncol = num_arms)  
                     self$B            = c()
                     self$C            = c()
                     self$D            = c()
                     for (i in 1:num_arms){
                       self$Count[i,i] = 0.000000000001
                       self$A[i,i]     = FALSE
                     }
                     self$U            = matrix(rep(1,num_arms*num_arms), ncol = num_arms)
                     self$L            = matrix(rep(0,num_arms*num_arms), ncol = num_arms)
                     self$z            = 0
                     self$w            = 0
                     self$k            = k
                     
                     
                   },
                   
                   
                   #' @description
                   #' resets the internal statistics of the learner
                   #' 
                   clear = function(){      
                     super$clear()  
                     self$Y_hat        = matrix(rep(0.5,self$n*self$n), ncol = self$n)
                     self$Count        = matrix(rep(0,self$n*self$n), ncol = self$n)
                     self$A            = matrix(rep(TRUE,self$n*self$n), ncol = self$n)  
                     self$B            = c()
                     self$C            = c()
                     self$D            = c()
                     for (i in 1:self$n){
                       self$Count[i,i] = 0.000000000001
                       self$A[i,i]     = FALSE
                     }
                     self$U            = matrix(rep(1,self$n*self$n), ncol = self$n)
                     self$L            = matrix(rep(0,self$n*self$n), ncol = self$n)
                     self$z            = 0
                     self$w            = 0
                   },
                   
                   
                   confidence_radius = function(arm_pair){
                     return(sqrt(1/(2*self$Count[arm_pair[1],arm_pair[2]]) * log(  (  pi^2*(self$n)^2*  (self$Count[arm_pair[1],arm_pair[2]])^2 )/(self$delta)  ) )  )
                   },
                   
                   getDefeatedArms = function(i) {
                     score = 0
                     for(j in 1:self$n){
                       if(i==j){
                         next
                       }
                       score = score + 1*(self$U[i,j]<0.5)
                     }
                     return (score)
                   },
                   
                   getSuperiorArms = function(i) {
                     score = 0
                     for(j in 1:self$n){
                       if(i==j){
                         next
                       }
                       score = score + 1*(self$L[i,j]>0.5)
                     }
                     return (score)
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
                     
                     temp_count          = self$Count 
                     temp_count[!self$A] = Inf
                     
                     return (which(temp_count==min(temp_count), arr.ind = TRUE)[1,])
                     
                   },
                   
                   
                   update = function(chosen_arms,data_model) { 
                     
                     temp 							= 	data_model$getFeedback(c(chosen_arms[1],chosen_arms[2]))
                     
                     self$Count[chosen_arms[1],chosen_arms[2]] = self$Count[chosen_arms[1],chosen_arms[2]] + 1
                     self$Count[chosen_arms[2],chosen_arms[1]] = self$Count[chosen_arms[1],chosen_arms[2]]
                     
                     self$Count[chosen_arms[1],chosen_arms[1]] = self$Count[chosen_arms[1],chosen_arms[1]] + 1
                     self$Count[chosen_arms[2],chosen_arms[2]] = self$Count[chosen_arms[2],chosen_arms[2]] + 1
                     
                     if (temp==1){
                       self$Y_hat[chosen_arms[1],chosen_arms[2]] = (1-1/self$Count[chosen_arms[1],chosen_arms[2]])*self$Y_hat[chosen_arms[1],chosen_arms[2]] + 1/self$Count[chosen_arms[1],chosen_arms[2]]
                       self$Y_hat[chosen_arms[2],chosen_arms[1]] = 1-self$Y_hat[chosen_arms[1],chosen_arms[2]]
                     }
                     if(temp==2){
                       # nothing happens as PBR-SSCO cannot deal with indifference feedback
                     }
                     if(temp==3){                         
                       self$Y_hat[chosen_arms[2],chosen_arms[1]] = (1-1/self$Count[chosen_arms[2],chosen_arms[1]])*self$Y_hat[chosen_arms[2],chosen_arms[1]] + 1/self$Count[chosen_arms[2],chosen_arms[1]]
                       self$Y_hat[chosen_arms[1],chosen_arms[2]] = 1-self$Y_hat[chosen_arms[2],chosen_arms[1]]
                     } 
                     
                     temp                                   = self$confidence_radius(chosen_arms)
                     self$L[chosen_arms[1],chosen_arms[2]]  = self$Y_hat[chosen_arms[1],chosen_arms[2]] - temp
                     self$U[chosen_arms[1],chosen_arms[2]]  = self$Y_hat[chosen_arms[1],chosen_arms[2]] + temp
                     self$L[chosen_arms[2],chosen_arms[1]]  = self$Y_hat[chosen_arms[2],chosen_arms[1]] - temp
                     self$U[chosen_arms[2],chosen_arms[1]]  = self$Y_hat[chosen_arms[2],chosen_arms[1]] + temp
                     
                     for (i in 1:self$n){
                       self$z[i]                            = self$getDefeatedArms(i)
                       self$w[i]                            = self$getSuperiorArms(i)
                     }
                     
                     for (i in 1:self$n){
                       if(is.element(i,unique(self$C,self$D))){
                         next
                       }                           
                       temp_c = 0
                       temp_d = 0
                       
                       for (j in 1:self$n){
                         temp_c = temp_c + 1*(self$n - self$z[j]<self$w[i])
                         temp_d = temp_d + 1*(self$n - self$w[j]<self$z[i])
                       }
                       # update C and D (if necessary)
                       if(self$n-self$k<temp_c){
                         self$C = c(self$C,i)
                       }
                       if(self$k<temp_d){
                         self$D = c(self$D,i)
                       }
                     }
 
                    
                     
                     for(i in (1:self$n)){
                       for (j in (1:self$n)){
                         if(self$A[i,j]==FALSE){
                           next
                         }
                         
                         if(self$L[i,j] > 0.5 || self$U[i,j] < 0.5 ||
                            ( is.element(i,unique(c(self$C,self$D)))  && is.element(j,unique(c(self$C,self$D))) )){
                           self$A[i,j]                          = FALSE
                         }
                       }
                     }
                     
                     
                   },
                   
                   finished = function() { 
                     
                     
                     if(sum(self$A)!=0){
                       return (FALSE)
                     }
                     
                     temp_cp_scores = self$getCopelandScores(self$Y_hat)
                     
                     
                     if (self$k ==1){
                       self$target_arm = (which(temp_cp_scores==max(temp_cp_scores), arr.ind = TRUE)[1])
                     }
                     else{
                       
                       top_k = (which(temp_cp_scores==max(temp_cp_scores), arr.ind = TRUE)[1])
                       
                       for(i in 1:(self$k-1)){
                         temp_cp_scores[top_k] = - Inf
                         top_k = c(top_k, which(temp_cp_scores==max(temp_cp_scores), arr.ind = TRUE)[1])
                       }
                       self$target_arm = top_k
                     }
                     return (TRUE) 
                     
                   }
                 )
)