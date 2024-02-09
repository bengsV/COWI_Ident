#' @title Feedback Class
#' 
#'
#' @description
#' This is the base class for feedback environment objects like [BTM], [Thurstone] and [MSLR].
#'


Feedback = R6Class("Feedback",
                   public = list(
                     
                     #' @field P (`numeric()`)\cr
                     #' The preference matrix for the arms.
                     P = NULL,
                     
                     #' @field P_indiff (`numeric()`)\cr
                     #' The indifference preference matrix for the arms.
                     P_indiff = NULL,
                     
                     #' @field num_arms (`integer()`)\cr
                     #' The number of arms.
                     num_arms = NULL,
                     
                     #' @field gap (`numeric()`)\cr
                     #' The gap parameter of the preference matrix.
                     gap = 0,
                     
                     #' @description
                     #' Creates a new instance of this [R6][R6::R6Class] class.
                     #'
                     #' Note that this object is typically constructed via a derived classes, e.g., [BTM], [Thurstone] and [MSLR].
                     initialize = function(P, P_indiff = 0) {
                       
                       self$P                  = P
                       if(P_indiff == 0){
                            self$P_indiff           = matrix(rep(0,ncol(P)*ncol(P)),ncol=ncol(P))
                       }
                       else{
                            self$P_indiff           = P_indiff
                       }
                       self$num_arms           = ncol(P)
                       self$gap                = min(abs(P[lower.tri(P, diag = FALSE)]-1/2))
                       
                     },
                     
                     
                     #' @description
                     #' Printer.
                     #' @param ... (ignored).
                     print = function() { 
                       cat("  P: ", self$P, "\n", sep = "")
                       cat("  P_indiff: ", self$P_indiff, "\n", sep = "")
                       cat("  number of arms: ", self$num_arms, "\n", sep = "")
                       cat("  gap: ", self$gap, "\n", sep = "")
                     },
                     
                     
                     #' @description
                     #' Returns the number of arms of the problem instance.
                     #'
                     #' 
                     #'
                     #' @return integer
                     #'  
                     getNumberArms = function() {
                       return (self$num_arms)
                     },

                     
                     #' @description
                     #' Returns the ground-truth strict pairwise probability of a specific arm pair.
                     #'
                     #' @param arm_pair (`integer(2)`)\cr.
                     #' 
                     #'
                     #' @return numeric.
                     getPairwiseProbability = function(arm_pair) {
                       return (self$P[arm_pair[1],arm_pair[2]])
                     },
                     
                     #' @description
                     #' Returns the ground-truth pairwise indifference probability of a specific arm pair.
                     #'
                     #' @param arm_pair (`integer(2)`)\cr.
                     #' 
                     #'
                     #' @return numeric.
                     getPairwiseIndifferenceProbability = function(arm_pair) {
                       return (self$P_indiff[arm_pair[1],arm_pair[2]])
                     },
      
                     
                     #' @description
                     #' Returns feedback for a specific pair of arms for the underlying pairwise preference matrices.
                     #' 1: arm_pair[1] is preferred over arm_pair[2]
                     #' 2: indifference between arm_pair[1] and arm_pair[2]
                     #' 3: arm_pair[2] is preferred over arm_pair[1]
                     #'
                     #' @param arm_pair (`integer(2)`)\cr.
                     #' 
                     #'
                     #' @return numeric.
                     getFeedback = function(arm_pair) {
                       
                       
                       return (sample(c(1,2,3),1,replace=F,prob=c(self$getPairwiseProbability(arm_pair), self$getPairwiseIndifferenceProbability(arm_pair)
                                                                  ,  1 - self$getPairwiseIndifferenceProbability(arm_pair) - self$getPairwiseProbability(arm_pair)  )))
                     },
                     
                     
                     #' @description
                     #' Computes the Copeland score of a given arm for the underlying pairwise preference matrix.
                     #'
                     #' @param arm (`integer(1)`)\cr.
                     #' 
                     #'
                     #' @return numeric(1).
                     #' 
                     getCopelandScore = function(i) {
                       score = 0
                       for(j in 1:self$num_arms){
                         if(i==j){
                           next
                         }
                         score = score + 1*(self$P[i,j]>=max(self$P_indiff[i,j],self$P[j,i])) + 0.5*(self$P_indiff[i,j] >=max(self$P[i,j],self$P[j,i])   )
                       }
                       return (score)
                     },
                     
                     #' @description
                     #' Returns the Copeland scores of all arms for the underlying pairwise preference matrix.
                     #'
                     #' 
                     #'
                     #' @return numeric().
                     getCopelandScores = function() {
                       sapply(1:self$num_arms,self$getCopelandScore)
                     },
                     
                     #' @description
                     #' Returns the set of Copeland winners for the underlying pairwise preference matrix.
                     #'
                     #' 
                     #'
                     #' @return integer().
                     getCopeleandWinners = function() {
                       scores = self$getCopelandScores()
                       which(scores == max(scores))
                     }
                     

                     
                   ),
                   
                   
                   
)

