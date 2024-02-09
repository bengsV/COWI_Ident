#' @title POCOWIST Learner
#'
#' @include Learner.R
#'
#' @description
#' This Learner specializes [Learner] to match the POCOWIST algorithm (also Tra-POCOWIST)
#'
POCOWIST = R6Class("POCOWIST", inherit = Learner,
                  public = list(
                    
                    
                    #' @field poco (`numeric(n)`)\cr
                    #' Stores the potential Copeland scores.
                    poco = NULL,
                    
                    #' @field hat_co (`numeric(n)`)\cr
                    #' Stores the current Copeland scores.
                    hat_co = NULL,
                    
                    #' @field D (`list()`)\cr
                    #' Stores for each arm the set of already compared arms.
                    D = NULL,
                    
                    #' @field n (`integer()`)\cr
                    #' Number of arms.
                    n = NULL,
                    
                    #' @field S (`integer()`)\cr
                    #' Auxiliary counter variable. 
                    S = c(0,0,0),
                    
                    #' @field transitive (`Boolean`)\cr
                    #' Whether the transitive version should be used.
                    transitive = FALSE,
                    
                    #' @field W (`list()`)\cr
                    #' Stores for each arm the set of defeated arms.
                    W = NULL,
                    
                    #' @field I (`list()`)\cr
                    #' Stores for each arm the set of indifference arms.
                    I = NULL,
                    
                    #' @field L (`list()`)\cr
                    #' Stores for each arm the set of superior arms.
                    L = NULL,
                    
                    #' @description
                    #' Creates a new instance of this [R6][R6::R6Class] class.
                    initialize = function(num_arms, delta, transitive = FALSE){
                      super$initialize(delta = delta)
                      
                      self$n            = num_arms 
                      self$poco         = rep(num_arms-1,num_arms)
                      self$hat_co       = rep(0,num_arms)
                      self$D            = lapply(1:num_arms, function(x) x)
                      self$S            = c(0,0,0)
                      self$delta        = delta/choose(num_arms,2)
                      
                      if(transitive){  
                        self$delta      = delta/(num_arms*log(num_arms))
                      }
                      self$transitive   = transitive
                      self$W            = lapply(1:num_arms, function(x) c())
                      self$I            = lapply(1:num_arms, function(x) c())
                      self$L            = lapply(1:num_arms, function(x) c())
                      
                    },
                    
                    #' @description
                    #' resets the internal statistics of the learner
                    #' 
                    clear = function(){      
                      super$clear() 
                      self$poco         = rep(self$n-1,self$n)
                      self$hat_co       = rep(0,self$n)
                      self$D            = lapply(1:self$n, function(x) x)
                      self$S            = c(0,0,0)
                      self$W            = lapply(1:self$n, function(x) c())
                      self$I            = lapply(1:self$n, function(x) c())
                      self$L            = lapply(1:self$n, function(x) c())
                    },
                    
                    #' @description
                    #' sets the seeked error probability of the learner
                    #' 
                    setDelta = function(delta) {
                      
                      self$delta        = delta/choose(self$n,2)
                      
                      if(self$transitive){  
                        self$delta      = delta/(self$n*log(self$n))
                      }
                    },
                    
                    score_update = function(arm_pair,k){
                      
                      if(k==1){
                        self$hat_co[arm_pair[1]] = self$hat_co[arm_pair[1]] + 1
                      }
                      if(k==2){
                        self$hat_co[arm_pair[1]] = self$hat_co[arm_pair[1]] + 0.5
                        self$hat_co[arm_pair[2]] = self$hat_co[arm_pair[2]] + 0.5
                      }
                      if(k==3){
                        self$hat_co[arm_pair[2]] = self$hat_co[arm_pair[2]] + 1
                      }
                      self$D[[arm_pair[1]]]     <- c(self$D[[arm_pair[1]]],arm_pair[2])
                      self$D[[arm_pair[2]]]     <- c(self$D[[arm_pair[2]]],arm_pair[1])
                      self$poco[arm_pair[1]]    <- self$n - length(self$D[[arm_pair[1]]]) + self$hat_co[arm_pair[1]]
                      self$poco[arm_pair[2]]    <- self$n - length(self$D[[arm_pair[2]]]) + self$hat_co[arm_pair[2]]
                      
                    },
                    
                    
                    transitive_score_update = function(arm_pair,k){
                      
                      if(k==1){
                        # 
                        # print("arm_pair")
                        # print(arm_pair)
                        # print("arm_pair1 infos")
                        # print("score")
                        # print(self$hat_co[arm_pair[1]])
                        # print("D")
                        # print(self$D[[arm_pair[1]]])
                        # print("W")
                        # print(self$W[[arm_pair[1]]])
                        # print("arm_pair2 W")
                        # print(self$W[[arm_pair[2]]])
                        
                        if(length(self$W[[arm_pair[2]]])!=0){
                          self$hat_co[arm_pair[1]] = self$hat_co[arm_pair[1]] + length( as.set(self$D[[arm_pair[1]]]),set_complement(as.set(self$W[[arm_pair[2]]])))
                        }
                        if(length(self$I[[arm_pair[2]]])!=0){
                          self$hat_co[arm_pair[1]] = self$hat_co[arm_pair[1]] + length( as.set(self$D[[arm_pair[1]]]),set_complement(as.set(self$I[[arm_pair[2]]])))
                        } 
                        
                        self$hat_co[arm_pair[1]] = self$hat_co[arm_pair[1]] + 1
                        

                        self$W[[arm_pair[1]]]     <- unique(c(self$W[[arm_pair[1]]], self$W[[arm_pair[2]]], self$I[[arm_pair[2]]], arm_pair[2])) # strict pref. transitivity
                        self$D[[arm_pair[1]]]     <- unique(c(self$D[[arm_pair[1]]], self$W[[arm_pair[2]]], self$I[[arm_pair[2]]], arm_pair[2]))
                        
                        self$D[[arm_pair[2]]]     <- c(self$D[[arm_pair[2]]],self$L[[arm_pair[1]]],self$I[[arm_pair[1]]], arm_pair[1])
                        self$L[[arm_pair[2]]]     <- c(self$L[[arm_pair[2]]],self$L[[arm_pair[1]]],self$I[[arm_pair[1]]], arm_pair[1])
                        
                        # 
                        # print("post update") 
                        # print("arm_pair1 1 wins")
                        # print("arm_pair1 infos")
                        # print("score")
                        # print(self$hat_co[arm_pair[1]])
                        # print("D")
                        # print(self$D[[arm_pair[1]]])
                        # print("W")
                        # print(self$W[[arm_pair[1]]])
                        # print("arm_pair2 infos")
                        # print("D")
                        # print(self$D[[arm_pair[2]]])
                        # print("W")
                        # print(self$W[[arm_pair[2]]])
                        # print("L")
                        # print(self$L[[arm_pair[2]]])
                        
                      }
                      if(k==2){
                        self$hat_co[arm_pair[1]] = self$hat_co[arm_pair[1]] 
                          length( set_complement(as.set(self$W[[arm_pair[2]]]),as.set(self$D[[arm_pair[1]]]) )   ) + 
                          length( set_complement(as.set(self$I[[arm_pair[2]]]),as.set(self$D[[arm_pair[1]]]) )   ) + 
                          0.5
                        self$hat_co[arm_pair[2]] = self$hat_co[arm_pair[2]] + 
                          length( set_complement(as.set(self$W[[arm_pair[1]]]),as.set(self$D[[arm_pair[2]]]) )   ) + 
                          length( set_complement(as.set(self$I[[arm_pair[1]]]),as.set(self$D[[arm_pair[2]]]) )   ) + 
                          0.5
                        
                        self$W[[arm_pair[1]]]     <- unique(c(self$W[[arm_pair[1]]], self$W[[arm_pair[2]]])) # IP transitivity
                        self$W[[arm_pair[2]]]     <- unique(c(self$W[[arm_pair[2]]], self$W[[arm_pair[1]]])) # IP transitivity
                        
                        self$I[[arm_pair[1]]]     <- unique(c(self$I[[arm_pair[1]]], self$I[[arm_pair[2]]]), arm_pair[2]) # indiff. transitivity
                        self$I[[arm_pair[2]]]     <- unique(c(self$I[[arm_pair[2]]], self$I[[arm_pair[1]]]), arm_pair[1]) # indiff. transitivity
                        
                        self$L[[arm_pair[1]]]     <- unique(c(self$L[[arm_pair[1]]], self$L[[arm_pair[2]]])) # IP transitivity
                        self$L[[arm_pair[2]]]     <- unique(c(self$L[[arm_pair[2]]], self$L[[arm_pair[1]]])) # IP transitivity
                        
                        self$D[[arm_pair[1]]]     <- unique(c(self$D[[arm_pair[1]]], self$W[[arm_pair[2]]], self$I[[arm_pair[2]]], self$L[[arm_pair[2]]], arm_pair[2]))
                        self$D[[arm_pair[2]]]     <- unique(c(self$D[[arm_pair[2]]], self$W[[arm_pair[1]]], self$I[[arm_pair[1]]], self$L[[arm_pair[1]]], arm_pair[1]))
                      }
                      if(k==3){
                        
                        # print("arm_pair")
                        # print(arm_pair)
                        # print("arm_pair1 W")
                        # print(self$W[[arm_pair[1]]])
                        # print("arm_pair2 infos")                        
                        # print("score")
                        # print(self$hat_co[arm_pair[2]])
                        # print("D")
                        # print(self$D[[arm_pair[2]]])
                        # print("W")
                        # print(self$W[[arm_pair[2]]])
                        
                        if(length(self$W[[arm_pair[1]]])!=0){
                          self$hat_co[arm_pair[2]] = self$hat_co[arm_pair[2]] + length( set_complement(as.set(self$D[[arm_pair[2]]]),as.set(self$W[[arm_pair[1]]])))
                        }
                        if(length(self$I[[arm_pair[1]]])!=0){
                          self$hat_co[arm_pair[2]] = self$hat_co[arm_pair[2]] + length( set_complement(as.set(self$D[[arm_pair[2]]]),as.set(self$I[[arm_pair[1]]])))
                        } 
                        
                        self$hat_co[arm_pair[2]] = self$hat_co[arm_pair[2]] + 1
                        
                        # print("set_comp_1")
                        # print(set_complement(as.set(as.set(self$D[[arm_pair[2]]]),self$W[[arm_pair[1]]]) ))
                        # print("set_comp_2")
                        # print(set_complement(as.set(self$I[[arm_pair[1]]]),as.set(self$D[[arm_pair[2]]]) ))
      
                        
                        self$W[[arm_pair[2]]]     <- unique(c(self$W[[arm_pair[2]]], self$W[[arm_pair[1]]], self$I[[arm_pair[1]]], arm_pair[1])) # strict pref. transitivity
                        self$D[[arm_pair[2]]]     <- unique(c(self$D[[arm_pair[2]]], self$W[[arm_pair[1]]], self$I[[arm_pair[1]]], arm_pair[1]))
                        
                        
                        self$D[[arm_pair[1]]]     <- c(self$D[[arm_pair[1]]],self$L[[arm_pair[2]]],self$I[[arm_pair[2]]], arm_pair[2])
                        self$L[[arm_pair[1]]]     <- c(self$L[[arm_pair[1]]],self$L[[arm_pair[2]]],self$I[[arm_pair[2]]], arm_pair[2])
                        

                        
                        # print("post update") 
                        # print("arm_pair2 infos")
                        # print("score")
                        # print(self$hat_co[arm_pair[2]])
                        # print("D")
                        # print(self$D[[arm_pair[2]]])
                        # print("W")
                        # print(self$W[[arm_pair[2]]])
                        
                      } 
                      self$poco[arm_pair[1]]    <- self$n - length(self$D[[arm_pair[1]]]) + self$hat_co[arm_pair[1]]
                      self$poco[arm_pair[2]]    <- self$n - length(self$D[[arm_pair[2]]]) + self$hat_co[arm_pair[2]]
                      

                      
                    },
                    
                    action = function(data_model) {
                      
                      
                      i_t			  = which(self$poco==max(self$poco), arr.ind = TRUE)[1]
                      
                      temp_hat_co = self$hat_co
                      
                      temp_hat_co[c(self$D[[i_t]])] = -1
                      
                      j_t			  = which(temp_hat_co==max(temp_hat_co), arr.ind = TRUE)[1]
                      
                      #Choice
                      #print("Choice")
                      #print(c(i_t,j_t))
                      
                      return (c(i_t,j_t))

                    },
                    
                    
                    update = function(chosen_arms,data_model) { 
                      
                      temp 							= 	data_model$getFeedback(c(chosen_arms[1],chosen_arms[2]))
                      
                    
                      if (temp==1){
                        self$S[1] <- self$S[1]+1
                      }
                      if(temp==2){
                        self$S[2] <- self$S[2]+1
                      }
                      if(temp==3){
                        self$S[3] <- self$S[3]+1
                      } 
                      
                      or = order(self$S, decreasing = T)
                      
                      # end of the 1v1PPR Procedure
                      if (dbeta(0.5, self$S[or][1]+1, self$S[or][2]+1) <= self$delta/2 ){
                        
                        k = which(self$S==max(self$S))
                        
                        if(self$transitive){
                          # todo: write transitive score update
                          self$transitive_score_update(chosen_arms,k)
                        }
                        else{
                          self$score_update(chosen_arms,k)
                        }
                        self$S <- c(0,0,0)
                      }
                      
                    },
                    
                    finished = function() { 
                      
                      temp = which(self$hat_co == max(self$hat_co ), arr.ind = TRUE)[1]
                      if(self$hat_co[temp] < max(self$poco[-temp])){
                        return (FALSE)
                      }

                      
                      self$target_arm = temp
                      return (TRUE) 
                      
                    }
                  )
)