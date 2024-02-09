#' @title Learner Class
#' 
#' @include Feedback.R
#'
#' @description
#' This is the abstract base class for learner objects like [POCOWIST], [SELECT], [SEEBS] and [ETV].
#'



Learner = R6Class("Learner",
                  public = list(
                    
                    
                    #' @field samples (`numeric()`)\cr
                    #' Stores the sample complexity of the learner.
                    samples = NULL,
                    
                    
                    #' @field choices (`integer()`)\cr
                    #' Stores the choices made by the learner over the time.
                    choices = NULL,
                    
                    
                    #' @field times (`numeric()`)\cr
                    #' Stores the time elapsed the learner needed to make an action and update its internal statistics.
                    times = NULL,
                    
                    #' @field delta (`numeric()`)\cr
                    #' Stores the seeked error porbability.
                    delta = 0,
                    
                    #' @field target_arm (`integer()`)\cr
                    #' Stores the returned arm of the learner.
                    target_arm = 0,
                    
                    
                    #' @description
                    #' Creates a new instance of this [R6][R6::R6Class] class.
                    #'
                    #' Note that this object is typically constructed via a derived classes, e.g., [POCOWIST], [SELECT], [SEEBS] and [ETV].
                    initialize = function(delta) {
                      
                      self$samples        = 0
                      self$choices        = list()
                      self$times          = c()
                      self$delta          = delta
                      self$target_arm     = 0
                    },
                    
                    
                    #' @description
                    #' Printer.
                    #' @param ... (ignored).
                    print = function() { 
                      cat(" BLA ")
                    },
                    
                    #' @description
                    #' provides the learner with the problem instance to interact with
                    #' 
                    setDataModel = function(feedback) {
                      data_model = feedback
                    },
                    
                    #' @description
                    #' sets the seeked error probability of the learner
                    #' 
                    setDelta = function(delta) {
                      self$delta = delta
                    },
                    
                    #' @description
                    #' resets the internal statistics of the learner
                    #' 
                    clear = function(){
                      self$samples        = 0
                      self$choices        = list()
                      self$times          = c()
                      self$delta          = 0.1
                      self$target_arm     = 0
                    },
                    
                    #' @description
                    #' performs an action of the learner.
                    #' Note that this function is defined via the derived classes, e.g., [POCOWIST], [SELECT], [SEEBS] and [ETV].
                    #'
                    #' @return
                    #' Returns the index of the arms to be chosen. 
                    action = function(data_model) {
                      
                    },
                    
                    #' @description
                    #' updates the internal statistics of the learner.
                    #' Note that this function is defined via the derived classes, e.g., [POCOWIST], [SELECT], [SEEBS] and [ETV].
                    #'
                    #' @param choice (`integer(action_size)`)\cr.
                    #' 
                    #' @param feedback \cr.
                    #' 
                    #'
                    update = function(chosen_arms, data_model) {
                      
                    },
                    
                    #' @description
                    #' returns whether the learner has found the target arm.
                    #' Note that this function is defined via the derived classes, e.g., [POCOWIST], [SELECT], [SEEBS] and [ETV].
                    #'
                    #' 
                    #' @return
                    #' Returns the TRUE if the learner has found the target arm, else FALSE.
                    #'
                    finished = function() {
                      
                    },
                    
                    #' @description
                    #' running the learner for the given problem instance.
                    #'
                    #' @param feedback \cr.
                    #' 
                    #'
                    run = function(data_model) {
                      
                      while (!self$finished()){
                        
                        start_time 							              = Sys.time()
                        chosen_arms   						            = self$action(data_model)
                        #print("chosen arm")
                        #print(chosen_arms)
                        self$update(chosen_arms, data_model)
                        end_time 							                = Sys.time()
                        #print("Time")
                        #print("sample number")
                        #print(self$samples)
                        
                        self$times[self$samples+1]              = end_time - start_time
                        self$choices[[self$samples+1]]          = chosen_arms
                        self$samples                          = self$samples +1		
                        
                      }
                      
                    }
                    
                    
                  ),
                  
                  
                  
)

