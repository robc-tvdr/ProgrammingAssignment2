## Put comments here that give an overall description of what your
## functions do
#######################################################
# Overall Description:
#
# makeCacheMatrix:
#     - establishes static parent environment for
#       objects input to and output from matrix 
#       inversion
#     - creates functions to: 
#         - set() (initialize) inversion objects
#         - get() input to matrix inversion
#         - set_inv() write inverted matrix to object
#           in static parent environment
#         - get_inv() get inverted matrix object from 
#           static parent environment 
#     - binds functions to list elements for use in
#       cacheSolve
#  
# cacheSolve
#     - gets current object for inverted matrix from 
#       static parent environment
#     - checks to see whether inversion is still cached
#     - if cached (non NULL), returns inversion
#     - if NULL (not cached) ...
#         - performs inversion
#         - assigns inversion to object (cache) in
#           static parent environment
#         - returns inversion
#
#
# NOTE: No directives re: (re)setting (reinitializing)
#       inversion environment, therefore flist$set()
#       is never called
#######################################################

# makeCacheMatrix ...
# rename x (in original code), mtrx_x for readability
# rename y (in original code), mtrx_y for readability

makeCacheMatrix <- function(mtrx_x = matrix()) {
      mtrx_inv <- NULL # locl var & formal arg in 
                       # static parent env      
      
      set <- function(mtrx_y){ # init input/ouput to 
                               # inversion
            mtrx_x <<- mtrx_y
            mtrx_inv <<- NULL
      }
      
      get <- function() mtrx_x # get input to inversion
      
      # set inversion output to object in static
      # parent environment
      set_inv <- function(mtrx_out) mtrx_inv <<- mtrx_out
      
      get_inv <- function() mtrx_inv # getinversion 
                                     # output
      # create function list
      flist <- list(set = set, get = get, 
                    set_inv = set_inv,
                    get_inv = get_inv)
}

# cacheSolve
# renamed x (in original code) to flist for readability

cacheSolve <- function(flist, ...) {
      mtrx_inv <- flist$get_inv # get current inverted
                                # matrix from static
                                # parent env
      
      # check if matrix NULL, i.e. inversion either
      # not done or lost, if not NULL, return
      if (!is.null(mtrx_inv)){
            message("getting cached data")
            return(mtrx_inv)
      }
      
      # steps to (re)create inversion
      mtrx <- flist$get() # get input to inversion
      mtrx_inv_temp <- solve(mtrx) # invert matrix
      flist$set_inv(mtrx_inv_temp)  # assign to object 
                                    # in static parent
                                    # env (Cache)
      mtrx_inv # return inverted matix   
}
