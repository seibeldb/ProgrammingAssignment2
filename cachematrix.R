## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## the makeCacheMatrix function below creates a list of four items in which each item is a function.
## Because all four functions are defined in the same environment, they can all access the same two variables that existed
## inside makeCacheMatrix when it defined these same functions.  We call this enironment our "cache" and it contains
## only two variables, specifically: "x" which will hold a matrix and "inverse" which will hold the inverse of "x".
##      Initially "x" and "inverse" are empty; but two of the functions can update them using the "<<-" operator
##      The programs assume "x" will be a square invertible matrix
##      This is what the functions do:
##        1) set(x)  --stores "x" into the cache variable that is also named "x" 
##                      and also sets the value in "inverse" to NULL (in case is was previously used)
##        2) get     --returns the value of "x" from the cache
##        3) setinverse(I)  -- stores "I" into the cache variable named "inverse".
##        4) getinverse  returns the value of "inverse" from the cache
##
##  The first step is to create our own function list and "cache" in the global environment.
##   Source the program and then run the following command:
##      myCM <- makeCacheMatrix()  ###  now we have a list "myCM" that has a Cache environment and four functions.
##          To inspect the environment of myCM, we can use these commands:
##              ls("myCM")  -- which displays the four function names
##              get("x",environment(myCM$get))  --  which displays the contents of cache variable "x"
##              get("inverse",environment())    --  which displays the contents of cache variable "inverse"
##      we can also use the functions we created to verify they display the same values shown by "get" above.
##      we invoke these functions using the "$" "function" to call functions stored in our "myCM"
##              myCM$get()  --- displays the contents of the cache variable "x"
##              myCM$getinverse()   --displays the contents of the cache variable "inverse"
##

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL  ## must calculate new inverse when matrix is updated
        }
        get <- function() x
        setinverse <- function(I) inverse <<- I
        getinverse <- function() inverse
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
##  Next we need to define a function that can caclute the inverse of a matrix.  
##  This function will check the values already stored in cache.
##     test "x" before trying to solve "x" for its inverse
##     case "x" is empty  --- send user an appropriate message.
##     case "x" has a determinant of zero -- send user an appropriate error message
##     test "inverse" to see if it is already stored in cache
##     case "inverse" is not NULL --- get and return the inverse from cache and sends user a message.
##     case "inverse" is NULL -- 
##                       function reads matrix "x" from cache and uses tryCatch(solve...) to calculate it's inverse
##                       case "solve" throws ERRORs - use tryCatch to send use an appropriate error message
##      finally          call myM$setinverse to store the inverse matrix in the cache variable named "inverse"
##                       and return the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if (!is.matrix(x$get())) {
                message("Sorry, there is no matrix in this cache")}
        else {
                I <- x$getinverse()
                if (!is.null(I)) {
                        message("This matrix was already solved, giving you the cached inverse")
                        return(I)
                }
                else {
                        matrix <- x$get()
                        deter<-det(matrix)   # if deter=0, the inverse does not exist
                        if (!deter) {   #!deter means zero
                                message("Sorry, your matrix has a determinant of zero, so the inverse cannot be determined")
                                message("You could ry this matrix instead: matrix(data=c(2,2,2,3),nrow=2,ncol=2)")
                                message("Test your matrix with det(matrix) -- is not 0")
                                message("returning your original matrix")
                                return(matrix)
                        }
                        else {
                                out <- tryCatch({
                                                message("Attempting to solve your matrix")
                                                I<-solve(matrix)
                                                x$setinverse(I)
                                                message("Done: solved and saved in cache !")
                                                I
                                               },
                                              error=function(cond) {
                                                      message("R cannot not solve this matrix:")
                                                      message(paste("R error message is:",cond))
                                                      message("returning your original matrix:")
                                                      return(matrix)
                                              }
                                              )
                                return(out)    
                        }
                }
        }
}
##  now create a matrix and store it into cache using myCM$set.
##  then call cacheSolve and verify it solves and returns the inverse of matrix "x" that is in cache
##  then call cacheSolve again to verify that it returns the saved inverse from cache
##  then update the matrix "x" in cache and call cacheSolve to verify it solves the new matrix
##  etc....
##  this matrix can be solved:  myM<-matrix(data=c(2,2,2,3),nrow=2,ncol=2)
##  this matrix has determinant of zero :  myBad<-matrix(data=c(1,1,1,1),nrow=2,ncol=2)