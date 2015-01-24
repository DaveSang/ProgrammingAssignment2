## The following functions allow the user to invert a matrix using the cache.
## makeCacheMatrix generates a customized matrix that can cache its inverse.
## CacheSolve takes an object from makeCacheMatrix and returns the inverse of its customised matrix.
## A demonstration is available at the end of this script.


makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
         x <<- y
         inv <<- NULL
       }
     get <- function() x
    setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }
 
## The following returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) { 
                                   
       inv <- x$getinverse()
       if(!is.null(inv)) {
           message("Nothing has changed - I'm going to be smart and get the cached data.")
      +    return(inv)
         }
       data <- x$get()
      inv <- solve(data)
       x$setinverse(inv)
       inv
     }

## And now a demonstration

x = rbind(c(-4/5, 11.2), c(11.2, -4/5)) ## combing 2 vectors to make a matrix and calling it 'x'.
m = makeCacheMatrix(x)                  ## generating a customised matrix that can cache its inverse.
m$get()                                 ## gets the customised, invertibale matrix.
cacheSolve(m)                           ## If the cacheSolve function run for the first time  
                                        ## it takes an object from makeCacheMatrix and returns the inverse of its 
                                        ## customised matrix via the set inverse function.
cacheSolve(m)                           ## If run again and nothing has changed, the function looks to the cache and 
                                        ## posts the message 
                                        ## "Nothing has changed - I'm going to be smart and return the cached data."
