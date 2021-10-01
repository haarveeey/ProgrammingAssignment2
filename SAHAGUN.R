> makeCacheMatrix <- function(x = matrix()) { ## TO MAKE CACHE
  +     inv <- NULL ## TO EMPTY THE MATRIX
  +     get <- function() x ## TO GET THE FUNCTION
  +     set <- function(y) {
    +         x <<- y ## INVERSE MATRIX
    +         inv <<- NULL
    +     }
  +     getinv <- function() inv ## SETTING THE FUNCTION
  +     setinv <- function(inverse) inv <<- inverse
  +     list(get=get, set=set, getinv=getinv, setinv=setinv)
  + }
> cacheSolve <- function(x, ...) { ## GETTING THE INVERSE MATRIX
  +     inv <- x$getinv() ## SETTING THE FUNCTION FOR INVERSE MATRIX 
  +     if (!is.null(inv)) {
    +         message("inverse is cached") ## RESULT
    +         return(inv) ## RETURN TO INVERSE MATRIX
    +     }
  +     m <- x$get() ## GET INVERSE
  +     inv <- solve(m, ...) ## SOLVING THE MATRIX
  +     x$setinv(inv)
  +     return(inv) ## RETURN TO INVERSE FUNCTION
  + }