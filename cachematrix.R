## WOW this was tough!!!

## The code works by using a control variable 'm'

## 'm' is a variable that will control wheter to use the cached matrix variables

## When 'cachSolve()'is first called 'm' will not be assigned NULL as
## 'm' only gets the NULL variable after 'makeCacheMatrix()' is first called

## 'm' is used in 'cacheSolve()'as the control variable to print
## "getting cached data" 
 
## the 'list' below assigns all the calling variables to named objects and
## is being used later in 'cacheSolve()'

## 'set' is the the matrix 'x' and 'y' will also be set to 'x' 
## 'get' returns 'x' 

## 'setInv' initially will return NULL until 'cacheSolve()' is applied
## 'getInv' returns 'm' 
 
makeCacheMatrix <- function(x = matrix()) {

  	m <- NULL			 
  	set <- function(y) {
    	x <<- y
    	m <<- NULL
  	}

  get <- function() x
  setInv <- function(Inv) m <<- Inv
  getInv <- function() m
  list(set = set, get = get,
  setInv = setInv,
  getInv = getInv)
  
  
}


## This function is passed the 'x' value; the matrix

## 'm' is defined initially as NULL so it will NOT envoke the
## 'if statement' 

## 'solve()' uses the 'get' object and is attached to a variable called 'data' and
## 'solve()' is used to invert the matrix and the product passed to 'm' and so NOT
## making it NULL

## The 'setInv' object gets the value of 'm' and 'm' is returned

## On calling this function again 'm' will be NOT NULL so WILL envoke the
## 'if statement' so the message "getting cached data" will be printed

cacheSolve <- function(x, ...) {
        
  m <- x$getInv()
  	
	if(!is.null(m)) {
    	message("getting cached data")
    	return(m)
  	}

  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m

}
