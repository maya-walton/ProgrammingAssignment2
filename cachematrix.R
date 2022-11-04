## Cache Matrix Functions
## 

## Function takes in a numeric matrix, x, and returns a list with 4 functions; set, get, setinverse, and getinverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #initialize inverse
  set <- function(y){ #function for setting new x
    x <<- y
    i <<- NULL
  }
  get <- function() x #function to return x
  setinverse <- function(inverse) i <<- inverse #function to set new inverse
  getinverse <- function() i #function to return inverse
  list(set = set, get = get, #create list for reference
       setinverse = setinverse,
       getinverse = getinverse)
}


## function takes in the cachematrix from above, checks if cached value exists (returns if true) or calculates 
##and sets inverse of matrix if not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() #check if inverse exists
  if(!is.null(i)){ 
    message("getting cached data")
    return(i) #return cached if exists
  }
  data <- x$get() 
  i <- solve(data, ...) #calculate inverse
  x$setinverse(i) #set new inverse
  i
}
