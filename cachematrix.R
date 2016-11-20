## The below functions are to create a matrix and get the inverse of it 

## makeCacheMatrix function creates a list of 4 subfunctions 1. to set a new matrix 2. to view the matrix created 3. to set the inverse 4. to get the inverse

makeCacheMatrix <- function(x = matrix()) {
         ## x is the parameter which is a square inversible matrix
         inv = NULL
         set = function(y) {
                 # use `<<-` to assign a value to an object in an environment 
                 # different from the current environment. 
                 x <<- y
                 inv <<- NULL
         }
         get = function() x
         setinv = function(inverse) inv <<- inverse 
         getinv = function() inv
         list(set=set, get=get, setinv=setinv, getinv=getinv)
 }


## calculate the inverse of matrix created in above function

cacheSolve <- function(x, ...) {
         ## x is the parameter which is the matrix created from makeCacheMatrix() set() function 

         inv = x$getinv()
         
         # if the inverse has already been calculated
         if (!is.null(inv)){
                 # get it from the cache and skips the computation. 
                 message("getting cached data")
                 return(inv)
         }
         
         # otherwise, calculates the inverse 
         mat.data = x$get()
         inv = solve(mat.data, ...)
         
         # sets the value of the inverse in the cache via the setinv function.
         x$setinv(inv)
         
         return(inv)
 }

## steps to execute and validate using a square matrix
temp = makeCacheMatrix(mat)
temp$set(matrix(1:4,2,2))
temp$get()
temp$getinv()
cacheSolve(temp)
temp$getinv()
