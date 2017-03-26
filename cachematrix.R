## Put comments here that give an overall description of what your
## functions do
##makeCacheMatrix: This function creates a list of functions.
##Remember that functions are also treated as objects in environment (and can be passed as arguments to other functions!!)
##cacheSolve function returns the inverse of a function from Cache (if available) else calculates invesrse, stores inverse in cache and returns the same.
##Usage instructuions:
##1 First create a matrix and associate the four functions with the matrix using makeCacheMatrix function
##2 Next calculate the inverse of the vector using cacheSolve function
##3 For the first run it will calculate and return the inverse along with storing the inverse in cache
##4 From next run cacheSolve will fecth the inverse from the previously calculated data
##------------------------------------##

## Write a short comment describing this function
##This function takes matrix as a input and creates a list of functions to:
##set the value of the vector (note the use of <<- instead of <-)
##get the value of vector
##set inverse of the vector in variable m (Note the use of <<- to commit the value instead of <-)
##get the value of inverse (return m)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinverse = setinv,
       getinverse = getinv)
}


## Write a short comment describing this function
##This function takes the matrix as an input and returns the inverse of the matrix
##First it tries to get the inverse of the matrix through its getnverse function
##and checks wether getinverse() returns null or not
##If the inverse is not null, it returns the data i.e. the cached inverse 
##(and specifies that through updating "getting cached data" message)
##However if it is null, the function get()s the matrix, 
##solve()s the matrix (calculate inverse),
##calls setinverse() to store the value and 
##returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
