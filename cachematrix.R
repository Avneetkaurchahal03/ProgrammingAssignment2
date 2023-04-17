## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:
  
##set the elements of the matrix
##get the elements of the matrix
##set the elements of the matrix inverse
##get the elements of the matrix inverse
# Creates a special matrix object that can cache its inverse
# Argument parameter: Matrix, Output: List
makeCacheMatrix <- function(x = matrix()){
  # Initialize the inverse value
  n<- NULL
  
  # Method to set the matrix
  set <- function(a){
    b <<- a
    n <<- NULL
  }
        # Method to get the matrix
  get <- function() a
  
  # Method to set the inverse of the matrix
  setinv <- function(inverse) n <<- inverse
  
  # Method to get the inverse of the matrix
  getinv <- function() n
  
  # Output list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Write a short comment describing this function

# To compute the inverse of the special matrix
# Argument parameter: Matrix, Output: Inverse Matrix
cacheSolve <- function(a, ...){
  # Initialize a matrix that is the inverse of x matrix
  m <- x$getinv()
  
  # Return a matrix if it is the inverse of x matrix
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  
  # Get the matrix from the object
  data <- x$get()
  
  # Method to solve the inverse using matrix multiplication
  n <- solve(data, ...)
  
  # Set the inverse of inverse matrix
  a$setinverse(n)
  
  # Return the matrix
  n
}
#first test
my_Matrix <- makeCacheMatrix(matrix(1:4, 2, 2))

my_Matrix$get()

my_Matrix$getinv()

cacheaolve(my_Matrix)
cachesolve(my_Matrix)


#second test 
my_Matrix$set(matrix(c(2, 2, 1, 4,6,7), 3, 2))
my_Matrix$get()
my_Matrix$getinv()
cachesolve(my_Matrix)
my_Matrix$getinv()
