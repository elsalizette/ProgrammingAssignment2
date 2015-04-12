# x <- matrix(c(3,1,0,2,1,1,1,-1,2), 3, 3)
# y <- makeCacheMatrix(x)
# cacheSolve(y)



## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
   m <- NULL

   get <- function() x
   set <- function(x1)
   {
      x <<- x1
      m <<- NULL
   }

   set_cache_matrix <- function(x) m <<- x
   get_cache_matrix <- function() m

   list( set = set,
         get = get,
         set_cache_matrix = set_cache_matrix,
         get_cache_matrix = get_cache_matrix)

}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated
## (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)
{
   ## Return a matrix that is the inverse of 'x'

   m <- x$get_cache_matrix()
   if(!is.null(m))
   {
      message("getting cached matrix")
      return(m)
   }
   data <- x$get()
   m <- solve(data)
   x$set_cache_matrix(m)
   m
}

