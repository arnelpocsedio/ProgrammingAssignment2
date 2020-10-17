## This set of functions calculate the inverse of a square matrix.
## Calculated inverse is "saved" so when the user needs to recover the inverse (of the same matrix), 
## it will use the cached data. Otherwise, the function will solve for the inverse of the given matrix (and save it).

## The first function, makeCacheMatrix, allows for caching of the data
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #initialize x and inv
        
        set <- function(y){
                x <<- y
                inv <- NULL
        }
        
        get <- function() x
        
        setinv <- function(invm) inv <<-invm
        getinv <- function() inv
        
        #set functions assign the data and the inverse in the function environment
        #get functions retrive it
        
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        #return all the objects in the function to make it availble for use of the next function
        
}


## The second function, cacheSolve, completes the first function. This function checks if the inverse 
#of the matrix is cached via the first function and returns it. If it is not available then it solves 
#for the inverse of the given matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #get inverse from cached data
        inv <- x$getinv() 
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        #if it is not available, use data to solve 
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}


##Examples

# R.version
# 
# # platform       x86_64-w64-mingw32          
# # arch           x86_64                      
# # os             mingw32                     
# # system         x86_64, mingw32             
# # status                                     
# # major          3                           
# # minor          6.2                         
# # year           2019                        
# # month          12                          
# # day            12                          
# # svn rev        77560                       
# # language       R                           
# # version.string R version 3.6.2 (2019-12-12)
# # nickname       Dark and Stormy Night
# 
# set.seed(123456789)
# mat1 = makeCacheMatrix(matrix(sample(1:16, 4000000, replace = T), 2000, 2000))
# 
# #First run
# system.time(cacheSolve(mat1))
# # user  system elapsed 
# # 8.17    0.00    8.22 
# 
# #Second run is faster because the function does not need to solve again
# system.time(cacheSolve(mat1))
# # getting cached data
# # user  system elapsed 
# # 0       0       0 
# 
# set.seed(123456789)
# mat2 = makeCacheMatrix(matrix(sample(1:16, 4000000, replace = T), 2000, 2000))
# system.time(cacheSolve(mat2)) #needs to compute again for the new matrix
# #user  system elapsed 
# #8.85    0.04    9.04 
# 
# cacheSolve(mat1)[1:10]
# cacheSolve(mat2)[1:10]