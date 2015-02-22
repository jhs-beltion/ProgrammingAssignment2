## Two functions make up this application:
## makeCacheMatrix - creates the matrix setting/getting functions  
## cacheSolve - 


## Creates and returns the setting/getting functions that are used by cacheSolve to 
## retrieve an existing or generate a new matrix inversion
makeCacheMatrix <- function(x, ...) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<-solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse
    )
    
}

## This function takes the matrix(anInveribleSquareMatrix) and creates a list of functions
## that permit operating on that matrix
## 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        print("getting cached inversion")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}

 testProg <- {
#These test (hopefully) show that the code operates correctly
#It is best to execute them one at a time to view the results as output    
    exampleMatrix1 <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), 3, 3)    
    print("exampleMatrix1 <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), 3, 3)")    

    print(matrixVector <- makeCacheMatrix(exampleMatrix))
    print ("(matrixVector <- makeCacheMatrix(exampleMatrix)")
    
    print("(matrixVector$get())")
    print(matrixVector$get())    
    
    print("(cacheSolve(matrixVector))")
    print(cacheSolve(matrixVector))

    print("(matrixVector$getinverse()")
    print(matrixVector$getinverse())

    print("(matrixVector$setinverse(exampleMatrix))")
    print(matrixVector$setinverse(exampleMatrix))

    print("---------- ")
    print("---------- ")
    print("exampleMatrix2 <- matrix(c(2, 1, 5, 3, 2, 7, 4, 5, 1), 3, 3)")    
    exampleMatrix2 <- matrix(c(2, 1, 5, 3, 2, 7, 4, 5, 1), 3, 3)    

    print("matrixVector2 <- makeCacheMatrix(exampleMatrix2)")
    print(matrixVector2 <- makeCacheMatrix(exampleMatrix2))

    print("matrixVector2$get()")
    print(matrixVector2$get())
    
    print("cacheSolve(matrixVector2)")
    print(cacheSolve(matrixVector2))


    print("matrixVector2$getinverse()")
    print(matrixVector2$getinverse())

    print("matrixVector2$setinverse(exampleMatrix2)")
    print(matrixVector2$setinverse(exampleMatrix2))
    
    print("matrixVector2$get()")
    print(matrixVector2$get())
    
    print("matrixVector2$setinverse(exampleMatrix2)")
    print(matrixVector2$setinverse(exampleMatrix2))
    
    print("matrixVector2$get()")
    print(matrixVector2$get())
    
    
 }