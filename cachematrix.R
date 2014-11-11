                                     #OHM#

#
# Coursera - R Programming
# Programming Assignment 2
#
# Caching the Inverse of a Matrix.
#




#
# Creates a special "matrix" object that can cache its inverse.
#
makeCacheMatrix <- function(x = matrix())
{
    matrixData <- NULL


    setMatrix <- function(matrix)
    {
        x <<- matrix
        matrixData <<- NULL
    }


    getMatrix <- function() x
    setInverse <- function(inversedMatrix) matrixData <<- inversedMatrix
    getInverse <- function() matrixData


    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse, getInverse = getInverse)
}


#
# Computes the inverse of the special matrix. It uses a cache for computational
# efficiency.
#
cacheSolve <- function(x, ...)
{
    inversedMatrix <- x$getInverse()
    if(!is.null(inversedMatrix))
    {
        message("Retrieving data from cache")
        return(inversedMatrix)
    }

    matrix <- x$getMatrix()
    inversedMatrix <- solve(matrix, ...)
    x$setInverse(inversedMatrix)
    inversedMatrix
}
