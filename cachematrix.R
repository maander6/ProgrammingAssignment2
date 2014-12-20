## This file contains 2 functions: makeCacheMatrix() and cacheSolve()
## The makeCacheMatrix function initializes the matrix for calculating and
## saving the inverse of the input matrix.
##
## The makeCacheMatrix() function creates an object of type 
## list, and the list stores the original matrix and the inverse matrix, 
## whose value is initally set to NULL by this function.
##
## The list object has 4 members that refer to functions that are defined
## within the makeCacheMatrix() function.  These functions allow 
## the matrix to be retrieved (function getM())and set (function setM());
## and the matrix inverse to be retrieved (function getInverse()), and set
## setInverse().  
## 
## The cacheSolve() function retrieves the original matrix, checks to see if 
## the inverse has been calculated previously and saved in memory.  If it has
## the cached inverse matrix is returned. If is has not, the function determines
## if an inverse can be calculated by determining if the input matrix is square
## (e.g. number of rows equals the number of columns), and if an inverse exists
## (e.g. the determinant of the matrix is not equal to zero).  If these two 
## conditions are met, then the inverse matrix is calculated with the solve()
## function, and the value of the inverse matrix is cached using the 
## setInverse() function.
##

makeCacheMatrix <- function(x = matrix()) {

        mInverse <- NULL	## Set the value of the Matrix Inverse to NULL, 
                                ## this resets the Inverse each time this 
                                ## function is called.  Using the "<-" 
				## assignment operator limits the scope of this 
                                ## assignment to the makeCacheMatrix function

        setM <- function(y) {
        
                x <<- y			## Assigns the input Matrix to the 
                                        ## variable x by the "<<-" super  
                                        ## assignment operator.  This 
                                        ## establishes that matix x is in the
                                        ## containing environments.
				
                mInverse <<- NULL	## Assigns the MInverse value to NULL 
                                        ## using the "<<-" global assignment
                                        ## operator
        }

        getM <- function() {x}		## Creates the getM function.  This 
                                        ## function recalls the matrix passed 
                                        ## when the makeCacheMatix function was 
                                        ## first called

        setInverse <- function(mI) {mInverse <<- mI}	##Creates the setInverse 
                                                        ## function to set the 
                                                        ## value of the Matrix 
                                                        ## Inverse.  The value 
                                                        ## of this funciton is 
                                                        ## undefined until the 
                                                        ## cacheSolve function  
                                                        ## is first called and 
                                                        ## returns an error 
                                                        ## unless a matrix (mI)
                                                        ## is passed to the  
                                                        ## function using the 
                                                        ## command 
                                                        ## (y$setInverse(mI)) 
                                                        ## where y is the 
                                                        ## original matrix  
                                                        ## passed to 
                                                        ## makeCacheMatrix and  
                                                        ## mI is the inverse of 
                                                        ## y. After cacheSolve  
                                                        ## has been called the  
                                                        ## first time, this
                                                        ## function saves the 
                                                        ## inverse matrix. 

        getInverse <- function() {mInverse}	## Creates the getInverse 
                                                ## function. This function 
                                                ## recalls the Inverse Matrix
                                                ## after it has been calculated 
                                                ## and saved. Until the inverse 
                                                ## Matrix has been set, calling 
                                                ## the function returns NULL

        list(setM=setM, getM=getM, setInverse=setInverse, getInverse=getInverse)

                                                ## This is a list with 4 values 
                                                ## set to the functions that (1) 
                                                ## saves the matrix, (2) recalls 
                                                ## the matrix, (3) saves the 
                                                ## Inverse Matrix, and (4) 
                                                ## recalls the Inverse Matrix. 
                                                ## Each value can be recalled by
                                                ## referring to the list 
                                                ## elements

}


## The cacheSolve() function checks to see if the inverse of the matrix has 
## already been calculated and its value saved to cached memory.  If so, the
## cached value is returned and the function exited.
##
## If not, then the function checks to see if the inverse can be solved by 
## checking to see if it is square, and then checking if the determinant of the 
## matrix is non-zero. Because of the precision of the computation, a 
## determinant value of less that 1e-10 is assumed to be equal to zero.  These 2
## conditions are required for an inverse of a matrix to be calculated.  Note 
## the instructions said to assume that the input matrix was inversible, these 
## error checks were introduced to be complete.
##
## Once the inverse matrix has been calculated, the value of the inverse matrix
## is saved to the cache memory for rapid recall in subsequent calls to the 
## function, saving computational time.

cacheSolve <- function(x, ...) {        ## Return a matrix that is the inverse
                                        ## of 'x'

        
        mInverse <- x$getInverse()      ## This recalls the Inverse Matrix and 
                                        ## assigns it to the variable mInverse. 
                                        ## If the inverse has not yet been 
                                        ## calclated, the value of mInverse is 
                                        ## NULL
	
        if(!is.null(mInverse)){         ## Evaluates the conditional statement, 
                                        ## if the mInverse is not NULL (e.g. the 
                                        ## Matrix inverse has been calculated 
                                        ## and set), then the function returns
                                        ## the cached data.  Otherwise, the
                                        ## function skips these steps and begins 
                                        ## to calculate the inverse matrix
                
                message("getting cached data")  ## if the conditional statment 
                                                ## is true, then the function
                                                ## returns the statement that it 
                                                ## is returning the cached data 
                                                ## (e.g. data saved in memory)
                
                return(mInverse)        ## if the function returns cached data, 
                                        ## this statement is executed and the 
                                        ## function terminates and returns the 
                                        ## inverse matrix to the calling 
                                        ## environment

        }

        data <- x$getM()        ## This statement is executed if the inverse 
                                ## matrix has not yet been calculated (and is
                                ## not saved in memory as cached data) the
                                ## variable data is assigned to the original 
                                ## matrix that is recalled by referring to the
                                ## list variable getM

	if(nrow(data) != ncol(data)){

                                ## Although the instructions said to assume that
                                ## the matrix had an inverse, this 
                                ## conditional insures that only square matrices 
                                ## are entered and if the input matrix is not, 
                                ## the function returns a message and exits 
                
		message("The input matrix is not square; consequently an
                        inverse cannot be calculated")
		return()
	}

        
        detM <- det(data, ...)  ## Calculate the Determinant of the matrix.
				## If the determinant equals zero, then an
				## inverse cannot be calculated

	if (abs(detM) < 1e-10) {        ## Although the instructions said 
                                        ## to assume that the matrix had an 
                                        ## inverse this conditional compares the 
                                        ## determinant to a very small number
                                        ## to see if the determinant is zero, if
                                        ## the determinant is zero, the inverse
                                        ## is undefined and the cacheSove
                                        ## function would result in an error.  
                                        ## This conditional statement checks to 
                                        ## be sure that an inverse matrix is 
					## calculable and serves as an error 
					## trap if the inverse cannot be 
                                        ## calculated.
		message("The Determinant of the matrix is zero.  Under this 
                        condition, the matrix inverse is undefined")
		return()
	
	}
    
        mInverse <- solve(data, ...)    ## This calculates the Inverse matrix of 
                                        ## the original matrix that was set in 
                                        ## the makeCacheMatrix function.
        
        x$setInverse(mInverse)  ## This sets the value of the setInverse 
                                ## function to the value of the inverse matrix
                                ## by recalling the setInverse function and
                                ## passing this function the just calculated 
                                ## inverse matrix. Once this function has run, 
                                ## subsequent calls to the cacheSolve() function
                                ## will recall the cached data rather than 
                                ## calculating the inverse matrix again.
        
        mInverse        ## This statement simply returns the inverse matrix to 
                        ## the calling environment.
        
}

## The microbenchmark package was used to benchmark the performance using an
## arbitrary matrix that was 100 rows x 100 columns. The benchmark results were
## averaged over 100 calculations using the solve() function, and 100 
## calculations using the cacheSolve()function.  When calculating the inverse 
## matrix using solve(), the average computation time was 1.604 milliseconds.  
## calculating the inverse matrix using the cacheSolve() function, the average 
## computation time was 0.269 milliseconds.
