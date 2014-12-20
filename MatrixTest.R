makeCacheMatrix <- function (x = numeric()) {
        
        mInverse <- NULL	#Set the value of the Matrix Inverse to NULL, 
                                #this resets the Inverse each time this function
                                #is called.  Using the "<-" assignment operator
				#limits the scope of this assignment to within
                                #the makeCacheMatrix function

        setM <- function(y) {
        
                x <<- y			#Assigns the input Matrix to the 
                                        #variable x by the "<<-" assignment 
                                        #operator.  This establishes search 
                                        #parameters for the variable
					#to be made through parent environments.
				
                mInverse <<- NULL	#Assigns the MInverse value to NULL also
                                        #using the "<<-" assignment operator
        }

        getM <- function() {x}		#Creates the getM function.  This 
                                        #function recalls the matrix passed when
                                        #the makeCacheMatix function was first
                                        #called

        setInverse <- function(mI) {mInverse <<- mI}	#Creates the setInverse 
                                                        #function to set the 
                                                        #value of the Matrix 
                                                        #Inverse.  The value of
                                                        #this funciton is 
                                                        #undefined until the 
                                                        #cacheSolve function is 
                                                        #first called and 
                                                        #returns an error unless
                                                        #a matrix (mI) is passed
                                                        #to the function using 
                                                        #the command 
                                                        #(y$setInverse(mI)) 
                                                        #where y is the original
                                                        #matrix passed to 
                                                        #makeCacheMatrix and mI 
                                                        #is the inverse of y.  
                                                        #After cacheSolve has 
                                                        #been called the first 
                                                        #time, this function 
                                                        #saves the inverse 
                                                        #matrix. 

        getInverse <- function() {mInverse}	#Creates the getInverse function
                                                #This function recalls the 
                                                #Inverse Matrix after it has 
                                                #been calculated and saved.  
                                                #Until the inverse Matrix has 
                                                #been set, calling the function 
                                                #returns NULL

        list(setM=setM, getM=getM, setInverse=setInverse, getInverse=getInverse)
                                                #This sets a list with 4 values 
                                                #set to the functions that (1) 
                                                #saves the matrix, (2) recalls 
                                                #the matrix, (3) saves the 
                                                #Inverse Matrix, and (4) recalls
                                                #the Inverse Matrix. Each value 
                                                #can be recalled by referring to
                                                #the list elements.

}

cacheSolve <- function(x, ...){
        
        mInverse <- x$getInverse()      #This recalls the Inverse Matrix and 
                                        #assigns it to the variable mInverse. If
                                        #the inverse has not yet been calculated
                                        #the value of mInverse is NULL

	
        if(!is.null(mInverse)){         #Evaluates the conditional statement, if
                                        #the mInverse is not NULL (e.g. the 
                                        #Matrix inverse has been calculated and 
                                        #set), then the function returns the 
                                        #cached data.  Otherwise, the function 
                                        #skips these steps and begins to 
                                        #calculate the inverse matrix
                
                message("getting cached data")  #if the conditional statment is 
                                                #true, then the function returns
                                                #the statement that it is 
                                                #returning the cached data (e.g.
                                                #data saved in memory)
                
                return(mInverse)        #if the function returns cached data, 
                                        #this statement is executed and the 
                                        #function terminates and returns the 
                                        #inverse matrix to the calling 
                                        #environment

        }

        data <- x$getM()        #This statement is executed if the inverse 
                                #matrix has not yet been calculated (and is not 
                                #saved in memory as cached data) the variable 
                                #data is assigned to the original matrix that is
                                #recalled by referring to the list variable getM

	if(nrow(data) != ncol(data)){
                                #Although the instructions said to assume that
                                #the matrix had an inverse, this 
                                #conditional insures that only square matrices 
                                #are entered and if the input matrix is not, 
                                #the function returnssends a message and exits 
                
		message("The input matrix is not square; consequently an
                        inverse cannot be calculated")
		return()
	}

        
        detM <- det(data, ...)  ## Determine if Matrix is invertible

	if (abs(detM) < 1e-10) {        #Although the instructions said 
                                        #to assume that the matrix had an 
                                        #inverse this conditional compares the 
                                        #determinant to a very small number
                                        # to see if the determinant is zero, if
                                        #the determinant is zero, the inverse
                                        #is undefined and the cacheSove function
                                        #would result in an error.  This        
                                        #conditional statement checks to be sure
                                        #that an inverse matrix is calculable

		message("The Determinant of the matrix is zero.  Under this 
                        condition, the matrix inverse is undefined")
		return()
	
	}
    
        mInverse <- solve(data, ...)    #this calculates the Inverse matrix of 
                                        #the original matrix that was set in the
                                        #makeCacheMatrix function.
        
        x$setInverse(mInverse)  #This sets the value of the setInverse function 
                                #to the value of the inverse matrix by recalling
                                #the setInverse function and passing this 
                                #function the just calculated inverse matrix.  
                                #Once this function has run, subsequent calls to
                                #the cacheSolve() function will recall the 
                                #cached data rather than calculating the inverse
                                #matrix again.
        
        mInverse        #This simply returns the inverse matrix to the calling 
                        #environment.
                    
}