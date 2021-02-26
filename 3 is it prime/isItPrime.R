isItPrime <- function(n) {
    # determine whether the positive integer n is prime
    # using the mod function, return TRUE or FALSE accordingly
    
    # check that the function argument is "admissible"
    # test that n is a positive integer (or a real number that equals a positive integer)
    n.int <- as.integer(n) 
    # if n was a real number such as 3.2 then n.int will be n truncated 
    # to an integer (for this example, 3)
    
    if(!(n.int == n)) stop("n is not an integer")
    if(n < 1) stop("n is not positive")
    
    # stop if n is "too large" to avoid a very long calculation
    if(n > 1000000) stop("n is > a million") 
    
    # code to test if n is prime using R's mod function %%
    
    # special cases
    if(n == 1) return(FALSE)
    if(n == 2) return(TRUE)
    
    ##### rest of code to test if n is prime when n is at least 3
    
    m = as.integer(sqrt(n)) + 1L
    
    for(i in 2:m){
        if(n %% i == 0) return(FALSE)
    }
    
    return(TRUE)
    
    
}