rm(list=ls())
entropy <- function(S){
    #
    # This function calculates entropy
    #

    sumOfS <- sum(S)
    H <- ifelse(S == 0, 0, (S/sumOfS)*log2(S/sumOfS))  
    H_S <- -sum(H)

    
    
    return(H_S)
}

# TEST
entropy(c(100,100,100,100,100,100,100))
entropy(c(10,10000,10,1000,100,10,1))
entropy(c(1,10,3400,5))


# average entropy (maybe for CERES project)
(entropy(c(4,0)) + entropy(c(0,4)) + entropy(c(4,0)) + entropy(c(0,4)) + entropy(c(0,4)) + entropy(c(4,0)))/6
(entropy(c(3,1,3,4)) + entropy(c(2,2,1,1)) + entropy(c(4,0,6,5)) + entropy(c(2,2,5,4)) + entropy(c(1,3,2,76)) + entropy(c(2,2,2,3)))/(6*log2(4)) # normalized entropy after average ( divided by log2(length(S)) )
(entropy(c(1,1,1,1)) + entropy(c(1,1,1,1)))/(2*log2(4))


