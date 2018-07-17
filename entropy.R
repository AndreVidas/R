entropy <- function(S){
    #
    # This function calculates entropy
    #
    
    H <- numeric(length(S))
    for(i in 1:length(S)){
        if(S[i] == 0){ # boundary case: 0*log2(0) -> 0
            H[i] <- 0
        }
        else{
            H[i] <- (S[i]/sum(S))*log2(S[i]/sum(S))
        }
    }
    H_S <- -sum(H)
    
    return(H_S)
}

# TEST
entropy(c(100,100,100,100,100,100,100))
entropy(c(10,10000,10,1000,100,10,1))
entropy(c(1,10,3400,5))

# average entropy (maybe for CERES project)
(entropy(c(1,3)) + entropy(c(3,1)) + entropy(c(4,0)) + entropy(c(2,2)) + entropy(c(2,2)) + entropy(c(2,2)))/6
