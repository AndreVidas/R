#' @author André Vidas Olsen
#' @description This function calculates Shannon's entropy
#' @param S a vector containing observations for each attribute
#' @param normalize boolean option for normalizing entropy
entropy <- function(S, normalize = FALSE){

    # calculate entropy
    sumOfS <- sum(as.numeric(S))
    H <- ifelse(S == 0, 0, (S/sumOfS)*log2(S/sumOfS))  
    H_S <- -sum(H)
    
    # normalize entropy option
    if(normalize){
        H_S <- H_S/log2(length(S))
    }
    
    
    return(H_S)
}

