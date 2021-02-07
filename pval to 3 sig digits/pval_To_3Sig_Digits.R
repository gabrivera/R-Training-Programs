## This function takes in a p-value as a numeric vector and returns the same
## rounded value up to three significant figures in as a character vector.

pval_To_3Sig_Digits <- function(pval){
    
    returnNumber = numeric()
    
    if (pval < 0.00001) {
        return("p < 0.00001") 
    } else if (pval < 0.0001) {
        returnNumber = round(pval, digits = 7)
    } else if (pval < 0.001) {
        returnNumber = round(pval, digits = 6)
    } else if (pval < 0.01) {
        returnNumber = round(pval, digits = 5)
    } else if (pval < 0.1) {
        returnNumber = round(pval, digits = 4)
    } else if (pval < 1) {
        returnNumber = round(pval, digits = 3)
    } else if (pval == 1) {
        return("1")
    }
        
    return( as.character(returnNumber) )
}