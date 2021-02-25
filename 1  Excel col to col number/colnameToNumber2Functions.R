# These functions convert column names from Excel's alphabet-based index to
# numbers. However, this implementation works only with 'two-digit' indexes.
# This particular version is two functions, taking advantage of R's l/sapply.

colnameToNumbers <- function(colnames){
    sapply(colnames, colnameToNumber)
}

colnameToNumber <- function(colname) {
    
    # Make sure colname is a lower-case!
    colname = tolower(colname)
    
    # Create a vector with the 'two digits' of the alphabet
    allTheLetters = letters
    for (j in letters){
        for (i in letters) {
            allTheLetters = c( allTheLetters, paste(j, i, sep = "") )
        }
    }
    
    # Create the vector that will be populated with the results
    colnumber = which(allTheLetters == colname)
    
    return(colnumber)
}