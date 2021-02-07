# This function converts column names from Excel's alphabet-based index to
# numbers. However, this implementation works only with 'two-digit' indexes.
# This particular version is a single function.

colnameToNumber <- function(colname) {
    
    # Make sure colname is a lower-case!
    colname = tolower(colname)
    
    # Create two digits of the letters
    allTheLetters = letters
    for (j in letters){
        for (i in letters) {
            allTheLetters = c( allTheLetters, paste(j, i, sep = "") )
        }
    }
    
    # Create the vector that will be populated with the results
    colnumber = numeric()
    
    # Iterate over the colnames and see whether there are matches
    for ( i in colname ){
        tempColNumber = numeric()
        tempColNumber = which(allTheLetters == i)
        if( length(tempColNumber) == 0 ) { stop("no match for colname") }
        else colnumber = c(colnumber, tempColNumber)
    }
    
    return(colnumber)
    
    # convert an Excel column name
    # to the corresponding column number;
    # colname should be a lower case letter in the
    # form of a character string, for example "a" or "r" 
    # "between" a and z (including a and z)
    #   coding lines
    #return(colnumber)
}