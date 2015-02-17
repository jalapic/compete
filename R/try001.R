try001 <- function(n){
    if(!require(rPython)){
        install.packages("rPython")
        if(!require(rPython)){
            stop("Package not found")
        }
    }
    if(!require(RJSONIO)){
        install.packages("RJSONIO")
        if(!require(RJSONIO,character.only = TRUE)){
            stop("Package not found")
        }
    }
    library(RJSONIO)
    library(rPython)
    path <- paste(system.file(package="compete"), "test.py", sep="/")
    python.load(path)
    k = python.call("test",n)
    return(k)
}
