try001 <- function(n){
    if(!require(rPython)){
        install.packages("rPython", dep = TRUE, repos = "http://star-www.st-andrews.ac.uk/cran/")
        if(!require(rPython)){
            stop("Package not found")
        }
    }
    if(!require(RJSONIO)){
        install.packages("RJSONIO", dep = TRUE, repos = "http://star-www.st-andrews.ac.uk/cran/")
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
