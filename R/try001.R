try001 <- function(n){
    if(!require(rPython, character.only = TRUE){
        install.packages(rPython, dep = TRUE, repos = "http://star-www.st-andrews.ac.uk/cran/")
        if(!require(rPython,character.only = TRUE)){
            stop("Package not found")
        }
    }
    if(!require(RJSONIO, character.only = TRUE){
        install.packages(RJSONIO, dep = TRUE, repos = "http://star-www.st-andrews.ac.uk/cran/")
        if(!require(RJSONIO,character.only = TRUE)){
            stop("Package not found")
        }
    }
    path <- paste(system.file(package="compete"), "test.py", sep="/")
    python.load(path)
    k = python.call("test",n)
    return(k)
}
