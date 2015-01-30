try001 <- function(n){
    require(rPython)
    require(RJSONIO)
    path <- paste(system.file(package="compete"), "test.py", sep="/")
    python.load(path)
    k = python.call("test",n)
    return(k)
}
