.First <- function()
    if(interactive()) try(utils::loadhistory("~/.Rhistory"))

.Last <- function()
    if(interactive()) try(savehistory("~/.Rhistory"))

sourceDir <- function(path, trace = FALSE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")          
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
