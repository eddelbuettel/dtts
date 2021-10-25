pkg <- "dtts"

doTest <- function(results, path, file) {
    results[[file]] <- runTestFile(paste0(path, file))
    results
}

checkForErrors <- function(l) {
    for (nm in names(l)) {
        if (getErrors(l[[nm]])$nErr || getErrors(l[[nm]])$nFail) {
            stop(paste("error or failure in", nm, ": ", l[[nm]]))
        }
    }
}

if (!require("RUnit", quietly = TRUE)) {
    cat("R package 'RUnit' cannot be loaded -- no unit tests run for package", pkg, "\n")
} else if (!require("nanotime", quietly = TRUE)) {
    cat("R package 'nanotime' cannot be loaded -- no unit tests run for package", pkg, "\n")
} else if (!require("bit64", quietly = TRUE)) {
    cat("R package 'bit64' cannot be loaded -- no unit tests run for package", pkg, "\n")
} else if (!require("data.table", quietly = TRUE)) {
    cat("R package 'data.table' cannot be loaded -- no unit tests run for package", pkg, "\n")
} else {
    library(dtts)

    path <- paste0("../", pkg, "/unitTests/")

    results <- list()
    results <- doTest(results, path, "test_dtts.R")
    
    checkForErrors(results)
}
