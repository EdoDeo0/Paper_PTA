library(fwildclusterboot)
cat("Version:", as.character(packageVersion("fwildclusterboot")), "\n")
cat("Args of boottest.fixest:\n")
print(names(formals(fwildclusterboot:::boottest.fixest)))
