#
#      Get all functions and install required packages
#
#  (c) Copyright 2013 Jean-Olivier Irisson
#      GNU General Public License v3
#
#--------------------------------------------------------------------------


# list all source files
rFiles <- list.files(".", pattern="\\.(R|r)$", full=TRUE)

# Detect which packages are used in those source files
requiredPackages <- c()
for (file in rFiles) {
    # read content
    file <- scan(file, what="character", quiet=T, sep="\n")
    # extract require calls
    m <- regexpr("require\\(.*?\\)", file)
    matched <- regmatches(file, m)
    # extract the name of the package inside the require call
    m <- regexpr("\\\".*?\\\"", matched)
    matched <- regmatches(matched, m)
    # remove quotes
    matched <- gsub("\\\"", "", matched)

    # store it as required
    requiredPackages <- c(requiredPackages, matched)
}
requiredPackages <- unique(requiredPackages)

# Install missing packages
library("utils")
# NB: utils has the function installed.packages()
#     utils is usually loaded automatically with R but apparently this happens *after* .Rprofile is read, so we need to lad it explicitely here
installedPackages <- row.names(installed.packages())
missingPackages <- setdiff(requiredPackages, installedPackages)
if (length(missingPackages) > 0) {
  message("DISCUSS requires a few R packages, installing them now")
  install.packages(missingPackages, repos="http://cran.at.r-project.org")
}

# load all source files
# message("Load all functions")
for (file in rFiles) {
  source(file)
}

# read options
# message("Read options")
disc.opts <- disc_read_options(quiet=TRUE)

# cleanup
rm(
  rFiles,
  file, matched, m,
  requiredPackages, installedPackages, missingPackages
)

message("\nDISCUSS ready\n")
