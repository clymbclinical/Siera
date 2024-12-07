
<!-- README.md is generated from README.Rmd. Please edit that file -->

# siera

<!-- badges: start -->
<!-- badges: end -->

With siera, users ingest Analysis Results Standard - ARS (a CDISC
Foundational standard) metadata and auto-generate R scripts that, when
run in with provided ADaM datasets, provide Analysis Results Datasets
(ARDs).

In order to use the readARS() function, users will need to provide the
following:

1.  A Functional JSON file, representing ARS Metadata for a Reporting
    Event (to get started, see TFL Designer)
2.  An output directory where the R scripts will be placed
3.  A folder containing the related ADaM datasets for the ARDs to be
    generated

## Installation

The current version (0.1.0) of siera can be installed from
[GitHub](https://github.com/clymbclinical/siera) with:

``` r
# install.packages("devtools")
Sys.unsetenv("GITHUB_PAT")
devtools::install_github("https://github.com/clymbclinical/siera")
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo clymbclinical/siera@HEAD
#> glue     (1.7.0 -> 1.8.0) [CRAN]
#> jsonlite (1.8.8 -> 1.8.9) [CRAN]
#> Installing 2 packages: glue, jsonlite
#> Installing packages into 'C:/Users/mbosm/AppData/Local/R/win-library/4.4'
#> (as 'lib' is unspecified)
#> package 'glue' successfully unpacked and MD5 sums checked
#> Warning: cannot remove prior installation of package 'glue'
#> Warning in file.copy(savedcopy, lib, recursive = TRUE): problem copying
#> C:\Users\mbosm\AppData\Local\R\win-library\4.4\00LOCK\glue\libs\x64\glue.dll to
#> C:\Users\mbosm\AppData\Local\R\win-library\4.4\glue\libs\x64\glue.dll:
#> Permission denied
#> Warning: restored 'glue'
#> package 'jsonlite' successfully unpacked and MD5 sums checked
#> Warning: cannot remove prior installation of package 'jsonlite'
#> Warning in file.copy(savedcopy, lib, recursive = TRUE): problem copying
#> C:\Users\mbosm\AppData\Local\R\win-library\4.4\00LOCK\jsonlite\libs\x64\jsonlite.dll
#> to
#> C:\Users\mbosm\AppData\Local\R\win-library\4.4\jsonlite\libs\x64\jsonlite.dll:
#> Permission denied
#> Warning: restored 'jsonlite'
#> 
#> The downloaded binary packages are in
#>  C:\Users\mbosm\AppData\Local\Temp\RtmpyWNrDR\downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\mbosm\AppData\Local\Temp\RtmpyWNrDR\remotes813c2dc66e9f\clymbclinical-siera-5751606/DESCRIPTION' ...  ✔  checking for file 'C:\Users\mbosm\AppData\Local\Temp\RtmpyWNrDR\remotes813c2dc66e9f\clymbclinical-siera-5751606/DESCRIPTION' (560ms)
#>       ─  preparing 'siera': (2.3s)
#>    checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts (905ms)
#>   ─  checking for empty or unneeded directories
#>      Omitted 'LazyData' from DESCRIPTION
#>       ─  building 'siera_0.1.0.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/mbosm/AppData/Local/R/win-library/4.4'
#> (as 'lib' is unspecified)
```

## Example

Using an example JSON ARS file, run readARS() function to produce the
ARD programs.

In this example, the following is used: 1. JSON ARS file: CDISC Pilot
Study Common Safety Displays JSON ARS. 2. ADaM datasets: CDISC Pilot
Study Common Safety Displays - ADSL, ADAE, ADVS

``` r
library(siera)

# the ARS JSON File:
json_path <- system.file("extdata", "ARS_V1_Common_Safety_Displays.json", package = "siera") 

# store ARD scripts in this folder:
output_folder <- file.path(paste0(getwd(),"/inst")) 

# this folder contains ADaM datasets to produce ARD:
ADaM_folder <- file.path(paste0(getwd(),"/inst/extdata")) 

# run the readARS function with these 3 parameters.  This creates R scripts (1 for each output in output_folder)
readARS(json_path, output_folder, adam_path = ADaM_folder)
#> Warning in stri_trim_both(string): argument is not an atomic vector; coercing
#> Warning in stri_trim_both(string): argument is not an atomic vector; coercing
#> Warning in stri_trim_both(string): argument is not an atomic vector; coercing
#> Warning in stri_trim_both(string): argument is not an atomic vector; coercing
#> Warning in stri_trim_both(string): argument is not an atomic vector; coercing
#> Warning in stri_trim_both(string): argument is not an atomic vector; coercing
#> Warning in stri_trim_both(string): argument is not an atomic vector; coercing
#> Warning in stri_trim_both(string): argument is not an atomic vector; coercing
#> Warning in stri_trim_both(string): argument is not an atomic vector; coercing
```

Once the R programs are created, they can be individually run, provided
that the ADaM datasets are in the location as provided to the readARS
function. In this example, one of the 5 outputs are Out14-1-1. Its
corresponding R script is ARD_Out14-1-1.R.

We also assume that there are CSV ADaMs in a folder called “adam_csv” in
the current working directory.

In this example, an ARD programs can be called as follows:

``` r
# location to one of the created R scripts:
# ARD_program <- file.path(paste0(output_folder,"/ARD_Out14-1-1.R"))

# run the program as-is.  This ingests the ADaM dataset(s) in the ADAM_folder location listed earlier.
# source(ARD_program)
```

Once the ARD program(s) is run, the ARD is created. Among all the helper
objects created, the ARD can be identified as the “ARD_outputname”
object. Let’s view this object created by the previous command:

``` r
# print(ARD_Out14_1_1)
```
