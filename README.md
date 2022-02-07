
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SOURCEScrubber

<!-- badges: start -->

<!-- badges: end -->

The goal of SOURCEScrubber is to prepares configuration files for
processing SOURCE files with Datavant Scrubber.

Required input includes

  - original configuration directory from Datavant,
  - customized list of names for blacklist, and
  - locations of the Scrubber tool and input/output directories.

It optionally runs Scrubber (in addition to creating the command line).

## Installation

You can install the development version of SOURCEScrubber from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("chrisandrewsphd/SOURCEScrubber")
```

## Example

1.  Download Scrubber from Datavant and install
2.  Download and install R (<https://cran.r-project.org/>) and RStudio
    (<https://www.rstudio.com/>)
3.  Double click on .Rproj icon to launch RStudio (e.g.,
    Deployment\_v0.3.Rproj)

Not yet available:

4.  In RStudio, open “./RCode/test call - one file.R” or (“./RCode/test
    call - all files.R”)
5.  Edit ‘scrub’ function call to contain the correct directories (see
    below)
6.  Run the script.

The REQUIRED arguments to the function ‘scrub’ are

1.  dirtoscrub: The (input) directory containing the files to be
    scrubbed. For example, dirtoscrub =
    “M:/EPIC-Ophthalmology/Scrubber/ToScrub”

2.  scrubbeddir: The (output) directory to write the scrubbed files. For
    example, scrubbeddir = “M:/EPIC-Ophthalmology/Scrubber/Scrubbed”

3.  namesforblacklist: The file containing names, cities, and counties
    created when EHR data were extracted. For example, namesforblacklist
    = “M:/EPIC-Ophthalmology/Scrubber/PHI\_scrubber\_processing.csv”

4.  toollocation: The directory containing the scrubbing tool downloaded
    from Datavant. For example, toollocation =
    “M:/EPIC-Ophthalmology/Scrubber/Tool/UniversityOfMichigan\_PHIRemoval-1.1.3.8385bcb”

The OPTIONAL arguments to the function ‘scrub’ are

5.  originalconfigdir: The directory of configuration files that arrived
    with Scrubber. A sensible default is guessed but might not be
    correct for implementations outside UM. For example,
    originalconfigdir =
    “M:/EPIC-Ophthalmology/Scrubber/Tool/UniversityOfMichigan\_PHIRemoval-1.1.3.8385bcb/UOMPHIRemoval”

6.  workfolder: By default, temporary intermediate files will be
    created, used, and destroyed. If you would like to keep them for
    error checking or record keeping, then provide a place for them. For
    example, workfolder = “./configs\_work”

7.  verbose: Larger values of verbose result in more information printed
    in R during execution. The default is 0. Other options are 1, 2, and
    3. For example, verbose = 1

8.  runscrubber: By default, the function ‘scrub’ creates the necessary
    configuration files and command string and then executes the
    command. You can change the default value (TRUE) to FALSE if you
    just want to create the framework but not actually scrub the data.
    For example, runscrubber = FALSE

9.  sh\_or\_bat: The scrubber has two versions “phiremoval.bat” and
    “phiremoval.sh”. The first is for Windows; the second is for
    Mac/UNIX. The default is sh\_or\_bat = “bat”. If you need the other
    version add this argument to the function call: sh\_or\_bat = “sh”

10. listofcolumnnamestoscrub: By default, the function ‘scrub’ will
    scrub 21 columns in 14 files expected in the UM deployment of
    SOURCE. You can change the files and columns scrubbed. Put the base
    of the filename on the left and the variables names on the right.
    For example,

<!-- end list -->

  - listofcolumnnamestoscrub = list(oph\_enc\_exam\_text =
    c(“SMRTDTA\_ELEM\_VALUE”))

  - listofcolumnnamestoscrub = list( oph\_order\_text =
    c(“ORDER\_COMMENT”, “NARRATIVE”), oph\_enc\_problem\_list\_text =
    c(“PROBLEM\_CMT”))

  - listofcolumnnamestoscrub = list( oph\_surgery\_text = c(“OP\_NOTE”,
    “BRIEF\_OP\_NOTE”))

The default value, which you don’t need to provide if you want to scrub
everything, is

  - listofcolumnnamestoscrub = list( oph\_order\_text =
    c(“ORDER\_COMMENT”, “NARRATIVE”), oph\_surgery\_text =
    c(“OP\_NOTE”, “BRIEF\_OP\_NOTE”), oph\_enc\_problem\_list\_text
    = c(“PROBLEM\_CMT”), oph\_enc\_hpi\_text = c(“HPI\_COMMENT”),
    oph\_enc\_visit\_summary\_text = c(“HPI”, “TECH\_COMMENT”,
    “PAT\_OCULAR\_HISTORY”), oph\_lab\_text = c(“LINE\_COMMENT”,
    “RESULTS\_COMP\_CMT”, “RESULTS\_CMT”), oph\_enc\_visit\_text =
    c(“PROGRESS\_NOTE”), oph\_rad\_text = c(“APPT\_NOTE”, “TEXT”),
    oph\_family\_hx\_text = c(“COMMENTS”, “FAM\_STAT\_NAME”),
    oph\_enc\_exam\_text = c(“SMRTDTA\_ELEM\_VALUE”),
    oph\_lab\_order\_text = c(“SPECIMEN\_COMMENTS”), oph\_med\_text =
    c(“INSTRUCTION”), OPH\_SURGERY\_all\_text = c(“PRE\_OP\_DIAG”),
    oph\_surgery\_procedure\_text = c(“COMMENTS”))

<!-- end list -->

``` r
library(SOURCEScrubber)
## basic example code
## Not Run
## scrub(dirtoscrub = "./toscrub",
## scrubbeddir = "./scrubbed",
## namesforblacklist = "./black.csv",
## toollocation = "./tool",
## verbose = 3)
```
