#' Use gsub() to remove strings of numbers that may be MRNs. 8 or 9 digit MRN can be specified.
#'
#' @param dirwithmrn location/folder/directory of files with MRNs.
#' @param dirwithoutmrn location/folder/directory to write files without MRNs.
#' @param verbose Amount of output provided to console. 0 (default) for none.  Higher values may provide more.
#' @param digits_in_mrn Number of digits in MRN. Default is 9.
#' @param flag Character string to replace found MRNs. Default is "XXX-MRNMRN".
#' @param perllike Should perl regex engine (PCRE) be used (TRUE, default) or TRE (FALSE).
#' @param listofcolumnnamestoscrub which columns in which files to scrub? This argument is a list. Each component is a vector of column names.  The component names are the unique parts of the file names of the files to scrub.  Default value is to include 22 variables from 14 files.
#'
#' @return invisible NULL. But several files are created as a side effect.
#' @export
removemrn <- function(
    dirwithmrn = ".",
    dirwithoutmrn = ".",
    verbose = 0,
    digits_in_mrn = 9,
    flag = "XXX-MRNMRN",
    perllike = TRUE,
    listofcolumnnamestoscrub =
      list(
        OPH_ORDER_text = c("ORDER_COMMENT", "NARRATIVE"),
        OPH_SURGERY_text = c("OP_NOTE", "BRIEF_OP_NOTE"),
        OPH_ENC_PROBLEM_LIST_text = c("PROBLEM_CMT"),
        OPH_ENC_HPI_text = c("HPI_COMMENT"),
        OPH_ENC_VISIT_SUMMARY_text = c("HPI", "TECH_COMMENT", "PAT_OCULAR_HISTORY"),
        OPH_LAB_text = c("LINE_COMMENT", "RESULTS_COMP_CMT", "RESULTS_CMT"),
        OPH_ENC_VISIT_text = c("PROGRESS_NOTE"),
        OPH_RAD_text = c("APPT_NOTE", "TEXT"),
        OPH_FAMILY_HX_text = c("COMMENTS", "FAM_STAT_NAME"),
        OPH_ENC_EXAM_text = c("SMRTDTA_ELEM_VALUE"),
        OPH_LAB_ORDER_text = c("SPECIMEN_COMMENTS"),
        OPH_MED_text = c("INSTRUCTION"),
        OPH_SURGERY_all_text = c("PRE_OP_DIAG"),
        OPH_SURGERY_PROCEDURE_text = c("COMMENTS")
      )
    
    ) {

  # 8/9 digits not surrounded by more digits
  regex <- if (isTRUE(perllike)) {
    # (using Perl-like Regular Expressions)
    sprintf("(?<!\\d)\\d{%s}(?!\\d)", as.character(digits_in_mrn)) # PCRE version
  } else if (isFALSE(perllike)) {
    # (using Extended Regular Expressions)
    sprintf("(^|[^[:digit:]])[[:digit:]]{%s}([^[:digit:]]|$)", as.character(digits_in_mrn))
  } else {
    stop("perllike must be TRUE or FALSE")
  }
  
  if (verbose > 0) {
    cat(
      "Replacing %d-digit MRNs with %s in %d variables in %d files\n",
      digits_in_mrn, flag,
      length(unlist(listofcolumnnamestoscrub)), length(listofcolumnnamestoscrub))
  }
  
  ###################
  # OUTPUT LOCATION #
  ###################
  
  # Scrubbed files directory
  # this is where the scrubbed output files should be deposited
  if (verbose > 0) {
    cat(sprintf("Scrubbed data will be written to\n%s\n", dirwithoutmrn))
  }
  if (dir.exists(dirwithoutmrn)) {
    if (verbose > 0) {
      cat("Directory existed. Contents are\n")
      print(dir(dirwithoutmrn))
    }
  } else {
    if (dir.create(dirwithoutmrn, recursive = TRUE)) {
      if (verbose > 0) {
        cat("Directory newly created for scrubbed data.\n")
      }
    } else {
      stop("Unable to create output directory for scrubbed files.")
    }
  }
  
  #######################
  # PRIMARY INPUT FILES #
  #######################

  # This directory contains the datafiles that need to be scrubbed.
  if (verbose > 0) {
    cat(sprintf("Data to be scrubbed will be read from\n%s\n", dirwithmrn))
  }
  if (dir.exists(dirwithmrn)) {
    if (verbose > 0) {
      cat("Existing files:\n")
      print(dir(dirwithmrn))
    }
  } else {
    stop(sprintf("%s does not exist.", dirwithmrn))
  }
  
  # link scrubbing request to files in directory
  directory <- dir(dirwithmrn)
  fileroots <- names(listofcolumnnamestoscrub)
  nfiles <- length(fileroots)
  filenames <- rep(NA_character_, nfiles)
  
  for (f in seq.int(nfiles)) {
    cat(sprintf("%0.2d: %s\n", f, fileroots[f]))
    filename <- grep(fileroots[f], directory, value = TRUE, ignore.case = TRUE)
    
    if (length(filename) == 0) { # No match
      if (verbose > 0) cat(sprintf("%s not matched in %s.\n Skipping.\n", fileroots[f], dirwithmrn))
    } else if (length(filename) > 1) { # several approximate matches
      if (verbose > 0) cat(sprintf("%s matched %d files in %s.\nChecking for exact match.\n", fileroots[f], length(filename), dirwithmrn))
      filename <- grep(sprintf("%s.csv", fileroots[f]), directory, value = TRUE, ignore.case = TRUE)
      if (length(filename) != 1) { # no exact match
        if (verbose > 0) cat(sprintf("%s.csv not matched in %s.\n Skipping.\n", fileroots[f], dirwithmrn))
      } else { # exact match
        if (verbose > 0) cat("Exact match found.\n")
        # remove ".csv" extension
        filenames[f] <- substr(filename, 1, stop = nchar(filename) - 4)
      }
    } else { # 1 approximate match
      # remove ".csv" extension
      filenames[f] <- substr(filename, 1, stop = nchar(filename) - 4)
    }
  }
  
  matched <- !is.na(filenames)
  
  if (verbose > 0) {
    if (isTRUE(all(matched))) {
      cat(sprintf("All %d listed file(s) located in %s.\n", sum(matched), dirwithmrn))
      print(filenames)
    } else {
      cat(sprintf("Only %d listed file(s) located in %s.\n", sum(matched), dirwithmrn))
      print(filenames[matched])
      cat(sprintf("But  %d listed file(s) not located in %s.\n", sum(!matched), dirwithmrn))
      print(fileroots[!matched])
    }
  }
  
  if (sum(matched) == 0) {
    stop("No files to scrub.")
  }
  
  # Check variable names
  for (f in seq.int(nfiles)) {
    if (is.na(filenames[f])) {
      if (verbose > 0) {
        cat(sprintf("Skipping %s\n", fileroots[f]))
      }
      next
    }
    if (verbose > 0) {
      cat(sprintf("Searching variable names of %s (%s)\n", filenames[f], fileroots[f]))
    }
    datvarnames <- names(data.table::fread(
      sprintf("%s/%s.csv", dirwithmrn, filenames[f]),
      nrows = 1, header = TRUE
    ))
    if (verbose > 0) {
      cat(sprintf("Variable names in file:\n"))
      print(datvarnames)
      cat(sprintf("Searching for\n"))
      print(listofcolumnnamestoscrub[[f]])
    }

    matched <- match(toupper(listofcolumnnamestoscrub[[f]]), toupper(datvarnames))

    if (verbose > 0) {
      if (all(!is.na(matched))) {
        cat(sprintf("All %d variables to scrub found in %s.\n", length(matched), filenames[[f]]))
      } else {
        cat(sprintf("%d of %d variables to scrub found in %s.\n", sum(!is.na(matched)), length(matched), filenames[[f]]))
        print(datvarnames[matched[!is.na(matched)]])
        cat(sprintf("%d of %d variables to scrub not found in %s.\n", sum(!matched), length(matched), filenames[[f]]))
        print(listofcolumnnamestoscrub[[f]][is.na(matched)])
      }
    }
    
    # removes NAs and matches capitalization to data file
    listofcolumnnamestoscrub[[f]] <- datvarnames[matched[!is.na(matched)]]
  }


  # do the replacements  
  for (i in seq.int(nfiles)) {
    if (is.na(filenames[i])) next
    
    infile <- sprintf("%s/%s.csv", dirwithmrn, filenames[i])
    outfile <- sprintf("%s/%s_nomrn.csv", dirwithoutmrn, filenames[i])
    
    # READ FILE
    dat <- data.table::fread(file = infile)
    # dat <- read.csv(file = infile) # alternative if data.table not available
    
    if (verbose > 0) {
      cat("%9d rows read from %s.\n", nrow(dat), infile)
      if (verbose > 1) {
        cat("Before\n")
        print(utils::head(dat))
      }
    }

    for (varname in listofcolumnnamestoscrub[[i]]) {
      if (varname %in% names(dat)) {
        dat[[varname]] <- if (isTRUE(perllike)) {
          # (using Perl-like Regular Expressions)
          gsub(regex, flag, dat[[varname]], perl = TRUE)
        } else {
          # (using Extended Regular Expressions)
          gsub(regex, sprintf("\\1%s\\2", flag), dat[[varname]])
        }
      } else {
        warning(sprintf("%s not a variable name in %s. SKIPPING.", varname, infile))
      }
    }
    
    if (verbose > 1) {
      cat("After\n")
      print(utils::head(dat))
    }
    
    # WRITE FILE
    data.table::fwrite(dat, file = outfile)
    # write.csv(dat, row.names = FALSE, na = "") # alternative if data.table not available
  }
  
  return(invisible(NULL))
}
