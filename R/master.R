# R code to generate files and commands needed to scrub files for SOURCE
# Chris Andrews
# 2022-01-28


#' Create configuration files for and possibly run the Datavant scrubber to scrub free text from select columns of SOURCE files
#'
#' @param dirtoscrub location/folder/directory of files to scrub.
#' @param scrubbeddir location/folder/directory to write scrubbed files.
#' @param workfolder (optional) configuration files used by scrubber will be written here. A temporary directory will be created and used if none is specified.
#' @param namesforblacklist full path and file name of custom file containing blacklist values.  This csv file should have five columns: PAT_FIRST_NAME, PAT_MIDDLE_NAME, PAT_LAST_NAME, CITY, and COUNTY.
#' @param toollocation location of scrubber tool. Use the entire path but not the program name ("phiremoval").
#' @param originalconfigdir (optional) location of original configuration directory provided by Datavant.  Not needed if it is in the subdirectory as originally provided.
#' @param termsforwhitelist Location of user-provided whitelist. text file that contains a single column of terms to be included in the whitelist. Variable name (first row) should be "WHITELIST".
#' @param deletewhitefromblack Should the terms in the whitelist be removed from the blacklists? Default is `FALSE` to match previous behavior. Might change to `TRUE` in the future. However, Scrubber may do this already.
#' @param runscrubber Should this function invoke scrubber after creating configuration files? FALSE by default, in which case the configuration files are created and the scrubber command lines are written to another file.
#' @param verbose Amount of output provided to console. 0 (default) for none.  Higher values may provide more.
#' @param sh_or_bat Choose appropriate script for operating system.  "bat" (default) for Windows. "sh" for Unix-like.
#' @param digits_in_mrn Number of digits in MRN. Default is 9.
#' @param listofcolumnnamestoscrub which columns in which files to scrub? This argument is a list. Each component is a vector of column names.  The component names are the unique parts of the file names of the files to scrub.  Default value is to include 21 variables from 14 files.
#'
#' @return invisible NULL. But several files are created as a side effect and possibly the scrubber is run.
#' @export
#'
#' @examples ## Not Run
#' ## scrub(dirtoscrub = "./toscrub",
#' ## scrubbeddir = "./scrubbed",
#' ## namesforblacklist = "./black.csv",
#' ## toollocation = "./tool",
#' ## verbose = 3,
#' ## listofcolumnsnamestoscrub = list(
#' ##   oph_lab_text = c("LINE_COMMENT", "RESULTS_COMP_CMT", "RESULTS_CMT"),
#' ##   oph_surgery_text = c("OP_NOTE", "BRIEF_OP_NOTE")))
scrub <- function(
  dirtoscrub, # path to files to scrub.
  scrubbeddir, # directory to write scrubbed files
  workfolder = tempdir(), # file specific configurations stored here
  namesforblacklist, # full path, or relative to dirtoscrub?
  toollocation, # location of scrubber tool
  originalconfigdir = sprintf("%s/UOMPHIRemoval", toollocation),
  termsforwhitelist = NULL, # Location of user-provided whitelist
  deletewhitefromblack = FALSE, # should whitelist terms be removed from blacklists?
  runscrubber = FALSE, # Execute system call? or just return scrubber call?
  verbose = 0,
  sh_or_bat = "bat",
  digits_in_mrn = 9,
  listofcolumnnamestoscrub =
    list(
      oph_order_text = c("ORDER_COMMENT", "NARRATIVE"),
      oph_surgery_text = c("OP_NOTE", "BRIEF_OP_NOTE"),
      oph_enc_problem_list_text = c("PROBLEM_CMT"),
      oph_enc_hpi_text = c("HPI_COMMENT"),
      oph_enc_visit_summary_text = c("HPI", "TECH_COMMENT", "PAT_OCULAR_HISTORY"),
      oph_lab_text = c("LINE_COMMENT", "RESULTS_COMP_CMT", "RESULTS_CMT"),
      oph_enc_visit_text = c("PROGRESS_NOTE"),
      oph_rad_text = c("APPT_NOTE", "TEXT"),
      oph_family_hx_text = c("COMMENTS", "FAM_STAT_NAME"),
      oph_enc_exam_text = c("SMRTDTA_ELEM_VALUE"),
      oph_lab_order_text = c("SPECIMEN_COMMENTS"),
      oph_med_text = c("INSTRUCTION"),
      OPH_SURGERY_all_text = c("PRE_OP_DIAG"),
      oph_surgery_procedure_text = c("COMMENTS")
    )

) {

  sh_or_bat <- match.arg(sh_or_bat, choices = c("sh", "bat"))
  
  if (is.null(workfolder)) workfolder <- tempdir()

  ##########################
  # ORIGINAL CONFIGURATION #
  ##########################
  if (verbose > 0) {
    cat(sprintf("The original configuration files supplied by Datavant should be here:\n%s\n", originalconfigdir))
  }
  if (!dir.exists(originalconfigdir)) {
    stop(sprintf("%s does not exist.", originalconfigdir))
  } else {
    if (verbose > 0) {
      cat("Directory existed. Contents are\n")
      print(dir(originalconfigdir))
      # should have these 4 items
      if (sum(c("interface.properties", "Maps", "Resources", "SanityFiles") %in% dir(originalconfigdir)) == 4) {
        cat("4 expected items are present.\n")
      }
    }
  }


  ###################
  # OUTPUT LOCATION #
  ###################

  # Scrubbed files directory
  # this is where the scrubbed output files should be deposited
  if (verbose > 0) {
    cat(sprintf("Scrubbed data will be written to\n%s\n", scrubbeddir))
  }
  if (dir.exists(scrubbeddir)) {
    if (verbose > 0) {
      cat("Directory existed. Contents are\n")
      print(dir(scrubbeddir))
    }
  } else {
    if (dir.create(scrubbeddir, recursive = TRUE)) {
      if (verbose > 0) {
        cat("Directory newly created for scrubbed data.\n")
      }
    } else {
      stop("Unable to create output directory for scrubbed files.")
    }
  }

  #################
  # WORK LOCATION #
  # UTILITY FILES #
  #################
  # Work Directory (function argument) will contain
  #   subfolder with expanded databases
  #   subfolder for each fileroot (function argument), which contains
  #     configuration directory, reconfigured
  #     log directory to store scrubber logs
  #     scrubber command text file

  # check existence of work directory.
  workfolder <- gsub("\\\\", "/", workfolder)
  if (verbose > 0) {
    cat(sprintf("Work folder is\n%s\n", workfolder))
  }
  if (dir.exists(workfolder)) {
    if (verbose > 0) {
      cat("Directory existed. Contents are\n")
      print(dir(workfolder))
    }
  } else {
    if (dir.create(workfolder, recursive = TRUE)) {
      if (verbose > 0) {
        cat("Directory newly created for work folder.\n")
      }
    } else {
      stop("Unable to create work directory.")
    }
  }

  #############
  # log files #
  #############
  logdirectory <- sprintf("%s/log", workfolder)
  if (verbose > 0) {
    cat(sprintf("log files will be saved here:\n%s\n", logdirectory))
  }
  if (dir.exists(logdirectory)) {
    if (verbose > 0) {
      cat("Directory existed.  Contents are\n")
      print(dir(logdirectory))
    }
  } else {
    if (dir.create(logdirectory, recursive = TRUE)) {
      if (verbose > 0) {
        cat("Directory newly created for log files.\n")
      }
    } else {
      stop("Unable to create log directory.\n")
    }
  }

  #################
  # Command files #
  #################
  # directory for files containing the command line to invoke scrubber
  commanddirectory <- sprintf("%s/call", workfolder)
  if (verbose > 0) {
    cat(sprintf("Scrubber command files will be saved here:\n%s\n", commanddirectory))
  }
  if (dir.exists(commanddirectory)) {
    if (verbose > 0) {
      cat("Directory existed.  Contents are\n")
      print(dir(commanddirectory))
    }
  } else {
    if (dir.create(commanddirectory, recursive = TRUE)) {
      if (verbose > 0) {
        cat("Directory newly created for command files.\n")
      }
    } else {
      stop("Unable to create command directory.\n")
    }
  }



  #########################
  #########################
  # GENERIC CONFIGURATION #
  #########################
  #########################
  # Create generic configuration files
  # configurations specific to a file to scrub will be developed below.
  # the default will be copied to here
  # then blacklists expanded via "expand databases.R"
  genericconfigdir <-  sprintf("%s/configgeneric", workfolder)

  if (verbose > 0) {
    cat(sprintf("A generic configuration directory with expanded blacklists and whitelists will be created here:\n%s", genericconfigdir))
  }
  if (dir.exists(genericconfigdir)) {
    if (verbose > 0) {
      cat("Directory existed. Contents are\n")
      print(dir(genericconfigdir))
    }
  } else {
    if (dir.create(genericconfigdir, recursive = TRUE)) {
      if (verbose > 0) {
        cat("Directory newly created for generic configuration files.\n")
      }
    } else {
      stop("Unable to create generic configuration directory.")
    }
  }

  # copy from original to generic
  if (isTRUE(all(file.copy(dir(originalconfigdir, full.names = TRUE), genericconfigdir, overwrite=TRUE, recursive=TRUE)))) {
    if (verbose > 0) {
      cat("Configuration copied from original to generic.\n")
    }
  } else {
    stop("Configuration copy unsuccessful.")
  }

  ####################
  # Expand databases #
  ####################
  expand(
    namesforblacklist = namesforblacklist,
    originalconfigdir = originalconfigdir,
    temporaryconfigdir = genericconfigdir,
    termsforwhitelist = termsforwhitelist,
    deletewhitefromblack = deletewhitefromblack,
    verbose = verbose,
    overwriteifexists = TRUE
  )

  ######################
  # Additional changes #
  ######################
  # to generic configuration files

  #### interface.propeties ####
  # Changes interface.properties
  #   to not do Sanity Check
  #   to not write map

  ip <- readLines(con = sprintf("%s/interface.properties", genericconfigdir), warn = FALSE)
  sanityline <- grep("IsSkipSanity", ip)
  ip[sanityline] <- "IsSkipSanity = TRUE"
  mapline <- grep("Print_Maps", ip)
  ip[mapline] <- "Print_Maps = FALSE"
  writeLines(ip, con = sprintf("%s/interface.properties", genericconfigdir))
  if (verbose > 0) cat("interface.properties updated.\n")

  #### errorcodes.config ####
  # hijack insuranceid line for oldage tag

  errcon <- readLines(con = sprintf("%s/Resources/errorcodes.config", genericconfigdir), warn = FALSE)
  insidline <- grep("INSURANCEID", errcon)
  errcon[insidline] <- "INSURANCEID=XXX-OLDAGE"
  writeLines(errcon, con = sprintf("%s/Resources/errorcodes.config", genericconfigdir))
  if (verbose > 0) cat("errorcodes.config updated.\n")

  #### regex.config ####
  # (1)
  # hijack insuranceid line for oldage regular expression
  # extra spaces or hyphens permitted
  oldageregex <- "((9[[:digit:]]|1[[:digit:]]{2})[ -]*(yo\\b|y\\. *o\\.|years?[ -]*old))"
  # (teststring <- c("117 y.o.", "95 years old", "91   y.o.", "99y.o.", "100  year  old", "98 y. o.", "155 year-old", "88 y.o."))
  # (newnote <- gsub(oldageregex, "XXX-AGEHIGH \\3", teststring))
  regcon <- readLines(con = sprintf("%s/Resources/regex.config", genericconfigdir), warn = FALSE)
  insregline <- grep("INSURANCEID", regcon)
  regcon[insregline] <- sprintf("INSURANCEID=%s", oldageregex)
  # (2)
  # allow the MRN length to be specified
  patidregex <- sprintf("(([0-9]{%s})|([0-9]{4}[-][0-9]{4}))", as.character(digits_in_mrn))
  patidregline <- grep("PATIENTID", regcon)
  regcon[patidregline] <- sprintf("PATIENTID=%s", patidregex) 

  writeLines(regcon, con = sprintf("%s/Resources/regex.config", genericconfigdir))
  if (verbose > 0) cat("regex.config updated.\n")

  ### PrefixWhitelistDictionary.psv
  pwd <- data.frame(
    Alias = character(0),
    Field = character(0),
    stringsAsFactors = FALSE
  )
  utils::write.table(
    pwd,
    file = sprintf("%s/Resources/%s.psv", genericconfigdir, "PrefixWhitelistDictionary"),
    sep="\t",
    row.names=FALSE,
    quote = FALSE)
  if (verbose > 0) cat("PrefixWhitelistDictionary updated.\n")

  ### flagtext.psv
  # WAS:
  # Alias	Field
  # 1	GENDER
  # 2	OFFICE
  # 3	PHONE
  # 4	MAX-1
  # 5	AABB
  # 6	AB12
  # 7	ABC-
  # 8	HOME
  # 9	CELL
  # 10	DOB
  # 11	SSN
  # 12	O
  # 13	H
  # 14	C
  # 15	G
  flg <- data.frame(
    Alias = seq.int(3),
    Field = c("NAME", "DOB", "SSN"),
    stringsAsFactors = FALSE
  )
  utils::write.table(
    flg,
    file = sprintf("%s/Resources/%s.psv", genericconfigdir, "flagtext"),
    sep="\t",
    row.names=FALSE,
    quote = FALSE)
  if (verbose > 0) cat("flagtext updated.\n")


  ### ScrubbingRulesMap ###
  srm <- data.frame(
    Field = c("lastname", "firstname", "city", "middlename"),
    Alias = c("cleansename||AtLeastTwoCharacters",
              "cleansename||AtLeastTwoCharacters",
              "cleanseAddress||AtLeastTwoCharacters",
              "cleansename||AtLeastTwoCharacters"),
    OutputField = c("lastname_out", "firstname_out", "city_out", "middlename_out"),
    stringsAsFactors = FALSE
  )
  utils::write.table(
    srm,
    file=sprintf("%s/Maps/%s.psv", genericconfigdir, "ScrubbingRulesMap"),
    sep="\t",
    row.names=FALSE,
    quote = FALSE)
  if (verbose > 0) cat("ScrubbingRulesMap updated.\n")

  ### SecondaryInputMap ###
  # The secondary input map is designed for row-matched scrubbing
  # We rely on global blacklists instead.
  # Create mock secondary data file.
  # This file is just a place holder
  secondary <- data.frame(
    PatientID = character(0),
    firstname = character(0),
    middlename = character(0),
    lastname = character(0),
    city = character(0),
    county = character(0),
    stringsAsFactors = FALSE
  )
  secondaryInputFile <- sprintf("%s/%s.psv", workfolder, "secondary")
  utils::write.table(
    secondary,
    file = secondaryInputFile,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE)

  sim <- data.frame(
    Field = seq.int(ncol(secondary)),
    Alias = names(secondary),
    stringsAsFactors = FALSE
  )
  utils::write.table(
    sim,
    file=sprintf("%s/Maps/%s.psv", genericconfigdir, "SecondaryInputMap"),
    sep="\t",
    row.names=FALSE,
    quote = FALSE)
  if (verbose > 0) cat("SecondaryInputMap updated.\n")

  ### Sanity files ###
  # shouldn't need this with sanitycheck=FALSE, but do (last I checked).
  cat(paste("field", seq_along(names(secondaryInputFile)), collapse = ",", sep=""),
      file=sprintf("%s/SanityFiles/sanity-secondary.txt", genericconfigdir))



  #######################
  #######################
  # PRIMARY INPUT FILES #
  #######################
  #######################

  # This directory contains the datafiles that need to be scrubbed.
  if (verbose > 0) {
    cat(sprintf("Data to be scrubbed will be read from\n%s\n", dirtoscrub))
  }
  if (dir.exists(dirtoscrub)) {
    if (verbose > 0) {
      cat("Existing files:\n")
      print(dir(dirtoscrub))
    }
  } else {
    stop(sprintf("%s does not exist.", dirtoscrub))
  }

  # link scrubbing request to files in directory
  directory <- dir(dirtoscrub)
  fileroots <- names(listofcolumnnamestoscrub)
  nfiles <- length(fileroots)
  filenames <- rep(NA_character_, nfiles)

  for (f in seq.int(nfiles)) {
    cat(sprintf("%0.2d: %s\n", f, fileroots[f]))
    filename <- grep(fileroots[f], directory, value = TRUE, ignore.case = TRUE)
    
    if (length(filename) == 0) { # No match
      if (verbose > 0) cat(sprintf("%s not matched in %s.\n Skipping.\n", fileroots[f], dirtoscrub))
    } else if (length(filename) > 1) { # several approximate matches
      if (verbose > 0) cat(sprintf("%s matched %d files in %s.\nChecking for exact match.\n", fileroots[f], length(filename), dirtoscrub))
      # print(filename)
      filename <- grep(sprintf("%s.csv", fileroots[f]), directory, value = TRUE, ignore.case = TRUE)
      if (length(filename) != 1) { # no exact match
        if (verbose > 0) cat(sprintf("%s.csv not matched in %s.\n Skipping.\n", fileroots[f], dirtoscrub))
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
  if (isTRUE(all(matched))) {
    if (verbose > 0) {
      cat(sprintf("All %d listed file(s) located in %s.\n", sum(matched), dirtoscrub))
      print(filenames)
    }
  } else {
    if (verbose > 0) {
      cat(sprintf("Only %d listed file(s) located in %s.\n", sum(matched), dirtoscrub))
      print(filenames[matched])
    }
    if (verbose > 0) cat(sprintf("%d listed file(s) to scrub not found in %s.\n", sum(!matched), dirtoscrub))
    print(fileroots[!matched])
  }
  if (sum(matched) == 0) {
    stop("No files to scrub.")
  }

  # link column names requested to column numbers
  # column numbers needed for configuration map
  listofvarnames <- listofcolumnnumberstoscrub <- vector("list", length = nfiles)
  names(listofvarnames) <- names(listofcolumnnumberstoscrub) <- names(listofcolumnnamestoscrub)

  for (f in seq.int(nfiles)) {
    if (!is.na(filenames[f])) {
      if (verbose > 0) {
        cat(sprintf("Searching variable names of %s (%s)\n", filenames[f], fileroots[f]))
      }
    } else {
      if (verbose > 0) {
        cat(sprintf("Skipping %s\n", fileroots[f]))
      }
      next
    }

    listofvarnames[[f]] <- names(utils::read.csv(
      sprintf("%s/%s.csv", dirtoscrub, filenames[f]),
      nrows = 1, header = TRUE
    ))
    if (verbose > 0) {
      cat(sprintf("Variable names in file:\n"))
      print(listofvarnames[[f]])
      cat(sprintf("Searching for\n"))
      print(listofcolumnnamestoscrub[[f]])
    }

    listofcolumnnumberstoscrub[[f]] <- match(
      toupper(listofcolumnnamestoscrub[[f]]),
      toupper(listofvarnames[[f]]))

    if (verbose > 0) {
      cat(sprintf("Where found?\n"))
      print(listofcolumnnumberstoscrub[[f]])
    }

    if (any(is.na(listofcolumnnumberstoscrub[[f]]))) {
      # might need more error checking/handling here.
      stop("column not found.")
    }
  }

  if (verbose > 0) {
    print(listofcolumnnamestoscrub)
    print(listofcolumnnumberstoscrub)
    if (verbose > 1) {
      print(listofvarnames)
    }
  }

  listofcommands <- vector("list", length = nfiles)
  names(listofcommands) <- names(listofcolumnnamestoscrub)

  ##############################
  # SPECIALIZED CONFIGURATIONS #
  ##############################
  for (f in seq.int(nfiles)) {
    if (is.na(filenames[f])) next

    # Create specific configuration files
    # generic configuration create above will be copied to here

    specificconfigdir <-  sprintf("%s/config_%s", workfolder, fileroots[f])

    if (verbose > 0) {
      cat(sprintf("A specific configuration directory to be here:\n%s", specificconfigdir))
    }
    if (dir.exists(specificconfigdir)) {
      if (verbose > 0) {
        cat("Directory existed. Contents are\n")
        print(dir(specificconfigdir))
      }
    } else {
      if (dir.create(specificconfigdir, recursive = TRUE)) {
        if (verbose > 0) {
          cat("Directory newly created for specific configuration files.\n")
        }
      } else {
        stop("Unable to create specific configuration directory.")
      }
    }

    # copy from generic to specific
    if (isTRUE(all(file.copy(dir(genericconfigdir, full.names = TRUE), specificconfigdir, overwrite=TRUE, recursive=TRUE)))) {
      if (verbose > 0) {
        cat("Configuration copied from original to generic.\n")
      }
    } else {
      stop("Configuration copy unsuccessful.")
    }


    ###########################
    # Create the format files #
    ###########################

    ### PrimaryInputMap ###
    names_primary   <- data.frame(
      Field=seq_along(listofvarnames[[f]]),
      Alias=toupper(listofvarnames[[f]]),
      stringsAsFactors = FALSE)

    # PatientID key needs to be tailored to datavant software
    # Use PatientID in place of MRN ("Mandatory")
    names_primary$Alias[grep("PAT_MRN", names_primary$Alias)[1]] <- "PatientID"

    utils::write.table(
      names_primary,
      file=sprintf("%s/Maps/%s", specificconfigdir, "PrimaryInputMap.psv"),
      sep="\t",
      row.names=FALSE,
      quote = FALSE)

    #########
    # RULES #
    #########
    ### CustomRulesMap ###
    crm <- data.frame(
      Alias = listofcolumnnumberstoscrub[[f]],
      Field = "firstname_out||lastname_out||city_out||middlename_out" # middle name trial
      # Field = "firstname_out||lastname_out||city_out" # a reduced set
      # Field = "email||firstname_out||lastname_out||address_street||city_out||birth_date||death_date||cell_phone||home_phone||zip" # the original set
    )
    utils::write.table(
      crm,
      file=sprintf("%s/Maps/%s", specificconfigdir, "CustomRulesMap.psv"),
      sep="\t",
      row.names=FALSE,
      quote = FALSE)

    ### PHIRulesMap ###
    prm <- data.frame(
      Alias = listofcolumnnumberstoscrub[[f]],
      Field = "URLRegexRule||EmailRegexRule||IPRegexRule||FNDBRule||LNDBRule||CountyDBRule||CityDBRule||PhoneRegexRule||ZIPDBPatternRule||SSNRegexRule||PatientIdRegexRule||RecordNumberRegexRule||InsuranceIdRegexRule||FlagTextRule||DateRegexRule",
      stringsAsFactors = FALSE)

    utils::write.table(
      prm,
      file=sprintf("%s/Maps/%s", specificconfigdir, "PHIRulesMap.psv"),
      sep="\t",
      row.names=FALSE,
      quote = FALSE)

    # command string
    listofcommands[[f]] <- 
      sprintf('"%s/phiremoval.%s" --template "%s" --primaryInputPath "%s" --secondaryInputPath "%s" --outputPath "%s" --logFile "%s.txt"',
              toollocation, # command location
              sh_or_bat, # shell or bat version of scrubber
              specificconfigdir, # template directory
              sprintf("%s/%s.csv", dirtoscrub, filenames[f]), # location of file to scrub (primaryInputPath)
              secondaryInputFile, # location of patient information (secondaryInputPath)
              sprintf("%s/scrubbed_%s.csv", scrubbeddir, filenames[f]), # scrubbed file location (outputPath)
              sprintf("%s/log_%s", logdirectory, filenames[f])) # log file (logFile)

    if (verbose > 0) {
      cat(listofcommands[[f]], "\n")
    }

    cat(listofcommands[[f]], file=sprintf("%s/sample_call_%s.txt", commanddirectory, filenames[f]))

  } # end loop

  #############
  # RUN EACH? #
  #############
  if (isTRUE(runscrubber)) {
    for (f in seq.int(nfiles)) {
      # Run from within R
      if (verbose > 0) {
        cat(sprintf("scrubbing %s\n", filenames[f]))
        print(pt <- proc.time())
      }
      system(listofcommands[[f]], intern=FALSE)
      # another possibility?
      # cmdvec <- strsplit(cmdstring, " ")
      # system2(cmdvec[1], args = cmdvec[-1])
      if (verbose > 0) print(proc.time()-pt)
    }
  }

  return(invisible(NULL))
}
