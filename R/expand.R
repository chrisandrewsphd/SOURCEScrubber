# Chris Andrews
# 2021 Project UM: revamp for new file structures.
# 2021-09-03

# AS FUNCTION
#' Helper file for master(). Not expected for direct call by user.
#'
#' @param namesforblacklist Location of the PHI file.  csv file that contains first names, middle names, last names, cities, and counties
#' @param originalconfigdir Location of the Datavant scrubber configuration directory
#' @param temporaryconfigdir Location to store expanded databases adapted to the file to be scrubbed.
#' @param termsforwhitelist Location of user-provided whitelist. text file that contains a single column of terms to be included in the whitelist. Variable name (first row) should be "WHITELIST".
#' @param deletewhitefromblack Should the terms in the whitelist be removed from the blacklists? Default is `FALSE` to match previous behavior. Might change to `TRUE` in the future. However, Scrubber may do this already.
#' @param verbose How much should be printed to the console? 0 (default) for none.  Higher values potentially display more
#' @param overwriteifexists If the expanded databases already exist, should they be overwritten
#'
#' @return invisible write.table()
expand <- function(
  namesforblacklist, # Location of the PHI file
  originalconfigdir, # Location of Datavant configuration
  temporaryconfigdir, # Expanded Databases will be stored here
  termsforwhitelist = NULL, # Location of user-provided whitelist
  deletewhitefromblack = FALSE, # should whitelist terms be removed from blacklists?
  verbose = 0,
  overwriteifexists = FALSE
) {

  # Expand First Name and Last Name Databases
  # Also, City and County Databases
  # Finally, whitelist

  # Sources for names, county, and city are are
  # the existing databases shipped by Datavant
  #   (firstname_db, lastname_db, citylist_db, county_db)
  # and a specially created file containing all names that are in local database
  # Probably should curate this some for, e.g., misspellings, common
  # language words, "XXX", ...

  # read local PHI file
  if (file.exists(namesforblacklist)) {
    if (verbose > 0) cat(sprintf("%s exists.\n", namesforblacklist))
  } else {
    stop(sprintf("%s does not exist.", namesforblacklist))
  }
  st <- system.time(patientdata <- utils::read.csv(
    namesforblacklist,
    colClasses="character",
    row.names=NULL, stringsAsFactors=FALSE,
    na.strings = NULL))

  if (verbose > 0) {
    print(dim(patientdata))
    print(names(patientdata))
    if (verbose > 1) print(st)
  }
  
  if (!all(c("PAT_FIRST_NAME", "PAT_MIDDLE_NAME", "PAT_LAST_NAME", "CITY", "COUNTY") %in% names(patientdata))) {
    stop(sprintf("namesforblacklist (%s) must contain columns named 'PAT_FIRST_NAME', 'PAT_MIDDLE_NAME', 'PAT_LAST_NAME', 'CITY', 'COUNTY'", namesforblacklist))
  }
  
  # read user-provided whitelist
  if (missing(termsforwhitelist)) {
    if (verbose > 0) cat("No user-defined whitelist provided.\n")
  } else {
    if (file.exists(termsforwhitelist)) {
      if (verbose > 0) cat(sprintf("%s exists.\n", termsforwhitelist))
    } else {
      stop(sprintf("%s does not exist.", termsforwhitelist))
    }
    st <- system.time(white.user <- utils::read.csv(
      termsforwhitelist,
      colClasses="character",
      row.names=NULL, stringsAsFactors=FALSE,
      na.strings = NULL))
    
    if (verbose > 0) {
      print(dim(white.user))
      print(names(white.user))
      if (verbose > 1) print(st)
    }
    
    if (!all(c("WHITELIST") %in% names(white.user))) {
      stop(sprintf("termsforwhitelist (%s) must contain column named 'WHITELIST'", termsforwhitelist))
    }
  }
  

  # read datavant databases
  first.datavant <- readoriginal("firstname_db", inconfigdir = originalconfigdir)
  last.datavant  <- readoriginal("lastname_db" , inconfigdir = originalconfigdir)
  city.datavant  <- readoriginal("citylist_db" , inconfigdir = originalconfigdir)
  county.datavant<- readoriginal("county_db"   , inconfigdir = originalconfigdir)
  white.datavant <- readoriginal("WhitelistDictionary", inconfigdir=originalconfigdir, quiet = TRUE)

  # customized ophthalmology Whitelist

  white <- c("TREACHER COLLINS", "STEVENS-JOHNSON", "FOSTER-KENNEDY", "BIELSCHOWSKY", "ARYGLL ARLT", "BERGMEISTER", "COGAN-REESE", "MURCUS GUNN", "SHERRINGTON", "WAARDENBURG", "WYURN-MASON", "HUTCHINSON", "KRUKENBERG", "MITTENDORF", "VON GRAEFE", "VON HIPPEL", "DALRYMPLE", "FLEISCHER", "MILKULICZ", "PURTSCHER", "STAGGARDT", "AXENFELD", "CHANDLER", "DESCEMET", "GOLDMANN", "LAURENCE", "MORGAGNI", "PARINAUD", "PURKINJE", "SCHWALBE", "THYGESON", "WESTPHAL", "WILBRAND", "BJERRUM", "CLOQUET", "EDINGER", "ELSHNIG", "MOEBIUS", "SATTLER", "SCHIMER", "SJOGREN", "TERRIEN", "AMSLER", "BECHET", "BONNET", "BOWMAN", "GRAVES", "HARADA", "HERING", "HORNER", "KAHOOK", "KRAUSE", "LINDAU", "MADDOX", "MARFAN", "MOOREN", "MULLER", "REITER", "SEIDEL", "STURGE", "TERSON", "UHTOFF", "BRUCH", "COATS", "COGAN", "DOYNE", "DUANE", "EALES", "FUCHS", "HENLE", "KNAPP", "LEBER", "TENON", "USHER", "WEBER", "ADIE", "BEHR", "BELL", "BEST", "HAAB", "HESS", "MOLL", "SCHS", "VOGT", "ZINN", "TAY")


  # construct whitelist from
  #   user-provided whitelist,
  #   UM created ophthalmology whitelist, and
  #   datavant list (NULL and NONE)
  white.df <- cleanjoinweed(
    phivec = white.user[, "WHITELIST"],
    datavantvec = c(white, white.datavant$Field),
    whitevec = character(0),
    split=FALSE,
    verbose = verbose)

  # merge user-provided PHI lists (blacklists) and datavant-provided lists
  first.df <- cleanjoinweed(
    phivec = patientdata[, "PAT_FIRST_NAME" ],
    datavantvec = first.datavant$Field,
    whitevec = if (isTRUE(deletewhitefromblack)) white.df$Field else character(0),
    verbose = verbose)
  middle.df<- cleanjoinweed(
    phivec = patientdata[, "PAT_MIDDLE_NAME"],
    datavantvec = first.datavant$Field,
    whitevec = if (isTRUE(deletewhitefromblack)) white.df$Field else character(0),
    verbose = verbose)
  last.df  <- cleanjoinweed(
    phivec = patientdata[, "PAT_LAST_NAME"  ],
    datavantvec = last.datavant$Field,
    whitevec = if (isTRUE(deletewhitefromblack)) white.df$Field else character(0),
    verbose = verbose)
  city.df  <- cleanjoinweed(
    phivec = patientdata[, "CITY"           ],
    datavantvec = city.datavant$Field,
    whitevec = if (isTRUE(deletewhitefromblack)) white.df$Field else character(0),
    verbose = verbose)
  county.df<- cleanjoinweed(
    phivec = patientdata[, "COUNTY"         ],
    datavantvec = county.datavant$Field,
    whitevec = if (isTRUE(deletewhitefromblack)) white.df$Field else character(0),
    verbose = verbose)


  writenew(
    df = white.df ,
    filebase = "WhitelistDictionary",
    outconfigdir = temporaryconfigdir,
    overwriteifexists = overwriteifexists)
  writenew(
    df = first.df ,
    filebase = "firstname_db",
    outconfigdir = temporaryconfigdir,
    overwriteifexists = overwriteifexists)
  writenew(
    df = middle.df,
    filebase = "middlename_db",
    outconfigdir = temporaryconfigdir,
    overwriteifexists = overwriteifexists)
  writenew(
    df = last.df  ,
    filebase = "lastname_db" ,
    outconfigdir = temporaryconfigdir,
    overwriteifexists = overwriteifexists)
  writenew(
    df = city.df  ,
    filebase = "citylist_db" ,
    outconfigdir = temporaryconfigdir,
    overwriteifexists = overwriteifexists)
  writenew(
    df = county.df,
    filebase = "county_db"   ,
    outconfigdir = temporaryconfigdir,
    overwriteifexists = overwriteifexists)

  return(invisible(NULL))
}



#' Function to read Datavant databases
#'
#' @param filebase which database? e.g., "firstname_db"
#' @param inconfigdir Directory with existing datavant configuration files
#' @param verbose How much should be printed to the console? 0 (default) for none.  Higher values potentially display more
#' @param quiet Suppress warning messages while reading?  Default = FALSE
#'
#' @return invisible data.frame
readoriginal <- function(filebase, inconfigdir, verbose = 0, quiet = FALSE) {
  if (file.exists(sprintf("%s/Resources/%s.psv", inconfigdir, filebase))) {
    if (verbose > 0) {
      cat(sprintf("%s exists.\n"))
    }
  } else {
    stop(sprintf("%s does not exist.", filebase))
  }
  
  dat <- if (quiet) { # to avoid 'incomplete final line' warning
    suppressWarnings(utils::read.delim(
      sprintf("%s/Resources/%s.psv", inconfigdir, filebase),
      row.names=NULL,
      stringsAsFactors=FALSE,
      na.strings = NULL))
  } else {
    utils::read.delim(
      sprintf("%s/Resources/%s.psv", inconfigdir, filebase),
      row.names=NULL,
      stringsAsFactors=FALSE,
      na.strings = NULL)
  }
  if (verbose > 0) {
    print(dim(dat))
    print(names(dat))
  }
  
  return(invisible(dat))
}

#' Function to write possibly augmented databases
#'
#' @param df data.frame to write
#' @param filebase which database? e.g., "firstname_db"
#' @param outconfigdir directory to write data
#' @param overwriteifexists Overwrite existing configuration file if it exists? Default is FALSE
#'
#' @return invisible NULL
writenew <- function(
    df,
    filebase,
    outconfigdir,
    overwriteifexists = FALSE) {
  if (!dir.exists(outconfigdir)) {
    message(sprintf("Directory %s does not exist...", outconfigdir))
    stop("Aborting.")
  }
  if (!dir.exists(sprintf("%s/Resources", outconfigdir))) {
    cat(sprintf("Directory %s does not exist.\nCreating.\n", sprintf("%s/Resources", outconfigdir)))
    # not sure if this always works.
    dir.create(sprintf("%s/Resources", outconfigdir))
  }
  
  fn <- sprintf("%s/Resources/%s.psv", outconfigdir, filebase)
  if (file.exists(fn)) {
    cat(sprintf("%s already exists...", fn))
    if (isTRUE(overwriteifexists)) {
      cat("overwriting.\n")
    } else {
      stop("Aborting.")
    }
  }
  
  return(invisible(utils::write.table(
    df, file=fn,
    quote=FALSE, sep="\t", na="", row.names=FALSE, col.names=TRUE)))
}



#' Merge the Datavant word list and the PHI word list, remove whitelist
#'
#' @param phivec vector of terms from user (usually)
#' @param datavantvec vector of terms from datavant (usually)
#' @param whitevec vector of terms to exclude from the new list. Default is empty.
#' @param verbose How much should be printed to the console? 0 (default) for none.  Higher values potentially display more
#' @param split Should terms with more than one word (space or hyphen) be split and subterms included? Default is TRUE
#'
#' @return A data.frame suitable for saving as a Scrubber configuration file, invisibly
cleanjoinweed <- function(
    phivec,
    datavantvec,
    whitevec = character(0),
    verbose = 0,
    split = TRUE) {
  # phivec is the vector of local PHI terms
  # datavantvec is the vector provided by Datavant
  
  # whitevec allows for keeping terms out of the global blacklist
  # this might help with eye terms we want to exclude (but should compare to stedman)
  
  # Capitalize
  datavantvec <- toupper(datavantvec) # this should already be true
  phivec <- toupper(phivec) # this might also
  whitevec <- toupper(whitevec)
  
  # unique terms only
  datavantvec <- unique(datavantvec) # this should already be true
  phivec <- unique(phivec)
  
  # split on blank or hyphen?
  #   add all pieces to blacklist as well as the original
  if (split) {
    phipieces <- unique(unlist(strsplit(phivec, "[ -]")))
    phivec <- union(phivec, phipieces) # keep both pieces and whole.
  }
  
  # remove names with fewer than 2 actual characters
  # replace non-alphabetic characters by empty string
  phicompressed <- gsub("[^[:alpha:]]", "", phivec)
  
  phivec <- phivec[nchar(phicompressed) >= 2]
  
  # merge with datavant
  joined <- union(datavantvec, phivec)
  
  # remove any terms from whitelist
  weeded <- setdiff(joined, whitevec) # joined - white
  if (length(weeded) < length(joined)) {
    if (verbose >= 1) {
      message(sprintf("%d terms found in Whitelist removed.", length(joined) - length(weeded)))
      if (verbose >= 2) {
        print(intersect(joined, whitevec))
      }
    }
  }
  
  # sort by length and then alphabetize
  weeded <- weeded[order(nchar(weeded), weeded, method="radix", decreasing = c(TRUE, FALSE))]
  
  
  # create data.frame
  weeded.df <- data.frame(Alias=seq_along(weeded), Field=weeded, stringsAsFactors = FALSE)
  
  if (verbose>=3) {
    cat("Head and tail of new database:\n")
    print(utils::head(weeded.df), row.names=FALSE)
    print(utils::tail(weeded.df), row.names=FALSE)
  }
  
  return(invisible(weeded.df))
}
