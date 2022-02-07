# Chris Andrews
# 2021 Project UM: revamp for new file structures.
# 2021-09-03

# AS FUNCTION
#' Helper file for master(). Not expected for direct call by user.
#'
#' @param namesforblacklist Location of the PHI file.  csv file that contains first names, middle names, last names, cities, and counties
#' @param originalconfigdir Location of the Datavant scrubber configuration directory
#' @param temporaryconfigdir Location to store expanded databases adapted to the file to be scrubbed.
#' @param verbose How much should be printed to the console? 0 (default) for none.  Higher values potentially display more
#' @param overwriteifexists If the expanded databases already exist, should they be overwritten
#'
#' @return invisible write.table()
#' @export
#'
expand <- function(
  namesforblacklist, # Location of the PHI file
  originalconfigdir, # Location of Datavant configuration
  temporaryconfigdir, # Expanded Databases will be stored here
  verbose = 0,
  overwriteifexists = FALSE
) {

  # Expand First Name and Last Name Databases
  # Also, City and County Databases

  # Sources for names, county, and city are are
  # the existing databases shipped by Datavant
  #   (firstname_db, lastname_db, citylist_db, county_db)
  # and a specially created file containing all names that are in local database
  # Probably should curate this some for misspellings

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

  # Function to read Datavant databases
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

  # Function to write possibly augmented databases
  writenew <- function(df, filebase, outconfigdir, overwriteifexists=FALSE) {
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



  # Merge the Datavant word list and the PHI word list
  # Return a data.frame suitable for saving as a Datavant configuration file

  cleanjoinweed <- function(phivec, datavantvec,
                            whitevec=character(0), verbose=0, split=TRUE) {
    # phivec is the vector of local PHI terms
    # datavantvec is the vector provided by Datavant

    # whitevec allows for keeping terms out of the global blacklist
    # this might help with eye terms we want to exclude (but should compare to stedman)

    # Capitalize
    datavantvec <- toupper(datavantvec) # this should already be true
    phivec <- toupper(phivec) # this might also

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


  # read datavant databases
  first.datavant <- readoriginal("firstname_db", inconfigdir = originalconfigdir)
  last.datavant  <- readoriginal("lastname_db" , inconfigdir = originalconfigdir)
  city.datavant  <- readoriginal("citylist_db" , inconfigdir = originalconfigdir)
  county.datavant<- readoriginal("county_db"   , inconfigdir = originalconfigdir)
  white.datavant <- readoriginal("WhitelistDictionary", inconfigdir=originalconfigdir, quiet = TRUE)

  # customized ophthalmology Whitelist

  white <- c("TREACHER COLLINS", "STEVENS-JOHNSON", "FOSTER-KENNEDY", "BIELSCHOWSKY", "ARYGLL ARLT", "BERGMEISTER", "COGAN-REESE", "MURCUS GUNN", "SHERRINGTON", "WAARDENBURG", "WYURN-MASON", "HUTCHINSON", "KRUKENBERG", "MITTENDORF", "VON GRAEFE", "VON HIPPEL", "DALRYMPLE", "FLEISCHER", "MILKULICZ", "PURTSCHER", "STAGGARDT", "AXENFELD", "CHANDLER", "DESCEMET", "GOLDMANN", "LAURENCE", "MORGAGNI", "PARINAUD", "PURKINJE", "SCHWALBE", "THYGESON", "WESTPHAL", "WILBRAND", "BJERRUM", "CLOQUET", "EDINGER", "ELSHNIG", "MOEBIUS", "SATTLER", "SCHIMER", "SJOGREN", "TERRIEN", "AMSLER", "BECHET", "BONNET", "BOWMAN", "GRAVES", "HARADA", "HERING", "HORNER", "KAHOOK", "KRAUSE", "LINDAU", "MADDOX", "MARFAN", "MOOREN", "MULLER", "REITER", "SEIDEL", "STURGE", "TERSON", "UHTOFF", "BRUCH", "COATS", "COGAN", "DOYNE", "DUANE", "EALES", "FUCHS", "HENLE", "KNAPP", "LEBER", "TENON", "USHER", "WEBER", "ADIE", "BEHR", "BELL", "BEST", "HAAB", "HESS", "MOLL", "SCHS", "VOGT", "ZINN", "TAY")


  # merge phi and datavant data
  white.df <- cleanjoinweed(white, white.datavant$Field, character(0), split=FALSE, verbose = verbose)

  first.df <- cleanjoinweed(patientdata$PAT_FIRST_NAME, first.datavant$Field, verbose = verbose) #, whitevec=white.df$Field, verbose=3)
  middle.df<- cleanjoinweed(patientdata$PAT_MIDDLE_NAME,first.datavant$Field, verbose = verbose) #, whitevec=white.df$Field, verbose=3)
  last.df  <- cleanjoinweed(patientdata$PAT_LAST_NAME ,  last.datavant$Field, verbose = verbose) #, whitevec=white.df$Field, verbose=3)
  city.df  <- cleanjoinweed(patientdata$CITY          ,  city.datavant$Field, verbose = verbose) #, whitevec=white.df$Field, verbose=3)
  county.df<- cleanjoinweed(patientdata$COUNTY        ,county.datavant$Field, verbose = verbose) #, whitevec=white.df$Field, verbose=3)


  writenew(white.df , "WhitelistDictionary", outconfigdir=temporaryconfigdir, overwriteifexists = overwriteifexists)
  writenew(first.df , "firstname_db", outconfigdir=temporaryconfigdir, overwriteifexists = overwriteifexists)
  writenew(middle.df,"middlename_db", outconfigdir=temporaryconfigdir, overwriteifexists = overwriteifexists)
  writenew(last.df  , "lastname_db" , outconfigdir=temporaryconfigdir, overwriteifexists = overwriteifexists)
  writenew(city.df  , "citylist_db" , outconfigdir=temporaryconfigdir, overwriteifexists = overwriteifexists)
  writenew(county.df, "county_db"   , outconfigdir=temporaryconfigdir, overwriteifexists = overwriteifexists)

  return(invisible(NULL))
}
