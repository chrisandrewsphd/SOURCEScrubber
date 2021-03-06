% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/master.R
\name{scrub}
\alias{scrub}
\title{Title}
\usage{
scrub(
  dirtoscrub,
  scrubbeddir,
  workfolder = tempdir(),
  namesforblacklist,
  toollocation,
  originalconfigdir = sprintf("\%s/UOMPHIRemoval", toollocation),
  runscrubber = FALSE,
  verbose = 0,
  sh_or_bat = "bat",
  listofcolumnnamestoscrub = list(oph_order_text = c("ORDER_COMMENT", "NARRATIVE"),
    oph_surgery_text = c("OP_NOTE", "BRIEF_OP_NOTE"), oph_enc_problem_list_text =
    c("PROBLEM_CMT"), oph_enc_hpi_text = c("HPI_COMMENT"), oph_enc_visit_summary_text =
    c("HPI", "TECH_COMMENT", "PAT_OCULAR_HISTORY"), oph_lab_text = c("LINE_COMMENT",
    "RESULTS_COMP_CMT", "RESULTS_CMT"), oph_enc_visit_text = c("PROGRESS_NOTE"),
    oph_rad_text = c("APPT_NOTE", "TEXT"), oph_family_hx_text = c("COMMENTS",
    "FAM_STAT_NAME"), oph_enc_exam_text = c("SMRTDTA_ELEM_VALUE"),     
    oph_lab_order_text = c("SPECIMEN_COMMENTS"), oph_med_text = c("INSTRUCTION"),
    OPH_SURGERY_all_text = c("PRE_OP_DIAG"), oph_surgery_procedure_text = c("COMMENTS"))
)
}
\arguments{
\item{dirtoscrub}{path to files to scrub.}

\item{scrubbeddir}{directory to write scrubbed files}

\item{workfolder}{(optional) file specific configurations stored here: temporary directory will be used if none specified.}

\item{namesforblacklist}{full path to custom file of blacklist values}

\item{toollocation}{location of scrubber tool}

\item{originalconfigdir}{(optional) location of original configuration direcotry provided by Datavant}

\item{runscrubber}{Run scrubber after configuration? FALSE by default.}

\item{verbose}{Amount of output provided to console. 0 (default) for none.  Higher values may provide more.}

\item{sh_or_bat}{Choose appropriate script for operating system.  "bat" (default) for Windows. "sh" for Unix-like.}

\item{listofcolumnnamestoscrub}{which columns in which files to scrub? This argument is a list. Each component is a vector of column names.  The component names are the unique parts of the file names of the files to scrub.  Default value is to include 21 variables from 14 files.}
}
\value{
invisible NULL
}
\description{
Title
}
\examples{
## Not Run
## scrub(dirtoscrub = "./toscrub",
## scrubbeddir = "./scrubbed",
## namesforblacklist = "./black.csv",
## toollocation = "./tool",
## verbose = 3,
## listofcolumnsnamestoscrub = list())
}
