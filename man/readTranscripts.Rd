% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readTranscripts.R
\name{readTranscripts}
\alias{readTranscripts}
\title{Read raw transcripts files}
\usage{
readTranscripts(path, standardiseZstacks = FALSE)
}
\arguments{
\item{path}{a character vector containing the path name to read the raw transcripts files from}

\item{standardiseZstacks}{a logical to determine if Z-slice nomenclature is to be standardised between FOvs}
}
\value{
A data frame containing the raw transcripts from all FOVs found in the specified folder.
}
\description{
Reads all CosMx SMI raw transcript files (*complete_code_cell_target_call_coord.csv) in the specified path. Raw transcript files can be found in the 'AnalysisResults' folder after exporting the data from AtoMx SIP.
}
\examples{
\dontrun{

transcripts <- readTranscripts("path/to/input/directory")
}
}
