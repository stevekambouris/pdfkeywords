#' Extract text containing a keyword from a PDF
#'
#' \code{extract_keyword} finds all occurrences of a given \code{keyword} in
#' the text of one or more PDFs, and returns a data frame containing a row for
#' each occurrence found. Each row of the data frame is made up of a string
#' containing the keyword in its immediate context, the page number of the PDF
#' on which the keyword was found, and the file name of the PDF.
#'
#' \code{extract_keyword} relies on the function \code{pdf_text} from the
#' package \code{pdftools} to extract the the text from each PDF and the
#' function \code{str_extract_all} from the package \code{stringr} to extract
#' the keywords from the PDF text; the rest of the function arranges the output
#' into a neat format.
#'
#' @param file A character vector of the PDF file names to search.
#' @param keyword The keyword to look for in the PDF(s).
#' @param nbefore The number of words to include before the keyword, can be an
#' integer zero or greater. The default is 0.
#' @param nafter The number of words to include after the keyword, can be an
#' integer zero or greater. The default is 0.
#'
#' @return A data frame with three variables: \code{PDF_file}, \code{PDF_page},
#'   and \code{Keyword_extract}. The number of rows in the data frame will
#'   correspond to the number of times that the keyword was found in the PDF(s)
#'   searched. If the keyword was not found in any PDFs, there will be no rows
#'   in the data frame. If the text extraction function fails for all PDFs
#'   specified in the \code{file} argument, NULL will be returned, along with a
#'   warning for each of the affected PDF files. If text can be extracted from
#'   at least one PDF file, the data frame will be returned, but the warnings
#'   for the affected files will still be shown.
#' @export
#'
#' @examples
#' \dontrun{
#' extract_keyword("example.pdf", "foo")
#' extract_keyword(c("a.pdf", "b.pdf"), "foo")
#' }
extract_keyword <- function(file, keyword, nbefore = 0L, nafter = 0L) {

  # Check that none of the essential arguments are missing.
  stopifnot(!missing(file), !missing(keyword), nbefore >= 0, nafter >= 0)

  # Create a regular expression based on the keyword and number of words to
  # look before and after the keyword.
  reg_expr <- paste0("(?i)([^\\s]+\\s){0,", nbefore, "}([^\\s]+){0,1}",
                     keyword,
                     "([^\\s]+){0,1}(\\s[^\\s]+){0,", nafter, "}")

  # Create a list to hold the output of each PDF in the file argument.
  out <- vector(mode = "list", length(file))

  # Create a safe version of the pdf_text function from pdftools.
  pdf_text_safely <- purrr::safely(pdftools::pdf_text)

  # Loop through the PDFs in the file argument and extract keywords from each.
  # Return null if the initial text extraction fails.

  # NOTE: This is essentially building in silent failure - the loop will just
  # move on to the next file after returning null without elaborating on why
  # the text extraction failed. Consider changing this to show warnings, at
  # least.

  for (i in seq_along(file)) {
    # Extract the text from the current PDF, safely.
    current_pdf_text <- pdf_text_safely(file[i])

    # If the result of the text extraction is not null, try extracting just
    # the keyword and its context from the text.
    if (!is.null(current_pdf_text$result)) {
      # Reduce all extraneous spaces down to single spaces between words.
      result_reduced <- stringr::str_trim(stringr::str_replace_all(
        current_pdf_text$result, "[[:space:]]+", " "), side = "both")

      # Extract all keywords in context from the space-reduced text.
      keyword_in_context <- stringr::str_extract_all(result_reduced, reg_expr)

      # Format the result as a data frame and add to the list.
      out[[i]] <- format_as_df(keyword_in_context, file[i])

    } else {
      # Show a warning that text could not be extracted.
      warning(paste0("Text could not be extracted from file ", file[i],". ",
                     "NULL was returned."))
      # Set the entry corresponding to the current PDF file to null.
      out[[i]] <- NULL
    }
  }

  # Compile the list of data frames into a single data frame.
  out_df <- do.call("rbind", out)

  return(out_df)
}

format_as_df <- function(inlist, infilename) {
  # Use melt to turn the list into a data frame, and change the variable
  # "value" to type "character".
  out <- reshape2::melt(inlist)
  out$value <- as.character(out$value)

  # If there are any rows in the data frame, create a variable "PDF_file"
  # which has the constant value of the passed PDF file name. If the data frame
  # has no rows, then this should have an empty value.
  if (dim(out)[1] > 0) {
    out$PDF_file <- infilename
  } else {
    out$PDF_file <- character(0)
  }

  # Set the names of the other variables in the data frame.
  names(out)[names(out) == "L1"] <- "PDF_page"
  names(out)[names(out) == "value"] <- "Keyword_extract"

  return(out)
}
