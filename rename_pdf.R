#' Rename PDFs (Interactively)
#'
#' @param filepath Directory containing PDF files (default is to ask)
#' @param n_words Number of words of text to use as alternative title when no title in PDF metadata (default of 20); NB: risk of exceeding maximum file name length causing error
#' @param tag Optional text tag appended to file name to identify renamed files (e.g. to use in exclude if function is re-run)
#' @param exclude Optional PDF filenames containing this text will be ignored
#' @param opw Optional string with owner password if encrypted PDF
#' @param upw Optional string with user password if encrypted PDF
#'
#' @return Character vector of old names (invisibly)
#' @export
#'
#' @examples
#' rename_pdf(tag = '_pdfrn')  # Renames PDF files in current directory, tagging renamed files with "_pdfrn"
rename_pdf <- function(filepath = rstudioapi::selectDirectory(),
                       n_words_short = 20,
                       n_words_long = 500,
                       tag = '',
                       exclude = '',
                       opw = '',
                       upw = '',
                       random = FALSE,
                       pattern = '\\.pdf$|\\.PDF$') {
  # Function to remove punctuation, newlines, trailing whitespace
  sanitise_filename <- function(x) {
    x <- gsub('[[:punct:] ]+', ' ', x)
    x <- gsub('\\r|\\n', ' ', x)
    x <- trimws(gsub('[[:space:] ]+', ' ', x))
    x
  }
  message('\nRenaming PDFs...\n')
  original_filenames <-
    list.files(filepath, pattern = pattern, full.names = TRUE)
  if (random) { 
    original_filenames <- original_filenames[sample(length(original_filenames))]
  } else {
    original_filenames <- original_filenames[order(nchar(original_filenames))]
  }
  if (!exclude %in% '')
    original_filenames <-
    original_filenames[!grepl(exclude, original_filenames)]
  for (o in original_filenames) {
    o_dirname <- dirname(o)
    o_basename <- basename(o)
    o_title <- pdftools::pdf_info(o, opw, upw)$keys$Title
    raw_text <-
      paste0(pdftools::pdf_text(o, opw, upw), collapse = ' ')
    raw_text_sane <- sanitise_filename(raw_text)
    if (is.null(o_title)) {
      o_title_sane <- stringr::word(raw_text_sane, 1, n_words_short)
    } else if (o_title %in% '') {
      o_title_sane <- stringr::word(raw_text_sane, 1, n_words_short)
    } else {
      o_title_sane <- sanitise_filename(o_title)
    }
    o_title_sane <-
      stringr::str_replace_all(
        o_title_sane,
        c(
          'Microsoft Word\\s*' = '',
          'PowerPoint Presentation\\s*' = '',
          '\\s+doc\\s*' = '',
          '\\s+docx\\s*' = '',
          '\\s+pdf\\s*' = '',
          '\\s+dvi\\s*' = '',
          '\\s+indd\\s*' = '',
          '\\s+doi\\s*' = '',
          '\\s+crossm\\s*' = ''
        ),
        ''
      )
    cat('Original filename: ', o_basename, '\n')
    cat('Alternative title: ', o_title_sane, '\n')
    decision <-
      readline('Change to alternative title? (y[es]/[n]o/[c]hange manually): ')
    if (decision %in% 'y') {
      # TODO truncate if too long
      o_newname <-
        file.path(o_dirname,
                  paste0(o_title_sane, tag, '.pdf', collapse = NULL))
      file.rename(o, o_newname)
      cat('File renamed to: ', o_newname, '\n\n')
    } else if (decision %in% c('n', '')) {
      cat('File name not changed\n\n')
    } else if (decision %in% c('c')) {
      cat('First ', n_words_long, ' words of document:\n')
      cat(stringr::word(raw_text_sane, 1, n_words_long), '\n')
      specified_name <- readline('Specify file name: ')
      if (!specified_name %in% '') { 
        o_newname <- file.path(o_dirname, paste0(specified_name, tag, '.pdf', collapse = NULL))
      file.rename(o, o_newname)
      cat('File renamed to: ', o_newname, '\n\n')
      } else { 
        cat('File name not changed\n\n')
      }
    }
  }
  message('\nFinished\n')
  invisible(original_filenames)
}
