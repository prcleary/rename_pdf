# read_pdf.r

sanitise_filename <- function(x) {
  x <- gsub('[[:punct:] ]+', ' ', x)
  x <- gsub('\\r|\\n', ' ', x)
  x <- trimws(gsub('[[:space:] ]+', ' ', x))
  x
}

n_words <- 20
cat('\n')
original_filenames <-
  list.files('test_files/', pattern = '\\.pdf$|\\.PDF$', full.names = TRUE)
for (o in original_filenames) {
  o_dirname <- dirname(o)
  o_basename <- basename(o)
  cat('Original filename: ', o_basename, '\n')
  o_title <- pdftools::pdf_info(o)$keys$Title
  if (is.null(o_title)) {
    raw_text <- paste0(pdftools::pdf_text(o), collapse = ' ')
    raw_text_sane <- sanitise_filename(raw_text) 
    o_title_sane <- stringr::word(raw_text_sane, 1, n_words)
  } else  if (o_title %in% '') {
    raw_text <- paste0(pdftools::pdf_text(o), collapse = ' ')
    raw_text_sane <- sanitise_filename(raw_text) 
    o_title_sane <- stringr::word(raw_text_sane, 1, 20)
  } else { 
    o_title_sane <- sanitise_filename(o_title)
  }
  cat('Suggested title: ', o_title_sane, '\n')
  decision <- readline('Change to suggested title? (y/n): ')
  if (decision %in% 'y') {
    o_newname <-
      file.path(o_dirname, paste0(o_title_sane, '.pdf', collapse = NULL))
    file.rename(o, o_newname)
    cat('File renamed to: ', o_newname, '\n')
    cat('\n')
  } else {
    cat('File name not changed\n')
    cat('\n')
  }
}

# TODO
# Shiny app
# Upload files
# Widget to define title for each
# Rename files https://stackoverflow.com/questions/10758965/how-do-i-rename-files-using-r
# Download files