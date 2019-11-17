# read_pdf.r

library(pdftools)
library(stringr)

# Function to create title from one file
suggest_title <- function(filename, start_word=1, n_words=14) {
  pdf_text <- paste(pdftools::pdf_text(filename), collapse='_')
  pdf_text <- trimws(gsub('[[:punct:] ]+',' ', pdf_text))
  pdf_text <- gsub('( [0-9] )', ' ', pdf_text)
  pdf_text <- gsub('\\r|\\n', ' ', pdf_text)
  pdf_text <- gsub('[[:space:] ]+',' ', pdf_text)
  first_words <- stringr::word(pdf_text, start=start_word, end=n_words)
  first_words
}

suggest_title('test_files/18-0409.pdf')
suggest_title('test_files/1-s2.0-S1473309916303863-main.pdf')
suggest_title('test_files/10.1038@s41579-018-0115-z.pdf')
suggest_title('test_files/769.full.pdf', 32, 50)

# TODO
# Shiny app
# Upload files
# Widget to define title for each
# Rename files https://stackoverflow.com/questions/10758965/how-do-i-rename-files-using-r
# Download files