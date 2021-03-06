## Description

This project generates my master's thesis document. In it, the data is
analyzed with R functions that I wrote. It was generated using the BYU thesis
template created by Dr. Macfarlane based on R Markdown and **bookdown**
(https://github.com/rstudio/bookdown).


## How to Run
A few steps need to be completed before creating the thesis. These steps are
outlined below.

  1. Download the data and put it into the data folder. Data is found at This
  link: (put link here).
  2. Run tar_make() to build the figures and tables used in the document.
  3. Dowload the 'RAM' package using this command: devtools::install_url
  ("https://cran.r-project.org/src/contrib/Archive/RAM/RAM_1.2.1.tar.gz")
  4. Build the book.

## Outputs

This thesis creates two outputs:

  1. A PDF conforming to the BYU thesis style guide.
  2. A `gitbook`, or a book-styled website that renders mathematics beautifully
  and can be easily posted online. There is also a download button at the top
  of each page where people can download the PDF.


To build the documents, press the "Build Book" button in the "Build" pane of
RStudio. Otherwise, you can also compile from the R prompt with

```r
# build website
bookdown::render_book(".", config_file="_bookdown.yml", output_yaml = "_output.yml")
# build PDF
bookdown::render_book(".", "bookdown::pdf_book", config_file="_bookdown.yml")
```
