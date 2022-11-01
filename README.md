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
  3. Download the WRS package running the following commands (https://www.r-bloggers.com/2013/04/installation-of-wrs-package-wilcox-robust-statistics/)
      * install.packages(c("MASS", "akima", "robustbase"))
      * install.packages(c("cobs", "robust", "mgcv", "scatterplot3d", "quantreg", "rrcov", "lars", "pwr", "trimcluster", "parallel", "mc2d", "psych", "Rfit"))
      *  install.packages("WRS", repos="http://R-Forge.R-project.org", type="source")
  4. Build the book.

As a note, some of the targets (especially `events_list`) are large and occupy
a fair amount of memory. You will likely need to increase the amount of virtual
memory available to R by placing the following line in your `.Renviron` file:
```
R_MAX_VSIZE=64000000000
```
It seems to be possible to set this value beyond the physical limits of your machine
(e.g., the 64Gb memory was used successfully on a MacBook Pro with only 32Gb of installed memory.)

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
