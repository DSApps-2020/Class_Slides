library(tidyverse)
library(pagedown)

# this should be run from parent Class_Slides directory
unit_dir <- "u0_d01-intro/"
html_filename <- str_c(str_sub(unit_dir, 1, str_length(unit_dir) - 1), ".html")

# copy files from parent directory to unit directory
copy_to_unit_dir <- function(file_name) file.copy(file_name, unit_dir)

files_to_copy <- c(
  "slides.css",
  "header.html",
  "DSApps_logo.jpg",
  "DSApps_logo_white.jpg"
)

walk(files_to_copy, copy_to_unit_dir)

# copy the libs directory to unit directory
file.copy("libs", unit_dir, recursive = TRUE)

# knit to pdf
chrome_print(str_c(unit_dir, html_filename))

# remove files
remove_from_unit_dir <- function(file_name) file.remove(str_c(unit_dir, "/", file_name))

walk(files_to_copy, remove_from_unit_dir)

# remove libs local folder
unlink(str_c(unit_dir, "/libs"), recursive = TRUE)
