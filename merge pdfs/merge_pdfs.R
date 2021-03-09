# README ------------------------------------------------------------------
# TODO: extract whole year as .zip from google drive to your computer (e.g.
# desktop), then set it as working directory

# Setup -------------------------------------------------------------------
library(pdftools)
library(magrittr)
library(EBImage)

wd <- "C:/Users/Philipp/Desktop/2019"
setwd(wd)

# Functions ---------------------------------------------------------------
combine_pdfs_in_subfolder <- function(folder_name) {
  # get file names
  files <- paste0(folder_name, "/Belege") %>% 
    list.files() %>% 
    paste0(paste0(folder_name, "/Belege/"), .)
  # replace ä,ö,ü
  files_cleaned <- stringi::stri_replace_all_fixed(
    files, 
    c("ä", "ö", "ü", "Ä", "Ö", "Ü", "ß"), 
    c("ae", "oe", "ue", "Ae", "Oe", "Ue", "ss"), 
    vectorize_all = FALSE
  )
  # rename files
  file.rename(files, files_cleaned)
  files <- files_cleaned
  
  if(length(files[which(!stringr::str_detect(files, ".pdf"))]) > 0) {
    print(paste0(">>> ", folder_name, ' <<< contains files that are not in pdf-format.'))
    {convert_photos <- readline(prompt = "Do you want to convert all photos (png/jpeg/tiff) to pdf? [y/N]")}
    if(convert_photos == "y") {
      for(x in files[which(!stringr::str_detect(files, ".pdf"))]) {
        file_pth <- x
        save_pth <- sub("\\.[[:alnum:]]+$", ".pdf", x)
        convert_images_to_pdf(filepath = file_pth, savepath = save_pth)
      }
    }
  } 
  
  files_with_converted <- paste0(folder_name, "/Belege") %>% 
    list.files() %>% 
    paste0(paste0(folder_name, "/Belege/"), .)
  only_pdfs <- files_with_converted[which(stringr::str_detect(files_with_converted, ".pdf"))]
  pdftools::pdf_combine(only_pdfs, output = paste0(folder_name, "_combined.pdf"))
  print(paste(folder_name, "done!"))
}

convert_images_to_pdf <- function(filepath, savepath) {
  i <- autoscale_image(readImage(filepath))
  pdf(savepath)
  plot(i)
  dev.off()
}

autoscale_image <- function(im, landscape_format = FALSE, short_side_length = 720) {
  width  <- dim(im)[1]
  height <- dim(im)[2]
  if(landscape_format == FALSE) {
    if(width > height) {
      im <- rotate(im, 90)
    }
    im <- resize(im, w = short_side_length)
  } else {
    if(width < height) {
      im <- rotate(im, 90)
    }
    im <- resize(im, h = short_side_length)
  }
}

# Script ------------------------------------------------------------------
file.rename(list.files(),
            list.files() %>%
              stringi::stri_replace_all_fixed(
                . , 
                c("ä", "ö", "ü", "Ä", "Ö", "Ü", "ß"), 
                c("ae", "oe", "ue", "Ae", "Oe", "Ue", "ss"), 
                vectorize_all = FALSE
              ))

for (sub_dir in list.files()) {
  combine_pdfs_in_subfolder(sub_dir)
}
