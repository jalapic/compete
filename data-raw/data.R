mouse <- read.csv("data-raw/mouse.csv",
                           stringsAsFactors = FALSE)
bonobos <- read.csv("data-raw/bonobos.csv",
                  stringsAsFactors = FALSE)
devtools::use_data(mouse, bonobos, overwrite = TRUE)
