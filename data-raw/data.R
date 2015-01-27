mouse <- read.csv("data-raw/mouse.csv",
                           stringsAsFactors = FALSE)
bonobos <- read.csv("data-raw/bonobos.csv",
                  stringsAsFactors = FALSE)
people <- read.csv("data-raw/people.csv",
                    stringsAsFactors = FALSE)
devtools::use_data(mouse, bonobos, people, overwrite = TRUE)
