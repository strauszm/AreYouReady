#############################
# Michael Strausz
# Rendering book
# 3/30/2023 : 4/19/2023
#############################
library(bookdown)

render_book(input = ".", clean = TRUE,
            envir = parent.frame(),
            output_dir = NULL, new_session = NA, preview = FALSE,
            config_file = "_bookdown.yml",)

options(knitr.graphics.auto_pdf = TRUE)

