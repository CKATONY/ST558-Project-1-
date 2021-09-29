

library(rmarkdown)
rmarkdown::render(
  input = "Project1.Rmd", 
  output_format = "github_document", 
  output_file = "README.md",
  params = NULL,
  quiet = FALSE
)