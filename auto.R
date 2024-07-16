
# Intended to run the main Rmd file
# (Rmd files cannot be directly automated/scheduled to be run)

# Define required variables:

# for rmarkdown to function:
Sys.setenv(RSTUDIO_PANDOC = "D:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")
# to ensure correct project dir
proj <- this.path::this.dir()

# Output run start to logfile
cat("Run started: ", as.character(Sys.time()),"\n") 

# Ensure libraries
renv::restore(project = proj)

# Run main script
renv::run(script = {
  rmarkdown::render(
    file.path(proj, "main.Rmd"))},
  project = proj)
