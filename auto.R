
# Intended to run the main Rmd file
# (Rmd files cannot be directly automated/scheduled to be run)

# Force install essential bootstrap packages
pkgs <- c("this.path", "rmarkdown", "renv")
for (p in pkgs[!(pkgs %in% installed.packages())]) utils::install.packages(p)

# Define required variables:
# ...for rmarkdown to function:
if (is.null(Sys.getenv("RSTUDIO_PANDOC")) || length(Sys.getenv("RSTUDIO_PANDOC")) == 0)
  Sys.setenv(RSTUDIO_PANDOC = rmarkdown::find_pandoc()$dir)
# ...to ensure correct project dir:
proj <- this.path::this.dir()


# Output run start to a logfile
cat("Run started: ", as.character(Sys.time()),"\n") 

# Ensure libraries
renv::restore(project = proj)

# Run main script
renv::run(script = {
  rmarkdown::render(
    file.path(proj, "main.Rmd"))},
  project = proj)
