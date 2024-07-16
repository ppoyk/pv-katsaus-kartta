# Tämä skripti on luotu automatisoimaan pohjavesikuvaajien ajo.


stop("This file is for interactive use only")

task_name <- "pv-kuvaajat"

# To delete task:
# (Must be run before redefining / re-creating the task)
taskscheduleR::taskscheduler_delete(task_name)


# Define task
taskscheduleR::taskscheduler_create(
  taskname = task_name,
  rscript = file.path(this.path::this.dir(), "auto.R"),
  # Output folder set to specific network share folder by "verkkolevytallennus"
  # Prev result files are wiped from (selected) results-folder by "clean_output"
  rscript_args = c("verkkolevytallennus","clean_output"),
  schedule = "WEEKLY", days = "TUE",
  starttime = "08:15",
  startdate = format(Sys.time(), "%d.%m.%Y"),
  debug = TRUE)


# To stop the automated schedule:
taskscheduleR::taskscheduler_stop(task_name)

# To run task now
taskscheduleR::taskscheduler_runnow(task_name)
