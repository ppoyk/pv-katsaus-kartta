# Function to check wether is a "Date" object

is_date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}
