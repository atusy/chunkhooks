set <- function(on, names, list_of_values) {
  do.call(on$set, stats::setNames(list_of_values, names))
}
