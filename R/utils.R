set <- function(on, names, list_of_values) {
  do.call(on$set, setNames(list_of_values, names))
}
