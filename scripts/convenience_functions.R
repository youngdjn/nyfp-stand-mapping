### Convenience functions to share across scripts in this repo

# prepend a given path with the path to the data folder
datadir = function(dir) {
  return (paste0(data_dir,dir))
}

modal = function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

deg2rad <- function(deg) {(deg * pi) / (180)}


rad2deg = function(rad) {
  return(rad*180/pi)
}

# Drop the last "/..." (e.g., the file or final folder) from a path
path_drop_last = function(x) {
  elements = strsplit(x,split="/")[[1]]
  elements_but_last = elements[1:(length(elements)-1)] 
  shortened_path = paste(elements_but_last,collapse="/")
  return(shortened_path)
}