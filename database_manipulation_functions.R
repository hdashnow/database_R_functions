# find and replace are vectors of the same length
find_replace = function(target, find, replace) {
  stopifnot(length(find) == length(replace))
  for (i in 1:length(find)) {
    target[target == find[i]] = replace[i]
  }
  return(target)
}

### Custom merging code

#test merging
# a = manual_genes[43:45,]
# b = manual.results.hgnc[17:19,]
# a
# b
# smart.merge(a, b, key="hgnc_id")

# compare two values and decide which to retain
compare <- function(A, B){
  if(identical(A,B)){
    return(A)
  } else if(is.na(A) | A == ""){
    return(B)
  } else if(is.na(B) | B == ""){
    return(A)
  } else if(A==B){
    return(A)
  } else {
    return(paste(A, B, sep="__"))
  }  
}

merge.cols <- function(col.x, col.y){
  # compare values over a pair of columns of same length
  col.x = as.character(col.x)
  col.y = as.character(col.y)
  index = 1:length(col.x)
  col.xy = sapply(index,
                  function(i){
                    compare(col.x[i], col.y[i])
                  }
  )
  return(col.xy)
}

# x and y are data frames with at least one column in common and sharing one column of unique keys
# key is a string containing the column heading that is shared by both contains unique keys
smart.merge <- function(x, y, key){
  # Merge the two tables by the key column
  merged = merge(x, y, by=key, all=TRUE, suffixes = c(".x",".y"))
  
  # Get a list of all the columns that are shared between the two tables excluding the key column
  x.names = names(x)
  y.names = names(y)
  x.and.y = intersect(x.names, y.names)
  shared.cols = x.and.y[!(x.and.y %in% key)]
  #print(shared.cols)
  for (col in shared.cols) {
    col.x.name = paste(col,".x", sep = "")
    col.y.name = paste(col,".y", sep = "")
    #print(merged[col.x.name])
    col.xy = merge.cols(merged[,col.x.name], merged[,col.y.name])
    all.names = names(merged)
    merged = cbind(merged,col.xy)
    all.names = append(all.names,col)
    names(merged) = all.names
    merged = merged[!(all.names %in% c(col.x.name,col.y.name))]
  }
  
  return(merged)
}