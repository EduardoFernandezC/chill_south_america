check_scenarios <- function(x){
  
  if (all(x[["data"]][["Tmin"]] == 0) |
      all(x[["data"]][["Tmin"]] == 0)){
    
    stop(paste("Scenario", x[[which(names(x) == "labels")]], "seems erroneous. Please check it"),
         call. = FALSE)
    
  }
  
  if (all(x[["data"]][["Tmin"]] >= 30) |
      all(x[["data"]][["Tmin"]] >= 30)){
    
    stop(paste("Scenario", x[[which(names(x) == "labels")]], "seems erroneous. Please check it"),
         call. = FALSE)
    
  }
  
  x
}