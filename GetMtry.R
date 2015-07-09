GetMtry = function(p, length){
  if(p < length){
    mtry = ceiling(sqrt(p))
  }
  else{
    mtry = ceiling(seq(2:(length+1)) * 0.5 * sqrt(p)) 
  }
  return(mtry)
}