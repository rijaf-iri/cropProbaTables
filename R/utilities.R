
convert_data_type <- function(mat, as.fun){
    dim.mat <- dim(mat)
    mat <- as.fun(mat)
    dim(mat) <- dim.mat
    return(mat)
}

fraction <- function(x, d){
    paste0(round(x * d), "/", d)
}
