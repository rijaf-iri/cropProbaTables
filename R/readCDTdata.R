
#' Read CDT station format file.
#'
#' Read CDT station format file.
#' 
#' @param file Full path to the file containing the CDT data
#' @param sep The column's separator of the data
#' @param missing The missing values flag
#'  
#' @return A list object
#' \itemize{
#'   \item{\strong{id}: }{Vector of the points/stations id}
#'   \item{\strong{lon}: }{Vector of the points/stations longitude}
#'   \item{\strong{lat}: }{Vector of the points/stations latitude}
#'   \item{\strong{dates}: }{Vector of the dates or times of the data}
#'   \item{\strong{data}: }{Matrix of the data, row indicates the dates and column the stations}
#' }
#' 
#' @export

readCDTStationData <- function(file, sep = ",", missing = "-99"){
    donne <- utils::read.table(file, sep = sep, na.strings = missing,
                               colClasses = "character", stringsAsFactors = FALSE)

    ###############
    seph <- rle(!grepl('[^[:digit:]]', as.character(donne[, 1])))
    ipos <- which(!seph$values & seph$lengths >= 3 & seph$lengths <= 4)
    if(length(ipos) == 0 | ipos[1] != 1)
        stop("Station data is not in a standard CDT format")

    pos <- seph$lengths[ipos[1]]

    ###############
    dat <- list(id = as.character(donne[1, -1]),
                lon = as.numeric(donne[2, -1]),
                lat = as.numeric(donne[3, -1]),
                elv = if(pos == 4) as.numeric(donne[4, -1]) else NULL,
                dates = as.character(donne[-(1:pos), 1]),
                data = local({
                            tmp <- donne[-(1:pos), -1, drop = FALSE]
                            ntmp <- dim(tmp)
                            tmp <- as.numeric(unlist(tmp))
                            dim(tmp) <- ntmp
                            tmp
                    })
                )
    dimnames(dat$data)[[2]] <- NULL
    dat$data <- convert_data_type(dat$data, as.numeric)

    return(dat)
}

#' Match CDT stations data.
#'
#' Filter CDT stations data to match the stations and dates.
#' 
#' @param ... CDT stations data objects, possibly named, output from \code{readCDTStationData}.
#' 
#' @return A list of CDT stations data objects, in same order as provided in the input arguments.
#' A named list if input arguments are named.
#' 
#' @export

matchCDTStationsData <- function(...){
    tmp <- matchCDTStationsIDs(...)
    do.call(matchCDTStationsDates, tmp)
}

#' Match CDT stations data IDs.
#'
#' Filter CDT stations data to match the stations.
#' 
#' @param ... CDT stations data objects, possibly named, output from \code{readCDTStationData}.
#' 
#' @return A list of CDT stations data objects, in same order as provided in the input arguments.
#' A named list if input arguments are named.
#' 
#' @export

matchCDTStationsIDs <- function(...){
    x <- list(...)
    if(nargs() == 1){
        cat("Nothing to do.\n")
        return(x)
    }

    id <- Reduce(intersect, lapply(x, "[[", "id"))
    if(length(id) == 0)
        stop("Stations do not overlap")

    lapply(x, function(l){
        ix <- match(id, l$id)
        l$id <- l$id[ix]
        l$lon <- l$lon[ix]
        l$lat <- l$lat[ix]
        l$elv <- l$elv[ix]
        l$data <- l$data[, ix, drop = FALSE]
        l
    })
}

#' Match CDT stations data dates.
#'
#' Filter CDT stations data to match the dates.
#' 
#' @param ... CDT stations data objects, possibly named, output from \code{readCDTStationData}.
#' 
#' @return A list of CDT stations data objects, in same order as provided in the input arguments.
#' A named list if input arguments are named.
#' 
#' @export

matchCDTStationsDates <- function(...){
    x <- list(...)
    if(nargs() == 1){
        cat("Nothing to do.\n")
        return(x)
    }

    it <- Reduce(intersect, lapply(x, "[[", "dates"))
    if(length(it) == 0)
        stop("Dates do not overlap")

    it <- it[order(it)]
    lapply(x, function(l){
        ix <- match(it, l$dates)
        l$dates <- l$dates[ix]
        l$data <- l$data[ix, , drop = FALSE]
        l
    })
}
