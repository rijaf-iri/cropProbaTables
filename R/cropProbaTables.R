#' Crop probability tables.
#'
#' Compute the crop probability tables.
#' 
#' @param onset A CDT stations data object containing the onset data in days.
#'  Output from CDT menu \strong{Season Onset}, the file \strong{Onset_days.txt}
#' @param cessation A CDT stations data object containing the onset data in days.
#'  Output from CDT menu \strong{Season Cessation}, the file \strong{Cessation_days.txt}
#' @param precip_daily A CDT stations data object containing the daily precipitation.
#' @param planting_dates A vector containing the planting dates, date format \code{"dd/mm"}.
#'  Must be in increasing order, e.g: c("10/11", "20/11", "01/12", "10/12"). For southern hemisphere, 
#'  the planting dates can continue to the next year, e.g: c("01/12", "10/12", "20/01", "01/02").
#' @param crop_water_requirements A vector containing the crop water requirement.
#' @param crop_growing_season_lengths A vector containing the crop growing season lengths.
#' @param minimum_fraction Minimum fraction of non-missing daily precipitation for each season.
#'  If the fraction of non-missing data for one season is less than this number, 
#'  the total of rainfall for that season will be missing.
#'  
#' @return A list object
#' \itemize{
#'   \item{\strong{corp_proba}: }{
#'     A named list containing the crop probability. The names of the list are the growing season lengths.
#'     Each element of the list contains a \code{data.frame}, the 1st column indicates the growing season lengths,
#'     the 2nd column for the planting date, the 3rd column for the water requirements 
#'     and the next columns contain the probabilities for the stations
#'    }
#'   \item{\strong{season_proba}: }{
#'      A data.frame containing the probability of rainy season start on or before the panting dates.
#'   }
#'   \item{\strong{stations}: }{
#'     A list containing the stations coordinates
#'     \itemize{
#'       \item{id: }{Vector of the stations id}
#'       \item{lon: }{Vector of the stations longitude}
#'       \item{lat: }{Vector of the stations latitude}
#'       \item{year: }{Vector of the start year for the season}
#'     }
#'   }
#'  \item{\strong{planting_data}: }{
#'    A named list containing the data for each planting date. The names of the list are the planting dates.
#'    Each element of the list contains a list of matrix of the amount of precipitation and the length of the season 
#'    starting from the planting date up to the cessation date. The row indicates the season and column the stations.
#'   }
#'  \item{\strong{seasonal_data}: }{
#'    A named list containing the seasonal data.
#'    \itemize{
#'      \item{season: }{Vector of the season}
#'      \item{origin: }{Vector origin date for the onset and cessation}
#'      \item{onset: }{Matrix of the onset data, row indicates the season and column the stations}
#'      \item{cessation: }{Matrix of the cessation data, same dimension as onset}
#'      \item{length: }{Matrix of the length of season, same dimension as onset}
#'      \item{amount: }{Matrix of the seasonal rainfall, same dimension as onset}
#'    }
#'   }
#' }
#' 
#' @examples
#' \dontrun{
#' 
#' library(cropProbaTables)
#' 
#' # read daily precipitation data
#' precip_file <- "D:/DATA/DGM2021/precip_daily_1981-2020.csv"
#' precip_daily <- readCDTStationData(precip_file, sep = ",", missing = "-99")
#' 
#' # read onset data
#' onset_file <- "D:/DATA/DGM2021/ONSET_data/CDTSTATIONS/Onset_days.txt"
#' onset <- readCDTStationData(onset_file, sep = " ", missing = "-99")
#' 
#' # read cessation data
#' cessation_file <- "D:/DATA/DGM2021/CESSATION_data/CDTSTATIONS/Cessation_days.txt"
#' cessation <- readCDTStationData(cessation_file, sep = " ", missing = "-99")
#' 
#' out <- cropProbaTables(
#'             onset, cessation, precip_daily,
#'             planting_dates = c("10/11", "20/11", "01/12", "10/12"),
#'             crop_water_requirements = c(300, 400, 500),
#'             crop_growing_season_lengths = c(90, 100, 110, 120),
#'             minimum_fraction = 0.95
#'           )
#' 
#' output_directory <- "D:/DATA/DGM2021/CROP_PROBA"
#' writeProbaTables(out$corp_proba, output_directory)
#' 
#' output_file <- "D:/DATA/DGM2021/CROP_PROBA/Season_start_proba_table.csv"
#' writeSeasonProba(out$season_proba, output_file)
#' 
#' }
#' 
#' @export

cropProbaTables <- function(onset, cessation, precip_daily,
                            planting_dates,
                            crop_water_requirements,
                            crop_growing_season_lengths,
                            minimum_fraction = 0.95
                           )
{
    don <- matchCDTStationsIDs(prc = precip_daily, deb = onset, fin = cessation)

    deb_date <- as.Date(don$deb$dates, "%Y%m%d")
    fin_date <- as.Date(don$fin$dates, "%Y%m%d")

    start_mon <- as.Date(paste0(2019, format(deb_date[1], "-%m-%d")))
    end_mon <- as.Date(paste0(2019, format(fin_date[1], "-%m-%d")))
    seas_year <- if(start_mon < end_mon) "one" else "two"

    ######
    deb_data <- lapply(seq_along(deb_date), function(j){
        x <- deb_date[j] + don$deb$data[j, ]
        s <- as.Date(format(deb_date[j], "%Y-01-01"))
        x <- as.numeric(difftime(x, s, units = "days"))
        list(origin = s, data = x)
    })
    deb_origin <- do.call(c, lapply(deb_data, "[[", "origin"))
    deb_data <- do.call(rbind, lapply(deb_data, "[[", "data"))
    deb_year <- as.numeric(format(deb_origin, "%Y"))
    deb_seas <- switch(seas_year,
                       "one" = paste0(deb_year, "_", deb_year),
                       "two" = paste0(deb_year, "_", deb_year + 1)
                      )
    deb_data <- list(dates = deb_seas, data = deb_data)

    ######
    fin_data <- lapply(seq_along(fin_date), function(j){
        x <- fin_date[j] + don$fin$data[j, ]
        yr <- as.numeric(format(fin_date[j], "%Y"))
        if(seas_year == "two") yr <- yr - 1
        s <- as.Date(paste0(yr, "-01-01"))
        x <- as.numeric(difftime(x, s, units = "days"))
        list(origin = s, data = x)
    })
    fin_origin <- do.call(c, lapply(fin_data, "[[", "origin"))
    fin_data <- do.call(rbind, lapply(fin_data, "[[", "data"))
    fin_year <- as.numeric(format(fin_origin, "%Y"))
    fin_seas <- switch(seas_year,
                       "one" = paste0(fin_year, "_", fin_year),
                       "two" = paste0(fin_year, "_", fin_year + 1)
                      )
    fin_data <- list(dates = fin_seas, data = fin_data)

    ######
    seas <- matchCDTStationsDates(deb = deb_data, fin = fin_data)
    year1 <- as.numeric(substr(seas$deb$dates, 1, 4))
    year2 <- as.numeric(substr(seas$deb$dates, 6, 9))

    ######
    coords <- list(id = don$prc$id, lon = don$prc$lon,
                   lat = don$prc$lat, year = year1)

    ######
    data <- list(season = seas$deb$dates)
    data$origin <- paste0(year1, "-01-01")
    data$onset <- seas$deb$data
    data$cessation <- seas$fin$data
    data$length <- seas$fin$data - seas$deb$data + 1
    data$amount <- array(NA, dim(seas$deb$data))

    daty <- as.Date(don$prc$dates, "%Y%m%d")
    for(i in 1:nrow(data$amount)){
        for(j in 1:ncol(data$amount)){
            s1 <- as.Date(data$onset[i, j], origin = data$origin[i])
            if(is.na(s1)) next
            s2 <- as.Date(data$cessation[i, j], origin = data$origin[i])
            if(is.na(s2)) next
            ns <- minimum_fraction * data$length[i, j]
            if(is.na(ns)) next
            ix <- which(daty >= s1 & daty <= s2)
            if(length(ix) < ns) next
            rr <- don$prc$data[ix, j]
            if(sum(!is.na(rr)) < ns) next
            data$amount[i, j] <- sum(rr, na.rm = TRUE)
        }
    }

    ######
    cdaty <- as.Date(paste0("2019/", planting_dates), "%Y/%d/%m")
    if(any(is.na(cdaty)))
         stop("Wrong planting dates format")

    pdaty <- rep('y1', length(cdaty))
    if(is.unsorted(cdaty)){
        is <- cdaty >= cdaty[1]
        pdaty[!is] <- 'y2'
    }
    p_dates <- format(cdaty, "%d-%b")

    data_pdate <- lapply(seq_along(pdaty), function(p){
        prc <- array(NA, dim(data$onset))
        len <- prc

        for(i in 1:nrow(prc)){
            for(j in 1:ncol(prc)){
                yr <- if(pdaty[p] == "y1") year1[i] else year2[i]
                s1 <- paste0(yr, "/", planting_dates[p])
                s1 <- as.Date(s1, "%Y/%d/%m")
                s2 <- as.Date(data$cessation[i, j], origin = data$origin[i])
                if(is.na(s2)) next
                len[i, j] <- as.numeric(s2 - s1 + 1)
                ns <- minimum_fraction * len[i, j]
                ix <- which(daty >= s1 & daty <= s2)
                if(length(ix) < ns) next
                rr <- don$prc$data[ix, j]
                if(sum(!is.na(rr)) < ns) next
                prc[i, j] <- sum(rr, na.rm = TRUE)
            }
        }

        list(amount = prc, length = len)
    })
    names(data_pdate) <- p_dates

    ######
    sprobs <- lapply(seq_along(pdaty), function(p){
            yr <- if(pdaty[p] == "y1") year1 else year2
            s1 <- paste0(yr, "/", planting_dates[p])
            s1 <- as.Date(s1, "%Y/%d/%m")
            pdates <- difftime(s1, as.Date(data$origin), units = "days")
            cond <- sweep(data$onset, 1, pdates, FUN = "<=")
            pr <- colMeans(cond, na.rm = TRUE)
            round(pr, 4)
        })
    sprobs <- do.call(rbind, sprobs)
    sprobs <- data.frame(p_dates, sprobs)
    names(sprobs) <- c('PDates', coords$id)

    ######
    probs <- lapply(crop_growing_season_lengths, function(cl){
        clen <- data.frame(Clength = cl)
        out <- lapply(seq_along(data_pdate), function(i){
            x <- data_pdate[[i]]
            pd <- data.frame(PDates = p_dates[i])
            CW <- lapply(crop_water_requirements, function(cw){
                cond <- x$amount >= cw & x$length >= cl
                pr <- colMeans(cond, na.rm = TRUE)
                pr <- matrix(round(pr, 4), nrow = 1)
                pr <- data.frame(cw, pr)
                names(pr) <- c('CWR', coords$id)
                pr
            })
            CW <- do.call(rbind, CW)
            cbind(pd, CW)
        })
        out <- do.call(rbind, out)
        cbind(clen, out)
    })
    names(probs) <- crop_growing_season_lengths

    list(corp_proba = probs,
         season_proba = sprobs,
         stations = coords,
         planting_data = data_pdate,
         seasonal_data = data
        )
}


#' Write crop probability tables.
#'
#' Write crop probability tables to CSV file.
#' 
#' @param crop_proba The crop probability computed from \code{cropProbaTables}.
#' @param output_directory Folder to save the output.
#' 
#' @export

writeProbaTables <- function(crop_proba, output_directory)
{
    proba <- lapply(crop_proba, function(x){
        idstn <- names(x)[-(1:3)]
        tab_stn <- lapply(idstn, function(s){
            y <- x[, c('PDates', 'CWR', s)]
            names(y) <- c('d', 'w', 's')
            y$s <- fraction(y$s, 10)
            d <- unique(y$d)
            w <- unique(y$w)
            tb <- reshape2::acast(y, w~d, value.var = 's')
            tb[, d]
        })
        names(tab_stn) <- idstn
        tab_stn
    })

    idstn <- names(proba[[1]])
    proba <- lapply(idstn, function(s){
        x <- lapply(proba, '[', s)
        lapply(x, '[[', 1)
    })

    for(s in seq_along(idstn)){
        x <- proba[[s]]
        d <- names(x)
        out <- lapply(seq_along(d), function(i){
            y <- x[[i]]
            y0 <- paste('', y)
            dim(y0) <- dim(y)
            rbind(
                c(paste(d[i], 'day crop'), 
                  "Chance of receiving the water requirement in the days to maturity after this date",
                  rep(NA, ncol(y) - 1)),
                c('Water requiremen t (mm)', dimnames(y)[[2]]),
                cbind(dimnames(y)[[1]], y0),
                NA
            )
        })

        out <- do.call(rbind, out)

        out_file <- file.path(output_directory, paste0('crop_proba_', idstn[s], '.csv'))
        utils::write.table(out, out_file, sep = ",", na = "", quote = FALSE,
                            row.names = FALSE, col.names = FALSE)
    }

    invisible()
}

#' Write rainy season start probability.
#'
#' Write rainy season start probability table to CSV file.
#' 
#' @param season_proba The rainy season start probability computed from \code{cropProbaTables}.
#' @param output_file The full path to the file to save the table.
#' 
#' @export

writeSeasonProba <- function(season_proba, output_file){
    pd <- season_proba$PDates
    pr <- as.matrix(season_proba[, -1])
    pr0 <- fraction(pr, 10)
    pr0 <- paste('', pr0, rep())
    dim(pr0) <- dim(pr)
    pr <- cbind(names(season_proba[, -1]), t(pr0))
    out <- rbind(c('', "Chance of season start on or before this date",
                    rep('', length(pd) - 1)),
                 c('Stations', pd), pr)

    utils::write.table(out, output_file, sep = ",", na = "",
                       quote = FALSE, row.names = FALSE,
                       col.names = FALSE)
    invisible()
}
