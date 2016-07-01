#' verify simple feature 
#' 
#' verifies simple feature list column's contents, and sets class
#' 
#' @param lst list with simple feature objects
#' @param epsg integer; epsg code
#' @param proj4string character; describing the coordinate reference systems in PROJ.4 syntax
#' 
#' @examples
#' pt1 = c(0,1)
#' class(pt1) = "Point"
#' pt2 = c(1,1)
#' class(pt2) = "Point"
#' (sfc = sfc(list(pt1, pt2)))
#' (d = data.frame(a = 1:2, geom = I(sfc)))
#' @export
sfc = function(lst, epsg = -1, proj4string = as.character(NA)) {
	u = unique(sapply(lst, class))
	if (length(u) != 1)
		stop("multiple types not allowed")
	class(lst) = c(u, "sf")
	attr(lst, "epsg") = epsg
	if (missing(proj4string) && epsg > 0)
		proj4string = CRS(paste0("+init=epsg:", epsg))@projargs
	attr(lst, "proj4string") = proj4string
	lst
}

#' @export
"[.sfc" = function(x, i, j, ...) {
    old = x
    x = NextMethod("[")
    attributes(x) = attributes(old)
    class(x) = class(old)
    x
}

#' @export
summary.sf = function(x, maxsum = 7, maxp4s = 10, ...) {
	u = factor(sapply(x, class))
    epsg = paste0("epsg:", attr(x, "epsg"))
	levels(u) = c(levels(u), epsg)
    p4s = attr(x, "proj4string")
	if (!is.na(p4s)) { 
		if (nchar(p4s) > maxp4s)
			p4s = paste0(substr(p4s, 1, maxp4s), "...")
		levels(u) = c(levels(u), p4s)	
	}
    summary(u, maxsum = maxsum, ...)
}

#' create sl_df object
#' 
#' create sl_df, which extends data.frame-like objects with a simple feature list column
#'
#' @examples
#' pt1 = c(0,1)
#' class(pt1) = "Point"
#' pt2 = c(1,1)
#' class(pt2) = "Point"
#' sf(list(pt1, pt2))
#' d = data.frame(a = 1:2)
#' d$geom = sfc(list(pt1, pt2))
#' df = sf_df(d)
#' d$geom2 = sfc(list(pt1, pt2))
#' sf_df(df) # warns
#' @export
sf_df = function(df) {
	sf = sapply(df, function(x) inherits(x, "sf"))
	if (!any(sf))
		stop("no simple features geometry column present")
	sf_column = which(sf)
	if (length(sf_column) > 1)
		warning("more than one geometry column not allowed, picking first")
	attr(df, "sf_column") = which(sf)[1]
	class(df) = c("sf", class(df))
	df
}
