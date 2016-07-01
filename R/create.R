#' @export
Point = function(x, third = "Z") { # PostGIS default: if not specified, 3rd dim is "Z"
	stopifnot(third %in% c("Z", "M"))
	if (length(x) == 2)
		cl = "Point"
	else if (length(x) == 3)
		cl = paste("Point", third)
	else if (length(x) == 4)
		cl = "Point ZM"
	else
		stop(paste(length(x), "is an illegal length for a Point"))
	class(x) = c(cl, "sfi")
	x
}

Mtrx = function(x, third = "Z", type) {
	stopifnot(third %in% c("Z", "M"))
	if (ncol(x) == 2)
		cl = type
	else if (ncol(x) == 3)
		cl = paste(type, third)
	else if (ncol(x) == 4)
		cl = paste(type, "ZM")
	else
		stop(paste(ncol(x), "is an illegal nr of rows for a", type))
	class(x) = c(cl, "sfi")
	x
}
MtrxSet = function(x, third = "Z", type) {
	stopifnot(third %in% c("Z", "M"))
	nc = unique(sapply(x, ncol))
	if (length(nc) != 1)
		stop("matrices having unequal number of columns")
	NotClosed = function(y) head(y, 1) != tail(y, 1)
	if (any(sapply(x, NotClosed)))
		stop("polygons not (all) closed")
	if (nc == 2)
		cl = type
	else if (nc == 3)
		cl = paste(type, third)
	else if (nc == 4)
		cl = paste(type, "ZM")
	else
		stop(paste(ncol(x[[1]]), "is an illegal nr of rows for a", type))
	class(x) = c(cl, "sfi")
	x
}

#' @export
MultiPoint = function(x, third = "Z", ...) Mtrx(x, third, type = "MultiPoint")
#' @export
LineString = function(x, third = "Z", ...) Mtrx(x, third, type = "LineString")
#' @export
Polygon = function(x, third = "Z", ...) MtrxSet(x, third, type = "Polygon")
#' @export
MultiLineString = function(x, third = "Z", ...) MtrxSet(x, third, type = "MultiLineString")

# print helper functions
prnt.Point = function(x, ...) {
	nr = paste0(x, collapse = " ")
	paste0(class(x)[1], "(", nr, ")")
}
prnt.Matrix = function(x, ...)
	paste0("(", paste0(apply(x, 1, paste0, collapse = " "), collapse = ", "), ")")

prnt.MatrixList = function(x, ...)
	paste0("(", paste0(unlist(lapply(x, prnt.Matrix)), collapse = ", "), ")")

prnt.MultiPoint = function(x, ...) paste0(class(x)[1], prnt.Matrix(x, ...))
prnt.LineString = function(x, ...) paste0(class(x)[1], prnt.Matrix(x, ...))
prnt.Polygon = function(x, ...) paste0(class(x)[1], prnt.MatrixList(x, ...))
prnt.MultiLineString = function(x, ...) paste0(class(x)[1], prnt.MatrixList(x, ...))

#' @export
print.sfi = function(x, ...) {
	fn = switch(class(x)[1], 
		"Point" = , "Point Z" = , "Point M" = , "Point ZM" = prnt.Point,
		"MultiPoint" = , "MultiPoint Z" = , "MultiPoint M" = , "MultiPoint ZM" = prnt.MultiPoint,
		"LineString" = , "LineString Z" = , "LineString M" = , "LineString ZM" = prnt.LineString,
		"Polygon" = , "Polygon Z" = , "Polygon M" = , "Polygon ZM" = prnt.Polygon,
		"MultiLineString" = , "MultiLineString Z" = , "MultiLineString M" = , 
			"MultiLineString ZM" = prnt.MultiLineString,
	)
	fn(x, ...)
}
