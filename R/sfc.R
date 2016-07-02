# create sfc: a list column with simple feature items (sfi objects)

#' @export
sfc = function(lst) {
	stopifnot(is.list(lst))
	type = checkTypes(lst)
	class(lst) = "sfc"
	attr(lst, "bbox") = bbox(lst)
	lst
}

checkTypes = function(lst) { # breaks on errors, or returns the unique class
	sfi = sapply(lst, function(x) inherits(x, "sfi"))
	if (any(!sfi))
		stop(paste("list item", which(i)[1], "is not of class sfi"))
	cls = unique(sapply(lst, function(x) class(x)[1]))
	if (length(cls) > 1)
		stop("multiple simple feature types not allowed in a simple feature list column")
	cls
}

bbox.Mtrx = function(x) {
	mn = apply(x, 2, min)
	mx = apply(x, 2, max)
	c(xmin = mn[1], xmax = mx[1], ymin = mn[2], ymax = mx[2])
}
bbox.MtrxSet = function(x) {
	s = sapply(x, bbox.Mtrx)
	c(xmin = min(s[1,]), xmax = max(s[2,]), ymin = min(s[3,]), ymax = max(s[4,]))
}
bbox.MtrxSetSet = function(x) {
	s = sapply(x, bbox.MtrxSet)
	c(xmin = min(s[1,]), xmax = max(s[2,]), ymin = min(s[3,]), ymax = max(s[4,]))
}
bbox.MtrxSetSetSet = function(x) {
	s = sapply(x, bbox.MtrxSetSet)
	c(xmin = min(s[1,]), xmax = max(s[2,]), ymin = min(s[3,]), ymax = max(s[4,]))
}
#' @export
bbox = function(x) UseMethod("bbox")

#' @export
bbox.POINT = function(x) c(xmin = x[1], xmax = x[1], ymin = x[2], ymax = x[2])
#' @export
bbox.MULTIPOINT = bbox.Mtrx
#' @export
bbox.LINESTRING = bbox.Mtrx
#' @export
bbox.POLYGON = bbox.MtrxSet
#' @export
bbox.MULTILINESTRING = bbox.MtrxSet
#' @export
bbox.MULTIPOLYGON = bbox.MtrxSetSet
#' @export
bbox.GEOMETRYCOLLECTION = function(x) {
	s = sapply(x, bbox) # dispatch on class
	c(xmin = min(s[1,]), xmax = max(s[2,]), ymin = min(s[3,]), ymax = max(s[4,]))
}

#' @export
bbox.sfc = function(lst) {
	switch(attr(lst, "type"),
		"POINT" = , "POINT Z" = , "POINT M" = , "POINT ZM" = bbox.Mtrx(do.call(rbind, lst)),
		"MULTIPOINT" = , "MULTIPOINT Z" = , "MULTIPOINT M" = , "MULTIPOINT ZM" = bbox.MtrxSet(lst),
		"LINESTRING" = , "LINESTRING Z" = , "LINESTRING M" = , "LINESTRING ZM" = bbox.MtrxSet(lst),
		"POLYGON" = , "POLYGON Z" = , "POLYGON M" = , "POLYGON ZM" = bbox.MtrxSetSet(lst),
		"MULTILINESTRING" = , "MULTILINESTRING Z" = , "MULTILINESTRING M" = , 
			"MULTILINESTRING ZM" = bbox.MtrxSetSet(lst),
		"MULTIPOLYGON" = , "MULTIPOLYGON Z" = , "MULTIPOLYGON M" = , 
			"MULTIPOLYGON ZM" = bbox.MtrxSetSetSet(lst),
		"GEOMETRYCOLLECTION" = , "GEOMETRYCOLLECTION Z" = , "GEOMETRYCOLLECTION M" = , 
			"GEOMETRYCOLLECTION ZM" = { 
				s = sapply(x, bbox)
				c(xmin = min(s[1,]), xmax = max(s[2,]), ymin = min(s[3,]), ymax = max(s[4,]))
			},
		stop("simple feature type not supported")
	)
}
