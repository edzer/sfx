
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
#' Return bounding of a simple feature or simple feature set
#'
#' Return bounding of a simple feature or simple feature set
#' @param x object to compute the bounding box from
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
bbox.sfc = function(x) {
	switch(attr(x, "type"),
		"POINT" = , "POINT Z" = , "POINT M" = , "POINT ZM" = bbox.Mtrx(do.call(rbind, x)),
		"MULTIPOINT" = , "MULTIPOINT Z" = , "MULTIPOINT M" = , "MULTIPOINT ZM" = bbox.MtrxSet(x),
		"LINESTRING" = , "LINESTRING Z" = , "LINESTRING M" = , "LINESTRING ZM" = bbox.MtrxSet(x),
		"POLYGON" = , "POLYGON Z" = , "POLYGON M" = , "POLYGON ZM" = bbox.MtrxSetSet(x),
		"MULTILINESTRING" = , "MULTILINESTRING Z" = , "MULTILINESTRING M" = , 
			"MULTILINESTRING ZM" = bbox.MtrxSetSet(x),
		"MULTIPOLYGON" = , "MULTIPOLYGON Z" = , "MULTIPOLYGON M" = , 
			"MULTIPOLYGON ZM" = bbox.MtrxSetSetSet(x),
		"GEOMETRYCOLLECTION" = , "GEOMETRYCOLLECTION Z" = , "GEOMETRYCOLLECTION M" = , 
			"GEOMETRYCOLLECTION ZM" = { 
				s = sapply(x, bbox)
				c(xmin = min(s[1,]), xmax = max(s[2,]), ymin = min(s[3,]), ymax = max(s[4,]))
			},
		stop("simple feature type not supported")
	)
}
