#' read simple features from file
#' 
#' read simple features from file, using GDAL and rgdal2
#'
#' @export
#' @examples
#' f = system.file("example-data/continents", package = "rgdal2")
#' x = read_sf(f)
#' x
read_sf = function(f) {
	if (!is.character(f))
		stop("file name (character) needed")
	l = openOGRLayer(f)
	lapply(getGeometries(l), getPoints)
}
