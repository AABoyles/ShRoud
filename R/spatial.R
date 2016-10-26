#' Points to Polygons
#'
#' @param points
#' @param x_offsets
#' @param y_offsets
#' @param dx
#' @param dy
#'
#' @return
#' @export
#'
#' @examples
PointsToPolygons <- function(points, x_offsets, y_offsets, dx = 1, dy = 1){
  grid = data.frame(coordinates(points))
  npoly = length(grid$x)
	ret = lapply(1:npoly, function(i){
	  list(
	    x = grid$x[i] + x_offsets,
		  y = grid$y[i] + y_offsets
	   )
	})
	Srl <- vector(mode="list", length=npoly)
	IDS = paste("ID", 1:npoly, sep="")
	for (i in 1:npoly)
		Srl[[i]] = Polygons(list(Polygon(ret[[i]])), IDS[i])
	res <- SpatialPolygons(Srl, proj4string=CRS(proj4string(points)))
	res
}

#' Points to Squares
#'
#' Takes a SpatialPoints object, draws squares around the points, and returns the SpatialPolygons object.
#'
#' @param points
#'
#' @return
#' @export
#'
#' @examples
PointsToSquares <- function(points){
  colnames(points@coords) <- c("x", "y")
  d <- min(diff(sort(unique(sp::coordinates(points)[,2]))))/2
  x_offsets <- c(-1,  1, 1, -1, -1) * d
  y_offsets <- c(-1, -1, 1,  1, -1) * d
  PointsToPolygons(points, x_offsets, y_offsets)
}

#' Points to Hexagons
#'
#' Takes a SpatialPoints object, draws hexagons around the points, and returns the SpatialPolygons object.
#'
#' @param points
#'
#' @return
#' @export
#'
#' @examples
PointsToHexagons <- function(points){
  dx <- 2 * min(diff(sort(unique(data.frame(sp::coordinates(points))$x))))
  dy <- dx / sqrt(3)
  x_offsets <- c(-dx/2,  0, dx/2,  dx/2,   0, -dx/2, -dx/2)
  y_offsets <- c( dy/2, dy, dy/2, -dy/2, -dy, -dy/2,  dy/2)
  PointsToPolygons(points, x_offsets, y_offsets)
}
