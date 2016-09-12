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
  df <- sp::coordinates(points)
  sp::SpatialPolygons(
    lapply(1:nrow(df), function(i){
      sp::Polygons(
        list(
          sp::Polygon(
            list(
              x = df[i,1] + x_offsets * dx,
              y = df[i,2] + y_offsets * dy
            )
          ), paste0("ID", i)
        )
      )
    }), proj4string = sp::CRS(sp::proj4string(points))
  )
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
  d <- min(diff(sort(unique(sp::coordinates(points)[,2]))))/2
  xs <- c(-1,  1, 1, -1, -1) * d
  ys <- c(-1, -1, 1,  1, -1) * d
  PointsToPolygons(points, xs, ys)
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
  xs <- c(-dx/2,  0, dx/2,  dx/2,   0, -dx/2, -dx/2)
  ys <- c( dy/2, dy, dy/2. -dy/2, -dy, -dy/2,  dy/2)
  PointsToPolygons(points, xs, ys)
}
