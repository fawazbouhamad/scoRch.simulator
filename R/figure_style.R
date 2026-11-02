# Style shared with compound-heatwave-typologies-SCORCH.
# Snapshot headings use the artwork colours; statistical plots retain the
# separate four-colour palette used by reference Figures 8-10.
SNAPSHOT_TYPE_COLOURS <- c("#FF0000", "#FF7401", "#FFBC01", "#7030A0")

figure_font <- function(name) {
  if (identical(name, "ellipse_geometry_by_type")) "serif" else figure_sans_font()
}

figure_sans_font <- function() {
  # Base PDF/PostScript devices use registered font families; system font
  # names such as Arial belong to the bitmap and screen devices.
  if (names(grDevices::dev.cur()) %in% c("pdf", "postscript")) "sans" else "Arial"
}

figure_bitmap_type <- function() {
  # Native Quartz works without XQuartz. macOS can report compiled-in Cairo
  # support even when its runtime X11 libraries are not installed.
  if (identical(Sys.info()[["sysname"]], "Darwin") && capabilities("aqua")) return("quartz")
  if (capabilities("cairo")) "cairo" else getOption("bitmapType")
}

panel_label <- function(letter, adj = 0.5) {
  graphics::mtext(paste0("(", letter, ")"), side = 3, line = 0.5,
                   adj = adj, font = 2, cex = 17 / 12, family = figure_sans_font())
}

row_label <- function(letter) {
  graphics::par(mai = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::text(0.5, 0.5, paste0("(", letter, ")"), font = 2,
                  cex = 17 / 12 / graphics::par("cex"), family = figure_sans_font())
}

# Clip vertical and horizontal mesh lines to the ellipse polygon. The fine
# mesh follows the quarter-cell mesh of the reference snapshot figures.
draw_ellipse_mesh <- function(ellipse, grid) {
  polygon <- ellipse_outline(ellipse)
  a <- polygon[-nrow(polygon), , drop = FALSE]
  b <- polygon[-1, , drop = FALSE]
  usr <- graphics::par("usr")
  fine_width <- max(0.1, graphics::par("pin")[1] * 96 /
                      (usr[2] - usr[1]) * grid$lon_step * 4 / 55.897)
  segment <- function(value, axis, colour, width) {
    other <- 3L - axis
    crossed <- (a[, axis] <= value & b[, axis] > value) |
               (b[, axis] <= value & a[, axis] > value)
    if (sum(crossed) < 2) return(invisible())
    ends <- a[crossed, other] + (value - a[crossed, axis]) *
      (b[crossed, other] - a[crossed, other]) /
      (b[crossed, axis] - a[crossed, axis])
    ends <- range(ends)
    if (axis == 1L) graphics::segments(value, ends[1], value, ends[2], col = colour, lwd = width)
    else graphics::segments(ends[1], value, ends[2], value, col = colour, lwd = width)
  }
  for (axis in 1:2) {
    step <- c(grid$lon_step, grid$lat_step)[axis]
    origin <- if (axis == 1) min(grid$lon) - step / 2 else min(grid$lat) - step / 2
    limits <- range(polygon[, axis])
    positions <- function(spacing) origin + seq(ceiling((limits[1] - origin) / spacing),
                                               floor((limits[2] - origin) / spacing)) * spacing
    for (value in positions(step / 4)) segment(value, axis, "#8B3F37", fine_width)
    for (value in positions(step)) segment(value, axis, grDevices::adjustcolor("black", 0.92), 0.24)
  }
  invisible()
}
