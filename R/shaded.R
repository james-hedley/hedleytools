# Function: Determine equivalent colour when a specified colour is set to specified transparency
#' @export
# Created by: James Hedley
# Date created: 18th August 2025

require(grDevices)

shaded <- function(col, alpha = 1, bg = "white") {
  # Convert to RGB (scale 0–1)
  fg_rgb <- grDevices::col2rgb(col) / 255
  bg_rgb <- grDevices::col2rgb(bg) / 255
  
  # Ensure alpha is same length as col
  if (length(alpha) == 1) alpha <- rep(alpha, ncol(fg_rgb))
  
  # Apply alpha blending: out = alpha*fg + (1-alpha)*bg
  out_rgb <- alpha * fg_rgb + (1 - alpha) * bg_rgb
  
  # Convert back to hex (columns are colors)
  grDevices::rgb(out_rgb[1,], out_rgb[2,], out_rgb[3,])
}
