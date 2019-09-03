jrd_colors <- c(
    "#212121",
    "#646464",
    "#003479",
    "#6f2e38",
    "#f37820",
    "#f5af25",
    "#edec2f",
    "#00a0df",
    "#003479",
    "#1c75bc",
    "#359942",
    "#6ebd44",
    "#7e2e7a"
  )

names(jrd_colors) <- c(
  "textlines",
  "warmgrey",
  "warmblue",
  "warmred",
  "warmorange1",
  "warmorange2",
  "warmyellow",
  "coolblue1",
  "coolblue2",
  "coolblue3",
  "coolgreen1",
  "coolgreen2",
  "coolmauve"
)

jc <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (jrd_colors)
  
  jrd_colors[cols]
}

jc_pal <- list(
  'cool' = jc(
    "coolblue1",
    "coolblue2",
    "coolblue3",
    "coolgreen1",
    "coolgreen2",
    "coolmauve"
  ),
  'warm' = jc(
    "warmgrey",
    "warmblue",
    "warmred",
    "warmorange1",
    "warmorange2",
    "warmyellow"
  ),
  'cool_ext' = jc(
    "coolblue1",
    "coolblue2",
    "coolblue3",
    "coolgreen1",
    "coolgreen2",
    "coolmauve",
    "warmgrey",
    "warmred",
    "warmorange1",
    "warmorange2",
    "warmyellow"
  ),
  'warm_ext' = jc(
    "warmgrey",
    "warmblue",
    "warmred",
    "warmorange1",
    "warmorange2",
    "warmyellow",
    "coolblue1",
    "coolblue3",
    "coolgreen1",
    "coolgreen2",
    "coolmauve"
  )
)

#' Return function for an interpolated jrd color palette
#'
#' @param palette Character name of palette in jrd_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
jrd_pal_ramp <- function(palette = "cool", reverse = FALSE, ...) {
  pal <- jc_pal[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


#' Return function for a discrete jrd color palette
#'
#' @param palette Character name of palette in jrd_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#'
jrd_pal_discrete <- function(palette = "cool", reverse = FALSE) {
  pal <- jc_pal[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  return(function(n) unname(pal[1:n]))
}

#' Color scale constructor for jrd colors
#'
#' @param palette Character name of palette in jrd_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param discrete Boolean indicating whether or not the scale should be discrete
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_jrd <- function(palette = "cool", discrete = TRUE, reverse = FALSE, ...) {
  if (discrete) {
    pal = jrd_pal_discrete(palette=palette, reverse=reverse)
    discrete_scale("colour", paste0("jrd_", palette), palette = pal, ...)    
  } else {
    pal = jrd_pal_ramp(palette=palette, reverse=reverse)
    scale_color_gradientn(colors=pal(256), ...)
  }
}

#' Fill scale constructor for jrd colors
#'
#' @param palette Character name of palette in jc_pal
#' @param discrete Boolean indicating whether or not the scale should be discrete
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_jrd <- function(palette = "cool", discrete=TRUE, reverse = FALSE, ...) {
  if (discrete) {
    pal = jrd_pal_discrete(palette=palette, reverse=reverse)
    discrete_scale("fill", paste0("jrd_", palette), palette = pal, ...)    
  } else {
    pal = jrd_pal_ramp(palette=palette, reverse=reverse)
    scale_fill_gradientn(colors=pal(256), ...)
  }
}

theme_jrd <- function(base_size=20, base_family="Verdana", ...) {
  theme_minimal(base_size=base_size, base_family=base_family, ...) +
    theme(
      text = element_text(color=jc("textlines")),
      title = element_text(face="bold"),
      axis.title = element_text(size=15),
      axis.text.x = element_text(margin=margin(-5, 10, 10, 10)),
      axis.text.y = element_text(margin=margin(10, 5, 10, 10)),
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(size=0.5),
      plot.title = element_text(face="plain")
    )
}