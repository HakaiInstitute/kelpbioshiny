# Line drawings of the two kelp species for the navbar brand. Drawn on a 28 x 36
# grid at 1.5 units per stroke, so at 36px tall the stroke is 1.5px. They take
# the navbar text colour through currentColor.

path_d <- function(fmt, ...) sprintf(paste0('<path d="', fmt, '"/>'), ...)

# Branched holdfast: haptera arching out and down from the base of the stipe,
# the outer ones forking once.
kelp_holdfast <- function(x, y) {
  paste0(
    path_d("M%.1f %.1fc-1.5.1-2.8.8-3.6 2.4", x, y),
    path_d("M%.1f %.1fc-.5.1-1 .6-1.2.9", x - 2.3, y + 0.6),
    path_d("M%.1f %.1fc-.5.6-.9 1.5-1 2.5", x, y),
    path_d("M%.1f %.1fc.5.6.9 1.5 1 2.5", x, y),
    path_d("M%.1f %.1fc1.5.1 2.8.8 3.6 2.4", x, y),
    path_d("M%.1f %.1fc.5.1 1 .6 1.2.9", x + 2.3, y + 0.6)
  )
}

# A cubic Bezier segment as a function of t: the point and the unit tangent.
bezier <- function(p0, p1, p2, p3) {
  function(t) {
    point <- (1 - t)^3 * p0 + 3 * (1 - t)^2 * t * p1 + 3 * (1 - t) * t^2 * p2 + t^3 * p3
    d <- 3 * (1 - t)^2 * (p1 - p0) + 6 * (1 - t) * t * (p2 - p1) + 3 * t^2 * (p3 - p2)
    list(point = point, dir = d / sqrt(sum(d^2)))
  }
}

xy <- function(p) sprintf("%.1f %.1f", p[1], p[2])

# Smooth cubic Bezier segments through a list of points (Catmull-Rom), as
# path data continuing from the first point.
smooth_segments <- function(points) {
  n <- length(points)
  at <- function(i) points[[min(max(i, 1), n)]]
  segments <- vapply(seq_len(n - 1), function(i) {
    c1 <- at(i) + (at(i + 1) - at(i - 1)) / 6
    c2 <- at(i + 1) - (at(i + 2) - at(i)) / 6
    sprintf("C%s %s %s", xy(c1), xy(c2), xy(at(i + 1)))
  }, "")
  paste(segments, collapse = "")
}

# A Macrocystis blade from the frond at `at` (a bezier() point), turned `angle`
# degrees from the frond (positive to the right): a small pear-shaped float,
# then a long, narrow blade that is widest near its base, tapers to a point,
# curls to the right by `bend`, as if streaming in a current, and ripples
# slightly along its length.
macro_blade <- function(at, angle, len, w = 0.85, bend = 0.1, float = TRUE) {
  a <- angle * pi / 180
  u <- c(at$dir[1] * cos(a) - at$dir[2] * sin(a), at$dir[1] * sin(a) + at$dir[2] * cos(a))
  n <- c(-u[2], u[1])
  pt <- function(t, s = 0) at$point + t * u + s * n
  out <- character()
  base <- 0
  if (float) {
    base <- 3
    out <- sprintf(
      '<path d="M%sC%s %s %sC%s %s %sZ"/>', xy(pt(0)),
      xy(pt(0.6, 1.1)), xy(pt(3.4, 1.8)), xy(pt(base)), xy(pt(3.4, -1.8)), xy(pt(0.6, -1.1)), xy(pt(0))
    )
  }
  t <- seq(0, 1, length.out = 9)
  centre <- lapply(t, function(t) {
    pt(base + t * len, len * (bend * t^2 + 0.03 * sin(2 * pi * t)))
  })
  half <- w * (t^0.6 * (1 - t)^1.4) / max(t^0.6 * (1 - t)^1.4)
  normal <- function(i) {
    d <- centre[[min(i + 1, 9)]] - centre[[max(i - 1, 1)]]
    c(-d[2], d[1]) / sqrt(sum(d^2))
  }
  # Each edge runs separately from base to tip, so the tip comes to a point.
  left <- lapply(1:9, function(i) centre[[i]] + half[i] * normal(i))
  right <- lapply(9:1, function(i) centre[[i]] - half[i] * normal(i))
  blade <- sprintf('<path d="M%s%s%sZ"/>', xy(centre[[1]]), smooth_segments(left), smooth_segments(right))
  paste(c(out, blade), collapse = "")
}

# Giant kelp frond: one long stipe arcing up and over to the right from the
# holdfast, as if swaying in a current.
macro_frond_points <- list(c(9.4, 32.2), c(6.2, 20.6), c(9.4, 8.6), c(18.4, 5.6))
macro_frond <- do.call(bezier, macro_frond_points)

kelp_paths <- list(
  # Bull kelp: a long, thin, curving stipe that widens into a single round
  # float, with many long ribbon blades streaming from the top of the float.
  nereo = paste0(
    kelp_holdfast(7, 32.2),
    '<path d="M7 32.2C6.2 28.4 9.4 26.6 9.2 23.4S8.6 19.8 9.8 18.2"/>',
    # The stipe widens into the float, drawn as one outline.
    '<path d="M9.8 18.2C9.7 16.8 9.2 15.4 8.9 14.5A3 3 0 1 1 12.3 14.5C11.4 15.6 10.1 16.8 9.9 18.2"/>',
    '<path d="M8.5 9.9C6.8 7.6 7.9 4.2 5.8 1.2"/>',
    '<path d="M9.6 9.2C8.8 6.6 11.6 4 11.4 1"/>',
    '<path d="M11.1 9.1C11.8 6.4 15.4 5.4 17.2 1.2"/>',
    '<path d="M12.3 9.5C14.4 7.2 18.8 7.8 22.6 3.4"/>',
    '<path d="M13.2 10.5C16.4 9.4 21 11 26.4 7.2"/>',
    '<path d="M13.6 11.7C17 12 20.6 14.4 25.8 12.6"/>'
  ),
  # Giant kelp: one frond from a branched holdfast, with long narrow blades
  # alternating along it, each on a small pear-shaped float.
  macro = paste0(
    kelp_holdfast(10, 32.2),
    sprintf('<path d="M%sC%s %s %s"/>', xy(macro_frond_points[[1]]), xy(macro_frond_points[[2]]), xy(macro_frond_points[[3]]), xy(macro_frond_points[[4]])),
    macro_blade(macro_frond(0.17), 44, 12),
    macro_blade(macro_frond(0.36), -32, 11),
    macro_blade(macro_frond(0.56), 50, 10),
    macro_blade(macro_frond(0.75), -34, 5.6),
    # Apical blade: the curved tip of the frond.
    macro_blade(macro_frond(1), 4, 6.5, w = 0.85, bend = 0.06, float = FALSE)
  )
)

kelp_art <- function(species, class = NULL) {
  HTML(sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 28 36" fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round" class="%s" role="img" aria-label="%s">%s</svg>',
    paste(c("kb-kelp-art", class), collapse = " "),
    sprintf("Line drawing of %s", species_info[[species]]$common),
    kelp_paths[[species]]
  ))
}
