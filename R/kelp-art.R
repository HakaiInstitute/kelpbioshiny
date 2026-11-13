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

# The footer's seabed: a band of seabed along the foot of the page, with
# pebbles and half-buried rocks, and a young kelp bed of the species, short
# plants with their holdfasts on the seabed. Drawn on a 1200 x 48 grid in px; the
# drawing is cropped at the sides on narrow screens, not squashed. The
# placement is fixed: noise() is a hash of its index, so the drawing is the
# same on every load.
noise <- function(i, salt = 0) {
  v <- sin((i + salt * 97.13) * 12.9898) * 43758.5453
  v - floor(v)
}

seabed_width <- 1200
seabed_height <- 48
seabed_y <- function(x) 34 + 2.5 * sin(x / 90) + 1.5 * sin(x / 37 + 1)

seabed_ground <- function() {
  x <- seq(0, seabed_width, by = 20)
  points <- lapply(x, function(x) c(x, seabed_y(x)))
  sprintf(
    '<path class="kb-seabed-ground" d="M%s%sL%d %dL0 %dZ"/>',
    xy(points[[1]]), smooth_segments(points), seabed_width, seabed_height, seabed_height
  )
}

# Pebbles scattered through the seabed, smaller and sparser with depth.
seabed_pebbles <- function(n = 90) {
  circles <- vapply(seq_len(n), function(i) {
    x <- noise(i, 1) * seabed_width
    depth <- noise(i, 2)^1.5
    y <- seabed_y(x) + 2.5 + depth * (seabed_height - seabed_y(x) - 3.5)
    sprintf('<circle cx="%.1f" cy="%.1f" r="%.1f"/>', x, y, 0.5 + 1.3 * noise(i, 3) * (1 - 0.5 * depth))
  }, "")
  sprintf('<g class="kb-seabed-pebbles">%s</g>', paste(circles, collapse = ""))
}

# Rocks sitting half-buried on the seabed: an irregular dome.
seabed_rocks <- function(n = 11) {
  rocks <- vapply(seq_len(n), function(i) {
    x <- (i - 0.5 + 0.8 * (noise(i, 4) - 0.5)) * seabed_width / n
    w <- 4 + 6 * noise(i, 5)
    h <- 4 + 5 * noise(i, 6)
    y <- seabed_y(x) + 1.2
    sprintf(
      '<path d="M%sC%s %s %sC%s %s %sZ"/>',
      xy(c(x - w, y)), xy(c(x - w * 1.05, y - h * 0.9)), xy(c(x - w * 0.35, y - h * 1.15)), xy(c(x + w * 0.15, y - h)),
      xy(c(x + w * 0.75, y - h * 0.9)), xy(c(x + w * 1.1, y - h * 0.5)), xy(c(x + w, y))
    )
  }, "")
  sprintf('<g class="kb-seabed-rocks">%s</g>', paste(rocks, collapse = ""))
}

# A young bull kelp plant: a holdfast on the seabed at `x`, a thin stipe
# leaning with the current to a small float at height `h`, and a few short
# blades trailing from the float to the right.
young_nereo <- function(i, x, h) {
  base <- c(x, seabed_y(x) - 0.4)
  lean <- (0.15 + 0.25 * noise(i, 7)) * h
  float_r <- 1.1 + 0.08 * h
  top <- base + c(lean, -h)
  stipe <- sprintf(
    '<path d="M%sC%s %s %s"/>',
    xy(base), xy(base + c(-0.15 * lean, -0.45 * h)), xy(top + c(-0.5 * lean, 0.3 * h)), xy(top + c(0, float_r))
  )
  float <- sprintf('<circle cx="%.1f" cy="%.1f" r="%.1f"/>', top[1], top[2], float_r)
  n_blades <- 2 + floor(3 * noise(i, 8))
  blades <- vapply(seq_len(n_blades), function(j) {
    len <- (0.35 + 0.35 * noise(i * 10 + j, 9)) * h
    rise <- (j - (n_blades + 1) / 2) * 0.12 * len - 0.15 * len
    start <- top + c(float_r * 0.7, -float_r * 0.4)
    end <- start + c(len, rise)
    sprintf(
      '<path d="M%sC%s %s %s"/>',
      xy(start), xy(start + c(0.35 * len, rise - 0.2 * len)), xy(start + c(0.7 * len, rise + 0.12 * len)), xy(end)
    )
  }, "")
  paste0(young_holdfast(base), stipe, float, paste(blades, collapse = ""))
}

# A young plant's holdfast: four haptera arching out and down from `base`.
young_holdfast <- function(base) {
  sprintf(
    '<path d="M%sc-1.2.2-2.4.9-3 2.1M%sc-.4.6-.7 1.3-.8 2.1M%sc.4.6.7 1.3.8 2.1M%sc1.2.2 2.4.9 3 2.1"/>',
    xy(base), xy(base), xy(base), xy(base)
  )
}

# A young giant kelp plant: one or two short fronds from a holdfast on the
# seabed at `x`, leaning with the current to height `h`, each with a few small
# blades alternating along it and a blade at its tip (macro_blade(), without
# the float, which is too small to show at this size).
young_macro <- function(i, x, h) {
  base <- c(x, seabed_y(x) - 0.4)
  n_fronds <- 1 + floor(2 * noise(i, 13))
  fronds <- vapply(seq_len(n_fronds), function(j) {
    height <- h * (1 - 0.3 * (j - 1))
    lean <- (0.2 + 0.25 * noise(i * 10 + j, 14)) * height
    p <- list(base, base + c(-0.1 * lean, -0.5 * height), base + c(0.4 * lean, -0.95 * height), base + c(lean, -height))
    frond <- do.call(bezier, p)
    len <- 0.28 * height
    blades <- vapply(c(0.4, 0.6, 0.8), function(t) {
      macro_blade(frond(t), if (t == 0.6) -40 else 45, len, w = 0.45, bend = 0.1, float = FALSE)
    }, "")
    paste0(
      sprintf('<path d="M%sC%s %s %s"/>', xy(p[[1]]), xy(p[[2]]), xy(p[[3]]), xy(p[[4]])),
      paste(blades, collapse = ""),
      macro_blade(frond(1), 4, 0.32 * height, w = 0.5, bend = 0.06, float = FALSE)
    )
  }, "")
  paste0(young_holdfast(base), paste(fronds, collapse = ""))
}

# `n` plants, one in each stretch of seabed at an uneven spot within it, kept
# low below the footer text.
young_bed <- function(species, n = 6) {
  spacing <- seabed_width / n
  plants <- vapply(seq_len(n), function(i) {
    x <- (i - 0.5) * spacing + 0.7 * spacing * (noise(i, 10) - 0.5)
    plant <- if (species == "macro") young_macro else young_nereo
    plant(i, x, 9 + 13 * noise(i, 11)^1.3)
  }, "")
  sprintf('<g class="kb-seabed-kelp">%s</g>', paste(plants, collapse = ""))
}

seabed_art <- function(species) {
  HTML(sprintf(
    paste0(
      '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" preserveAspectRatio="xMidYMax slice" ',
      'class="kb-seabed" aria-hidden="true" focusable="false">%s%s%s%s</svg>'
    ),
    seabed_width, seabed_height, seabed_ground(), seabed_pebbles(), seabed_rocks(), young_bed(species)
  ))
}
