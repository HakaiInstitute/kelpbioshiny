# Line drawings of the two kelp species: the navbar brand (kelp_art()) and the
# young plants of the footer's seabed (seabed_art()). Drawn from botanical
# illustrations of each species: bull kelp's thin stipe swelling into a single
# float, with ribbon blades with ruffled margins in two bundles from its top;
# giant kelp's conical holdfast of branching haptera, fronds with toothed
# blades each on a pear-shaped float, a sickle-shaped blade at each frond's
# tip, and sporophylls (blades without floats) at the base. Strokes and fills
# take the text colour through currentColor: blades are solid, floats are
# outlined and filled faintly, as they are hollow.

xy <- function(p) sprintf("%.1f %.1f", p[1], p[2])

pt_list <- function(x, y) lapply(seq_along(x), function(i) c(x[i], y[i]))

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

line_segments <- function(points) paste0("L", vapply(points[-1], xy, ""), collapse = "")

blade_fill <- ' fill="currentColor" fill-opacity=".3" stroke-width=".5"'
ribbon_fill <- ' fill="currentColor" stroke="none"'
solid_fill <- ' fill="currentColor"'

svg_path <- function(d, fill = "") sprintf('<path d="%s"%s/>', d, fill)

open_path <- function(points) svg_path(paste0("M", xy(points[[1]]), smooth_segments(points)))

# A curve traced as by a walker: from `start`, `len` long, setting off at
# `angle` degrees (0 points right, -90 up) and turning steadily by `curl`
# degrees over its length (positive turns clockwise), with a sway of `wave`
# degrees swinging `waves` times along it. Returns `n` + 1 points.
walk <- function(start, angle, len, curl = 0, wave = 0, waves = 1, phase = 0, n = 16) {
  t <- (seq_len(n) - 0.5) / n
  theta <- (angle + curl * t + wave * sin(2 * pi * waves * t + phase)) * pi / 180
  pt_list(
    start[1] + cumsum(c(0, len / n * cos(theta))),
    start[2] + cumsum(c(0, len / n * sin(theta)))
  )
}

# The point at fraction `t` of the way along a list of points, and the unit
# direction there.
along <- function(points, t) {
  n <- length(points)
  i <- min(max(round(1 + t * (n - 1)), 1), n)
  d <- points[[min(i + 1, n)]] - points[[max(i - 1, 1)]]
  list(point = points[[i]], dir = d / sqrt(sum(d^2)))
}

# A closed outline around a centre line: `half` is the half-width at each
# point and `teeth` the depth of small teeth along both edges, alternating
# point by point. Each edge runs from base to tip, so a tip of width zero
# comes to a point.
ribbon <- function(centre, half, teeth = 0) {
  n <- length(centre)
  normal <- function(i) {
    d <- centre[[min(i + 1, n)]] - centre[[max(i - 1, 1)]]
    c(-d[2], d[1]) / sqrt(sum(d^2))
  }
  tooth <- teeth * (seq_len(n) %% 2) * (half > 0)
  left <- lapply(seq_len(n), function(i) centre[[i]] + (half[i] + tooth[i]) * normal(i))
  right <- lapply(rev(seq_len(n)), function(i) centre[[i]] - (half[i] + tooth[i]) * normal(i))
  edge <- if (teeth > 0) line_segments else smooth_segments
  paste0("M", xy(left[[1]]), edge(left), "L", xy(right[[1]]), edge(right), "Z")
}

# A blade's half-width along its length: widest at fraction `peak`, tapering
# to a point at the tip, with `ruffle` rippling the margin `ruffles` times.
blade_half <- function(n, width, peak = 0.3, base = 0, ruffle = 0, ruffles = 5, phase = 0) {
  t <- seq(0, 1, length.out = n)
  a <- peak / (1 - peak)
  shape <- t^a * (1 - t)
  half <- width * shape / max(shape)
  half <- pmax(half, base * (1 - t)^2)
  half * (1 + ruffle * sin(2 * pi * ruffles * t + phase))
}

# A holdfast: haptera arching out and down from points along a low dome over
# `base`, each forking once or twice towards its tip, so the holdfast reads as
# a tangled mass gripping the rock. `spread` is the dome's half-width.
holdfast <- function(base, spread, height, n = 6, len = 3, forks = 1) {
  hapteron <- function(start, angle, len, depth) {
    curl <- if (angle > 90) -40 else 40
    points <- walk(start, angle, len, curl = curl, n = 6)
    out <- open_path(points)
    if (depth > 0) {
      tip <- points[[length(points)]]
      end_angle <- angle + curl
      out <- c(
        out,
        hapteron(tip, end_angle - 30, len * 0.55, depth - 1),
        hapteron(tip, end_angle + 25, len * 0.5, depth - 1)
      )
    }
    out
  }
  s <- seq(-1, 1, length.out = n)
  paste(unlist(lapply(seq_len(n), function(i) {
    start <- base + c(s[i] * spread * 0.35, -height * (1 - s[i]^2))
    angle <- 90 - 70 * s[i]
    hapteron(start, angle, len * (1 - 0.25 * abs(s[i])), if (abs(s[i]) > 0.5) forks else max(forks - 1, 0))
  })), collapse = "")
}

# A pear-shaped float from `start`, pointing along `u`, `len` long and
# `width` in half-width.
pear_float <- function(start, u, len, width) {
  n <- c(-u[2], u[1])
  pt <- function(t, s = 0) start + t * u + s * n
  svg_path(sprintf(
    "M%sC%s %s %sC%s %s %sZ", xy(pt(0)),
    xy(pt(0.2 * len, 0.6 * width)), xy(pt(1.1 * len, width)), xy(pt(len)),
    xy(pt(1.1 * len, -width)), xy(pt(0.2 * len, -0.6 * width)), xy(pt(0))
  ), blade_fill)
}

# A giant kelp blade from the frond at `at` (an along() point), turned
# `angle` degrees from the frond: a pear-shaped float (when `float` > 0, its
# length), then a solid lance-shaped blade with toothed margins, curling by
# `curl` degrees as it streams in the current.
macro_blade <- function(at, angle, len, width = 0.9, curl = 30, float = 2.4, teeth = 0.25, wave = 8) {
  a <- angle * pi / 180
  u <- c(at$dir[1] * cos(a) - at$dir[2] * sin(a), at$dir[1] * sin(a) + at$dir[2] * cos(a))
  out <- character()
  start <- at$point
  if (float > 0) {
    out <- pear_float(start, u, float, float * 0.38)
    start <- start + float * u
  }
  n <- 25
  centre <- walk(start, atan2(u[2], u[1]) * 180 / pi, len, curl = curl, wave = wave, waves = 1.5, n = n - 1)
  half <- blade_half(n, width, peak = 0.3, base = 0.15)
  paste(c(svg_path(ribbon(centre, half, teeth), ribbon_fill), out), collapse = "")
}

# A bull kelp blade from the top of the float: a long ribbon with a ruffled
# margin, its centre line swaying as it streams in the current.
nereo_blade <- function(start, angle, len, width = 0.6, curl = 60, wave = 22, phase = 0) {
  n <- 33
  centre <- walk(start, angle, len, curl = curl, wave = wave, waves = 1.8, phase = phase, n = n - 1)
  half <- blade_half(n, width, peak = 0.25, base = 0.2, ruffle = 0.45, ruffles = 7, phase = phase)
  svg_path(ribbon(centre, half), ribbon_fill)
}

# A stipe that tapers from `w0` at its base to `w1` at its top, filled solid.
tapered_stipe <- function(centre, w0, w1, power = 2) {
  t <- seq(0, 1, length.out = length(centre))
  svg_path(ribbon(centre, w0 + (w1 - w0) * t^power), solid_fill)
}

# Bull kelp: a long stipe, thread-thin at the holdfast, swelling into a round
# float, with two bundles of ribbon blades streaming from the top of the float
# in the current to the right.
nereo_plant <- function(base, height, n_blades = 8, blade_len = NULL, scale = 1, holdfast_n = 7) {
  stipe <- walk(base, -86, height, curl = 14, wave = 12, waves = 1.1, phase = 0.4, n = 24)
  top <- along(stipe, 1)
  r <- 2.1 * scale
  centre <- top$point + top$dir * (r * 0.8)
  float <- sprintf('<circle cx="%.1f" cy="%.1f" r="%.1f"%s/>', centre[1], centre[2], r, blade_fill)
  crown <- centre + top$dir * r * 0.8
  blade_len <- blade_len %||% (height * 0.8)
  angles <- seq(-95, -15, length.out = n_blades)
  blades <- vapply(seq_len(n_blades), function(i) {
    side <- if (i <= n_blades / 2) -0.6 else 0.6
    start <- crown + side * c(-top$dir[2], top$dir[1]) * scale
    len <- blade_len * (0.85 + 0.2 * sin(i * 2.1))
    nereo_blade(start, angles[i], len, width = 0.55 * scale, curl = 40 + 30 * (i / n_blades), phase = i * 1.7)
  }, "")
  paste0(
    holdfast(base, 2.6 * scale, 1.3 * scale, n = holdfast_n, len = 2.4 * scale),
    tapered_stipe(stipe, 0.12 * scale, 0.6 * scale, power = 3),
    paste(blades, collapse = ""),
    float
  )
}

# Giant kelp: fronds from a broad conical holdfast, each with toothed blades
# alternating along it on pear-shaped floats and a sickle-shaped blade at its
# tip, and smooth sporophylls without floats at the base.
macro_frond <- function(base, angle, len, curl, blades, scale = 1, floats = TRUE, apical = TRUE) {
  frond <- walk(base, angle, len, curl = curl, wave = 6, waves = 1.2, n = 30)
  out <- open_path(frond)
  for (b in blades) {
    out <- c(out, macro_blade(
      along(frond, b$t), b$angle, b$len * scale,
      width = 0.6 * scale, curl = b$curl %||% 35, float = if (floats) 2.1 * scale else 0, teeth = 0.22 * scale
    ))
  }
  if (apical) {
    tip <- along(frond, 1)
    out <- c(out, macro_blade(tip, 8, 6.5 * scale, width = 1.25 * scale, curl = 70, float = 0, teeth = 0.22 * scale, wave = 4))
  }
  paste(out, collapse = "")
}

macro_plant <- function(base) {
  sporophylls <- vapply(list(c(-150, 4.2, -30), c(-125, 5, -20), c(-40, 4.6, 30)), function(s) {
    macro_blade(list(point = base + c(0, -1.6), dir = c(1, 0)), s[1], s[2], width = 0.7, curl = s[3], float = 0, teeth = 0)
  }, "")
  paste0(
    holdfast(base, 6, 2, n = 9, len = 3, forks = 2),
    paste(sporophylls, collapse = ""),
    macro_frond(base + c(0.6, -1.6), -90, 27, curl = 42, blades = list(
      list(t = 0.16, angle = 40, len = 10, curl = 45),
      list(t = 0.3, angle = -35, len = 9.5, curl = 60),
      list(t = 0.44, angle = 42, len = 9.5, curl = 40),
      list(t = 0.58, angle = -38, len = 8.5, curl = 60),
      list(t = 0.72, angle = 45, len = 7.5, curl = 35),
      list(t = 0.85, angle = -40, len = 5.5, curl = 60)
    )),
    macro_frond(base + c(-0.6, -1.6), -100, 15, curl = -12, blades = list(
      list(t = 0.4, angle = -45, len = 7, curl = 45),
      list(t = 0.7, angle = 45, len = 6, curl = 40)
    ), scale = 0.85)
  )
}

kelp_paths <- list(
  nereo = nereo_plant(c(6, 34.2), 16, blade_len = 17),
  macro = macro_plant(c(9, 34.2))
)

kelp_art <- function(species, class = NULL) {
  HTML(sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 28 36" fill="none" stroke="currentColor" stroke-width="0.9" stroke-linecap="round" stroke-linejoin="round" class="%s" role="img" aria-label="%s">%s</svg>',
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

# A young bull kelp plant on the seabed at `x`, `h` tall: nereo_plant() at a
# smaller scale, with fewer blades.
young_nereo <- function(i, x, h) {
  base <- c(x, seabed_y(x) - 0.4)
  scale <- 0.35 + 0.025 * h
  nereo_plant(base, h, n_blades = 3 + floor(3 * noise(i, 8)), blade_len = 0.6 * h, scale = scale, holdfast_n = 4)
}

# A young giant kelp plant on the seabed at `x`, `h` tall: one or two short
# fronds, without the floats, which are too small to show at this size.
young_macro <- function(i, x, h) {
  base <- c(x, seabed_y(x) - 0.4)
  n_fronds <- 1 + floor(2 * noise(i, 13))
  fronds <- vapply(seq_len(n_fronds), function(j) {
    height <- h * (1 - 0.3 * (j - 1))
    angle <- -90 + 12 * (j - 1) - 8 * noise(i * 10 + j, 14)
    macro_frond(base, angle, height, curl = 45, scale = 0.06 * height, floats = FALSE, blades = list(
      list(t = 0.35, angle = 45, len = 4.5),
      list(t = 0.55, angle = -40, len = 4.2, curl = 50),
      list(t = 0.75, angle = 45, len = 3.8)
    ))
  }, "")
  paste0(holdfast(base, 2.4, 0.8, n = 4, len = 2), paste(fronds, collapse = ""))
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
