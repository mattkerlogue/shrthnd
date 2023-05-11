library(ggplot2)
library(systemfonts)

# image is of the word "shorthand" in Pitman shorthand and a star/asterisk
# coordinates from original drawing (shorthand-drawing.svg) and then modified
# for plotting in {ggplot2} with {ggforce}
#
# the shorthand shape is drawn from hand based on the representation in
# Pitman's English and Shorthand Dictionary, retrived from the Internet
# Archive on 2023-05-11
# https://archive.org/details/in.ernet.dli.2015.449114/page/n641/mode/1up
# (British Library catalogue record: http://explore.bl.uk/BLVU1:LSCOP-ALL:BLL01002926466)

star_shape <- tibble::tribble(
  ~x, ~y, ~group, ~id,
  -85.24202,-75.39072, 1, 15,
  23.17478,-2.04598, 1, 1,
  54.81188,-5.03715, 1, 2,
  115.06839,-10.19635, 1, 3,
  115.06839,-10.19635, 2, 3,
  -18.6896,30.25121, 2, 4,
  -70.67242,60.94304, 2, 5,
  -93.94761,77.57356, 2, 6,
  -93.94761,77.57356, 3, 6,
  8.85244,-17.21984, 3, 7,
  12.32075,-47.19819, 3, 8,
  32.95773,-112.94889, 3, 9,
  32.95773,-112.94889, 4, 9,
  12.00598,-3.64816, 4, 10,
  21.92736,58.62924, 4, 11,
  24.16351,120.9624, 4, 12,
  24.16351,120.9624, 5, 12,
  -30.20068,-25.59698, 5, 13,
  -59.36762,-53.74589, 5, 14,
  -85.24202,-75.39072, 5, 15
) |>
  dplyr::mutate(
    x = scales::rescale(-x),
    y = scales::rescale(y),
  )

ggplot(star_shape, aes(x = x, y, colour = as.character(group))) +
  geom_point() +
  geom_text(aes(label = id), vjust = 2, size = 3, colour = "#000000") +
  # ggforce::geom_bezier() +
  ggforce::geom_bspline_closed0(aes(x = x, y = y), alpha = 0.5, inherit.aes = FALSE) +
  coord_fixed()

shorthand_shape <- tibble::tribble(
  ~x, ~y, ~ogroup, ~group, ~id,
  -36.03824,323.26558, 2, 1, 0,
  # 83.85055,-53.03898, 2, 1, 1,
  57.96176,82.26558, 2, 1, 2,
  -23,178, 2, 1, 3,
  -23,178, 2, 4, 10,
  23.14945,156.03898, 2, 4, 11,
  -36.03824,323.26558, 2, 4, 12,

  -36.03824,323.26558, 1, 2, 4,
  69.88382,399.66672, 1, 2, 5,
  128,307, 1, 2, 6,
  128,307, 1, 3, 7,
  78,350.66667, 1, 3, 8,
  -36.03824,323.26558, 1, 3, 9,

) |>
  dplyr::mutate(
    x = scales::rescale(x),
    y = scales::rescale(-y)
  )

ggplot(shorthand_shape, aes(x, y, colour = as.character(group))) +
  # geom_path(colour = "grey80") +
  # geom_point() +
  geom_text(aes(label = id), vjust = 2, size = 3, colour = "#000000") +
  # ggforce::geom_bezier() +
  ggforce::geom_bspline_closed0(aes(x = x, y = y), alpha = 0.5, colour = "#000000", inherit.aes = FALSE) +
  coord_fixed() +
  theme_minimal()

both_shapes <- dplyr::bind_rows(
  shorthand = shorthand_shape,
  star = star_shape,
  .id = "shape"
) |>
  dplyr::mutate(
    x = dplyr::if_else(shape == "star", x/3 + 0.95, x*2/3 + 1-(5/12)),
    y = dplyr::if_else(shape == "star", y/3 + 0.85, y + 0.3)
  )

ggplot(both_shapes, aes(x = x, y = y, group = shape)) +
  ggforce::geom_bspline_closed0(fill = "#000000", colour = "#000000") +
  theme_void() +
  coord_fixed()

shrthnd_sticker <- ggplot() +
  hexSticker::geom_hexagon(size = 2, fill = "#0A4F6B", color = "#1BA8E0") +
  ggforce::geom_bspline_closed0(
    data = both_shapes, aes(x, y, group = shape), fill = "#ffffff", colour = "#ffffff") +
  annotate(
    geom = "text",
    label = "shrthnd",
    x = 1,
    y = 1.36,
    vjust = 0,
    size = 7.25,
    colour = "#ffffff",
    family = "Victor Mono",
    fontface = "bold"
  ) +
  coord_fixed() +
  hexSticker::theme_transparent() +
  hexSticker::theme_sticker(2)

ggsave("man/figures/tidyods_hex.svg", shrthnd_sticker, width = 43.9,
       height = 50.8, units = "mm", bg = "transparent")

ggsave("man/figures/tidyods_hex.png", shrthnd_sticker, width = 43.9,
       height = 50.8, units = "mm", bg = "transparent")
