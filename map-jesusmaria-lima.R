library(tidyverse)
library(osmdata)
library(showtext)
library(showtextdb)

load_showtext_fonts()
font_add_google("Cinzel Decorative", "titlefont")
font_add_google("Cutive Mono", "captionfont")
showtext_auto()

#available_tags("highway")
#available_features()

if (!file.exists("jesusmaria-lima-map-sf.Rdata")) {
  # this bounding box gets Jesus Maria, Lima
  jm_lima_bb <- matrix(
    c(-77.06, -77.03, -12.06, -12.09),
    nrow = 2, ncol = 2
  )
  colnames(jm_lima_bb) <- c("min", "max")
  row.names(jm_lima_bb) <- c("x", "y")

  jm_lima_bb <- getbb("Jesus Maria Lima Perú")
  streets <- jm_lima_bb %>%
    opq() %>%
    add_osm_feature(
      key = "highway",
      value = c(
        "turning circle",
        "turning loop",
        "mini roundabout",
        "motorway",
        "primary",
        "secondary",
        "tertiary"
      )
    ) %>%
    osmdata_sf()

  small_streets <- jm_lima_bb %>%
    opq() %>%
    add_osm_feature(
      key = "highway",
      value = c(
        "residential",
        "living_street",
        "unclassified",
        "service",
        "footway"
      )
    ) %>%
    osmdata_sf()

  save(jm_lima_bb,
       streets,
       small_streets,
       file = here::here("jesusmaria-lima-map-sf.Rdata"))
} else {
  load(here::here("jesusmaria-lima-map-sf.Rdata"))
}

jm_lima_map <- ggplot() +
  geom_sf(
    data = streets$osm_lines,
    inherit.aes = FALSE,
    color = "skyblue",
    alpha = .8,
    size = .5
  ) +
  geom_sf(
    data = small_streets$osm_lines,
    inherit.aes = FALSE,
    color = "lightcoral",
    alpha = .8,
    size = .3
  ) +
  coord_sf(
    xlim = c(-77.070, -77.030),
    ylim = c(-12.095, -12.060),
    expand = FALSE
  ) +
  geom_rect(
    inherit.aes = FALSE,
    aes(
    xmin = -77.069,
    xmax = -77.031,
    ymin = -12.094,
    ymax = -12.061
    ),
    color = "yellow",
    fill = NA,
    size = .7
  ) +
  labs(
    title = "// @jmcastagnetto, Jesus M. Castagnetto, 2019-11-28 //",
    caption = "{ Jesús Maria, Lima, Perú }"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey15",
                                   color = "peru",
                                   size = 3),
    plot.title = element_text(family = "captionfont",
                              size = 18,
                              hjust = 1,
                              color = "white"),
    plot.caption = element_text(family = "titlefont",
                                size = 52,
                                hjust = 0.5,
                                color = "white"),
    plot.margin = unit(c(1, 1, .5, 1), "cm")
  )

pdf(
  file = here::here("jesusmaria-lima-map-20191128.pdf"),
  title = "Map of Lima, Peru (@jmcastagnetto, 2011-11-28)",,
  height = 11.7,  # A3 size in inches
  width = 16.5
  )
print(jm_lima_map)
dev.off()
