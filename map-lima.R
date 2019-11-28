library(tidyverse)
library(osmdata)
library(ggsn) # to put a north symbol in the map
library(showtext)
library(showtextdb)

load_showtext_fonts()
font_add_google("Cinzel Decorative", "titlefont")
font_add_google("Cutive Mono", "captionfont")
showtext_auto()

#available_tags("highway")
#available_features()


if (!file.exists("lima-map-sf.Rdata")) {
  lima_bb <- getbb("Lima Perú")

  streets <- lima_bb %>%
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

  small_streets <- lima_bb %>%
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

  trains <- lima_bb %>%
    opq() %>%
    add_osm_feature(
      key = "railway",
      value = c(
       # "abandoned",
      #  "preserved",
      #  "level crossing",
      #  "crossing",
        "rail" #,
       # "station"
      )
    ) %>%
    osmdata_sf()

  river <-  lima_bb %>%
    opq() %>%
    add_osm_feature(key = "waterway",
                    value = "river") %>%
    osmdata_sf()

  save(lima_bb,
       streets,
       small_streets,
       river,
       trains,
       file = here::here("lima-map-sf.Rdata"))
} else {
  load(here::here("lima-map-sf.Rdata"))
}


lima_map <- ggplot() +
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
  geom_sf(
    data = river$osm_lines,
    inherit.aes = FALSE,
    color = "white",
    alpha = .8,
    size = 2
  ) +
  geom_sf(
    data = trains$osm_lines,
    inherit.aes = FALSE,
    color = "yellow",
    alpha = .8,
    size = 1.2,
    linetype = "dotdash"
  ) +
  coord_sf(
    xlim = c(-77.091, -76.999),
    ylim = c(-12.081, -12.029),
    expand = FALSE
  ) +
  geom_rect(
    inherit.aes = FALSE,
    aes(
    xmin = -77.090,
    xmax = -77.000,
    ymin = -12.08,
    ymax = -12.03
    ),
    color = "yellow",
    fill = NA,
    size = .7
  ) +
  labs(
    title = "// @jmcastagnetto, Jesus M. Castagnetto, 2019-11-28 //",
    caption = "{ Lima, Perú }"
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
                                size = 64,
                                hjust = 0.5,
                                color = "white"),
    plot.margin = unit(c(1, 1, .5, 1), "cm")
  )

pdf(
  file = here::here("lima-map-20191128.pdf"),
  title = "Map of Lima, Peru (@jmcastagnetto, 2011-11-28)",,
  height = 11.7,  # A3 size in inches
  width = 16.5
  )
print(lima_map)
dev.off()

ggsave(
  lima_map,
  filename = here::here("lima-map-20191128-a4.png"),
  width = 11.7,  # A4 size in inches
  height = 8.3
)

ggsave(
  lima_map,
  filename = here::here("lima-map-20191128-a5.png"),
  width = 8.3,  # A5 size in inches
  height = 5.8
)

