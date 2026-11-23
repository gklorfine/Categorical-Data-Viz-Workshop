library(ggfourfold)
library(ggmosaic2)
library(ggplot2)
library(vcdExtra)
library(patchwork)
library(magick)

dpi <- 300

# logist_density for Donner data grouped by age
lgp <- logist_plot(
   survived ~ age, 
   data = Donner, 
   group = "sex",
   marginal = "density",
   marginal.args = list(alpha = 0.35, linewidth = 0.6)
) +
   labs(
      x = "Age", 
      y = "Pr(Survived)", 
      color = "Sex", 
      fill = "Sex"
   ) +  
   theme(
      text = element_text(family = "serif", size = 13),
      legend.position = "none"
   )

# Mosaic of HEC data
hec_mos <- HairEyeColor |> 
   as.data.frame() |>
   ggplot(aes(x = product(Sex, Eye, Hair), weight = Freq)) +
   mosaic_settings(expected = "independence") +
   geom_mosaic() +
   scale_fill_residual(limits = c(-4, 4)) +
   theme_mosaic(rot_labels = 45) +
   theme(
      text = element_text(family = "serif"),
      legend.position = "none"
   )

# FF display of Berkley data (unstratified)
ff <- UCBAdmissions |>
   as.data.frame() |>
   ggplot(aes(x = Gender, y = Admit, weight = Freq)) +
   geom_fourfold() +
   theme_fourfold(base_family = "serif")

# Combine via patchwork
title_pic <- lgp + hec_mos + ff

# Save image
ggsave("images/title-pic.png", title_pic, dpi = 300, width = 8, height = 4, units = "in")

image_read("images/title-pic.png") |>
   image_trim() |>
   image_border("white", "20x20") |>   # optional: add back a small even margin
   image_write("images/title-pic.png")
