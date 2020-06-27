#---------------------------------------
# This file makes the images for my site
# Should make two files:
#   cover.png
#   icon.png
#---------------------------------------

#----------
# Libraries
#----------
library(here)
library(ggplot2)
library(grid)

#-------------
# Read in data
#-------------
freqs <- read.csv(here::here("freqs.csv"))
freqs

#-------------
# Color Scheme
#-------------
background <- "#263121"

col <- c(
  "Overall" = "#85B258",
  "Data Science" = "#CAD3C1",
  "Background" = background
)

png(filename = here::here("images", "color_scheme.png"))
op <- par(mar = rep(0, 4))
plot(0, 0, pch = NA, axes = F,
     xlab = "", ylab = "",
     xlim = c(0, 1), ylim = c(0, 1),
     asp = 1)
for(i in seq_len(length(col))) {
  rect((i / length(col)) - (1 / length(col)), 0, (i / length(col)), 1,
       col = col[i], border = background)
}
par(op)
dev.off()

#-------
# Cover
#-------

# Flip one set of freqs to get back-to-back
freqs[freqs$Source == "Overall", ]$Frequency <-
  -freqs[freqs$Source == "Overall", ]$Frequency

# Actual plot
p <- ggplot(freqs, aes(x = rev(Letter), y = Frequency, fill = Source)) +
  geom_bar(stat = "identity", color = background, alpha = .5) +
  scale_fill_manual(values = col) +
  # No guides since just for looks
  guides(fill = FALSE) +
  # Double y-max limit size so the site's title doesn't block the image
  scale_y_continuous(expand = c(0, 0), limits = c(-15, 75), breaks = 0) +
  scale_x_discrete(limits = rev(levels(freqs$Letter))) +
  coord_flip() +
  theme(
    panel.background = element_rect(fill = background),
    text = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    title = element_blank())

# How to just get the plot
# http://stackoverflow.com/questions/14313285/ggplot2-theme-with-no-axes-or-grid
gt <- ggplot_gtable(ggplot_build(p))
ge <- subset(gt$layout, name == "panel")
grid.draw(gt[ge$t:ge$b, ge$l:ge$r])

# Cover needs to be 1920 x 1080 according to image used on demo site
png(filename = here::here("images", "cover.png"),
    width = 1920, height = 1080)
grid.draw(gt[ge$t:ge$b, ge$l:ge$r])
dev.off()

#-----------
# Icon
#-----------
# Undo "Flip one set of freqs to get back-to-back"
freqs[freqs$Source == "Overall", ]$Frequency <-
  -freqs[freqs$Source == "Overall", ]$Frequency

# reset plot
plot(0, 0,
     pch = NA, axes = F,
     xlab = "", ylab = "",
     xlim = c(0, 1), ylim = c(0, 1))

# Just use "Tyler"
freqs_sub <- freqs[freqs$Letter %in% c("t", "y", "l", "e", "r"), ]

# Get in right order
freqs_sub$Letter <- factor(freqs_sub$Letter,
                           levels = c("t", "y", "l", "e", "r"))
p <- ggplot(freqs_sub, aes(x = Letter, y = Frequency, fill = Source)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_color_manual(values = col) +
  scale_fill_manual(values = col) +
  geom_hline(yintercept = 0, color = background) +
  # No guides since just for looks
  guides(fill = FALSE) +
  # Double y-max limit size so the site's title doesn't block the image
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15), breaks = 0) +
  theme(
    # transparent background, so it matches any background
    panel.background = element_rect(fill = "transparent", color = NA),
    text = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    title = element_blank(),
    axis.line.x = element_line(color = background))

# http://stackoverflow.com/questions/14313285/ggplot2-theme-with-no-axes-or-grid
gt <- ggplot_gtable(ggplot_build(p))
ge <- subset(gt$layout, name == "panel")
grid.draw(gt[ge$t:ge$b, ge$l:ge$r])

# Site Icon needs to be 512x512 according to site design
# transparent background, so it matches any background
png(filename = here::here("images", "icon.png"),
    width = 512,
    height = 512,
    bg = "transparent")
grid.draw(gt[ge$t:ge$b, ge$l:ge$r])
dev.off()
