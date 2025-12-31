# helper-fun
Functions that make my life easier
=================================

## Installation

```r
# Install from local source
devtools::install("packages/helper-fun")
```
## Usage

```r
library(helper-fun)
library(ggplot2)

# View all available TG corporate colors
tg_corp_color()

# Get specific colors
tg_corp_color("green", "royal_blue")

# View available palettes
tg_corp_palette("all")

# Use in ggplot2
ggplot(mtcars, aes(x = mpg, y = hp, color = factor(cyl))) +
  geom_point(size = 3) +
  scale_color_tg()

# Fill scale
ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() +
  scale_fill_tg(palette = "ae")
```

## Available Palettes

| Palette | Colors | Use Case |
|---------|--------|----------|
| `main` | green, royal_blue, navy_blue | Primary visualizations |
| `highlight` | green, ice_blue | Highlighting key data |
| `ae` | sky_blue, warm_gray, yellow, red | Adverse event severity |

Each individual color (e.g., `"ice_blue"`, `"green"`) also has a ramped hue palette available.

## Color Reference

| Color | Hex Code |
|-------|----------|
| ice_blue | #BBCBD3 |
| navy_blue | #121B4D |
| green | #99CC33 |
| sky_blue | #3EA7F3 |
| royal_blue | #003494 |
| warm_gray | #909FA7 |
| yellow | #FEE568 |
| red | #FF2600 |

## Functions

- `tg_corp_color()` - Extract TG colors as hex codes
- `ramp_hues()` - Generate color ramps from a center color
- `tg_corp_palette()` - Get predefined color palettes
- `palette_gen()` - Generate a palette function for n colors
- `scale_color_tg()` - ggplot2 color scale
- `scale_fill_tg()` - ggplot2 fill scale