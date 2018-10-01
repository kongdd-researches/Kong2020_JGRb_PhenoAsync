# load plspm
library(plspm)
library(purrr)
library(magrittr)

# define path matrix (inner model)
APAR    <- c(0, 0, 0) 
epsilon <- c(0, 0, 0)
GPP     <- c(1, 1, 0)

# define list of blocks (outer model)
sat_path   <- rbind(APAR, epsilon, GPP)
sat_blocks <- list(c("EVI", "Rs"), c("T", "VPD", "Prcp"), "GPP") %>%
    map(~match(.x, colnames(d)))

# vector of modes (reflective indicators)
sat_modes <- rep("B", length(sat_blocks))

# apply plspm with bootstrap validation
satpls <- plspm(d, sat_path, sat_blocks, modes = sat_modes,
                scaled = FALSE, boot.val = TRUE)

# default print
satpls

# plot inner model results
plot(satpls, what = "inner")

# plot outer model loadings
plot(satpls, what = "loadings")

# plot outer model weights
plot(satpls, what = "weights")