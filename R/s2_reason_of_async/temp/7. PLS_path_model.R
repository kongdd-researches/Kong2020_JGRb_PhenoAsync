# load plspm
library(plspm)

# load dataset satisfaction
data(satisfaction)

# define path matrix (inner model)
IMAG <- c(0,0,0,0,0,0)
EXPE <- c(1,0,0,0,0,0)
QUAL <- c(0,1,0,0,0,0)
VAL <- c(0,1,1,0,0,0)
SAT <- c(1,1,1,1,0,0)
LOY <- c(1,0,0,0,1,0)
sat_path <- rbind(IMAG, EXPE, QUAL, VAL, SAT, LOY)

# define list of blocks (outer model)
sat_blocks <- list(1:5, 6:10, 11:15, 16:19, 20:23, 24:27)

# vector of modes (reflective indicators)
sat_modes <- rep("A", 6)

# apply plspm with bootstrap validation
satpls <- plspm(satisfaction, sat_path, sat_blocks, modes = sat_modes,
                scaled = FALSE, boot.val = TRUE)

# default print
satpls

# summary of results
summary(satpls)

oldpar <- par()
par(mfrow = c(3,1))
# plot inner model results
plot(satpls, what = "inner")

# plot outer model loadings
plot(satpls, what = "loadings")

# plot outer model weights
plot(satpls, what = "weights")
