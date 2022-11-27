# rstudioapi::restartSession() # Remove in-memory detritus.

library(targets)

## Check pipeline
# tar_manifest(fields = all_of("command"))
# tar_visnetwork()
# tar_glimpse()
# tar_outdated()

tar_make()
beepr::beep("mario")
