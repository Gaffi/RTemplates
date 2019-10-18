library(roxygen2)
library(devtools)
detach('package:RTemplates', unload=T)
setwd("z:/Utilities/R/Templates/RTemplates")
document()

setwd("z:/Utilities/R/Templates")
install("RTemplates")
