library(readxl)
library(writexl)
#csv <- read.csv("metaframework.csv")
#writexl::write_xlsx(csv,"metaframework.xlsx")

c <- read_excel("metaframework.xlsx")
write.csv(c, "metaframework.csv")
