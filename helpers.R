percent_map <- function(var, legend.title, min = 0, max = 100) {
  
  # generate vector of fill colors for map
  shades <- colorRampPalette(c("white", "darkgreen"))(5)
  inc <- (max - min) / 4
  
  percents <- as.integer(cut(var, c(min, min+inc, min+2*inc, min+3*inc, min+4*inc, 100),
                             include.lowest = TRUE, ordered = TRUE, include.highest = TRUE))
  
  fills <- shades[percents]
  print(percents)
  
  gadm <- readRDS("data/gadm/gadm41_DEU_1_pk.rds")
  plot(gadm, col = fills, border = 'black')
  
  # add a legend
  legend.text <- c(paste0(min, "-", min+inc, "%"),
                   paste0(min + inc, "-", min + 2 * inc, "%"),
                   paste0(min + 2 * inc, "-", min + 3 * inc, "%"),
                   paste0(min + 3 * inc, "-", min + 4 * inc, "%"),
                   paste0(">", max, "%"))
  
  legend("bottomright", 
         legend = legend.text, 
         fill = shades, 
         title = legend.title)
}