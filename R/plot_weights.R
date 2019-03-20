plot_weights <- function(w, nm) {
  bp <- graphics::barplot(w, names.arg = nm, ylim = c(0, 1), xlab = "Risk factor", ylab = "Weight")
  text(bp, 1, round(w, 2), xpd = TRUE)
}  
