plot_weights <- function(w, nm) {
  bp <- barplot(w, names.arg = nm, ylim = c(0, 1))
  text(bp, 1, round(w, 2), xpd = TRUE)
}  
