#===============================================================================
# SORTEE code club - Decmeber 2025: Reproducubility bingo 
#===============================================================================

# Specify the prompts
prompts <- c("1",
             "2",
             "3", 
             "4",
             "5",
             "6",
             "7",
             "8",
             "9",
             "10",
             "11",
             "12",
             "13",
             "14",
             "15",
             "16",
             "17",
             "18",
             "19",
             "20",
             "21",
             "22",
             "23",
             "24",
             "25",
             "26",
             "27",
             "28",
             "29",
             "30")

# Alternative if we will just use numbers on the bingo sheet
n_promopts <- 30
prompts <- 1:n_prompts

# assign prompts to a n^2 matrix/bingo card  
n <- 5
  
bingo_card <- matrix(sample(prompts, n^2, replace = FALSE), 
                     nrow = n, ncol = n)

# Specify directory where to save the bingo card
wd <- getwd()

# Save as PDF
pdf(file = paste(wd,"/SORTEE_code_club_reproducibility_bingo.pdf", sep = ""),
      width = 14, height = 14)

# Set up empty plot with equal aspect ratio
plot(1:n, 1:n, type = "n", axes = FALSE, xlab = "", ylab = "", asp = 1,
     xlim = c(0.5, n + .5), ylim = c(0.5,n + .5), 
     main = "SORTEE December 2025 reproducibility bingo",
     cex.main = 1.5)

# Draw grid lines
for(i in 0:n) {
  lines(c(0.5, n + .5), c(i + 0.5, i + 0.5))  # horizontal
  lines(c(i + 0.5, i + 0.5), c(0.5, n + .5))  # vertical
}

# Add text labels
for(i in 1:n){
  for(j in 1:n){
    text(j, n+1 - i, bingo_card[i, j], cex = 1.5, col = "black")
  }
}

dev.off()

#===============================================================================
