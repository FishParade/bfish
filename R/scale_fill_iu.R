# Function for applying IU colors.
iu_cols <- c("#990000",
             "#006298",
             "#DC8823",
             "#512A44",
             "#285C4D",
             "#191919",
             "#4A3C31",
             "#4C1213",
             "#83786F",
             "#DD0031",
             "#01426A",
             "#A38E9C")

scale_fill_iu <- function() {
  scale_fill_manual(values = iu_cols)
}
