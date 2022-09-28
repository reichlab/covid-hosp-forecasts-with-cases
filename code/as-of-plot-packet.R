source("code/as-of-data-plots.R")

as_of_dates <- as.character(seq.Date(as.Date("2021-01-04"), as.Date("2022-07-26"), by="7 days")) 
states <- c("ca", "ma")
pdf(file = "plots/all-dates-as-of.pdf", height=5, width=8)
for (state in states){
  for(as_of_date in as_of_dates){
    try(print(plot_as_of_case_data(as_of_date = as_of_date, state=state)))
  }
}
dev.off()