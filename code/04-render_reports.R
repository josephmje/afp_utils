source("calc_final_awards.R")

final_award_df <-
  read_csv("data/final_award_v2.csv")

for (row in 1:nrow(final_award_df)) {
  user_id <- final_award_df[[row, "user_id_id"]]
  name <- sub(" ", "_", final_award_df[[row, "name"]])
  
  rmarkdown::render(input = "reports.qmd", output_file=paste0("reports/AFP_Preliminary_Report_", name, "_", Sys.Date(), ".html"), params = list(user = user_id))
}
