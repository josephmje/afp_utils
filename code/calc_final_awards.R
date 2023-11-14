library(dplyr)
library(glue)
library(readr)
library(tidyr)

rtf_value <- 2000426.98

user_df <-
  read_csv(here("data/db/accounts_customuser_202301281711.csv")) %>%
  select(id, first_name, last_name) %>%
  mutate(name = glue("{first_name} {last_name}"))

## Promotions

promotion_df <-
  read_csv(here("data/db/claims_promotion_202301281711.csv")) %>%
  select(user_id_id, promoted_to_id, eligible, decision_comments) %>%
  mutate(
    value = case_when(
      eligible == 1 & promoted_to_id == 3 ~ 2000,
      eligible == 1 & (promoted_to_id == 4 | promoted_to_id == 5) ~ 10000,
      eligible == 0 ~ 0
    )
  ) %>%
  left_join(
    read_csv(here("data/db/accounts_rank_202301281711.csv")), by=c("promoted_to_id" = "id")
  ) %>%
  rename(promoted_to = name) %>%
  select(user_id_id, promoted_to, eligible, value, decision_comments) %>%
  arrange(user_id_id)

promotion_value <- sum(promotion_df$value, na.rm = TRUE)

## Awards

award_df <-
  read_csv(here("data/db/claims_award_202301281711.csv")) %>%
  select(user_id_id, award_level_id, name, organization, cash_prize, eligible, decision_comments) %>%
  left_join(
    read_csv(here("data/db/claims_awardlevel_202301281711.csv")), by=c("award_level_id" = "id")
  ) %>%
  rename(award_name = name.x,
         award_level = name.y) %>%
  select(user_id_id, award_level, award_name, organization, cash_prize, eligible, value, decision_comments) %>%
  mutate(value = eligible * value) %>%
  arrange(user_id_id, award_level)

award_by_physician <-
  award_df %>%
  group_by(user_id_id) %>%
  summarize(total_value = sum(value, na.rm = TRUE))

award_value <- sum(award_by_physician$total_value, na.rm = TRUE)

## CPAs

cpa_df <-
  read_csv(here("data/db/claims_cpa_202301281711.csv")) %>%
  rename(value = cpa_value) %>%
  select(user_id_id, cpa_file, comments, eligible, value, decision_comments)

cpa_by_physician <-
  cpa_df %>%
  group_by(user_id_id) %>%
  summarize(total_value = sum(value, na.rm = TRUE))

cpa_value <- sum(cpa_by_physician$total_value, na.rm = TRUE)

rtf_prime <- rtf_value - promotion_value - cpa_value - award_value

research_value = rtf_prime * (2/3)

## Grant Reviews

grantreview_df <-
  read_csv(here("data/db/claims_grantreview_202301281711.csv")) %>%
  select(user_id_id, type_id, agency, date, is_member, num_days, num_reviewed, eligible, decision_comments) %>%
  left_join(
    read_csv(here("data/db/claims_grantreviewtype_202301281711.csv")), by=c("type_id" = "id")
  ) %>%
  rename(review_type = name) %>%
  mutate(
    value = case_when(
      eligible == 1 & type_id == 1 ~ weight * num_days,
      eligible == 1 & (type_id == 2 | type_id == 3) ~ weight,
      eligible == 1 & type_id == 4 ~ weight / 12 * num_days,
      eligible == 0 ~ 0
    )
  ) %>%
  select(user_id_id, review_type, agency, date, is_member, num_days, num_reviewed, eligible, value, decision_comments)

grantreview_by_physician <-
  grantreview_df %>%
  group_by(user_id_id) %>%
  summarize(total_value = sum(value, na.rm = TRUE)) %>%
  filter(!is.na(total_value))

grantreview_value <- sum(grantreview_by_physician$total_value, na.rm = TRUE)

## Editorial Boards

editorialboard_df <-
  read_csv(here("data/db/claims_editorialboard_202301281711.csv")) %>%
  select(user_id_id, journal_id, other_journal_name, eligible, decision_comments) %>%
  left_join(
    read_csv(here("data/db/claims_journal_202301281711.csv")),
    by=c("journal_id" = "id")
  ) %>%
  mutate(
    value = case_when(
      eligible == 1 ~ impact_factor * 100,
      eligible == 0 ~ 0
    )
  ) %>%
  rename(journal = name) %>%
  select(user_id_id, journal, other_journal_name, eligible, value, decision_comments)

editorialboard_df[editorialboard_df$journal == "Cochrane Database Syst Rev", "value"] <- 400

editorialboard_by_physician <-
  editorialboard_df %>%
  group_by(user_id_id) %>%
  summarize(total_value = sum(value, na.rm = TRUE)) %>%
  filter(!is.na(total_value))

editorialboard_value <- sum(editorialboard_by_physician$total_value, na.rm = TRUE)

## Books

book_df <-
  read_csv(here("data/db/claims_publicationlink_202301281711.csv")) %>%
  select(-c(id, created_at, modified_at)) %>%
  left_join(
    read_csv(here("data/db/claims_publication_202301281711.csv")) %>% 
      select(id, pub_type_id, title, chapter_title, publisher, city, pub_year),
    by=c("publication_id" = "id")
  ) %>%
  filter(pub_type_id %in% c(3, 4, 5)) %>%
  left_join(
    read_csv(here("data/db/claims_publicationtype_202301281711.csv")),
    by=c("pub_type_id" = "id")
  ) %>%
  mutate(
    value = case_when(
      eligible == 1 ~ weight,
      eligible == 0 ~ 0
    )
  ) %>%
  rename(pub_type = name) %>%
  select(user_id_id, pub_type, title, chapter_title, publisher, city, pub_year, eligible, value, decision_comments)

book_by_physician <-
  book_df %>%
  group_by(user_id_id) %>%
  summarize(total_value = sum(value, na.rm = TRUE)) %>%
  filter(!is.na(total_value))

book_value <- sum(book_by_physician$total_value, na.rm = TRUE)

research_prime = research_value - grantreview_value - book_value - editorialboard_value

publication_value <- research_prime * 0.6
grant_value <- research_prime - publication_value

## Journal Articles & Conferences

article_df <-
  read_csv(here("data/db/claims_publicationlink_202301281711.csv")) %>%
  select(-c(id, created_at, modified_at)) %>%
  left_join(
    read_csv(here("data/db/claims_publication_202301281711.csv")) %>% 
      select(id, pub_type_id, article_type_id, pmid, title, journal_id, other_journal_name, volume, issue, is_epub, pub_year, conf_name, conf_date),
    by=c("publication_id" = "id")
  ) %>%
  filter(pub_type_id %in% c(1, 2)) %>%
  left_join(
    read_csv(here("data/db/claims_publicationtype_202301281711.csv")) %>%
      rename(pub_type = name,
             pub_type_weight = weight),
    by=c("pub_type_id" = "id")
  ) %>%
  left_join(
    read_csv(here("data/db/claims_publicationrole_202301281711.csv")) %>%
      rename(pub_role = name,
             pub_role_weight = weight),
    by=c("role_id" = "id")
  ) %>%
  left_join(
    read_csv(here("data/db/claims_articletype_202301281711.csv")) %>%
      rename(article_type = name,
             article_weight = weight),
    by=c("article_type_id" = "id")
  ) %>%
  left_join(
    read_csv(here("data/db/claims_journal_202301281711.csv")) %>%
      rename(journal = name),
    by=c("journal_id" = "id")
  ) %>%
  mutate(
    value = case_when(
      eligible == 1 & pub_type_id == 2 & role_id == 1 ~ pub_type_weight * pub_role_weight,
      eligible == 1 & pub_type_id == 1 & is_corresponding == TRUE ~ pub_type_weight * article_weight * (pub_role_weight + 1) * impact_factor,
      eligible == 1 & pub_type_id == 1 & is_corresponding == FALSE ~ pub_type_weight * article_weight * pub_role_weight * impact_factor,
      eligible == 0 ~ 0
    )
  ) %>%
  select(user_id_id, pub_type, pub_role, is_corresponding, article_type, pmid, title, journal, other_journal_name, impact_factor, volume, issue, is_epub, pub_year, conf_name, conf_date, eligible, value, decision_comments)

article_by_physician <-
  article_df %>%
  group_by(user_id_id) %>%
  summarize(total_value = sum(value, na.rm = TRUE)) %>%
  filter(!is.na(total_value))

article_points <- sum(article_by_physician$total_value)

article_by_physician <-
  article_by_physician %>%
  mutate(total_dollar = (publication_value/article_points) * total_value)

## Grants

grant_df <-
  read_csv(here("data/db/claims_grantlink_202301281711.csv")) %>%
  select(-c(id, created_at, modified_at)) %>%
  left_join(
    read_csv(here("data/db/claims_grant_202301281711.csv")) %>%
      select(id, name, amount, agency_id, other_grant_agency, start_date, end_date, at_camh) %>%
      rename(grant_title = name),
    by=c("grant_id" = "id")
  ) %>%
  left_join(
    read_csv(here("data/db/claims_grantrole_202301281711.csv")) %>%
      rename(grant_role = name,
             grant_weight = weight),
    by=c("role_id" = "id")
  ) %>%
  left_join(
    read_csv(here("data/db/claims_grantagency_202301281711.csv")) %>%
      rename(grant_agency = name,
             agency_weight = category_id) %>%
      mutate(agency_weight = recode(agency_weight, `1`=3, `2`=2, `3`=1)),
    by=c("agency_id" = "id")
  ) %>%
  mutate(
    value = case_when(
      eligible == 1 ~ agency_weight * grant_weight,
      eligible == 0 ~ 0
    )
  ) %>%
  select(user_id_id, grant_role, grant_title, grant_agency, other_grant_agency, agency_weight, amount, start_date, end_date, at_camh, eligible, value, decision_comments)

grant_by_physician <-
  grant_df %>%
  group_by(user_id_id) %>%
  summarize(total_value = sum(value)) %>%
  filter(!is.na(total_value))

grant_points <- sum(grant_by_physician$total_value)

grant_by_physician <-
  grant_by_physician %>%
  mutate(total_dollar = (grant_value/grant_points) * total_value)

# Teaching

teaching_value = rtf_prime - research_value

## Committee Work

committee_df <-
  read_csv(here("data/db/claims_committeework_202301281711.csv")) %>%
  select(user_id_id, name, hours, eligible, decision_comments) %>%
  mutate(ty1 = case_when(
    eligible == 1 ~ hours * 75,
    eligible == 0 | eligible == -2 ~ 0)
  )

committee_by_physician <-
  committee_df %>%
  group_by(user_id_id) %>%
  summarize(ty1 = sum(ty1)) %>%
  filter(!is.na(ty1))

## Exams

exam_df <-
  read_csv(here("data/db/claims_exam_202301281711.csv")) %>%
  select(user_id_id, exam_type_id, other_exam_name, student_name, hours, date, eligible, decision_comments) %>%
  left_join(
    read_csv(here("data/db/claims_examtype_202301281711.csv")) %>%
      rename(exam_type = name,
             exam_weight = weight),
    by=c("exam_type_id" = "id")
  ) %>%
  mutate(value = hours * eligible * exam_weight)

exam_by_physician <-
  exam_df %>%
  group_by(user_id_id, exam_type) %>%
  summarize(total_value = sum(value)) %>%
  filter(!is.na(total_value)) %>%
  pivot_wider(names_from = exam_type, values_from = total_value) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(ex = sum(`STACER`, `OSCE/SAQ Marker`, `MCQ`, `Graduate degree defense exam (SGS)`, `Departmental oral`, `Mock oral`, `Royal College OSCE`, na.rm = TRUE),
         ty2 = sum(`UofT medical student admission interviewer or file reviewer`, `CaRMS interviewer or file reviewer`, `Subspecialty post graduate admissions interviewer`, na.rm = TRUE)) %>%
  select(user_id_id, ex, ty2)

## Lectures

lecture_df <-
  read_csv(here("data/db/claims_lecture_202301281711.csv")) %>%
  select(user_id_id, lecture_type_id, other_lecture_type, name, course_code, start_date, end_date, is_cash, hours, num_sessions, eligible, decision_comments) %>%
  left_join(
    read_csv(here("data/db/claims_lecturetype_202301281711.csv")) %>%
      rename(lecture_type = name,
             lecture_weight = weight),
    by=c("lecture_type_id" = "id")
  ) %>%
  mutate(value = case_when(
    eligible == 1 ~ hours * num_sessions * lecture_weight,
    eligible == 0 | eligible == -2 ~ 0
  )
  )  

lecture_by_physician <-
  lecture_df %>%
  group_by(user_id_id, lecture_type) %>%
  summarize(total_value = sum(value)) %>%
  filter(!is.na(total_value)) %>%
  pivot_wider(names_from = lecture_type, values_from = total_value) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(lv = sum(`Postgraduate Core Lecture`, `Mandatory Medical Student Lecture`, `UofT Numbered Course`, `Grand Rounds at CAMH`, na.rm = TRUE),
         ty3 = sum(`Other Postgraduate Lecture`, `Elective Medical Student Lecture`, `Accredited CME`, na.rm = TRUE),
         xw = sum(`Community Lecture`, `Other Lecture`, na.rm = TRUE)) %>%
  select(user_id_id, lv, ty3, xw)

## Supervision

supervision_df <-
  read_csv(here("data/db/claims_supervision_202301281711.csv")) %>%
  select(user_id_id, supervision_type_id, student_name, duration, hours, frequency_id, eligible, decision_comments) %>%
  left_join(
    read_csv(here("data/db/claims_supervisiontype_202301281711.csv")) %>%
      rename(supervision_type = name,
             supervision_weight = weight),
    by=c("supervision_type_id" = "id")
  ) %>%
  left_join(
    read_csv(here("data/db/claims_workfrequencytype_202301281711.csv")) %>%
      select(id, days_equal),
    by=c("frequency_id" = "id")
  ) %>%
  mutate(value = case_when(
    eligible == 1 & (supervision_type_id == 1 | supervision_type_id == 2 | supervision_type_id == 4 | supervision_type_id == 5 | supervision_type_id == 6 | supervision_type_id == 10) ~ duration * (days_equal/5) * supervision_weight,
    eligible == 1 & supervision_type_id == 3 & hours <= 4 ~ (hours / 4) * supervision_weight,
    eligible == 1 & supervision_type_id == 3 & hours > 4 ~ 1 * supervision_weight,
    eligible == 1 & supervision_type_id == 8 ~ (duration / 3) * supervision_weight,
    eligible == 1 & supervision_type_id == 9 ~ (duration / 2) * supervision_weight,
    eligible == 1 & supervision_type_id == 11 ~ (duration * (days_equal/5) / 12) * supervision_weight,
    eligible == 1 & supervision_type_id == 12  ~ 1 * supervision_weight, 
    eligible == 0 | eligible == -2 ~ 0,
  ))

supervision_by_physician <-
  supervision_df %>%
  group_by(user_id_id, supervision_type) %>%
  summarize(total_value = sum(value)) %>%
  filter(!is.na(total_value)) %>%
  pivot_wider(names_from = supervision_type, values_from = total_value) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(d = `Graduate Program Advisory Committee (PAC)`,
         s = sum(`Master's Student`, `PhD Student`, `Research Fellow`, `CSS/CSP Resident`, `Resident Remediation`, na.rm = TRUE),
         r = `Resident Research Elective`,
         rho1 = `Resident Quality Improvement Project`,
         rho2 = `CBD Coaching`,
         u = `Medical Student Core/LInC`,
         mu = `Medical Student/IMG Elective`) %>%
  mutate(s = ifelse(s > 12000, 12000, s)) %>%
  select(user_id_id, d, s, r, rho1, rho2, u, mu)

teaching_by_physician <-
  user_df %>%
  rename(user_id_id = id) %>%
  select(user_id_id, name) %>%
  left_join(committee_by_physician) %>%
  left_join(exam_by_physician) %>%
  left_join(lecture_by_physician) %>%
  left_join(supervision_by_physician) %>%
  rowwise() %>%
  mutate(
    rho = sum(rho1, rho2, na.rm = TRUE),
    ty = sum(ty1, ty2, ty3, na.rm = TRUE),
    core_teaching = sum(ex, d, s, rho, lv, u, na.rm = TRUE),
    noncore_teaching = sum(r, mu, ty, xw, na.rm = TRUE))

core_teaching_points <- sum(teaching_by_physician$core_teaching)
noncore_teaching_points <- sum(teaching_by_physician$noncore_teaching)

core_teaching_value = teaching_value * 0.88
noncore_teaching_value = teaching_value - core_teaching_value

teaching_by_physician <-
  teaching_by_physician %>%
  mutate(`Core Teaching` = (core_teaching_value/core_teaching_points) * core_teaching,
         `Non-core Teaching` = (noncore_teaching_value/noncore_teaching_points) * noncore_teaching) %>%
  select(user_id_id, core_teaching:`Non-core Teaching`)

final_award_df <-
  user_df %>%
  rename(user_id_id = id) %>%
  select(user_id_id, name) %>%
  left_join(promotion_df %>% select(user_id_id, value)) %>%
  rename(`Promotion` = value) %>%
  left_join(award_by_physician) %>%
  rename(`Awards` = total_value) %>%
  left_join(cpa_by_physician) %>%
  rename(`CPA` = total_value) %>%
  left_join(grantreview_by_physician) %>%
  rename(`Grant Reviews` = total_value) %>%
  left_join(editorialboard_by_physician) %>%
  rename(`Editorial Boards` = total_value) %>%
  left_join(book_by_physician) %>%
  rename(`Books` = total_value) %>%
  left_join(article_by_physician) %>%
  rename(`Publication Points` = total_value,
         `Publications` = total_dollar) %>%
  left_join(grant_by_physician) %>%
  rename(`Grant Points` = total_value,
         `Grants` = total_dollar) %>%
  left_join(teaching_by_physician) %>%
  rowwise() %>%
  mutate(`Final Award` = sum(`Promotion`, `Awards`, `CPA`, `Grant Reviews`, `Editorial Boards`, `Books`, `Publications`, `Grants`, `Core Teaching`, `Non-core Teaching`, na.rm = TRUE)) %>%
  arrange(desc(`Final Award`))
