# ------------------------------------------------------------
library(dplyr)
library(tibble)

set.seed(42)

# --- Grade scale (letter <-> points) ---
grade_scale <- tibble(
  student_grade = c("F","D","D+","C-","C","C+","B-","B","B+","A-","A"),
  grade_points  = c(0,  1,  1.33, 1.67, 2,  2.33, 2.67, 3,  3.33, 3.67, 4)
)

# --- Professors: "toughness" factor (0.75 tough ... 1.25 easy) ---
profs <- tibble(
  prof_id        = c("P01","P02","P03","P04","P05"),
  prof_name      = c("Elena Park","Raj Singh","Maria Lopez","Kenji Tanaka","Aisha Khan"),
  prof_toughness = c(1.25, 1.20, 1.00, 0.85, 0.75)
)

# --- Courses (each course has ONE professor) ---
courses <- tibble(
  course_id   = c("C101","C102","C103","C104",  # easy-ish profs
                  "C201","C202","C207","C208",  # average prof
                  "C203","C204","C205","C206"), # tough profs
  course_name = c("Acct I","Marketing","Microecon","Stats I",
                  "Finance","Ops Mgmt","HR Mgmt","Business Analytics",
                  "Management","Business Law","Info Systems","Strategy"),
  prof_id     = c("P01","P02","P01","P02",
                  "P03","P03","P03","P03",
                  "P04","P05","P04","P05")
) %>%
  left_join(profs, by = "prof_id")

# ------------------------------------------------------------
# Enrollment plan (5 students × 4 courses = 20 rows)
# Designed ranks:
#   Raw GPA:      S01 > S02 > S03 > S05 > S04
#   Adjusted GPA: S03 > S02 > S04 > S05 > S01   (S03 becomes #1)
# ------------------------------------------------------------

enrollments <- tibble(
  student_id  = rep(c("S01","S02","S03","S04","S05"), each = 4),
  course_id   = c(
    # S01: mostly easy graders
    "C101","C102","C103","C104",
    # S02: average graders
    "C201","C202","C207","C208",
    # S03: tough graders (two 0.85 and two 0.75)
    "C203","C204","C205","C206",
    # S04: tough graders, weaker raw
    "C203","C204","C205","C206",
    # S05: mixed
    "C101","C201","C203","C202"
  ),
  grade_points = c(
    # S01 raw (highest): great grades, but from easy graders
    3.67, 3.67, 3.33, 3.67,     # avg = 3.585
    
    # S02 raw (second): strong grades from average graders
    3.67, 3.33, 3.33, 3.33,     # avg = 3.415
    
    # S03 raw (third): good-not-great grades from tough graders
    3.00, 3.00, 3.00, 3.00,     # avg = 3.000  (but adjusted will jump)
    
    # S04 raw (fifth): lower grades even with tough graders
    2.33, 2.67, 2.33, 2.67,     # avg = 2.500
    
    # S05 raw (fourth): mixed schedule, mid raw
    3.33, 3.00, 2.67, 3.00      # avg = 3.000 (tie broken by adjusted)
  )
)

# Build dataset + compute adjusted points (uncapped)
gpa_data <- enrollments %>%
  left_join(grade_scale, by = "grade_points") %>%
  left_join(
    courses %>% select(course_id, course_name, prof_id, prof_toughness),
    by = "course_id"
  ) %>%
  mutate(adjusted_points = grade_points / prof_toughness) %>%
  select(student_id, course_id, course_name, prof_id,
         student_grade, grade_points,
         prof_toughness, adjusted_points)

gpa_data

# Summaries + ranks
gpa_summary <- gpa_data %>%
  group_by(student_id) %>%
  summarise(
    raw_gpa      = mean(grade_points),
    adjusted_gpa = mean(adjusted_points),
    .groups = "drop"
  ) %>%
  mutate(
    raw_rank      = dense_rank(desc(raw_gpa)),
    adjusted_rank = dense_rank(desc(adjusted_gpa))
  ) %>%
  arrange(raw_rank)

gpa_summary

gpa_data |> 
  select(-prof_toughness, -adjusted_points) |> 
  write_csv("gpa_data_basic.csv")

gpa_data |> 
  write_csv("gpa_data_adjusted.csv")


gpa_data_basic <- read_csv("gpa_data_basic.csv")
gpa_data_adjusted <- read_csv ("gpa_data_adjusted.csv")

gpa_data_basic |> 
  summarize(gpa = mean(grade_points), .by = student_id) |> 
  arrange(-gpa) |> 
  kable()

gpa_data_adjusted |> 
  summarize(adj_gpa = mean(adjusted_points), .by = student_id) |> 
  arrange(-adj_gpa) |> 
  kable()
