employee_id <- c(10210, 24532, 56437, 20320, 67582)
first_name <- c("Betty", "Jason", "Shim", "Ravi", "Emily")
last_name <- c("Chu", "Fingleton", "Sung", "Shankar", "Weitz")
base_salary <- c(67000, 85000, 76000, 100000, 87000)
phone_no <- c("+1 (777) 555-1212", "+1 (732) 555-1212", NA, "+91 81487 03210", NA)
base_office <- c("New Jersey", NA, "New York", "Shankar", "Weitz")

employees <- data.frame(employee_id, first_name, last_name,
                        base_salary, phone_no, base_office)
knitr::kable(employees)

write_csv(employees, "lst-extras-datasets/employees.csv")
