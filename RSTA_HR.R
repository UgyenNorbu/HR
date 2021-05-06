library(tidyverse)
library(readxl)
library(ggtheme)
library(lubridate)
library(ggridges)
library(viridis)
library(hrbrthemes)

# old_code ----------------------------------------------------------------

# data_RSTA <- read_xlsx("data/RSTA_HR.xlsx")
# data_DIT <- read_xlsx("data/RSTA_HR.xlsx", sheet = 2)
# data_DoIM <- read_xlsx("data/RSTA_HR.xlsx", sheet = 3)
# test_data <- read_xlsx("data/RSTA_HR.xlsx", sheet = 4)
# 
# test_data <- test_data %>%
#     select(name, DoB)
# # 
# colnames(test_data) <- c("name", "DoB_1")
# # 
# all_RSTA <- left_join(all_RSTA, test_data, by = "name")
# # 
# all_RSTA <- all_RSTA %>%
#     select(!c(DoB, appointment, Remarks))

# 
# master_list <- all_RSTA %>%
#     filter(grepl("Master", qualification, ignore.case = TRUE)) %>%
#     select(qualification) %>%
#     unique() %>%
#     pull()
# 
# bachelor_list <- all_RSTA %>%
#     filter(grepl("Bachelor|P.G|B.A|PGD|BA", qualification,
#                  ignore.case = TRUE)) %>%
#     select(qualification) %>%
#     unique() %>%
#     pull()
# 
# diploma <- all_RSTA %>%
#     filter(grepl("^Diploma", qualification, ignore.case = TRUE)) %>%
#     select(qualification) %>%
#     unique() %>%
#     pull()
# 
# diploma <- append(diploma, "Automobile Mechanics (NC2)")
# 
# certificate <- all_RSTA %>%
#     filter(grepl("^Certificate|Cetificate", qualification,
#                  ignore.case = TRUE)) %>%
#     select(qualification) %>%
#     unique() %>%
#     pull()
# 
# school_list <- all_RSTA %>%
#     filter(grepl("Class", qualification, ignore.case = TRUE)) %>%
#     select(qualification) %>%
#     unique() %>%
#     pull()
# 
# school_list <- append(school_list, "Junior Education")
# 
# final_RSTA$new_qual <- 0
# 
# all_RSTA <- all_RSTA %>%
#     mutate(new_qual = ifelse(qualification %in% master_list,
#                              "Masters", new_qual)) %>%
#     mutate(new_qual = ifelse(qualification %in% bachelor_list, "Bachelors", new_qual)) %>%
#     mutate(new_qual = ifelse(qualification %in% diploma,
#                              "Diploma", new_qual)) %>%
#     mutate(new_qual = ifelse(qualification %in% certificate, "Certificate", new_qual)) %>%
#     mutate(new_qual = ifelse(qualification %in% school_list,
#                              "School", new_qual))
# 

# new_data ----------------------------------------------------------------
all_RSTA <- read_xlsx("final_RSTA_staff.xlsx")

# all_RSTA <- all_RSTA %>% 
#     mutate(age = year(Sys.Date()) -year(DoB_1))
all_RSTA$DoB_1 <- as.Date(all_RSTA$DoB_1, "%Y-%m-%d")
str(all_RSTA)


all_RSTA$PL <- factor(all_RSTA$PL, levels = c("EX2", "P1", "P2","P3", 
    "P4", "P5","SS1", "SS2", "SS3", "SS4", "S1", "S2", "S3", "S4", 
    "S5", "O1", "O2", "O3", "O4", "ESP", "GSP I", "GSP II"))

all_RSTA$new_qual <- factor(all_RSTA$new_qual,
    levels = c("Masters", "Bachelors", "Diploma", "Certificate", "School"))

# EX <- "EX2"
# PnM <- c("P1", "P2","P3", "P4", "P5","SS1", "SS2", "SS3", "SS4")
# SS <- c("S1", "S2", "S3", "S4", "S5")
# Olevel <- c("O1", "O2", "O3", "O4")
# 
# all_RSTA <- all_RSTA %>% 
#     mutate(grade = 0) %>% 
#     mutate(grade = ifelse(PL %in% EX, "Executive", grade)) %>% 
#     mutate(grade = ifelse(PL %in% PnM, "Prof. and Management", grade)) %>% 
#     mutate(grade = ifelse(PL %in% SS, "Support Section", grade)) %>% 
#     mutate(grade = ifelse(PL %in% Olevel, "Operational", grade)) %>% 
#     mutate(grade = ifelse(grade == 0, "ESP/GSP", grade)) 

all_RSTA$grade <- factor(all_RSTA$grade,
                         levels = c("Executive", "Prof. and Management","Support Section",
                                    "Operational", "ESP/GSP"))

all_RSTA %>% 
    group_by(office) %>% 
    tally() %>%
    ggplot(aes(reorder(x = office, -n), y = n)) +
    geom_bar(stat = "identity", fill = "#BCD2EE", width = 0.5) +
    coord_flip() +
    ylim(0, 60) +
    labs(x = "", y = "Number of staff") +
    theme_minimal()

ggsave("output/staff_by_region.jpg", width = 20, height = 12, units = "cm")

# official_RSTA <-all_RSTA %>% 
#     filter(!is.na(qualification))
# 
# official_RSTA %>% 
#     count(new_qual) %>% 
#     View()
# 
# official_RSTA %>% 
#     count(PL) %>% 
#     View()

all_RSTA %>% 
    group_by(grade, sex) %>% 
    tally() %>% 
    mutate(n = ifelse(sex == "F", (-1)*n, n)) %>% 
    ggplot(aes(x = fct_rev(grade), y = n, fill= sex)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.5) +
    coord_flip() +
    labs(x = "", y = "Number of staff") +
    ylim(-30, 70) +
    theme_minimal() +
    theme(legend.position = "none")

ggsave("output/postion_levels.jpg", width = 20, height = 12, units = "cm")

fct_count(all_RSTA$new_qual)

all_RSTA %>% 
    group_by(new_qual, sex) %>% 
    tally() %>% 
    mutate(n = ifelse(sex == "F", -1*n, n)) %>% 
    ggplot(aes(x =fct_rev(new_qual), y = n, fill = sex)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.5) +
    coord_flip() +
    labs(x = "", y = "Number of staffs") +
    ylim(-20, 60) +
    theme_minimal() +
    theme(legend.position = "none")

ggsave("output/qualification.jpg", width = 20, height = 12, units = "cm")


all_RSTA %>% 
    group_by(grade, age) %>% 
    tally() %>% 
    ggplot(aes(x = age)) +
    geom_density(fill = c("#104E8B"), color = c("#104E8B"), alpha = 0.1) +
    facet_wrap(~grade, nrow = 5) +
    xlim(0, 70) +
    theme_minimal()

all_RSTA %>% 
    group_by(grade, age) %>% 
    tally() %>% 
    ggplot(aes(x = age, y = fct_rev(grade), fill = grade, color = grade)) +
    geom_density_ridges(alpha = 0.1) +
    labs(x = "Age", y = "") +
    theme_ridges() +
    theme(legend.position = "none")

ggsave("output/grade_ridges.jpg", width = 25, height = 20, units = "cm")

data_DIT <- read_xlsx("data/RSTA_HR.xlsx", sheet = 2)



