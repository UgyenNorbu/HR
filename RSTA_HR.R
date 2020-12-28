library(tidyverse)
library(readxl)
library(ggplot2)
library(ggtheme)

data_RSTA <- read_xlsx("data/RSTA_HR.xlsx")
data_DIT <- read_xlsx("data/RSTA_HR.xlsx", sheet = 2)
data_DoIM <- read_xlsx("data/RSTA_HR.xlsx", sheet = 3)
test_data <- read_xlsx("data/RSTA_HR.xlsx", sheet = 4)

test_data <- test_data %>% 
    select(name, DoB)

colnames(test_data) <- c("name", "DoB_1")

final_RSTA <- left_join(data_RSTA, test_data, by = "name")

final_RSTA <- final_RSTA %>% 
    select(!c(DoB, appointment, Remarks))

final_RSTA %>% 
    group_by(office) %>% 
    tally() %>%
    ggplot(aes(reorder(x = office, -n), y = n)) +
    geom_bar(stat = "identity", fill = "#BCD2EE", width = 0.5) +
    coord_flip() +
    ylim(0, 60) +
    labs(x = "", y = "Number of staff") +
    theme_minimal()

ggsave("output/staff_by_region.jpg", width = 20, height = 12, units = "cm")

final_RSTA$PL <- factor(final_RSTA$PL, levels = c("EX2", "P1", "P2","P3", 
    "P4", "P5","SS1", "SS2", "SS3", "SS4", "S1", "S2", "S3", "S4", 
    "S5", "O1", "O2", "O3", "O4", "ESP", "GSP I", "GSP II"))

final_RSTA %>% 
    group_by(PL) %>% 
    tally() %>% 
    ggplot(aes(x = fct_rev(PL), y = n)) +
    geom_bar(stat = "identity", fill = "#BCD2EE", width = 0.5) +
    coord_flip() +
    ylim(0, 30) +
    labs(x = "", y = "Number of staff") +
    theme_minimal()

master_list <- final_RSTA %>% 
    filter(grepl("Master", qualification, ignore.case = TRUE)) %>% 
    select(qualification) %>%
    unique() %>%
    pull()

bachelor_list <- final_RSTA %>% 
    filter(grepl("Bachelor|P.G|B.A|PGD|BA", qualification, 
                 ignore.case = TRUE)) %>%
    select(qualification) %>%
    unique() %>%
    pull()

diploma <- final_RSTA %>% 
    filter(grepl("^Diploma", qualification, ignore.case = TRUE)) %>% 
    select(qualification) %>%
    unique() %>%
    pull()

is.vector(diploma)

diploma <- append(diploma, "Automobile Mechanics (NC2)")

certificate <- final_RSTA %>% 
    filter(grepl("^Certificate|Cetificate", qualification, 
                 ignore.case = TRUE)) %>%     
    select(qualification) %>%
    unique() %>%
    pull()

school_list <- final_RSTA %>% 
    filter(grepl("Class", qualification, ignore.case = TRUE)) %>% 
    select(qualification) %>% 
    unique() %>% 
    pull()

school_list <- append(school_list, "Junior Education")

final_RSTA$new_qual <- 0
final_RSTA <- final_RSTA %>% 
    mutate(new_qual = ifelse(qualification %in% master_list,
                             "Masters", new_qual)) %>% 
    mutate(new_qual = ifelse(qualification %in% bachelor_list, "Bachelors", new_qual)) %>% 
    mutate(new_qual = ifelse(qualification %in% diploma,
                             "Diploma", new_qual)) %>% 
    mutate(new_qual = ifelse(qualification %in% certificate, "Certificate", new_qual)) %>% 
    mutate(new_qual = ifelse(qualification %in% school_list, 
                             "School", new_qual))

final_RSTA$new_qual <- factor(final_RSTA$new_qual,
    levels = c("Masters", "Bachelors", "Diploma", "Certificate", "School"))

fct_count(final_RSTA$new_qual)

final_RSTA %>% 
    filter(is.na(new_qual)) %>% 
    View()

final_RSTA %>% 
    filter(!is.na(new_qual)) %>% 
    group_by(new_qual) %>% 
    tally() %>% 
    ggplot(aes(x = fct_rev(new_qual), y = n)) +
    geom_bar(stat = "identity", fill = "#BCD2EE") +
    coord_flip() +
    ylim(0, 80) +
    labs(x = "", y = "Number of staffs") +
    theme_minimal()

WriteXLS::WriteXLS(data_RSTA, "final_RSTA_staff.xlsx")
