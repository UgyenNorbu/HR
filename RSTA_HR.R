library(tidyverse)
library(readxl)
library(ggplot2)
library(ggtheme)

data_RSTA <- read_xlsx("data/RSTA_HR.xlsx")
data_DIT <- read_xlsx("data/RSTA_HR.xlsx", sheet = 2)
data_DoIM <- read_xlsx("data/RSTA_HR.xlsx", sheet = 3)

data_RSTA %>% 
    group_by(office) %>% 
    tally() %>% 
    ggplot(aes(reorder(x = office, -n), y = n)) +
    geom_bar(stat = "identity", fill = "#BCD2EE", width = 0.5) +
    coord_flip() +
    ylim(0, 50) +
    theme_linedraw()
ggsave("output/staff_by_region.jpg", width = 20, height = 12, units = "cm")

PL_list <- data_RSTA %>% 
    select(PL) %>% 
    unique() %>% 
    pull()

data_RSTA$PL <- factor(data_RSTA$PL, levels = c("EX2", "P1", "P2","P3", 
    "P4", "P5","SS1", "SS2", "SS3", "SS4", "S1", "S2", "S3", "S4", 
    "S5", "O1", "O2", "O3", "O4", "ESP", "GSP I", "GSP II"))
levels(data_RSTA$PL)

str(data_RSTA$PL)

data_RSTA %>% 
    group_by(PL) %>% 
    tally() %>% 
    ggplot(aes(x = fct_rev(PL), y = n)) +
    geom_bar(stat = "identity", fill = "#BCD2EE", width = 0.5) +
    coord_flip() +
    theme_linedraw()

master_list <- data_RSTA %>% 
    filter(grepl("Master", qualification, ignore.case = TRUE)) %>% 
    select(qualification) %>%
    unique() %>%
    pull()

bachelor_list <- data_RSTA %>% 
    filter(grepl("Bachelor|P.G|B.A|PGD", qualification, 
                 ignore.case = TRUE)) %>%
    select(qualification) %>%
    unique() %>%
    pull()

diploma <- data_RSTA %>% 
    filter(grepl("^Diploma", qualification, ignore.case = TRUE)) %>% 
    select(qualification) %>%
    unique() %>%
    pull()

certificate <- data_RSTA %>% 
    filter(grepl("^Certificate|Cetificate", qualification, 
                 ignore.case = TRUE)) %>%     
    select(qualification) %>%
    unique() %>%
    pull()

school_list <- data_RSTA %>% 
    filter(grepl("Class", qualification, ignore.case = TRUE)) %>% 
    select(qualification) %>% 
    unique() %>% 
    pull()

data_RSTA <- data_RSTA %>% 
    mutate(new_qual = ifelse(qualification %in% master_list, "Masters", new_qual)) %>% 
    mutate(new_qual = ifelse(qualification %in% bachelor_list, "Bachelors", new_qual)) %>% 
    mutate(new_qual = ifelse(qualification %in% diploma, "Diploma", new_qual)) %>% 
    mutate(new_qual = ifelse(qualification %in% certificate, "Certificate", new_qual)) %>% 
    mutate(new_qual = ifelse(qualification %in% school_list, "School", new_qual))

data_RSTA$new_qual <- factor(data_RSTA$new_qual,
    levels = c("Masters", "Bachelors", "Diploma", "Certificate", "School"))

fct_count(data_RSTA$new_qual)

data_RSTA %>% 
    filter(is.na(new_qual)) %>% 
    View()

data_RSTA %>% 
    group_by(new_qual) %>% 
    tally() %>% 
    ggplot(aes(x = fct_rev(new_qual), y = n)) +
    geom_bar(stat = "identity", fill = "#BCD2EE") +
    coord_flip() +
    ylim(0, 80) +
    theme_linedraw()
