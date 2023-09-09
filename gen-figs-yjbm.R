library(shiny)
library(tidyverse)
library(scholar)
library(scales)
library(htmltools)
library(cowplot)
library(viridis)

source("tweak-scholar.R")

source("process-gscholar.R")

url <- "https://scholar.google.com/citations?user=lwWjgGYAAAAJ&hl=en"

id <- gsub(".*user=", "", url)

start_pos <- str_locate(stringi::stri_reverse(id), "[[:punct:]]+") %>% as_tibble() %>% dplyr::slice(1) %>% pull(start)

id <- substr(id, 1, nchar(id) - start_pos)


cites_per_year <- get_citation_history(id) %>% type.convert(as.is=TRUE) 
year_range <- get_pubs(id)$year_range
cites_per_year <- cites_per_year %>% right_join(year_range, by=c("year"="years")) %>% mutate(cites = ifelse(is.na(cites), 0, cites))
cites_per_year <- cites_per_year %>% arrange(year) %>% mutate(cumcites = cumsum(cites)) %>% filter(year <= 2022)

yearlabel <- function(limits.pubrate){

        lims <- seq(limits.pubrate[1], limits.pubrate[2], 3)
        scale_x_continuous(breaks=lims, labels=lims)
}

fig_cpy <- ggplot(cites_per_year, aes(x = year, y = cites)) +
    geom_line(color = "#1F3A87", size=1.25) +
    geom_point(color = "#1F3A87", size=2.5) +
    theme_minimal() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        text = element_text(family = "Arial"),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(20, 20, 20, 20),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 18), 
        axis.ticks= element_line(color = "black")
    ) +
    labs(
        title = "",
        x = "Year",
        y = "Citations"
    ) + scale_y_continuous(breaks = pretty_breaks(n = 6)) + yearlabel(c(min(cites_per_year$year), max(cites_per_year$year)))

fig_cpycum <- ggplot(cites_per_year, aes(x = year, y = cumcites)) +
    geom_line(color = "#1F3A87", size=1.25) +
    geom_point(color = "#1F3A87", size=2.5) +
    theme_minimal() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        text = element_text(family = "Arial"),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(20, 20, 20, 20),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 18), 
        axis.ticks= element_line(color = "black")
    ) +
    labs(
        title = "",
        x = "Year",
        y = "Cumulative Citations"
    ) + scale_y_continuous(breaks = pretty_breaks(n = 6)) + yearlabel(c(min(cites_per_year$year), max(cites_per_year$year)))

p <- get_pubs(id)

pubrate <- p$pubrate
year_range <- p$year_range

pubrate <- left_join(year_range, pubrate, by=c("years"="year")) %>% mutate(n.pubs=ifelse(is.na(n.pubs), 0, n.pubs)) %>% mutate(n.pubs = as.numeric(n.pubs)) %>% as_tibble()

limits.pubrate <- c(min(pubrate$years), max(pubrate$years))

pubrate <- pubrate %>% mutate(cumpubs  = cumsum(n.pubs))



fig_pub <- ggplot(pubrate %>% filter(years <= 2022), aes(x = years, y = n.pubs)) +
    geom_line(color = "#AA1C3B", size=1.25) +
    geom_point(color = "#AA1C3B", size=2.5) +
    theme_minimal() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        text = element_text(family = "Arial"),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(20, 20, 20, 20),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 18), 
        axis.ticks= element_line(color = "black")
    ) +
    labs(
        title = "",
        x = "Year",
        y = "Publications"
    ) + scale_y_continuous(breaks = pretty_breaks(n = 12)) + yearlabel(c(min(pubrate$years), max(pubrate$years)))

fig_pubcum <- ggplot(pubrate %>% filter(years <= 2022), aes(x = years, y = cumpubs)) +
    geom_line(color = "#AA1C3B", size=1.25) +
    geom_point(color = "#AA1C3B", size=2.5) +
    theme_minimal() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        text = element_text(family = "Arial"),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(20, 20, 20, 20),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 18), 
        axis.ticks= element_line(color = "black")
    ) +
    labs(
        title = "",
        x = "Year",
        y = "Cumulative Publications"
    ) + scale_y_continuous(breaks = pretty_breaks(n = 6)) + yearlabel(c(min(pubrate$years), max(pubrate$years)))

journalcounts <- get_pubs(id)$pubs %>% filter(year <= 2022) %>% pull(journal) %>% table() %>% as_tibble() %>% magrittr::set_colnames(c("journal", "count"))

jc <- journalcounts %>% filter(journal != "") %>% arrange(desc(count)) %>% mutate(idx = 1:n()) %>% filter(idx <= 25)

fig_journalcounts <- jc %>% ggplot(aes(y = factor(idx), x = count)) + geom_col(alpha = 0.75, color="#4A90E2", fill = "#4A90E2") + xlab("# of Publications") + ggtitle("Most frequently\npublished journals") + theme_minimal() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        text = element_text(family = "Arial"), 
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.margin = margin(20, 20, 20, 20),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 18), 
        axis.ticks= element_line(color = "black")
    ) + scale_y_discrete(name = "", labels = str_wrap(jc$journal, 60)) + scale_x_continuous(breaks = pretty_breaks(8))

baseinfo <- get_base_info(id)
auth_positions <- get_pubs(id)$pubs_all %>% select(author, year) %>% as_tibble() %>% filter(year <= 2022)
auth_positions$lead <- lapply(auth_positions$author, function(x) extract_author(x, baseinfo$author) ) %>% unlist()
auth_positions_processed <- auth_positions %>% filter(!is.na(lead)) %>% pull(lead) %>% table() %>% as_tibble() %>% magrittr::set_colnames(c("pos", "n")) %>% filter(pos <= 4) %>% mutate(percent = round(((n / nrow(auth_positions))*100), 2)) 

fig_authpos <- auth_positions_processed %>% ggplot(aes(x=pos, y=n)) + geom_col(alpha = 0.75, color="#6C6C6C", fill = "#6C6C6C") + xlab("Author Position") + ylab("# of Pubs") + scale_y_continuous(
    sec.axis = sec_axis(trans = ~ (. / nrow(auth_positions))*100, name = "% of Total Pubs")) + ggtitle("Lead authorships") + theme(plot.title = element_text(size = 18, hjust = 0.5)) + theme_minimal() + theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        text = element_text(family = "Arial"),
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.margin = margin(20, 20, 20, 20),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 18), 
        axis.ticks= element_line(color = "black")
    )


#heatmap
p <- get_pubs(id)
pubs <- p$pubs
year_range <- p$year_range
journalbyyear <-  pubs %>% select(journal, year) %>% filter(year <= 2022) %>% group_by(journal, year) %>% summarize(n = n()) %>% ungroup() %>% group_by(year) %>% mutate(frac = n / n()) %>% ungroup()
journalcounts <- pubs %>% pull(journal) %>% table() %>% as_tibble() %>% magrittr::set_colnames(c("journal", "count"))
jc <- journalcounts %>% filter(journal != "") %>% arrange(desc(count)) %>% mutate(idx = 1:n())
t10j<- jc %>% filter(idx <= 10) %>% pull(journal)
all_combinations <- expand.grid(var1 = unique(journalbyyear$journal), var2 = year_range$year)
jby <- left_join(all_combinations, journalbyyear, by=c("var1"="journal", "var2" = "year")) %>% as_tibble() %>% magrittr::set_colnames(c("journal", "year", "n", "frac"))
jby <- jby %>% filter(journal %in% t10j) 

fig_journalheatmap <- ggplot(jby, aes(year, str_wrap(journal, 25), fill = n)) +
  geom_tile(color = "white") + 
  scale_fill_gradient(low = "#AED0FF", high = "#081035", na.value = "white", name = "# of Pubs") +
  theme_minimal() +
  labs(title = "Top 10 most published journals over time", x = "", y = "") + theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        text = element_text(family = "Arial"),
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.margin = margin(20, 20, 20, 20),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 18), 
        axis.ticks= element_line(color = "black")
    ) + yearlabel(c(min(journalbyyear$year),max(journalbyyear$year)))

figs <- list(heatmap=fig_journalheatmap,
authpos=fig_authpos,
journalcounts=fig_journalcounts,
pubcum=fig_pubcum,
pub=fig_pub,
cpycum=fig_cpycum,
cpy=fig_cpy)

saveRDS(object = figs, file = "../figs-yjbm/figs.rds")
























