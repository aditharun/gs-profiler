library(tidyverse)
library(scholar)
library(scales)
library(cowplot)

extract_author <- function(s, author_name){

        author_name <- sub(".+\\s", "", author_name)

        n.authors <- str_count(s, ",") + 1
        comma_pos <- s %>% str_locate_all(., ",")
        author_pos <- s %>% str_locate_all(., paste0(author_name))

        cpos <- comma_pos[[1]] %>% as_tibble() %>% pull("end")

        apos <- author_pos[[1]] %>% as_tibble() %>% pull("end")

        apos <- apos[1]

        author_pos <- which(apos+1 == cpos)

        if (identical(author_pos, integer(0))){
            
            if (grepl(author_name, s)){

                author_pos <- n.authors

            } else{

                author_pos <- NA
            }

        } 

        return(author_pos)
}

yearlabel <- function(limits.pubrate){

        lims <- seq(limits.pubrate[1], limits.pubrate[2], 1)
        scale_x_continuous(breaks=lims, labels= function(x) ifelse(x %% 2 == 1, x, ""))
}

get_base_info <- function(id){

    profiles <- get_profile(id)
    total_cites <- profiles$total_cites
    h_index <- profiles$h_index
    author <- profiles$name

    return(list(profiles = profiles, author = author, total_cites = total_cites, h_index = h_index))
}


get_pubs <- function(id){

    pubs_all <- get_publications(id) 

    pubs <- pubs_all %>% select(journal, cites, year) %>% filter(!is.na(year)) %>% as_tibble() %>% mutate(journal = str_to_title(journal))

    pubrate <- pubs  %>% pull(year) %>% table() %>% as_tibble() %>% magrittr::set_colnames(c("year", "n.pubs")) %>% type.convert(as.is = TRUE)

    prange <- pubrate$year %>% range() %>% as.numeric()

    year_range <- data.frame(years=(prange[1]:prange[2]) %>% as.numeric())

    return(list(pubs_all=pubs_all, pubrate = pubrate, year_range = year_range, pubs = pubs))

}

citations_per_year <- function(id, sizing_theme, panel_theme){

    cites_per_year <- get_citation_history(id) %>% type.convert(as.is=TRUE) 

    year_range <- get_pubs(id)$year_range

    cites_per_year <- cites_per_year %>% right_join(year_range, by=c("year"="years")) %>% mutate(cites = ifelse(is.na(cites), 0, cites))

    cites_per_year <- cites_per_year %>% arrange(year) %>% mutate(cumcites = cumsum(cites))

    nejm_colors <- c("#1A237E", "#039BE5", "#D32F2F", "#388E3C", "#FBC02D", "#9E9E9E")

    citesperyear.fig <- cites_per_year %>% ggplot(aes(x=year, y=cites)) + geom_point(size = 2.5, color=nejm_colors[1]) + geom_line(linewidth = 1, color=nejm_colors[1]) + sizing_theme + panel_theme + xlab("Year") + ylab("# of citations") + ggtitle("Citations") + theme(plot.title = element_text(hjust = 0.5)) + yearlabel(c(min(cites_per_year$year), max(cites_per_year$year))) + scale_y_continuous(breaks = pretty_breaks(6)) + theme(plot.title = element_text(size = 18), axis.title = element_text(size=16), axis.text.y = element_text(size = 12)) + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

    cumcitesperyear.fig <- cites_per_year %>% ggplot(aes(x=year, y=cumcites), color=nejm_colors[2]) + geom_point(size = 2.5) + geom_line(linewidth = 1) + sizing_theme + panel_theme + xlab("Year") + ggtitle(paste0("Cumulative citations")) + theme(plot.title = element_text(hjust = 0.5)) + yearlabel(c(min(cites_per_year$year), max(cites_per_year$year))) + scale_y_continuous(breaks = pretty_breaks(6)) + theme(plot.title = element_text(size = 18), axis.title = element_text(size=16), axis.text.y = element_text(size = 12))  + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) + ylab("")

    return(plot_grid(citesperyear.fig, cumcitesperyear.fig, nrow = 1))

}


pubs_per_year <- function(id, sizing_theme, panel_theme){

    p <- get_pubs(id)

    pubrate <- p$pubrate
    year_range <- p$year_range

    pubrate <- left_join(year_range, pubrate, by=c("years"="year")) %>% mutate(n.pubs=ifelse(is.na(n.pubs), 0, n.pubs)) %>% mutate(n.pubs = as.numeric(n.pubs)) %>% as_tibble()

    limits.pubrate <- c(min(pubrate$years), max(pubrate$years))

    pubrate <- pubrate %>% mutate(cumpubs  = cumsum(n.pubs))

    cumpubs.fig <- pubrate %>% ggplot(aes(x=years, y=cumpubs)) + geom_point(size = 2.5) + geom_line(linewidth = 1) + sizing_theme + panel_theme + xlab("Year") + ylab("") + theme(panel.grid.major.y = element_line(color = "grey70", linewidth = 0.2)) + yearlabel(limits.pubrate) + ggtitle(paste0("Cumulative pubs")) + theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(breaks = pretty_breaks(5)) + theme(plot.title = element_text(size = 18), axis.title = element_text(size=16), axis.text.y = element_text(size = 12)) + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

    pubsperyear.fig <- pubrate %>% ggplot(aes(x=years, y=n.pubs)) + geom_point(size = 2.5) + geom_line(linewidth = 1) + sizing_theme + panel_theme + xlab("Year") + ylab("# of Publications") + theme(panel.grid.major.y = element_line(color = "grey70", linewidth = 0.2)) + yearlabel(limits.pubrate) + ggtitle(paste0("Publications")) + theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(breaks = pretty_breaks(10)) + theme(plot.title = element_text(size = 18), axis.title = element_text(size=16), axis.text.y = element_text(size = 12)) + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

    ppy.fig <- plot_grid(pubsperyear.fig, cumpubs.fig, nrow = 1)

    return(ppy.fig)


}

journal_counts <- function(id, sizing_theme, panel_theme){

    journalcounts <- get_pubs(id)$pubs %>% pull(journal) %>% table() %>% as_tibble() %>% magrittr::set_colnames(c("journal", "count"))

        #Top 20 journals 
    jc <- journalcounts %>% filter(journal != "") %>% arrange(desc(count)) %>% mutate(idx = 1:n()) %>% filter(idx <= 25)

    journalcounts.fig <- jc %>% ggplot(aes(y = factor(idx), x = count)) + geom_col(alpha = 0.75, color="grey50", fill = "grey50") + scale_y_discrete(name = "", labels = str_wrap(jc$journal, 60)) + sizing_theme + panel_theme + xlab("# of Publications") + theme(panel.grid.major.y =  element_blank(), panel.grid.major.x = element_line(color = "grey70", linewidth = 0.2)) + scale_x_continuous(breaks = pretty_breaks(8)) + ggtitle("Most frequently published journals") + theme(plot.title = element_text(size = 18, hjust = 0.5), axis.title = element_text(size=16), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 10)) 

    return(journalcounts.fig)


}

journal_per_year <- function(id, sizing_theme, panel_theme){

    p <- get_pubs(id)

    pubs <- p$pubs

    year_range <- p$year_range

    journalbyyear <-  pubs %>% select(journal, year) %>% group_by(journal, year) %>% summarize(n = n()) %>% ungroup() %>% group_by(year) %>% mutate(frac = n / n()) %>% ungroup()

    journalcounts <- pubs %>% pull(journal) %>% table() %>% as_tibble() %>% magrittr::set_colnames(c("journal", "count"))

    jc <- journalcounts %>% filter(journal != "") %>% arrange(desc(count)) %>% mutate(idx = 1:n())

    t10j<- jc %>% filter(idx <= 10) %>% pull(journal)

    all_combinations <- expand.grid(var1 = unique(journalbyyear$journal), var2 = year_range$year)

   jby <- left_join(all_combinations, journalbyyear, by=c("var1"="journal", "var2" = "year")) %>% as_tibble() %>% magrittr::set_colnames(c("journal", "year", "n", "frac"))

    jby.fig <- jby %>% filter(journal %in% t10j) %>% ggplot(aes(x=year, y=str_wrap(journal, 25), fill = n)) + geom_tile() + yearlabel(c(min(journalbyyear$year),max( journalbyyear$year))) + panel_theme + sizing_theme  + scale_fill_gradient(name = "# of Pubs", na.value = "grey92", low = "grey60", high = "black", breaks = c(0, 1, seq(0, max(jby$n, na.rm=T), 5)[-1])) + ggtitle(paste0("Top ", length(t10j) , " most published journals over time")) + ylab("") + xlab("") + theme(panel.grid = element_blank()) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(axis.text.y = element_text(size = 10))

    return(jby.fig)


}

auth_numbers <- function(id, sizing_theme, panel_theme){

    baseinfo <- get_base_info(id)
    
    auth_positions <- get_pubs(id)$pubs_all %>% select(author, year) %>% as_tibble()

    auth_positions$lead <- lapply(auth_positions$author, function(x) extract_author(x, baseinfo$author) ) %>% unlist()

    authpos.fig <- auth_positions %>% filter(!is.na(lead)) %>% pull(lead) %>% table() %>% as_tibble() %>% magrittr::set_colnames(c("pos", "n")) %>% filter(pos <= 4) %>% mutate(percent = round(((n / nrow(auth_positions))*100), 2)) %>% ggplot(aes(x=pos, y=n)) + geom_col(alpha = 0.75, color="grey50", fill = "grey50") + panel_theme + sizing_theme + xlab("Author Position") + ylab("# of Pubs") + scale_y_continuous(
    sec.axis = sec_axis(trans = ~ (. / nrow(auth_positions))*100, name = "% of Total Pubs")) + ggtitle("Lead authorships") + theme(plot.title = element_text(size = 18, hjust = 0.5))


    return(authpos.fig)

}












