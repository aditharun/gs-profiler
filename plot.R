library(tidyverse)
library(cowplot)
library(grid)
library(gridExtra)

args = commandArgs(trailingOnly=TRUE)

subjectid <- args[1]

cpy <- file.path("tmp", subjectid, "cpy.csv") %>% read_csv(., col_names=FALSE) %>% magrittr::set_colnames(c("year", "cites"))

p.cpy <- ggplot(cpy, aes(x=year, y=cites, group=year)) + geom_col(color="grey70", fill="grey70") + theme_bw() + scale_y_continuous(n.breaks=10) + ylab("# of citations") + xlab("") + theme(axis.title=element_text(size=17), axis.text.x = element_text(size=15), axis.text.y=element_text(size=13)) + theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y=element_blank())


pubrate <- file.path("tmp", subjectid, "pubhistory.csv") %>% read_csv(., col_names=FALSE) %>% magrittr::set_colnames(c("year", "no_pubs"))

hind5y <- pubrate %>% filter(year=="hind5y") %>% pull(no_pubs)

hind <- pubrate %>% filter(year=="hind") %>% pull(no_pubs)

name <- pubrate %>% filter(year=="name") %>% pull(no_pubs)

pubrate <- pubrate %>% filter(!grepl("hind|name", year))

prange <- pubrate$year %>% range() %>% as.numeric()

year_range <- data.frame(years=(prange[1]:prange[2]) %>% as.character())

pubrate <- left_join(year_range, pubrate, by=c("years"="year")) %>% mutate(no_pubs=ifelse(is.na(no_pubs), 0.01, no_pubs)) %>% mutate(no_pubs = as.numeric(no_pubs))

p.pubr <- ggplot(pubrate, aes(x=years, y=no_pubs, group=years)) + geom_col(color="steelblue", fill="steelblue") + theme_bw()  + ylab("# of papers") + xlab("") + theme(axis.title=element_text(size=17), axis.text.x = element_text(size=15), axis.text.y=element_text(size=13)) + theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y=element_blank())


jcsv <- file.path("tmp", subjectid, "journalhistory.csv")

max_cols <- count.fields(jcsv, sep = ",") %>% max()

journalhistory <- read.table(jcsv ,sep=",",fill=TRUE,col.names=1:max_cols)

jhist <- journalhistory %>% pivot_longer(-X1) %>% select(-name) %>% filter(!is.na(value)) %>% mutate(value = gsub("\\[|]|\\'", "", value)) %>% mutate(X1=as.character(X1)) %>% mutate(value=trimws(tolower(value), which="both"))

jhist <- jhist %>% mutate(is_empty=ifelse(value=="", "yes", "no")) %>% filter(is_empty=="no") %>% select(-is_empty)

jhist <- left_join(year_range, jhist, by=c("years"="X1")) %>% mutate(value=ifelse(is.na(value), "NA", value))


ct.table <- jhist %>% group_by(value) %>% summarize(n=n()) %>% filter(!is.na(value)) %>% arrange(desc(n)) %>% magrittr::set_colnames(c("Journal", "# of Pubs")) %>% ungroup()

jhist <- jhist %>% group_by(years, value) %>% summarize(n=n()) %>% mutate(n=ifelse(value=="NA", 0.01, n)) %>% mutate(n = as.numeric(n))

maxpubs_yr <- jhist %>% group_by(years) %>% summarize(n=sum(n)) %>% pull(n) %>% max()

#3 pubs (7height), 35 pubs (40height) form linear relationship
nh_j <- (33/32)*maxpubs_yr + 3.9

p.jhist <- ggplot(jhist, aes(fill=value, y=n, x=years)) + geom_bar(position="stack", stat="identity") + theme_minimal() + ylab("# of publications") + xlab("") + theme(axis.title=element_text(size=13), axis.text.x = element_text(size=13), axis.text.y=element_text(size=11)) + theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y=element_blank()) + theme(legend.title=element_text(size=13), legend.text=element_text(size=12), legend.position="right") + labs(fill="Journal")

prow <- cowplot::plot_grid(p.cpy, p.pubr, nrow=2)

ptitle <- paste0("Publication Stats: ", name, " (", subjectid, ")")

title <- ggdraw() + 
  draw_label(ptitle, fontface = 'bold', x = 0, hjust = 0) + theme(plot.margin = margin(0, 0, 0, 7))

resultspath <- file.path("results", subjectid)

dir.create(resultspath, recursive=TRUE)

plotpath <- file.path(resultspath, "pub-stats.pdf")

p <- plot_grid(title, prow, ncol = 1, rel_heights = c(0.08, 1))

nyrs <- pubrate$years %>% length()

nh <- (3/16)*nyrs + 8.25
nw <- (11/16)*nyrs + 4.25

#guidelines for linear set of plot dimensions
#height = 9 (4yr) - 12 (20yr)
#width = 7 (4yr) - 18 (20yr)

ggsave(plotpath, p, height=nh, width=nw, units="in", limitsize=FALSE)

tablepath <- file.path(resultspath, "journal-count-table.csv")

write_csv(x=ct.table, file=tablepath)

pct.table <- ct.table %>% arrange(desc(`# of Pubs`)) %>% mutate(j=Journal) %>% mutate(j=str_wrap(j, width=40)) %>% mutate(j=factor(j, levels=unique(j))) %>% ggplot(aes(y=`# of Pubs`, groups=j, x=j)) + geom_col() + theme_minimal() + theme(axis.text.y = element_text(size=13), axis.text.x=element_text(size=17)) + coord_flip() + xlab("") 

ggsave(file.path(resultspath, "journal-counts.pdf"), pct.table, height=nh_j, width=nw, units="in", limitsize=FALSE)

pj <- p.jhist + ggtitle(ptitle) + theme(plot.title=element_text(hjust=0.5, size=17)) + theme(legend.position="bottom")

ggsave(file.path(resultspath, "journal-counts-by-year.pdf"), pj, height=nh_j, width=nw, units='in', limitsize=FALSE)



#