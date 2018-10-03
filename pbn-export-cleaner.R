rm(list = ls())

suppressPackageStartupMessages({
    library(dplyr)
    library(progress)
    library(rvest)
})

DOCUMENT.FILE = "data/eksport.html"

doc = read_html(DOCUMENT.FILE)

doc.articles = doc %>% html_nodes(xpath = "/html/body/table[1]/tr")

articles = data.frame()

pb = progress::progress_bar$new(
  format = "articles: [:bar] :percent eta: :eta",
  total = length(doc.articles) - 1, clear = FALSE)

for (i in 2:length(doc.articles))
{
    curr.record = doc.articles[[i]] %>% html_nodes(xpath = "td")

    #  2 - title
    #  3 - publication year
    #  4 - autors
    #  5 - affiliation
    #  6 - employment
    # 15 - DOI
    # 17 - journal
    # 18 - ISSN
    # 19 - eISSN
    # 22 - points

    doi   = curr.record[15] %>% html_text
    doi   = ifelse(doi != "", trimws(doi), NA)

    issn  = curr.record[18] %>% html_text
    issn  = ifelse(issn != "", trimws(issn), NA)

    eissn = curr.record[19] %>% html_text
    eissn = ifelse(eissn != "", trimws(eissn), NA)

    points = curr.record[22] %>% html_text
    points = ifelse(!is.na(points), as.integer(points), NA)
    if (!is.na(points) && points <= 0)
        points = NA

    publication =
        data.frame(title   = curr.record[2] %>% html_text %>% trimws,
                   year    = curr.record[3] %>% html_text %>%
                                                substr(0, 4) %>%
                                                as.integer,
                   points  = points,
                   journal = curr.record[17] %>% html_text %>% trimws,
                   doi     = doi,
                   issn    = issn,
                   eissn   = eissn,
                   stringsAsFactors = FALSE)

    authors      = curr.record[4] %>% html_nodes("td")
    affiliations = curr.record[5] %>% html_nodes("td")
    employments  = curr.record[6] %>% html_nodes("td")

    persons = data.frame()

    for (j in 1:length(authors))
    {
        author      = authors[j] %>% html_text
        affiliation = affiliations[j] %>% html_text
        employment  = employments[j] %>% html_text

        # "\U2714" == Unicode HEAVY CHECK MARK

        author      = gsub("\\s+", " ", author)
        affiliation = ifelse(affiliation == "\U2714", TRUE,
                             ifelse(affiliation == "", NA, FALSE))
        employment  = ifelse(employment == "\U2714", TRUE,
                             ifelse(employment == "", NA, FALSE))

        persons = rbind(persons, data.frame(author      = author,
                                            affiliation = affiliation,
                                            employment  = employment,
                                            stringsAsFactors = FALSE))
    }

    articles = rbind(articles, merge(publication, persons))

    pb$tick()
}

# remove obvious duplicates
articles = articles %>% unique

# remove year duplicates

titles.duplicated =
    articles %>%
    filter(duplicated(.[, c("author", "title")])) %>%
    select(title) %>%
    unique %>%
    unlist %>%
    unname

for (article.title in titles.duplicated)
{
    year.newest =
        articles %>%
        select(title, year) %>%
        filter(title == article.title) %>%
        select(year) %>%
        unlist %>%
        unname %>%
        max

    articles =
        articles %>%
        filter(!(title == article.title & year < year.newest))
}

# remove articles without points and misplaced proceedings chapters

articles =
    articles %>% filter(!is.na(points))

# fix author names

articles =
    articles %>%
    mutate(author = replace(author, author == "Jose Gabriel Carrasquel Vera", "Jose Carrasquel")) %>%
    mutate(author = replace(author, author == "Małecka Agnieszka", "Agnieszka Małecka")) %>%
    mutate(author = replace(author, author == "Jasiczak Michał", "Michał Jasiczak")) %>%
    mutate(author = replace(author, author == "Reczkowski Michał", "Michał Rzeczkowski")) %>%
    mutate(author = replace(author, author == "Małgorzata Bednarska", "Małgorzata Bednarska-Bzdęga")) %>%
    rowwise %>%
    mutate(author = replace(author, TRUE, paste(strsplit(author, " ")[[1]][c(sum(charToRaw(author) == charToRaw(" ")) + 1, 1)], collapse = ", "))) %>%
    ungroup

# fix missing authors

articles = articles %>% bind_rows(
        articles %>%
            filter(doi %in% c("10.1016/j.ygyno.2016.06.020",
                              "10.1016/j.asoc.2016.05.029")) %>%
            group_by(doi) %>%
            filter(row_number() == 1) %>%
            mutate(author = "Wójtowicz, Andrzej", employment = FALSE) %>%
            ungroup
    )

# statistics

articles %>% filter(affiliation == TRUE, points >= 15, year >= 2015) %>% group_by(author) %>% summarise(`sum of points` = sum(points), `articles` = n(), `avg. points per article` = round(sum(points) / n(), 1)) %>% arrange(desc(`sum of points`)) %>% as.data.frame
