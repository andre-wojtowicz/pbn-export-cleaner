rm(list = ls())

DOCUMENT.HTML.PATH = "data/eksport.html"
DOCUMENT.XML.PATH  = "data/eksport.xml"

library(checkpoint)

cfg = list(checkpoint =
           list(snapshot.date     = "2019-04-15", # default for MRO 3.5.3
                scan.for.packages = TRUE,
                verbose           = TRUE)
          )

try(suppressWarnings(source("config.R.user")), silent = TRUE)

with(cfg$checkpoint,
     checkpoint(snapshotDate    = snapshot.date,
                scanForPackages = scan.for.packages,
                verbose         = verbose))

library(dplyr)
library(progress)
library(readr)
library(xml2)

#______________________________________________________________________________

articles = list()
chapters = list()
books    = list()

parse_author = function(node)
{
    author = list(
        `given-names`        = NA,
        `family-name`        = NA,
        `id-system`          = NA,
        `id-pbn`             = NA,
        `id-orcid`           = NA,
        `affiliated-to-unit` = NA,
        `employed-in-unit`   = NA
    )

    for (field in xml_children(node))
        switch (xml_name(field),
            "given-names" = { author$`given-names` = xml_text(field)},
            "family-name" = { author$`family-name` = xml_text(field)},
            "system-identifier" = {
                switch(xml_attr(field, "system"),
                       "PBN-ID" = { author$`id-pbn` = xml_text(field)},
                       "ORCID"  = { author$`id-orcid` = xml_text(field) },
                       "NA"     = { author$`id-system` = xml_text(field) }
                )
            },
            "affiliated-to-unit" = { author$`affiliated-to-unit` = xml_text(field)},
            "employed-in-unit" = { author$`employed-in-unit` = xml_text(field)},
        )

    author
}

parse_article = function(node)
{
    article = list(
        title               = NA,
        `system-identifier` = NA,
        `publication-date`  = NA,
        `doi`               = NA,
        `journal-title`     = NA,
        `journal-id-system` = NA,
        `journal-id-pbn`    = NA,
        `journal-ministerial-list` = NA,
        `journal-points`    = NA
        )

    authors = list()

    parse_journal = function(node)
    {
        for (field in xml_children(node))
            switch (xml_name(field),
                "title" = { article$`journal-title` <<- xml_text(field) },
                "system-identifier" = {
                    switch(xml_attr(field, "system"),
                        "PBN-ID" = { article$`journal-id-pbn` <<- xml_text(field) },
                        "NA" = { article$`journal-id-system` <<- xml_text(field) }

                    )},
                "type-ministerial-list" = { article$`journal-ministerial-list` <<- xml_text(field)}
            )
    }

    for (field in xml_children(node))
        switch (xml_name(field),
            "title"             = { article$title = xml_text(field) },
            "doi"               = { article$doi   = xml_text(field) },
            "system-identifier" = { article$`system-identifier` = xml_text(field) },
            "publication-date"  = { article$`publication-date` = xml_text(field) },
            "journal" = parse_journal(field),
                "author"  = { authors[[length(authors) + 1]] = parse_author(field) }
        )

    list(article, authors)
}

parse_chapter = function(node)
{

}

parse_book = function(node)
{

}


doc.xml = read_xml(DOCUMENT.XML.PATH)

for (work in xml_children(doc.xml))
{
    switch (xml_name(work),
        "article" = parse_article(work),
        "chapter" = parse_chapter(work),
        "book"    = parse_book(work)
    )

    if (xml_name(work) == "article") break
}

doc.html = read_html(DOCUMENT.HTML.PATH)






stop()

DOCUMENT.FILE = "data/eksport.html"

doc = read_html(DOCUMENT.FILE)

# ---- articles ----

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
        author      = authors[j] %>% html_text %>% trimws
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

# fix author names

articles =
    articles %>%
    mutate(author = replace(author, author == "Jose Gabriel Carrasquel Vera", "José Carrasquel-Vera")) %>%
    mutate(author = replace(author, author == "Małecka Agnieszka", "Agnieszka Małecka")) %>%
    mutate(author = replace(author, author == "Jasiczak Michał", "Michał Jasiczak")) %>%
    mutate(author = replace(author, author == "Rzeczkowski Michał", "Michał Rzeczkowski")) %>%
    mutate(author = replace(author, author == "Górnisiewicz Krzysztof", "Krzysztof Górnisiewicz")) %>%
    mutate(author = replace(author, author == "Izabela Bondecka", "Izabela Bondecka-Krzykowska")) %>%
    mutate(author = replace(author, author == "Julian Musialak", "Julian Musielak")) %>%
    mutate(author = replace(author, author == "Małgorzata Bednarska", "Małgorzata Bednarska-Bzdęga")) %>%
    mutate(author = replace(author, author == "Eliza Jackowska", "Eliza Jackowska-Boryc")) %>%
    mutate(author = replace(author, author == "Schoen Tomasz", "Tomasz Schoen")) %>%
    rowwise %>%
    mutate(author = replace(author, TRUE, paste(strsplit(author, " ")[[1]][c(sum(charToRaw(author) == charToRaw(" ")) + 1, 1)], collapse = ", "))) %>%
    ungroup

# remove articles without points and misplaced proceedings chapters

extra.chapters = articles %>% filter(is.na(points))

articles =
    articles %>% filter(!is.na(points))


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

#articles %>% filter(affiliation == TRUE, points >= 15, year >= 2015) %>% group_by(author) %>% summarise(`sum of points` = sum(points), `articles` = n(), `avg. points per article` = round(sum(points) / n(), 1)) %>% arrange(desc(`sum of points`)) %>% as.data.frame

write_excel_csv(articles, "pbn-articles.csv", na = "")

# ---- chapters ----

doc.chapters = doc %>% html_nodes(xpath = "/html/body/table[2]/tr")

chapters = data.frame()

pb = progress::progress_bar$new(
  format = "chapters: [:bar] :percent eta: :eta",
  total = length(doc.chapters) - 1, clear = FALSE)

for (i in 2:length(doc.chapters))
{
    curr.record = doc.chapters[[i]] %>% html_nodes(xpath = "td")

    #  2 - title
    #  3 - publication year
    #  4 - autors
    #  5 - affiliation
    #  6 - employment
    # 10 - DOI
    # 14 - place (book title)

    doi   = curr.record[10] %>% html_text
    doi   = ifelse(doi != "", trimws(doi), NA)

    publication =
        data.frame(title   = curr.record[2] %>% html_text %>% trimws,
                   year    = curr.record[3] %>% html_text %>%
                                                substr(0, 4) %>%
                                                as.integer,
                   doi     = doi,
                   place   = curr.record[14] %>% html_text %>% trimws,
                   stringsAsFactors = FALSE)

    authors      = curr.record[4] %>% html_nodes("td")
    affiliations = curr.record[5] %>% html_nodes("td")
    employments  = curr.record[6] %>% html_nodes("td")

    persons = data.frame()

    for (j in 1:length(authors))
    {
        author      = authors[j] %>% html_text %>% trimws
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

    chapters = rbind(chapters, merge(publication, persons))

    pb$tick()
}

# change author names

chapters = chapters %>%
    rowwise %>%
    mutate(author = replace(author, TRUE, paste(strsplit(author, " ")[[1]][c(sum(charToRaw(author) == charToRaw(" ")) + 1, 1)], collapse = ", "))) %>%
    ungroup

# add extra chapters

chapters = rbind(chapters, select(rename(extra.chapters, place = journal),
                                  title, year, author,
                                  affiliation, employment, doi, place))

# save file

write_excel_csv(chapters, "pbn-chapters.csv", na = "")


# ---- books ----

doc.books = doc %>% html_nodes(xpath = "/html/body/table[3]/tr")

books = data.frame()

pb = progress::progress_bar$new(
  format = "books: [:bar] :percent eta: :eta",
  total = length(doc.books) - 1, clear = FALSE)

for (i in 2:length(doc.books))
{
    curr.record = doc.books[[i]] %>% html_nodes(xpath = "td")

    #  2 - title
    #  3 - publication year
    #  4 - autors
    #  5 - affiliation
    #  6 - employment
    # 17 - DOI

    doi   = curr.record[17] %>% html_text
    doi   = ifelse(doi != "", trimws(doi), NA)

    publication =
        data.frame(title   = curr.record[2] %>% html_text %>% trimws,
                   year    = curr.record[3] %>% html_text %>%
                                                substr(0, 4) %>%
                                                as.integer,
                   doi     = doi,
                   stringsAsFactors = FALSE)

    authors      = curr.record[4] %>% html_nodes("td")
    affiliations = curr.record[5] %>% html_nodes("td")
    employments  = curr.record[6] %>% html_nodes("td")

    persons = data.frame()

    for (j in 1:length(authors))
    {
        author      = authors[j] %>% html_text %>% trimws
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

    books = rbind(books, merge(publication, persons))

    pb$tick()
}

# fix author names

books =
    books %>%
    mutate(author = replace(author, author == "Przybylski Bartłomiej", "Bartłomiej Przybylski"))

# change author names

books = books %>%
    rowwise %>%
    mutate(author = replace(author, TRUE, paste(strsplit(author, " ")[[1]][c(sum(charToRaw(author) == charToRaw(" ")) + 1, 1)], collapse = ", "))) %>%
    ungroup

# save file

write_excel_csv(books, "pbn-books.csv", na = "")
