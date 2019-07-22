rm(list = ls())

DOCUMENT.HTML.PATH = "data/eksport.html"
DOCUMENT.XML.PATH  = "data/eksport.xml"

if (!("checkpoint" %in% rownames(installed.packages())))
    install.packages("checkpoint")

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
library(R6)
library(readr)
library(stringr)
library(xml2)

#______________________________________________________________________________

Author = R6Class("Author",
    public = list(
        `given-names`        = NA,
        `family-name`        = NA,
        `id-system`          = NA,
        `id-pbn`             = NA,
        `id-orcid`           = NA,
        `affiliated-to-unit` = NA,
        `employed-in-unit`   = NA,
        to.tibble = function()
        {
            tibble(`author-given-names` =
                       as.character(self$`given-names`),
                   `author-family-name` =
                       as.character(self$`family-name`),
                   `author-id-system` =
                       as.character(self$`id-system`),
                   `author-id-pbn` =
                       as.character(self$`id-pbn`),
                   `author-id-orcid` =
                       as.character(self$`id-orcid`),
                   `author-affiliated-to-unit` =
                       as.logical(self$`affiliated-to-unit`),
                   `author-employed-in-unit`=
                       as.logical(self$`employed-in-unit`))
        }
    )
)

Work = R6Class("Work",
    public = list(
        title               = NA,
        `system-identifier` = NA,
        `publication-date`  = NA,
        doi                 = NA,
        authors             = list(),
        add.author = function(a) {
            self$authors[[length(self$authors) + 1]] = a },
        to.tibble = function()
        {
            work =
                tibble(title              = as.character(self$title),
                      `system-identifier` =
                          as.character(self$`system-identifier`),
                      `publication-date`  =
                          as.integer(self$`publication-date`),
                      doi                 = as.character(self$doi))

            work = work %>% bind_cols(private$extra.to.tibble())

            authors.tbl = private$get.authors()

            work %>% mutate(foo = 1) %>%
                full_join((authors.tbl %>% mutate(foo = 1)), by = "foo") %>%
                select(-foo)
        }
    ),
    private = list(
        extra.to.tibble = function()
        {
            tibble()
        },
        get.authors = function()
        {
            do.call("rbind",
                    lapply(self$authors, function(x) {
                           x$to.tibble()}))
        }
    )
)

Article = R6Class("Article", inherit = Work,
    public = list(
        `journal-title`            = NA,
        `journal-id-system`        = NA,
        `journal-id-pbn`           = NA,
        `journal-ministerial-list` = NA,
        `journal-points`           = NA
    ),
    private = list(
        extra.to.tibble = function()
        {
            tibble(`journal-title`     = as.character(self$`journal-title`),
                   `journal-id-system` =
                       as.character(self$`journal-id-system`),
                   `journal-id-pbn`    = as.character(self$`journal-id-pbn`),
                   `journal-ministerial-list` =
                       as.character(self$`journal-ministerial-list`),
                   `journal-points`    = as.integer(self$`journal-points`))
        }
    )
)

Chapter = R6Class("Chapter", inherit = Work,
    public = list(
        `book-title` = NA
    ),
    private = list(
        extra.to.tibble = function()
        {
            tibble(`book-title` = as.character(self$`book-title`))
        }
    )
)

Book = R6Class("Book", inherit = Work,
    public = list(
        editors = list(),
        add.editor = function(e) {
            self$editors[[length(self$editors) + 1]] = e }
        ),
    private = list(
        get.authors = function()
        {
            a_tbl = do.call("rbind",
                            lapply(self$authors, function(x) {
                                   x$to.tibble()})) %>%
                    as_tibble() %>% # convert possible NULL
                    mutate(`is-author` = TRUE,
                           `is-editor` = FALSE)
            e_tbl = do.call("rbind",
                            lapply(self$editors, function(x) {
                                   x$to.tibble()})) %>%
                    as_tibble() %>% # convert possible NULL
                    mutate(`is-author` = FALSE,
                           `is-editor` = TRUE)

            if (nrow(e_tbl) == 0)
                return(a_tbl)
            if (nrow(a_tbl) == 0)
                return(e_tbl)

            r_tbl = tibble()

            for (i in seq_along(a_tbl))
            {
                curr_author = a_tbl %>% slice(i)
                if (any(!(is.na(curr_author$`author-id-pbn`)) &
                        curr_author$`author-id-pbn` %in% e_tbl$`author-id-pbn`,
                        !(is.na(curr_author$`author-id-orcid`)) &
                        curr_author$`author-id-orcid` %in% e_tbl$`author-id-orcid`,
                        !(is.na(curr_author$`author-id-system`)) &
                        curr_author$`author-id-system` %in% e_tbl$`author-id-system`))
                {
                    curr_author$`is-editor` = TRUE
                    e_tbl = e_tbl %>% # remove matched editor record
                        filter(!((!(is.na(curr_author$`author-id-pbn`)) &
                                curr_author$`author-id-pbn` == `author-id-pbn`) |
                                (!(is.na(curr_author$`author-id-orcid`)) &
                                curr_author$`author-id-orcid` == e_tbl$`author-id-orcid`) |
                                (!(is.na(curr_author$`author-id-system`)) &
                                curr_author$`author-id-system` == e_tbl$`author-id-system`)))
                }

                r_tbl = r_tbl %>% bind_rows(curr_author)
            }

            r_tbl = r_tbl %>% bind_rows(e_tbl)
        }
    )
)

#______________________________________________________________________________

idgen = function(v = 0)
{
    force(v)
    function()
    {
        v <<- v + 1
        v
    }
}

parse_author = function(node)
{
    author = Author$new()

    for (field in xml_children(node))
        switch (xml_name(field),
            "given-names" = { author$`given-names` =
                                xml_text(field) %>% trimws() },
            "family-name" = { author$`family-name` =
                                xml_text(field) %>% trimws() },
            "system-identifier" = {
                switch(xml_attr(field, "system"),
                       "PBN-ID" = { author$`id-pbn` = xml_text(field)},
                       "ORCID"  = { author$`id-orcid` = xml_text(field) },
                       "NA"     = { author$`id-system` = xml_text(field) }
                )
            },
            "affiliated-to-unit" = { author$`affiliated-to-unit` =
                                        xml_text(field)},
            "employed-in-unit"   = { author$`employed-in-unit` =
                                        xml_text(field)}
        )

    author
}

parse_article = function(node, points.lookup)
{
    article = Article$new()

    parse_journal = function(node)
    {
        for (field in xml_children(node))
            switch (xml_name(field),
                "title" = { article$`journal-title` <<-
                                xml_text(field) %>% trimws() },
                "system-identifier" = {
                    switch(xml_attr(field, "system"),
                        "PBN-ID" = { article$`journal-id-pbn` <<-
                                        xml_text(field) },
                        "NA" = { article$`journal-id-system` <<-
                                        xml_text(field) }

                    )},
                "type-ministerial-list" =
                    { article$`journal-ministerial-list` <<- xml_text(field)}
            )
    }

    for (field in xml_children(node))
        switch (xml_name(field),
            "title"             = { article$title =
                                        xml_text(field) %>% trimws() },
            "doi"               = { article$doi   =
                                        xml_text(field) %>% trimws() },
            "system-identifier" = { article$`system-identifier` =
                                        xml_text(field) %>% trimws() },
            "publication-date"  = { article$`publication-date` =
                                        xml_text(field) %>% trimws() %>%
                                        substr(0, 4) %>% as.integer() },
            "journal"           = parse_journal(field),
            "author"            = { article$add.author(parse_author(field)) }
        )

    pts = points.lookup %>%
        filter(system.identifier == article$`system-identifier`) %>%
        select(points) %>%
        pull

    if (length(pts) != 0 && !is.na(pts)) # catch integer(0)
        article$`journal-points` = pts

    article
}

parse_chapter = function(node)
{
    chapter = Chapter$new()

    parse_book = function(node)
    {
        for (field in xml_children(node))
            switch (xml_name(field),
                "title" = { chapter$`book-title` <<-
                                xml_text(field) %>% trimws() }
            )
    }

    for (field in xml_children(node))
        switch (xml_name(field),
            "title"             = { chapter$title =
                                        xml_text(field) %>% trimws() },
            "doi"               = { chapter$doi   =
                                        xml_text(field) %>% trimws() },
            "system-identifier" = { chapter$`system-identifier` =
                                        xml_text(field) %>% trimws() },
            "publication-date"  = { chapter$`publication-date` =
                                        xml_text(field) %>% trimws() %>%
                                        substr(0, 4) %>% as.integer() },
            "book"              = parse_book(field),
            "author"            = { chapter$add.author(parse_author(field)) }
        )

    chapter
}

parse_book = function(node)
{
    book = Book$new()

    for (field in xml_children(node))
        switch (xml_name(field),
            "title"             = { book$title =
                                        xml_text(field) %>% trimws() },
            "doi"               = { book$doi   =
                                        xml_text(field) %>% trimws() },
            "system-identifier" = { book$`system-identifier` =
                                        xml_text(field) %>% trimws() },
            "publication-date"  = { book$`publication-date` =
                                        xml_text(field) %>% trimws() %>%
                                        substr(0, 4) %>% as.integer() },
            "author"            = { book$add.author(parse_author(field)) },
            "editor"            = { book$add.editor(parse_author(field)) }
        )

    book
}

#______________________________________________________________________________

doc.html = read_html(DOCUMENT.HTML.PATH)
doc.html.articles = xml_find_all(doc.html, "/html/body/table[1]/tr")
no.articles = length(doc.html.articles) - 1

points.lookup = list(system.identifier = vector(length = no.articles),
                     points = vector(length = no.articles))
points.next.id = idgen()

pb = progress::progress_bar$new(
    format = "points lookup: [:bar] :percent :elapsedfull eta: :eta",
    total = no.articles)

for (i in 2:(no.articles+1))
{
    curr.record = xml_find_all(doc.html.articles[[i]], "td")

    points = curr.record[22] %>% xml_text()
    points = ifelse(!is.na(points), as.integer(points), NA)
    if (!is.na(points) && points <= 0)
        points = NA

    system.identifier = curr.record[33] %>% xml_text() %>% trimws()

    id = points.next.id()
    points.lookup$system.identifier[id] = system.identifier
    points.lookup$points[id] = points

    pb$tick()
}

points.lookup = points.lookup %>% as_tibble()



doc.xml = read_xml(DOCUMENT.XML.PATH)
doc.xml.children = xml_children(doc.xml)

articles = list()
chapters = list()
books    = list()

articles.next.id = idgen()
chapters.next.id = idgen()
books.next.id    = idgen()

pb = progress::progress_bar$new(
        format = "works: [:bar] :percent :elapsedfull eta: :eta",
        total  = length(doc.xml.children))

for (work in doc.xml.children)
{
    switch (xml_name(work),
        "article" = { articles[[articles.next.id()]] =
                          parse_article(work, points.lookup) },
        "chapter" = { chapters[[chapters.next.id()]] = parse_chapter(work) },
        "book"    = { books[[books.next.id()]]       = parse_book(work) }
    )

    pb$tick()
}

stop("halt")





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
