rm(list = ls())

library(progress)
library(rvest)

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
    # 17 - journal
    # 22 - points

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
        affiliation = ifelse(affiliation == "\U2714", TRUE, FALSE)
        employment  = ifelse(employment == "\U2714", TRUE, FALSE)

        persons = rbind(persons, data.frame(author      = author,
                                            affiliation = affiliation,
                                            employment  = employment,
                                            stringsAsFactors = FALSE))
    }

    articles = rbind(articles, merge(publication, persons))

    pb$tick()
}
