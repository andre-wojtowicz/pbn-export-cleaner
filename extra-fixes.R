articles = articles %>% bind_rows(
            merge(
                articles %>%
                filter(doi %in% c("10.1016/j.ygyno.2016.06.020",
                                  "10.1016/j.asoc.2016.05.029")) %>%
                group_by(doi) %>%
                slice(1) %>%
                select(-starts_with("author")) %>%
                ungroup(),

                articles %>%
                filter(`author-id-orcid` == "0000-0003-1385-6572") %>%
                select(starts_with("author")) %>%
                mutate(`author-affiliated-to-unit` = TRUE,
                       `author-employed-in-unit` = FALSE) %>%
                unique()
            ) %>% as_tibble()
    )
