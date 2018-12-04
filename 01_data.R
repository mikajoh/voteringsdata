## Mikael Poul Johannesson
## 2018

## Fetches all Norwegian parliament (Stortinget) roll-calls that are
## available via the Stortinget API (data.stortinget.no) and preps
## them for further analysis. it should include all votes since the
## 2011/2012.

## Start matter ------------------------------------------------------

library(here)
library(dplyr)
library(tidyr)
library(forcats)
library(purrr)
library(rvest)
library(xml2)
library(httr)

## Fetch sessions ----------------------------------------------------

fetch_sessions <- function() {

  r <- GET("http://data.stortinget.no/eksport/sesjoner")
  raw <- content(r, type = "text/html", encoding = "UTF-8")

  raw %>%
    xml_node("sesjoner_liste") %>%
    as_list() %>%
    map_df(as_data_frame) %>%
    unnest() %>%
    mutate(
      ses_id = id,
      ses_from = as.Date(fra),
      ses_to = as.Date(til)
    ) %>%
    select(matches("ses"))  
}

sessions <- fetch_sessions()

write.csv(
  sessions,
  file = here("data", "sessions.csv"),
  row.names = FALSE
)

## Fetch all proposals -----------------------------------------------

fetch_proposals <- function(ses_id) {

  fetch_vote_data <- function(prop_id) {
    r <- GET(
      "http://data.stortinget.no/eksport/voteringer",
      query = list(sakid = prop_id)
    )
    raw <- content(r, type = "text/html", encoding = "UTF-8")
    
    data_frame(
      prop_id = prop_id,
      vote_id = raw %>%
        xml_node("votering_id") %>%
        xml_text() %>%
        as.numeric(),
      vote_date = raw %>%
        xml_nodes("votering_tid") %>%
        xml_text() %>%
        as.Date() %>%
        last()
    )
  }

  r <- GET(
    "http://data.stortinget.no/eksport/saker",
    query = list(sesjonid = ses_id)
  )
  raw <- content(r, type = "text/html", encoding = "UTF-8")
  
  out <- data_frame(
    ses_id = ses_id,
    prop_id = raw %>%
      xml_nodes("sak") %>%
      xml_nodes("henvisning + id") %>%
      xml_text() %>%
      as.numeric(),
    prop_title = raw %>%
      xml_nodes("sak") %>%
      xml_nodes("korttittel") %>%
      xml_text(),
    prop_title_long = raw %>%
      xml_nodes("sak") %>%
      xml_nodes("tittel") %>%
      xml_text(),
    prop_type = raw %>%
      xml_nodes("sak") %>%
      xml_nodes("type") %>%
      xml_text()
  )
  out_add <- map_df(out$prop_id, fetch_vote_data)

  left_join(out, out_add, by = "prop_id")
}

proposals <- map_df(sessions$ses_id, fetch_proposals)

write.csv(
  proposals,
  file = here("data", "proposals.csv"),
  row.names = FALSE,
  na = ""
)


## Fetch all roll-calls ----------------------------------------------

fetch_votes <- function(vote_id) {

  r <- GET(
    "http://data.stortinget.no/eksport/voteringsresultat",
    query = list(VoteringId = vote_id)
  )
  raw <- content(r, type = "text/html", encoding = "UTF-8")

  data_frame(
    vote_id = vote_id,
    rep_id = raw %>%
      xml_nodes("representant_voteringsresultat > representant > id") %>%
      xml_text(),
    rep_first = raw %>%
      xml_nodes("representant_voteringsresultat > representant > fornavn") %>%
      xml_text(),
    rep_last = raw %>%
      xml_nodes("representant_voteringsresultat > representant > etternavn") %>%
      xml_text(),
    rep_gender = raw %>%
      xml_nodes("representant_voteringsresultat > representant > kjoenn") %>%
      xml_text(),
    rep_county = raw %>%
      xml_nodes("representant_voteringsresultat > representant > fylke > navn") %>%
      xml_text(),
    rep_party = raw %>%
      xml_nodes("representant_voteringsresultat > representant > parti > navn") %>%
      xml_text(),
    vote_chr = raw %>%
      xml_nodes("representant_voteringsresultat > votering") %>%
      xml_text()
  )
}

votes_raw <- map_df(proposals$vote_id[!is.na(proposals$vote_id)], fetch_votes)

write.csv(
  votes_raw,
  file = here("data", "votes_raw.csv"),
  row.names = FALSE
)

## Extract data on MPs -----------------------------------------------

reps <-
  votes_raw %>%
  left_join(proposals, by = "vote_id") %>%
  group_by_at(vars(matches("^rep"))) %>%
  summarize(
    rep_name = paste(rep_first, rep_last)[1],
    rep_sessions = paste0(ses_id, collapse = ", ")
  ) %>%
  ungroup() %>%
  mutate(
    rep_party = lvls_reorder(
      rep_party, c(9, 6, 8, 1, 7, 5, 10, 4, 3, 2)),
    rep_party_short = case_when(
      rep_party == "Rødt"                      ~ "R",
      rep_party == "Sosialistisk Venstreparti" ~ "SV",
      rep_party == "Arbeiderpartiet"           ~ "AP",
      rep_party == "Senterpartiet"             ~ "SP",
      rep_party == "Miljøpartiet De Grønne"    ~ "MDG",
      rep_party == "Venstre"                   ~ "V",
      rep_party == "Kristelig Folkeparti"      ~ "KRF",
      rep_party == "Høyre"                     ~ "H",
      rep_party == "Fremskrittspartiet"        ~ "FRP"),
    rep_party_short = lvls_reorder(
      rep_party_short, c(6, 8, 1, 7, 5, 9, 4, 3, 2))
  ) %>%
  arrange(rep_party, rep_last)

write.csv(
  reps,
  file = here("data", "reps.csv"),
  row.names = FALSE
)

## Prep roll-calls ---------------------------------------------------

## In the IRT model I have fixed the first and second represententive at
## -1 and 1 idealpoints, as priors for the left and right wing,
## respectively. Bjørnar Moxnes (R) is the left-wing prior and Sylvi
## Listhaug (FRP) is the right-wing prior.
votes_index_pri <- c(
  which(votes_raw$rep_last == "Moxnes" & votes_raw$vote_chr != "ikke_tilstede")[1],
  which(votes_raw$rep_last == "Listhaug" & votes_raw$vote_chr != "ikke_tilstede")[1]
)
votes_index <- c(
  votes_index_pri,
  setdiff(1:nrow(votes_raw), votes_index_pri)
)  

votes <-
  votes_raw[votes_index, ] %>%
  filter(vote_chr != "ikke_tilstede") %>%
  left_join(
    proposals %>%
      select(vote_id, vote_date) %>%
      filter(!duplicated(vote_id)),
    by = "vote_id"
  ) %>%
  left_join(
    reps %>% select(rep_id, rep_name, rep_party_short),
    by = "rep_id"
  ) %>%
  mutate(
    rep_id = ifelse(is.na(rep_id), "NA", rep_id),
    representative = as.numeric(fct_inorder(rep_id)),
    proposal = as.numeric(fct_inorder(factor(vote_id))),
    vote = case_when(
      vote_chr == "for" ~ 1,
      vote_chr == "mot" ~ 0)
  ) %>%  
  select(
    matches("vote_"), matches("rep_"),
    representative, proposal, vote
  )

write.csv(
  votes,
  file = here("data", "votes.csv"),
  row.names = FALSE
)

## END ---------------------------------------------------------------
