## Mikael Poul Johannesson
## 2018

## Start matter ------------------------------------------------------

library(here)
library(tidyverse)
library(magrittr)
library(rstan)

## Model results -----------------------------------------------------

## All availble vote tallys from the Norwegian parliament. Downloaded
## via data.stortinget.no API. See `01_data.R` for details.
## Md5 checksum: 98ca6900ea68fb2efcc01ac3dacbfdeb
## tools::md5sum(here("data", "votes.csv"))
votes <- read.csv(
  here("data", "votes.csv"),
  stringsAsFactors = FALSE
)

proposals <- read.csv(
  here("data", "proposals.csv"),
  stringsAsFactors = FALSE
)

reps <- read.csv(
  here("data", "reps.csv"),
  stringsAsFactors = FALSE
)

## ## One-dimensional IRT.
## ## MD5 checksum: 
## ## tools::md5sum(here("output", "fit_irt_onedim.rds"))
## fit_irt_onedim <- readRDS(here("output", "fit_irt_onedim.rds"))

## One-dimensional IRT using vb.
## MD5 checksum: 
## tools::md5sum(here("output", "fit_irt_vb_onedim.rds"))
fit_irt_onedim <- readRDS(here("output", "fit_irt_vb_onedim.rds"))

## Two-dimensional IRT using vb.
## MD5 checksum: 
## tools::md5sum(here("output", "fit_irt_vb_twodim.rds"))
fit_irt_twodim <- readRDS(here("output", "fit_irt_vb_twodim.rds"))

cols_party <- c(
  "Rødt"                      = "red4",
  "Sosialistisk Venstreparti" = "brown2",
  "Arbeiderpartiet"           = "red",
  "Senterpartiet"             = "springgreen3",
  "Miljøpartiet De Grønne"    = "olivedrab4",
  "Venstre"                   = "green3",
  "Kristelig Folkeparti"      = "yellow2",
  "Høyre"                     = "royalblue3",
  "Fremskrittspartiet"        = "purple4"
)

cols_party_short <- c(
  "R"   = "red4",
  "SV"  = "brown2",
  "AP"  = "red",
  "SP"  = "springgreen3",
  "MDG" = "olivedrab4",
  "V"   = "green3",
  "KRF" = "yellow2",
  "H"   = "royalblue3",
  "FRP" = "purple4"
)

## One-dim model -----------------------------------------------------
         
theta_onedim <-
  fit_irt_onedim %>%
  rstan::extract() %>%
  magrittr::extract2("theta") %>%
  as_data_frame() %>%
  gather(var, theta) %>%
  mutate(representative = as.numeric(gsub("\\D", "", var))) %>%
  group_by(representative) %>%
  summarize(
    est = mean(theta),
    se = sd(theta) / sqrt(n())
  ) %>%
  ungroup()

onedim_idealpoints <-
  theta_onedim %>%
  left_join(reps, by = "representative") %>%
  mutate(
    rep_name = fct_reorder(rep_name, est),
    session_max = sapply(sapply(rep_sessions, strsplit, ","), max),
    session_max = as.numeric(gsub("(\\d+)-(\\d+)", "\\1", session_max))
  )

onedim_idealpoints %>%
  filter(rep_last == "Moxnes")

onedim_idealpoints %>%
  filter(rep_last == "Hoksrud")

fig_data <-   
  onedim_idealpoints %>%
  arrange(rep_party, desc(rep_name)) %>%
  mutate(rep_name = paste0("plain('", rep_name, "')")) %>%
  bind_rows(
    data_frame(
      rep_name = paste0("bold('", names(cols_party), "')"),
      rep_party = factor(names(cols_party)),
      rep_party_short = names(cols_party_short),
      session_max = 2018
    )
  ) %>%
  mutate(
    rep_name = fct_inorder(rep_name),
    rep_party = fct_inorder(rep_party)
  ) %>%
  arrange(desc(rep_party), rep_name) %>%
  mutate(rep_name = fct_inorder(rep_name))

fig_left <-
  fig_data %>%
  filter(
    session_max == 2018,
    rep_party_short %in% c("R", "SV", "AP", "SP", "MDG")
  ) %>%
  mutate(rep_name = fct_inorder(factor(rep_name))) %>%
  ggplot(aes(
    x = est, y = rep_name,
    xmin = est - (2 * se), xmax = est + (2 * se),
    group = rep_party
  )) +
  facet_grid(rep_party ~ ., scales = "free_y", space = "free_y") +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  geom_point() +
  geom_errorbarh(height = 0) +
  scale_x_continuous(
    limits = c(-7.5, 7.5),
    expand = c(0, 0),
    breaks = seq(-7.5, 7.5, 2.5)) +
  scale_y_discrete(
    labels = function(x) parse(text = as.character(x))) +
  scale_colour_manual(values = cols_party) +
  labs(x = "Ideal Point (theta)", y = "Member of Parliament") +
  descr2::theme_m() +
  theme(
    axis.text.y = element_text(size = 7),
    panel.spacing.y = unit(2, "mm")
  )
fig_left

fig_right <-
  fig_data %>%
  filter(
    session_max == 2018,
    !(rep_party_short %in% c("R", "SV", "AP", "SP", "MDG"))
  ) %>%
  mutate(rep_name = fct_inorder(factor(rep_name))) %>%           
  ggplot(aes(
    x = est, y = rep_name,
    xmin = est - (2 * se), xmax = est + (2 * se),
    group = rep_party
  )) +
  facet_grid(rep_party ~ ., scales = "free_y", space = "free_y") +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  geom_point() +
  geom_errorbarh(height = 0) +
  scale_x_continuous(
    limits = c(-7.5, 7.5),
    expand = c(0, 0),
    breaks = seq(-7.5, 7.5, 2.5)) +
  scale_y_discrete(
    labels = function(x) parse(text = as.character(x))) +
  scale_colour_manual(values = cols_party) +
  labs(x = "Ideal Point (theta)", y = "Member of Parliament") +
  descr2::theme_m() +
  theme(
    axis.text.y = element_text(size = 7),
    panel.spacing.y = unit(2, "mm")
  )
fig_right

onedim_idealpoints %>%
  filter(!is.na(est), !is.na(rep_party_short)) %>%
  group_by(rep_party_short) %>%
  summarize(
    avg = mean(est),
    avg_se = sd(est) / sqrt(n())
  ) %>%
  ungroup() %>%
  mutate(rep_party_short = fct_reorder(rep_party_short, avg, .desc = TRUE)) %>%
  ggplot(aes(
    x = avg, y = rep_party_short,
    xmin = avg - (2 * avg_se),
    xmax = avg + (2 * avg_se))
    ) +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  geom_errorbarh(height = 0, size = .75) +
  scale_x_continuous(
    limits = c(-4, 4),
    expand = c(0, 0),
    breaks = seq(-4, 4, 2)) +
  descr2::theme_m()


## Two-dim model -----------------------------------------------------

theta_twodim <-
  fit_irt_twodim %>%
  rstan::extract() %>%
  magrittr::extract2("theta") %>%
  as_data_frame() %>%
  gather(var, theta) %>%
  mutate(
    representative = as.numeric(gsub("^(\\d+)\\.\\d+$", "\\1", var)),
    dim = as.numeric(gsub("^\\d+\\.(\\d)$", "\\1", var))
  ) %>%
  group_by(representative, dim) %>%
  summarize(
    est = mean(theta),
    se = sd(theta) / sqrt(n())
  ) %>%
  ungroup()

twodim_idealpoints <-
  theta_twodim %>%
  dplyr::select(representative, est, dim) %>%
  mutate(dim = paste0("dim_", dim)) %>%
  spread(dim, est) %>%
  left_join(reps, by = "representative") %>%
  filter(rep_party != "Uavhengig representant") %>%
  group_by(rep_party) %>%
  mutate(
    highlight = ifelse(
      abs(dim_1) == abs(dim_1)[1],
      rep_party, NA
    )
  ) %>%
  ungroup() %>%
  mutate(
    rep_party = lvls_reorder(rep_party, c(6, 8, 1, 7, 5, 9, 4, 3, 2)),
    rep_party_short = case_when(
      rep_party == "Rødt" ~ "R",
      rep_party == "Sosialistisk Venstreparti" ~ "SV",
      rep_party == "Arbeiderpartiet"           ~ "AP",
      rep_party == "Senterpartiet"             ~ "SP",
      rep_party == "Miljøpartiet De Grønne"    ~ "MDG",
      rep_party == "Venstre"                   ~ "V",
      rep_party == "Kristelig Folkeparti"      ~ "KRF",
      rep_party == "Høyre"                     ~ "H",
      rep_party == "Fremskrittspartiet"        ~ "FRP"),
    rep_party_short = lvls_reorder(
      rep_party_short, c(6, 8, 1, 7, 5, 9, 4, 3, 2)
    ),
    highlight = ifelse(!is.na(highlight), as.character(rep_party_short), NA)
  )

fig_twodim <-
  twodim_idealpoints %>%
  ggplot(aes(x = dim_1, y = dim_2, colour = rep_party_short)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  geom_point(size = 2.5, alpha = .75) +
  ggrepel::geom_text_repel(
    aes(label = highlight),
    colour = "black", size = 3
  ) +
  scale_x_continuous(
    limits = c(-4, 4),
    expand = c(0, 0),
    breaks = seq(-4, 4, 2)
  ) +
  scale_y_continuous(
    limits = c(-4, 4),
    expand = c(0, 0),
    breaks = seq(-4, 4, 2)
  ) +
  scale_colour_manual(values = cols_party_short) +
  labs(
    x = "MP ideal point\n(first dimension)",
    y = "MP ideal point\n(second dimension)"
  ) +
  guides(col = guide_legend(ncol = 3, title = NULL, placement = "right")) +
  descr2::theme_m() +
  theme(
    legend.position = c(.8, .9),
    legend.background = element_rect(fill = "grey90", colour = "grey90")
  )
fig_twodim



