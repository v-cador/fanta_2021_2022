library(dplyr)
library(tidyr)

# Chargement des données

nb_joueurs <- 6
last_journee = 38

my_score <- read.table("data/scores.txt", sep="\t", dec=",", header = TRUE, stringsAsFactors = FALSE)

my_score_long <- my_score %>%
  gather(journee, score, -c(joueur, points_moy)) %>%
  mutate(score = (score - 60) %/% 4 + 1,
         score = if_else(score < 0, 0, score))

# my_score_long <- my_score_long %>%
#   filter(journee != "J22")

rencontres <- read.table("data/rencontres.txt", sep="\t", dec=",", header = TRUE, stringsAsFactors = FALSE) %>%
  select(-starts_with("score_"))
rencontres <- purrr::map_dfr(1:10, ~rencontres) %>%
  mutate(journee = rep(1:((nb_joueurs-1)*10), each = nb_joueurs/2)) %>%
  filter(journee <= last_journee) %>%
  mutate(journee = paste0("J",journee))

malus <- read.table("data/malus.txt", sep="\t", dec=",", header = TRUE, stringsAsFactors = FALSE) %>%
  select(-oubli.compo) %>%
  gather(journee, malus, -joueur)

table_factorial_joueurs <- arrangements::permutations(x = unique(rencontres$joueur1))

source("functions.R")


# Récupération du score
score_rencontres <- get_score_rencontres(rencontres, my_score_long, random_rencontres = FALSE)
# Récupération du classement
get_classement(score_rencontres, malus = malus)




# Simulation du classement avec désordre dans les rencontres :
library(furrr)
plan(multisession, workers = nb_joueurs)
nb_permutations <- factorial(nb_joueurs)
res <- furrr::future_map_dfr(1:nb_permutations, function(i){
  score_rencontres <- get_score_rencontres(rencontres, my_score_long, random_rencontres = FALSE, indice_factorial = i)
  get_classement(score_rencontres, malus = malus) %>%
    mutate(indice_factorial = i)
})
res = res %>% 
  mutate(ordre = rep(1:nb_joueurs, nb_permutations))
saveRDS(res, "resultat_tous_classements.RDS")
# res = res %>% 
#   mutate(ordre = rep(1:nb_joueurs, 10000))
# saveRDS(res, "resultat_10000_simulations.RDS")

classement_moyen <- res %>% 
  group_by(joueur) %>% 
  summarise(points_moy = mean(score_final),
            ordre_moy = mean(ordre),
            ordre_min = min(ordre),
            ordre_max = max(ordre)) %>% 
  arrange(desc(points_moy))
classement_moyen


library(ggplot2)
# Graphique de densité de points par joueur
ggplot(res %>% mutate(joueur = factor(joueur, levels = classement_moyen$joueur), ordered = TRUE)) +
  geom_density(aes(x=score_final, fill = joueur), alpha = 0.5, adjust = 2) +
  facet_wrap(~joueur, ncol = 1) +
  theme_minimal() +
  labs(title = "Densité des points avec toutes les permutations de noms possibles") +
  guides(color = FALSE) +
  scale_fill_viridis_d()
# Graphique de densité de classement par joueur
ggplot(res %>% mutate(joueur = factor(joueur, levels = classement_moyen$joueur), ordered = TRUE)) +
  geom_bar(aes(x=ordre, fill = joueur), alpha = 0.5, adjust = 5) +
  theme_minimal() +
  labs(title = "Classements avec toutes les permutations de noms possibles") +
  guides(color = FALSE) +
  scale_fill_viridis_d()

# Récupération de situations étonnantes :

# Meilleur classement de rodolphe
best_classement_player <- function(res, name){
  temp <- res %>% 
    filter(joueur == name) %>%
    filter(ordre == min(ordre)) %>%
    pull(indice_factorial)
  res %>% filter(indice_factorial == temp[1]) 
}
best_classement_player(res, "Rodolphe")
# best_classement_player(res, "Mallory")

# Moins bon classement de Valentin
worst_classement_player <- function(res, name){
  temp <- res %>% 
    filter(joueur == name) %>%
    filter(ordre == max(ordre)) %>%
    pull(indice_factorial)
  res %>% filter(indice_factorial == temp[1]) 
}
worst_classement_player(res, "Valentin")
