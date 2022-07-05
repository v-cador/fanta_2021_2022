get_rencontres <- function(rencontres, is_random, seed = NULL, indice_factorial = 0){
  if(is_random){
    set.seed(seed)
    liste_joueurs <- unique(rencontres$joueur1)
    liste_joueurs_random <- liste_joueurs
    names(liste_joueurs_random) <- sample(liste_joueurs, 8, replace = FALSE)
    # remplacement dans les rencontres :
    rencontres <- rencontres %>%
      mutate_at(vars(starts_with("joueur")), ~liste_joueurs_random[.]) %>%
      mutate(seed = seed)
  }else if(indice_factorial > 0){
    liste_joueurs <- unique(rencontres$joueur1)
    liste_joueurs_indice <- liste_joueurs
    names(liste_joueurs_indice) <- table_factorial_joueurs[indice_factorial,]
    # remplacement dans les rencontres :
    rencontres <- rencontres %>%
      mutate_at(vars(starts_with("joueur")), ~liste_joueurs_indice[.]) %>%
      mutate(seed = seed)
  }
  rencontres
}

get_score_rencontres <- function(rencontres, score_journees_long, random_rencontres = FALSE, indice_factorial = 0){
  
  # Mise dans le désordre des rencontres
  if(random_rencontres){
    graine <- round(runif(1)*10000000)
    rencontres <- get_rencontres(rencontres, is_random = TRUE, seed = graine)
    # graine <<- graine + 1
  }else if(indice_factorial != 0){
    rencontres <- get_rencontres(rencontres, is_random = FALSE, indice_factorial = indice_factorial)
  }
  
  rencontres$is_random = random_rencontres
  
  # Calcul des scores de chaque joueur pour chaque journée
  score_rencontres <- rencontres %>%
    left_join(my_score_long %>% 
                select(score_joueur1 = score, joueur, journee), 
              by=c("joueur1" = "joueur", "journee")) %>%
    left_join(my_score_long %>% 
                select(score_joueur2 = score, joueur, journee), 
              by=c("joueur2" = "joueur", "journee")) %>% 
    filter(!is.na(score_joueur1)) %>%
    mutate(resultat_1n2 = case_when(score_joueur1 > score_joueur2 ~ "1",
                                    score_joueur1 == score_joueur2 ~ "n",
                                    score_joueur1 < score_joueur2 ~ "2"))
  
  score_rencontres = score_rencontres %>% 
    gather(type_joueur, joueur, c(joueur1, joueur2)) %>% 
    mutate(score = case_when(type_joueur == "joueur1" & resultat_1n2 == "1" ~ 3,
                             type_joueur == "joueur1" & resultat_1n2 == "n" ~ 1,
                             type_joueur == "joueur1" & resultat_1n2 == "2" ~ 0,
                             type_joueur == "joueur2" & resultat_1n2 == "1" ~ 0,
                             type_joueur == "joueur2" & resultat_1n2 == "n" ~ 1,
                             type_joueur == "joueur2" & resultat_1n2 == "2" ~ 3))
  
  return(score_rencontres)
}



get_classement <- function(score_rencontres, malus = NULL){
  if(!is.null(malus)){
    score_rencontres = score_rencontres %>%
      left_join(malus, by=c("joueur", "journee")) %>%
      mutate(score = score - malus)
  }
  
  # Classement final
  classement <- score_rencontres %>%
    mutate(buts_pour = if_else(type_joueur == "joueur1", score_joueur1, score_joueur2),
           buts_contre = if_else(type_joueur == "joueur1", score_joueur2, score_joueur1)) %>%
    group_by(joueur) %>%
    summarise(score_final = sum(score),
              nb_journees = n_distinct(journee),
              nb_vict = sum(score == 3),
              nb_nul = sum(score == 1),
              nb_def = sum(score == 0),
              buts_pour = sum(buts_pour),
              buts_contre = sum(buts_contre),
              gav = buts_pour - buts_contre) %>%
    arrange(desc(score_final), desc(gav))
  
  if(score_rencontres$is_random[1]){
    classement$seed = score_rencontres$seed[1]
  }
  
  return(classement)
}


best_classement_player <- function(table_res, name){
  temp <- table_res %>% 
    filter(joueur == name) %>%
    filter(ordre == min(ordre)) %>%
    pull(indice_factorial)
  table_res %>% filter(indice_factorial == temp[1]) 
}

worst_classement_player <- function(table_res, name){
  temp <- table_res %>% 
    filter(joueur == name) %>%
    filter(ordre == max(ordre)) %>%
    pull(indice_factorial)
  table_res %>% filter(indice_factorial == temp[1]) 
}