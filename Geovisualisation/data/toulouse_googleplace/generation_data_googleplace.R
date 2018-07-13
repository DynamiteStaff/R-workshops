rm(list=ls())
#devtools::install_github("SymbolixAU/googleway")
library(googleway)

## not specifying the api will add the key as your 'default'
key <- "XXX"


Resto_Toulouse_1 <- google_places(location = c(43.603235,  1.444683), #avant dernier en metre, x=hautbas,y=droitegauche
                                keyword = "Restaurant",
                                radius = 5000,
                                key = key)

# Ou : 200 lignes mais juste l'info de la géolocalisation (pas le nom du resto, sa note, son prix... )
# Radar1 <- google_places(location = c(43.603235,  1.444683), #avant dernier en metre, x=hautbas,y=droitegauche
#                                   keyword = "Restaurant",
#                                   radius = 5000,
#                                   radar=TRUE,
#                                   key = key)


#next_page_token n'est pas renvoyé s'il n'y a aucun résultat supplémentaire à afficher.
#Le nombre maximum de résultats pouvant être renvoyés est de 60. Il existe un délai entre la
#génération d'un jeton next_page_token et son entrée en vigueur.
# Bug : la colonne price_level n'est pas renvoyée en sortie quand on spécifie un next_page_token
for(i in 2:3){
   
  toto <- google_places(location = c(43.603235,  1.444683),
                                  keyword = "Restaurant",
                                  radius = 5000,
                                  page_token = with(get(paste0("Resto_Toulouse_",i-1)),next_page_token),
                                  key = key)
  assign(paste0("Resto_Toulouse_",i),toto)
}


# Fonction tirée par les cheveux car sinon je n'arrive pas à faire de rbind...
rbind_2 <- function(x,y){
  result <- x
  if(class(result) == "data.frame"){
    if(all(!sapply(result,class) %in% c("data.frame","list"))){
      result <- rbind(result, y)
    }else{
      noms_col <- names(result)
      n_col <- noms_col[1]
      res_temp <- lapply(noms_col,function(n_col){
        print(n_col)
        rbind_2(x = result[[n_col]],
                y = y[[n_col]])
      })
      names(res_temp) <- noms_col
      d <- data.frame(res_temp[[1]])
      noms_temp <- names(d)
      
      
      for(n_col in noms_col){
        d[[n_col]] <- res_temp[[n_col]]
      }
       d <- d[-c(1:length(noms_temp))]
      result <-  d
    }
  }else{
    if(class(result) == "list"){
      noms_elems <- names(result)
      nb_elem <- length(result)
      result <- lapply(1:nb_elem,function(i){
        rbind_2(x = result[[i]],
                y = y[[i]])
      })
      names(result) <- noms_elems
      
    }else{
      result <- c(result,y)
    }
  }
  
  result
}

# Agrégation des résultats.
Resto_Toulouse_2$results$price_level <- NA
Resto_Toulouse_3$results$price_level <- NA
Resto_Toulouse <- rbind_2(Resto_Toulouse_1$results,rbind_2( Resto_Toulouse_2$results,Resto_Toulouse_3$results))

save(Resto_Toulouse, file="Resto_Toulouse_test.RData")
