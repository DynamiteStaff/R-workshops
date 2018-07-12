

Erez Hatnaa and Itzhak Benensonb (2012)
The Schelling Model of Ethnic Residential Dynamics: Beyond the Integrated - Segregated Dichotomy of Patterns
http://jasss.soc.surrey.ac.uk/15/1/6.html


bouts de code pas très explicites mais qui marche 

http://rsnippets.blogspot.com/2012/04/animating-schellings-segregation-model.html

https://simulatingcomplexity.wordpress.com/2016/01/06/building-a-schelling-segregation-model-in-r/


Super blogpost bien détaillé , code clair 
https://datashenanigan.wordpress.com/2015/05/12/agent-based-modelling-with-data-table-or-how-to-model-urban-migration-with-r/

TODO:

ne pas prendre en compte les cases vide  dans le similarité !!!!!!!


Modèle stylisé de villes pour jouer a Schelling 


Localisation aléatoire dans un shapefile de la ville avec des Unités Spatiales aggrégées par gros districts ~ 15


La vacance reste un paramètre , on l'ajoute par district au prorata du nombre de gens

et l'idée est de retrouver le paramètrage d'un Schelling qui reproduise la segragtion observée dans les données air  bnb et potentiellement tout autre source big data !


segrégation mesurée par les indicateurs classique de segrégation




Pistes d'approfondissement :


changement de  ville

réplication et stochasticité 

changement d'échelle (découpage plus fin district )

changement de colonnes.


changement de la définition du voisinage 

 ## Autres règles plus réalistes que celles de Schelling


 - c'est pas parce que tu regardes ton voisinage que t'as envie de bouger 
 - 

Lors de la dernière heure : 

Présenter tryptique  Perception Décision Action 

+ pistes de raffinage du modèle

Perception 
 - Voisinnage 
 - distance 

Décision 
	- critères d'évaluation des locations (et pas simplement vide )
	- les critères sont pas "durs", bouger a un coup, => buffer autour dd'un critère, et test de probabilité jusqu'à 10 pt autour du critère.
 	- contraintes à respecter "il FAUT que ce soit dans le meme district  à cause de l'école des gosses" mais je continue à regarder au cas ou y ait une place qui se libère
Action 
	- delayer l'action à n tours pour les pauvres le temps d'pargner le cout du déménagement alors que les salauds de riches peuvent déménager tout de suite
	- introduire la possibilité de ne pas agir (car sur-contraint, pas de thunes , pas de localisation acceptable etc....)







# Plan


lors de la Présentation de Schelling : introduire Perception Décision Action

multi modeling intervient après SChelling stylisé, 

dire qu'il y a un chemin CAUSAL 




















