# Fractures numériques, sociales et spatiales


Selon une étude de l’INSEE, la région Centre-Val de Loire est l’une des régions ayant connu l’une des plus fortes extensions de ses aires périurbaines entre la fin du XXème siècle et le début des années 2000. Demeurant toutefois relativement rural, il s’agit d’un territoire à la situation ambigüe, bordé par l’Ile de France au Nord-Est et par des régions bien moins urbanisées au Sud. Aussi, nous avons décidé de nous intéresser à cette zone géographique en potentielle transition. 
Prenant comme point de départ les données de connexion internet dans cette région, nous avons ainsi souhaité étudier la répartition de cette dernière sur le territoire. Dans un second temps, nous avons entrepris de déterminer s’il existait une corrélation entre la qualité des connexions internet et la répartition sociale sur l’ensemble de ce territoire. 

Pour mener à bien cette étude, nous nous sommes servis de neuf jeux de données, contenant les informations nécessaires à nos différentes problématiques : 
➢Centre-Val-de-Loire-Base-Eligibilité (2021) : Il s’agit du point central de notre étude. Cette base contient une ligne par foyer dans un immeuble. Ce jeu de données présente notamment des données sur la classe d’éligibilité à internet d’un foyer. 

➢Centre-Val-de-Loire-Base-Immeuble (2021) : Cette base complète la base de données précédente. Elle apporte de l’information sur les immeubles. 

➢CONTOURS-IRIS (2020) : Ce jeu de données au format shapefile regroupe les coordonnées géographiques de chaque IRIS de France. 

➢INSEE_LOGEMENT_IRIS-COMMUNES (2015) : Ce fichier contient de nombreuses informations sur les logements et ménages dans chaque IRIS de France. 

➢INSEE_REVENUS_DISPONIBLES_IRIS (2016) : De même que le fichier logements, cette base donnes des informations sur les revenus de la population au sein de chaque IRIS. 

➢INSEE_REVENUS_DISPONIBLES_COMMUNES (2016) : Identique au fichier précédent, à l’exception de quelques variables. Ce fichier regroupe les informations de revenus au niveau communal. 

➢Niveau_Diplome_CVL (2017) : Ce jeu de données présente le plus haut niveau de diplôme obtenu par les habitants de chaque commune du Centre-Val de Loire. Une ligne corresponde à une commune. 

➢Tranche_Age_CVL (2017) : Ce jeu de données présente la répartition des tranches d’âge des habitants de chaque commune du Centre-Val de Loire. Une ligne corresponde à une commune. 

➢CSP_Emploi (2017) : Ce jeu de données présente la répartition des catégories socio-professionnelles des habitants de chaque commune du Centre-Val de Loire. Une ligne corresponde à une commune. 
Aussi, nous avons entrepris de réaliser cette étude en deux temps. En premier lieu, ayant une bonne connaissance de la manipulation de données avec le langage R, nous avons décidé de nettoyer et joindre regrouper nos fichiers avec le logiciel Rstudio. Une fois modifiés, nous importons alors nos fichiers « propres » sur l’outil QGIS afin de réaliser des cartes et de procéder à une analyse spatiale des données. Par ailleurs, nous avons procédé à une analyse statistique de nos données en langage Python, pour des raisons d’optimisation.

Les étapes de notre approche méthodologique, le rapport d’analyse ainsi que les scripts r et python sont consultables au sein du repository. 

