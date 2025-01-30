# Installation
`install.packages("bloomr.fleetregister")`


# Prérequis

Avant de commencer à utiliser le package **bloomr.fleetregister**, il faut déclarer les variables d'environnement pour vous connecter à la base de données.

Pour cela, dans la console R, lancer la commande `usethis::edit_r_environ()` et renseigner les variables communiquées dans .Renviron :

`POSTGRES_DB="????"`  
`POSTGRES_USER="????"`  
`POSTGRES_PASSWORD="????"`  
`POSTGRES_PORT=XXXX`  
`POSTGRES_HOSTNAME="????"`

Sauvegarder et redémarrer R pour appliquer les changements.

Vous pouvez maintenant utiliser le package ! Enjoy :)
