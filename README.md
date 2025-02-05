# Installation
`remotes::install_github("associationbloom/bloomr.fleetregister")`

# Prérequis

Avant de commencer à utiliser le package **bloomr.fleetregister**, il faut déclarer les variables d'environnement pour vous connecter à la base de données.

Pour cela, dans la console R, lancez la commande `usethis::edit_r_environ()` et renseignez les variables communiquées dans .Renviron :

`POSTGRES_DB="????"`  
`POSTGRES_USER="????"`  
`POSTGRES_PASSWORD="????"`  
`POSTGRES_PORT=XXXX`  
`POSTGRES_HOSTNAME="????"`

Sauvegardez et redémarrez R pour appliquer les changements avec `.rs.restartR()`.

Vous pouvez maintenant utiliser le package ! Enjoy :)
