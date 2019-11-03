# Blockchain_TPEA

Utiliser le serveur en mode -no-check-sigs. Par défaut, le serveur doit être lancé en parallèle sur l'adresse localhost.

Pour lancer un tour à tour de 6 étapes entre 6 auteurs et 2 politiciens, exécuter la commande :

make run

Pour modifier le nombre d'auteurs et de politiciens, voir la partie run : all du Makefile

Pour modifier le nombre de tours de jeu, modifier la variable nb\_tour\_max dans utils.ml

Pour nettoyer les fichiers créés : 

make clean
