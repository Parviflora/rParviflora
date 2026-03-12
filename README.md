# [rParviflora]

## Description

rParviflora est un package R permettant de récupérer et de synthétiser automatiquement des informations de conservation sur les espèces végétales à partir des API REST de l'INPN (TaxRef) et d'OpenObs. Pour une espèce et un département donnés, il retourne en une seule ligne les statuts réglementaires, les listes rouges, les ratios de représentativité et un score d'enjeu de conservation.

⚠️ Depuis la cyber-attaque subie cet été 2025 par le Muséum national d’Histoire naturelle, le package dependant de leur API n'est donc pas utilisable pour le moment.

## Installation

Le package n'est pas encore sur le CRAN. Pour l'installer depuis GitHub :

```{r}
# install.packages("devtools")
devtools::install_github("Parviflora/rParviflora")
```

## Fonctions

-   `extraction_INPN()` — Fonction principale : agrège en un data.frame d'une ligne le taxref, le nom vernaculaire, les ratios de représentativité et les statuts INPN pour une espèce et un département donnés.
-   `match_inpn()` — Récupère l'identifiant TaxRef et le premier nom vernaculaire d'une espèce via un fuzzy matching sur l'API TaxRef.
-   `recuperer_statuts()` — Récupère et filtre les statuts INPN (liste rouge, protection, ZNIEFF, réglementation, directives européennes) pour un taxon et un département donnés.
-   `calculer_ratios()` — Calcule les ratios d'occurrences département/région, département/national et région/national via avec les données d'OpenObs, accessible via leur API.
-   `vulnerabilite()` — Évalue le niveau de vulnérabilité (1 à 5) d'une espèce à partir de ses statuts en liste rouge nationale et régionale.
-   `enjeu()` — Calcule un score d'enjeu de conservation

## Usages

```{r}
library(rParviflora)

# Cas d'usage principal : récupérer toutes les infos pour une espèce et un département
res <- extraction_INPN("Helianthemum ledifolium", departement = "aude")

# Avec calcul du score d'enjeu de conservation
res <- extraction_INPN("Helianthemum ledifolium", departement = "aude", cal_enjeu = TRUE)

# Utilisation des fonctions individuelles
id  <- match_inpn("Helianthemum ledifolium")
print(id$id)   # Identifiant TaxRef
print(id$nv)   # Nom vernaculaire

statuts <- recuperer_statuts(id$id, departement = "aude")
ratios  <- calculer_ratios(id$id, departement = "aude")

# Calcul manuel de la vulnérabilité et de l'enjeu
vulnerabilite("EN", "VU")
enjeu("VU", "EN", reg_nat = 0.8)
```

## Dependances

Le package repose sur httr et jsonlite pour les appels aux API REST.

R \>= 4.0.0 est recommandé.

Les données sont issues de :

-   TaxRef / INPN — référentiel taxonomique national OpenObs — observations naturalistes agrégées

## Contribution

Les issues et pull requests sont les bienvenues sur GitHub. Pour toute contribution, merci d'ouvrir une issue au préalable pour discuter des changements envisagés.

## Licence

MIT — voir le fichier LICENSE.md.

## Auteur

Martin DAVY — [martindavy25\@gmail.com](mailto:martindavy25@gmail.com){.email}
