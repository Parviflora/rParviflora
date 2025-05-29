anciennes_nouvelles_regions <- utils::data("anciennes_nouvelles_regions", package = "rParviflora", envir = environment())

#' Cette fonction permet de recuperer plusieurs informations d'interets de l'INPN sur une
#' espece donnee en interrogeant les API REST de l'INPN et de OpenObs
#'
#'
#' @param espece Une chaine de caracteres correspondant au nom scientifique de l'espece.
#' @param departement Une chaine de caracteres specifiant le nom du departement pour lequel les
#'   statuts doivent etre recuperes.
#'
#' @return Un data.frame d'une ligne contenant :
#'   \itemize{
#'     \item \code{taxref} : l'identifiant TaxRef de l'espece.
#'     \item \code{espece} : nom scientifique donne en parametre.
#'     \item \code{nom.vernaculaires} : Le premier nom vernaculaire associe a l'espece, si disponible.
#'     \item \code{dep.reg} : Le ratio entre le nombre d'occurrences dans le departement et le nombre
#'       d'occurrences dans la region.
#'     \item \code{dep.nat} : Le ratio entre le nombre d'occurrences dans le departement et le nombre
#'       d'occurrences au niveau national.
#'     \item \code{reg.nat} : Le ratio entre le nombre d'occurrences dans la region et le nombre
#'       d'occurrences au niveau national.
#'     \item \code{Les statuts ...} : Les informations sur le type de statut (protection, liste rouge, reglementation, ZNIEFF)...
#'     \item \code{enjeux} : score d'enjeu de conservation d'une espece
#'
#'   }
#'
#' @details
#'  \itemize{
#'    \item Pour les colonnes \code{taxref} et \code{nom.vernaculaire} voir \code{\link{definir_id_inpn}}.
#'    \item Pour \code{dep.reg}, \code{dep.nat} et \code{reg.nat} voir \code{\link{calculer_ratios}}.
#'    \item Pour \code{Les Statuts ...} voir \code{\link{recuperer_statuts}}.
#'    \item Pour \code{enjeux} voir \code{\link{enjeu}}
#'  }
#'
#' @note
#' Cette fonction necessite les packages \code{httr} et \code{jsonlite}. Si ces derniers ne sont pas installes,
#' la fonction s'arrete et invite l'utilisateur a les installer.
#'
#' @examples
#' \dontrun{
#' # Exemple d'utilisation
#' espece <- "Helianthemum ledifolium"
#' departement <- "aude"
#'
#' res <- extraction_IPNI(espece, departement)
#' }
#' @export
extraction_INPN = function(espece, departement, cal_enjeu = FALSE){
  if(grepl(" sp\\.", espece)){
    return(data.frame("espece" = espece))
  }

  id = definir_id_inpn(espece)
  if(is.na(id$id)){
    return(data.frame("espece" = espece))
  }
  statuts = recuperer_statuts(id$id, departement)
  ratio = calculer_ratios(id$id, departement)

  # vecteur des valeurs de status, en prenant soin de fusion les strings qui ont
  # le meme nom
  # cas ou il y a plusieurs annexe d'une directive par exemple
  valeurs_statuts = NULL
  for(i in seq_along(statuts$statusTypeGroup)){
    nom    = statuts$col_names[i]
    valeur = statuts$statusCode[i]

    if(nom %in% names(valeurs_statuts)){
      valeurs_statuts[nom] = paste(valeurs_statuts[nom], valeur, sep = " / ")
    } else {
      valeurs_statuts = c(valeurs_statuts, valeur)
      names(valeurs_statuts)[length(valeurs_statuts)] = nom
    }
  }

  # compilation des donnees
  res = c(
    id$id,
    espece,
    id$nv,
    unlist(ratio),
    valeurs_statuts
  )
  names(res) = c(
    "taxref",
    "espece",
    "nom vernaculaires",
    names(ratio),
    names(valeurs_statuts)
  )

  # formatage des donnees
  res.df = data.frame(t(res))

  res.df$taxref = as.numeric(res.df$taxref)
  res.df$dep.reg = as.numeric(res.df$dep.reg)
  res.df$dep.nat = as.numeric(res.df$dep.nat)
  res.df$reg.nat = as.numeric(res.df$reg.nat)


  # calcul de l'enjeu
  index_lr_nat = grep("Liste.rouge.nationale.france.metropolitaine", colnames(res.df))
  index_lr_reg = grep("Liste.rouge.regionale", colnames(res.df))

  lr_nat = ifelse(length(index_lr_nat) == 1, res.df[,index_lr_nat], NA)
  lr_reg = ifelse(length(index_lr_reg) == 1, res.df[,index_lr_reg], NA)
  reg_nat = res.df$reg.nat

  if(cal_enjeu){
    res.df$enjeux = enjeu(lr_nat, lr_reg, reg_nat)
  }

  return(res.df)
}




#' Identifier l'ID INPN et le nom vernaculaire d'une espece
#'
#' Cette fonction permet de recuperer l'identifiant unique (ID) INPN ainsi que le nom vernaculaire
#' associe a une espece donnee en interrogeant l'API REST de l'INPN
#'
#' @param espece Une chaine de caracteres correspondant au nom scientifique de l'espece.
#'   Les espaces en trop seront automatiquement supprimes.
#'
#' @return Une liste contenant :
#'   \itemize{
#'     \item \code{id} : L'identifiant numerique correspondant l'identifiant TaxRef de l'espece.
#'     \item \code{nv} : Le premier nom vernaculaire associe a l'espece, si disponible.
#'   }
#'   Si aucun ID ne peut etre determine, \code{id} sera \code{NA}. Si aucun nom vernaculaire n'est trouve,
#'   \code{nv} sera \code{NA}.
#'
#' @details
#' La fonction effectue une recherche approximative (\emph{fuzzy matching}) sur le nom scientifique de l'espece
#' pour recuperer les informations depuis l'API TaxRef. Les sous-especes, varietes et formes, ainsi que les noms
#' illegitimes, sont exclus de la recherche.
#'
#' @note
#' Cette fonction necessite les packages \code{httr} et \code{jsonlite}. Si ces derniers ne sont pas installes,
#' la fonction s'arrete et invite l'utilisateur a les installer.
#'
#' @examples
#' \dontrun{
#' # Exemple d'utilisation
#' resultat <- definir_id_inpn("Helianthemum ledifolium")
#' print(resultat$id)  # Affiche l'ID INPN
#' print(resultat$nv)  # Affiche le nom vernaculaire
#' }
#'
#' @seealso \code{\link[httr]{GET}}, \code{\link[jsonlite]{fromJSON}}
#'
#' @export
definir_id_inpn <- function(espece) {
  # chargement des packages
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Please install the following packages : httr, jsonlite")
  }
  # Suppression des espaces en trop
  espece.clean <- gsub("(^ +)|( +$)", "", espece)

  # Construction de URL pour faire la requete
  target <- gsub(" +", "%20", espece.clean)
  call <- paste(
    "https://taxref.mnhn.fr/api/taxa/fuzzyMatch?term=",
    target,
    sep = ""
  )

  # Extraction de l'identifiant INPN via la REST API
  res <- httr::GET(url = call)
  res.content <- httr::content(res, "text", encoding = "UTF-8")
  res.json <- jsonlite::fromJSON(res.content, flatten = TRUE)
  taxa <- res.json$`_embedded`$taxa

  if(is.null(taxa)){
    return(
      list(
        "id" = NA,
        "nv" = NA
      )
    )
  }

  # Identifier les sous-especes, varietes et forme pour ne pas les prendre en compte
  aIgnorer <- grepl("nom\\. illeg\\.", taxa$authority)

  if(length(strsplit(espece.clean, " ")[[1]]) == 2){
    aIgnorer <- aIgnorer |
      grepl(" subsp\\. ", taxa$referenceName) |
      grepl(" var\\. "  , taxa$referenceName) |
      grepl(" f\\. "    , taxa$referenceName)
  }

  # Identifier les lignes associees uniquement a l'espece
  i <- grepl(espece, taxa$referenceName) & !aIgnorer

  # Extraire l'identifiant des donnees
  id <- NA
  if (sum(i) == 1) {
    id <- taxa[i, "referenceId"]
  } else {
    ref_id <- taxa$referenceId[i]
    if (length(unique(ref_id)) == 1) {
      j <- taxa$id == ref_id[1] & i
      if (sum(j) == 1) {
        id <- taxa[j, "referenceId"]
      }
    } else {
      # cas ou il y a plusieurs id proposé
    }
  }

  nv = NA

  if(!is.na(taxa$frenchVernacularName[1])){
    nv = unlist(strsplit(taxa$frenchVernacularName, ", "))[1]
  }

  return(
    list(
      "id" = id,
      "nv" = nv
    )
  )

}

#' Recuperation des statuts INPN d'un taxon
#'
#' Cette fonction recupere les statuts INPN associes a un taxon donne pour un departement specifique,
#' ainsi que pour les anciennes et nouvelles regions correspondantes.
#'
#' @param id Un identifiant numerique correspondant l'identifiant TaxRef de l'espece
#' @param departement Une chaine de caracteres specifiant le nom du departement pour lequel les
#'   statuts doivent etre recuperes.
#'
#' @return Un tableau (\code{data.frame}) contenant les statuts filtres pour le taxon, avec les colonnes suivantes :
#'   \itemize{
#'     \item Les informations sur le type de statut (protection, liste rouge, reglementation, ZNIEFF).
#'     \item Les localisations pertinentes (France metropolitaine, departement, ancienne et nouvelle region).
#'     \item Une colonne supplementaire \code{col_names} combinant le type de statut et la localisation pour faciliter l'interpretation.
#'   }
#'   Les lignes non pertinentes sont exclues du resultat.
#'
#' @details
#' La fonction interroge l'API REST de l'INPN pour obtenir les statuts d'un taxon. Ces statuts sont filtres pour inclure
#' uniquement les localisations et categories d'interet (liste rouge, protection, ZNIEFF, reglementation).
#'
#' @note
#' Cette fonction necessite les packages \code{httr} et \code{jsonlite}. Si ces derniers ne sont pas installes,
#' la fonction s'arrete et invite l'utilisateur a les installer.
#'
#' @examples
#' \dontrun{
#' # Exemple d'utilisation
#' id = definir_id_inpn("Helianthemum ledifolium")
#'
#' result <- recuperer_statuts(id$id, departement = "aude")
#' print(result)
#' }
#'
#' @seealso \code{\link[httr]{GET}}, \code{\link[jsonlite]{fromJSON}}
#'
#' @export
recuperer_statuts <- function(id, departement) {
  # chargement des packages
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Please install the following packages : httr, jsonlite")
  }

  # Construction de URL pour faire la requete
  call <- paste(
    "https://taxref.mnhn.fr/api/taxa/",
    id,
    "/status/lines",
    sep = ""
  )

  # Extraction des statuts INPN via la REST API
  res <- httr::GET(url = call)
  res.content <- httr::content(res, "text", encoding = "UTF-8")
  res.json <- jsonlite::fromJSON(res.content, flatten = TRUE)
  statuts <- res.json$`_embedded`$status


  # enlever accents, majuscule et espace inutile dans group de statuts
  statuts$statusTypeGroup = gsub( # enlever les espaces inutiles
    "(^ *| *$|( ) *)",
    "\\2",
    tolower( #mettre en minuscule
      iconv( # enlever les accents
        statuts$statusTypeGroup, from = "UTF-8", to = "ASCII//TRANSLIT"
      )
    )
  )

  # ne garder que les statuts lies :
  # - france
  # - departement
  # - ancienne et nouvelle region du departement
  l_dep = rParviflora::anciennes_nouvelles_regions$dep_name == departement
  ancienne_region <- rParviflora::anciennes_nouvelles_regions$anciennes_regions[l_dep]
  nouvelle_region <- rParviflora::anciennes_nouvelles_regions$nouvelles_regions[l_dep]

  # nettoyage du noms des localisations
  #     - enlever les majuscules
  localisations <- tolower(statuts$locationName)
  #     - enlever les accents
  localisations <- iconv(localisations, from = "UTF-8", to = "ASCII//TRANSLIT")
  #     - remplacer les tirets - par un espace
  localisations <- gsub("-", " ", localisations)

  # identifier les lignes d'interets dans les donnees recues
  l <- localisations %in% c(departement, ancienne_region, nouvelle_region, "france metropolitaine")
  s <- statuts$statusTypeGroup %in% c("liste rouge", "protection", "znieff", "reglementation", "statut biogeographique", "directives europeennes")
  i <- statuts$taxon.id == id

  res_statuts = statuts[l & s & i, ]
  res_statuts$col_names = paste(statuts$statusTypeName[l & s & i], localisations[l & s & i])

  aIgnorer = grepl("statusRemarks", colnames(statuts))
  res_statuts = unique(res_statuts[,!aIgnorer])

  return(res_statuts)
}



#' Calcul des ratios de representativite
#'
#' Cette fonction calcule les ratios de representativite pour un taxon donne aux niveaux departemental,
#' regional et national en utilisant les donnees fournies par l'API REST OpenObs.
#'
#' @param id Un identifiant numerique correspondant l'identifiant TaxRef de l'espece
#' @param departement Une chaine de caracteres specifiant le nom du departement pour lequel les
#'   ratios doivent etre calcules.
#'
#' @return Une liste nommee contenant trois ratios :
#'   \itemize{
#'     \item \code{dep/reg} : Le ratio entre le nombre d'occurrences dans le departement et le nombre
#'       d'occurrences dans la region.
#'     \item \code{dep/nat} : Le ratio entre le nombre d'occurrences dans le departement et le nombre
#'       d'occurrences au niveau national.
#'     \item \code{reg/nat} : Le ratio entre le nombre d'occurrences dans la region et le nombre
#'       d'occurrences au niveau national.
#'   }
#'   Si l'un des niveaux (departement, region ou national) ne contient aucune occurrence, le ratio
#'   correspondant est defini comme \code{NA}.
#'
#' @details
#' La fonction interroge l'API REST d'OpenObs pour recuperer le nombre d'occurrences d'un taxon specifique
#' aux niveaux departemental, regional et national. Ces valeurs sont ensuite utilisees pour calculer
#' les ratios de representativite.
#'
#' @note
#' Cette fonction necessite les packages \code{httr} et \code{jsonlite}. Si ces derniers ne sont pas installes,
#' la fonction s'arrete et invite l'utilisateur a les installer.
#'
#' @examples
#' \dontrun{
#' # Exemple d'utilisation
#' id = definir_id_inpn("Helianthemum ledifolium")
#'
#' result <- calculer_ratios(id$id, departement = "aude")
#' print(result)
#' }
#'
#' @seealso \code{\link[httr]{GET}}, \code{\link[jsonlite]{fromJSON}}
#'
#' @export
calculer_ratios <- function(id, departement) {
  # chargement des packages
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Please install the following packages : httr, jsonlite")
  }

  # Construire l'URL pour avoir les ratios de representativite
  # niveaux regional et national d'interet
  niveauNational <- "CERTAIN,PROBABLE,NON_REALISABLE"
  niveauRegional <- "CERTAIN,PROBABLE,NON_REALISABLE,NON_EVALUE"

  # obtenir le code INSEE du departement et de la region
  l_dep = rParviflora::anciennes_nouvelles_regions$dep_name == departement
  codeDepartement <- rParviflora::anciennes_nouvelles_regions$code_insee_departement[l_dep]
  codeRegion      <- rParviflora::anciennes_nouvelles_regions$code_insee_nouvelle_region[l_dep]

  # url pour la requete pour le niveau DEPARTEMENT faite via l'API REST de OpenObs
  urlDepartement <- paste(
    "https://openobs.mnhn.fr/api/",
    "occurrences/stats/taxon/",
    id,
    "?observationStatus=PRESENT",
    "&validationLevelNationale=", niveauNational,
    "&validationLevelRegionale=", niveauRegional,
    "&departmentInseeId=", codeDepartement,
    sep = ""
  )


  res_dep <- httr::GET(url = urlDepartement)
  res_dep.content <- httr::content(res_dep, "text", encoding = "UTF-8")
  res_dep.json <- jsonlite::fromJSON(res_dep.content, flatten = TRUE)

  # url pour la requete pour le niveau REGION faite via l'API REST de OpenObs
  urlRegion <- paste(
    "https://openobs.mnhn.fr/api/",
    "occurrences/stats/taxon/",
    id,
    "?observationStatus=PRESENT",
    "&validationLevelNationale=", niveauNational,
    "&validationLevelRegionale=", niveauRegional,
    "&regionInseeId=", codeRegion,
    sep = ""
  )

  res_reg <- httr::GET(url = urlRegion)
  res_reg.content <- httr::content(res_reg, "text", encoding = "UTF-8")
  res_reg.json <- jsonlite::fromJSON(res_reg.content, flatten = TRUE)

  # url pour la requete pour le niveau NATIONAL faite via l'API REST de OpenObs
  urlNational = paste(
    "https://openobs.mnhn.fr/api/",
    "occurrences/stats/taxon/",
    id,
    "?observationStatus=PRESENT",
    "&validationLevelNationale=", niveauNational,
    "&validationLevelRegionale=", niveauRegional,
    sep = ""
  )

  res_nat = httr::GET(url = urlNational)
  res_nat.content = httr::content(res_nat,"text", encoding = "UTF-8")
  res_nat.json = jsonlite::fromJSON(res_nat.content, flatten = TRUE)

  # extraction du nombre d'observation
  dep = res_dep.json$occurrenceCount
  reg = res_reg.json$occurrenceCount
  nat = res_nat.json$occurrenceCount

  # calcul des ratio
  dep_reg = if(reg == 0 | is.null(dep) | is.null(reg)) NA else dep/reg
  dep_nat = if(nat == 0 | is.null(dep) | is.null(nat)) NA else dep/nat
  reg_nat = if(nat == 0 | is.null(reg) | is.null(nat)) NA else reg/nat


  return(
    list(
      "dep/reg" = dep_reg,
      "dep/nat" = dep_nat,
      "reg/nat" = reg_nat
    )
  )
}


#' Calculer la vulnerabilite d'une espece
#'
#' Cette fonction evalue le niveau de vulnerabilite d'une espece en fonction de son statut dans
#' les listes rouges nationales et regionales.
#'
#' @param lr_nat Statut dans la liste rouge nationale (\code{"LC"}, \code{"NT"}, \code{"VU"}, \code{"EN"}, \code{"CR"}, \code{"DD"}, \code{"NA"}, \code{"NE"}).
#'   Ce parametre est obligatoire.
#' @param lr_reg Statut dans la liste rouge regionale (\code{"LC"}, \code{"NT"}, \code{"VU"}, \code{"EN"}, \code{"CR"}, \code{"DD"}, \code{"NA"}, \code{"NE"}).
#'   Par defaut, il est defini a \code{NA}.
#'
#' @return Un entier representant le niveau de vulnerabilite, avec une echelle de 1 (faible) a 5 (tres elevee).
#'   Si \code{lr_reg} est \code{NA} ou egal a \code{"RE"} (\emph{region eteinte}), la vulnerabilite est calculee uniquement
#'   en fonction du statut national. Sinon, une matrice de correspondance est utilisee pour combiner les statuts nationaux
#'   et regionaux.
#'
#' @details
#' La fonction applique une logique en deux etapes :
#' \itemize{
#'   \item Si le statut regional (\code{lr_reg}) est absent (\code{NA}) ou "RE", seule la vulnerabilite nationale est utilisee.
#'   \item Sinon, une matrice predefinie est utilisee pour combiner les deux statuts et retourner un niveau de vulnerabilite.
#' }
#'
#' Les codes des listes rouges sont interpretes comme suit :
#' \itemize{
#'   \item \code{"LC"} : Preoccupation mineure.
#'   \item \code{"NT"} : Quasi menace.
#'   \item \code{"VU"} : Vulnerable.
#'   \item \code{"EN"} : En danger.
#'   \item \code{"CR"} : En danger critique.
#'   \item \code{"DD"} : Donnees insuffisantes.
#'   \item \code{"NA"} : Non applicable.
#'   \item \code{"NE"} : Non evalue.
#' }
#'
#' @examples
#' \dontrun{
#' # Calcul de la vulnerabilite avec seulement le statut national
#' vul1 <- vulnerabilite("VU")
#' print(vul1)  # Retourne 3
#'
#' # Calcul de la vulnerabilite avec les statuts national et regional
#' vul2 <- vulnerabilite("EN", "VU")
#' print(vul2)  # Retourne 4
#' }
#'
#' @export
vulnerabilite = function(lr_nat, lr_reg = NA) {
  if(is.na(lr_nat)){
    return(NA)
  }
  if(is.na(lr_reg) | lr_reg == "RE"){
    vul = switch(
      EXPR = lr_nat,
      "NA" = 3,
      "NE" = 3,
      "DD" = 3,
      "LC" = 1,
      "NT" = 2,
      "VU" = 3,
      "EN" = 4,
      "CR" = 5
    )
    return(vul)
  } else {
    table_vulnerabilite = matrix(
      c(
        2, 3, 4, 4, 4, 4, 5, 5,
        1, 2, 3, 3, 3, 3, 4, 5,
        1, 2, 3, 3, 3, 3, 4, 5,
        1, 2, 3, 3, 3, 3, 4, 5,
        1, 2, 3, 3, 3, 3, 4, 5,
        1, 2, 3, 3, 3, 3, 4, 5,
        1, 1, 2, 2, 2, 2, 3, 4,
        1, 1, 2, 2, 2, 2, 3, 4
      ),
      nrow = 8,
      ncol = 8,
      dimnames = list(
        c("CR", "EN", "VU", "DD", "NA", "NE", "NT", "LC"),
        c("LC", "NT", "VU", "DD", "NA", "NE", "EN", "CR")
      ),
      byrow = TRUE
    )
    return(table_vulnerabilite[lr_nat, lr_reg])
  }
}

#' Calculer l'enjeu de conservation d'une espece
#'
#' Cette fonction evalue l'enjeu de conservation d'une espece en combinant son niveau de vulnerabilite,
#' son importance regionale par rapport au niveau national, et un facteur multiplicatif fixe.
#'
#' @param lr_nat Statut dans la liste rouge nationale (\code{"LC"}, \code{"NT"}, \code{"VU"}, \code{"EN"}, \code{"CR"}, \code{"DD"}, \code{"NA"}, \code{"NE"}).
#'   Ce parametre est obligatoire.
#' @param lr_reg Statut dans la liste rouge regionale (\code{"LC"}, \code{"NT"}, \code{"VU"}, \code{"EN"}, \code{"CR"}, \code{"DD"}, \code{"NA"}, \code{"NE"}).
#'   Par defaut, il est defini a \code{NA}.
#' @param reg_nat ratio du nombre d'observations regionales avec le nombre d'observation nationales  (\code{numeric}).
#'   Ce parametre est obligatoire.
#'
#' @return Un score numerique representant l'enjeu de conservation.
#'   Le score est calcule comme suit :
#'   \deqn{\text{enjeu} = \text{vulnerabilite} \times \text{reg\_nat} \times 10 \times 2}
#'
#' @details
#' La fonction utilise le score de vulnerabilite obtenu via la fonction \code{\link{vulnerabilite}} et applique un
#' coefficient d'importance regionale (\code{reg_nat}) pour ponderer l'enjeu de conservation. Le score final est amplifie
#' par un facteur fixe de 20 (\code{10 * 2}).
#'
#' @examples
#' \dontrun{
#' # Exemple avec un statut national "VU" et un ratio regional-national de 0.8
#' enjeu_score <- enjeu("VU", "EN", reg_nat = 0.8)
#' print(enjeu_score)
#'
#' # Exemple sans statut regional
#' enjeu_score2 <- enjeu("VU", reg_nat = 0.8)
#' print(enjeu_score2)  # Retourne un score base uniquement sur le statut national
#' }
#'
#' @seealso \code{\link{vulnerabilite}}
#'
#' @export
enjeu = function(lr_nat, lr_reg = NA, reg_nat){
  if(is.na(lr_nat)){
    return(NA)
  }
  e = vulnerabilite(lr_nat, lr_reg) * reg_nat * 10 * 2
  return(e)
}
