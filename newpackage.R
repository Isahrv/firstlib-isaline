# Question 4

#' @description Une fonction pour compter le nombre d'élus par ville/département
#' @param df le dataframe doit contenir les colonnes correspondantes au schéma décrit
#' @return le nombre d'élus uniques
#' @importFrom dplyr
#' @export
#' @examples
#' compter_nb_elus(df_Nantes)
compter_nb_elus <- function(df){
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier", "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune", "Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu", "Code.sexe", "Date.de.naissance", "Code.de.la.catégorie.socio.professionnelle", "Libellé.de.la.catégorie.socio.professionnelle", "Date.de.début.du.mandat", "Libellé.de.la.fonction", "Date.de.début.de.la.fonction", "Code.nationalité")
  stopifnot(identical(colnames(df), schema))

  df |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance) |>
    distinct() |>
    nrow()
}

#' @description Une fonction pour compter le nombre d'adjoints par ville/département
#' @param df le dataframe doit contenir les colonnes correspondantes au schéma décrit
#' @return le nombre d'adjoints uniques
#' @importFrom stringr
#' @export
#' @examples
#' compter_nombre_d_adjoints(df_Nantes)
compter_nombre_d_adjoints <- function(df){
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier", "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune", "Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu", "Code.sexe", "Date.de.naissance", "Code.de.la.catégorie.socio.professionnelle", "Libellé.de.la.catégorie.socio.professionnelle", "Date.de.début.du.mandat", "Libellé.de.la.fonction", "Date.de.début.de.la.fonction", "Code.nationalité")
  stopifnot(identical(colnames(df), schema))

  sum(str_detect(df$Libellé.de.la.fonction, "adjoint"))
}

#' @description Une fonction pour valider le schéma du dataframe
#' @param df le dataframe doit contenir les colonnes correspondantes au schéma décrit
#' @return rien mais stop la fonction si le df ne correspond pas
#' @export
validate_schema <- function(df){
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier", "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune", "Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu", "Code.sexe", "Date.de.naissance", "Code.de.la.catégorie.socio.professionnelle", "Libellé.de.la.catégorie.socio.professionnelle", "Date.de.début.du.mandat", "Libellé.de.la.fonction", "Date.de.début.de.la.fonction", "Code.nationalité")
  stopifnot(identical(colnames(df), schema))
}

#' @description Une fonction pour trouver l'élu le plus agé par ville/département
#' @param df le dataframe doit contenir les colonnes correspondantes au schéma
#' @return le nom, le prénom, et la date de naissance de l'élu le plus agé de la commune/département
#' @importFrom lubridate
#' @export
#' @examples
#' trouver_l_elu_le_plus_age(df_Nantes)
trouver_l_elu_le_plus_age <- function(df){
  validate_schema(df)

  df |>
    mutate(Date.de.naissance = dmy(Date.de.naissance))|>
    slice(which.min(Date.de.naissance))|>
    select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance)
}

# Question 5

library(readr)

chemin <- "C:/Users/isali/OneDrive/Documents/cours/M1 ECAP/S2/R avancé & Git/semaine_5/elus_sample.csv"
donnees_elus <- read_delim(chemin, delim = ";")

library(usethis)

use_data(donnees_elus)

#' Echantillon d'élus
#'
#' Tableau contenant un échantillon de 9753 élus des communes françaises
#'
#' @format
#' Un data frame avec 9753 lignes et 16 colonnes:
#' \describe{
#'   \item{Code du département}{code du département}
#'   \item{Libellé du département}{nom du département}
#'   \item{Code de la collectivité à statut particulier}{code de la collectivité à statut particulier}
#'   \item{Libellé de la collectivité à statut particulier}{nom de la collectivité à statut particulier}
#'   \item{Code de la commune}{code de la commune}
#'   \item{Libellé de la commune}{nom de la commune}
#'   \item{Nom de l'élu}{nom de l'élu}
#'   \item{Prénom de l'élu}{prénom de l'élu}
#'   \item{Code sexe}{"M" si l'élu est un homme, "F" si l'élu est une femme}
#'   \item{Date de naissance}{date de naissance de l'élu au format "jj/mm/aaaa"}
#'   \item{Code de la catégorie socio-professionnelle}{code correspondant à la catégorie socio-professionnelle de l'élu}
#'   \item{Libellé de la catégorie socio-professionnelle}{nom de la catégorie socio-professionnelle de l'élu}
#'   \item{Date de début du mandat}{date de début du mandat au format "jj/mm/aaaa"}
#'   \item{Libellé de la fonction}{nom de la fonction de l'élu}
#'   \item{Date de début de la fonction}{date de début de la fonction au format "jj/mm/aaaa"}
#'   \item{Code nationalité}{code indiquant la nationalité de l'élu ("FR" s'il est français)}
#' }
#' @source https://rnedellec-r-advanced-git.netlify.app/schedule

# Question 6

# Tests pour "compter_nb_elus"

test_that("compter_nb_elus ignore les doublons correctement", {
  df <- data.frame(
    Code.du.département = c("44", "44"),
    Libellé.du.département = c("Loire-Atlantique", "Loire-Atlantique"),
    Code.de.la.collectivité.à.statut.particulier = c(NA, NA),
    Libellé.de.la.collectivité.à.statut.particulier = c(NA, NA),
    Code.de.la.commune = c("44109", "44109"),
    Libellé.de.la.commune = c("Nantes", "Nantes"),
    Nom.de.l.élu = c("Dupont", "Dupont"),
    Prénom.de.l.élu = c("Jean", "Jean"),
    Code.sexe = c("M", "M"),
    Date.de.naissance = c("1970-01-01", "1970-01-01"),
    Code.de.la.catégorie.socio.professionnelle = c(1, 1),
    Libellé.de.la.catégorie.socio.professionnelle = c("Cadres", "Cadres"),
    Date.de.début.du.mandat = c("2020-01-01", "2020-01-01"),
    Libellé.de.la.fonction = c("Maire", "Maire"),
    Date.de.début.de.la.fonction = c("2020-01-01", "2020-01-01"),
    Code.nationalité = c("FR", "FR")
  )

  expect_equal(compter_nb_elus(df), 1)
})

test_that("compter_nb_elus retourne une erreur avec un schéma incorrect", {
  df_incorrect <- data.frame(
    Nom.de.l.élu = c("Haouache", "Loas"),
    Prénom.de.l.élu = c("Jean", "Victoria")
  )

  expect_error(compter_nb_elus(df_incorrect), "identical")
})

# Tests pour "compter_nombre_d_adjoints"

library(stringr)

test_that("compter_nombre_d_adjoints fonctionne sur un dataframe valide", {
  df <- data.frame(
    Code.du.département = c("44", "44", "44"),
    Libellé.du.département = c("Loire-Atlantique", "Loire-Atlantique", "Loire-Atlantique"),
    Code.de.la.collectivité.à.statut.particulier = c(NA, NA, NA),
    Libellé.de.la.collectivité.à.statut.particulier = c(NA, NA, NA),
    Code.de.la.commune = c("44109", "44109", "44109"),
    Libellé.de.la.commune = c("Nantes", "Nantes", "Nantes"),
    Nom.de.l.élu = c("Dupont", "Martin", "Durand"),
    Prénom.de.l.élu = c("Jean", "Paul", "Sophie"),
    Code.sexe = c("M", "M", "F"),
    Date.de.naissance = c("1970-01-01", "1980-05-12", "1990-11-20"),
    Code.de.la.catégorie.socio.professionnelle = c(1, 2, 3),
    Libellé.de.la.catégorie.socio.professionnelle = c("Cadres", "Employés", "Professions libérales"),
    Date.de.début.du.mandat = c("2020-01-01", "2021-03-15", "2022-06-10"),
    Libellé.de.la.fonction = c("Adjoint", "Maire", "Adjoint à la culture"),
    Date.de.début.de.la.fonction = c("2020-01-01", "2021-03-15", "2022-06-10"),
    Code.nationalité = c("FR", "FR", "FR")
  )

  expect_equal(compter_nombre_d_adjoints(df), 2)
})

test_that("compter_nombre_d_adjoints gère les NA dans Libellé.de.la.fonction", {
  df <- data.frame(
    Code.du.département = c("44", "44"),
    Libellé.du.département = c("Loire-Atlantique", "Loire-Atlantique"),
    Code.de.la.collectivité.à.statut.particulier = c(NA, NA),
    Libellé.de.la.collectivité.à.statut.particulier = c(NA, NA),
    Code.de.la.commune = c("44109", "44109"),
    Libellé.de.la.commune = c("Nantes", "Nantes"),
    Nom.de.l.élu = c("Dupont", "Martin"),
    Prénom.de.l.élu = c("Jean", "Paul"),
    Code.sexe = c("M", "M"),
    Date.de.naissance = c("1970-01-01", "1980-05-12"),
    Code.de.la.catégorie.socio.professionnelle = c(1, 2),
    Libellé.de.la.catégorie.socio.professionnelle = c("Cadres", "Employés"),
    Date.de.début.du.mandat = c("2020-01-01", "2021-03-15"),
    Libellé.de.la.fonction = c(NA, "Adjoint"),
    Date.de.début.de.la.fonction = c("2020-01-01", "2021-03-15"),
    Code.nationalité = c("FR", "FR")
  )

  expect_equal(compter_nombre_d_adjoints(df), 1)
})

# Tests pour "validate_schema"

test_that("validate_schema fonctionne avec un schéma valide", {
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier",
              "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune", "Libellé.de.la.commune",
              "Nom.de.l.élu", "Prénom.de.l.élu", "Code.sexe", "Date.de.naissance",
              "Code.de.la.catégorie.socio.professionnelle", "Libellé.de.la.catégorie.socio.professionnelle",
              "Date.de.début.du.mandat", "Libellé.de.la.fonction", "Date.de.début.de.la.fonction",
              "Code.nationalité")
  df <- data.frame(matrix(ncol = length(schema), nrow = 0))
  colnames(df) <- schema

  expect_silent(validate_schema(df))
})

test_that("validate_schema détecte un schéma incomplet", {
  df <- data.frame(
    Code.du.département = c("44"),
    Libellé.du.département = c("Loire-Atlantique")
  )

  expect_error(validate_schema(df), "identical")
})

# Tests pour "trouver_l_elu_le_plus_age"

test_that("trouver_l_elu_le_plus_age gère plusieurs élus avec la même date de naissance", {
  df <- data.frame(
    Nom.de.l.élu = c("Dupont", "Martin", "Durand"),
    Prénom.de.l.élu = c("Jean", "Marie", "Paul"),
    Date.de.naissance = c("01/01/1930", "01/01/1930", "01/01/1940"),
    stringsAsFactors = FALSE
  )

  result <- trouver_l_elu_le_plus_age(df)

  expect_true(result$Nom.de.l.élu %in% c("Dupont", "Martin"))
  expect_equal(as.character(result$Date.de.naissance), "1930-01-01")
})

test_that("trouver_l_elu_le_plus_age gère les dates dans un format inattendu", {
  df <- data.frame(
    Nom.de.l.élu = c("Dupont", "Martin"),
    Prénom.de.l.élu = c("Jean", "Marie"),
    Date.de.naissance = c("1940-01-01", "1950-01-01"),
    stringsAsFactors = FALSE
  )

  expect_error(trouver_l_elu_le_plus_age(df), "dmy")
})

