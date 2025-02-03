# Test 1

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

# Test 2

test_that("compter_nb_elus retourne une erreur avec un schéma incorrect", {
  df_incorrect <- data.frame(
    Nom.de.l.élu = c("Haouache", "Loas"),
    Prénom.de.l.élu = c("Jean", "Victoria")
  )

  expect_error(compter_nb_elus(df_incorrect), "identical")
})
