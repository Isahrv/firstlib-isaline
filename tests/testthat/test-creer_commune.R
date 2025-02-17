# Test 1

test_that("creer_commune crée un objet de classe 'commune'", {
  df_commune <- data.frame(
    Code.du.département = c("44", "44"),
    Libellé.du.département = c("Loire-Atlantique", "Loire-Atlantique"),
    Code.de.la.collectivité.à.statut.particulier = c(NA, NA),
    Libellé.de.la.collectivité.à.statut.particulier = c(NA, NA),
    Code.de.la.commune = c("44109", "44109"),
    Libellé.de.la.commune = c("Nantes", "Nantes"),
    Nom.de.l.élu = c("Haouache", "Loas"),
    Prénom.de.l.élu = c("Zahia", "Lucie"),
    Code.sexe = c("F", "F"),
    Date.de.naissance = c("10/04/1980", "25/12/1975"),
    Code.de.la.catégorie.socio.professionnelle = c(1, 1),
    Libellé.de.la.catégorie.socio.professionnelle = c("Cadres", "Cadres"),
    Date.de.début.du.mandat = c("03/01/2020", "01/06/2022"),
    Libellé.de.la.fonction = c("Maire", "Maire"),
    Date.de.début.de.la.fonction = c("03/01/2020", "01/06/2022"),
    Code.nationalité = c("FR", "FR")
  )
  commune <- creer_commune(df_commune)
  expect_s3_class(commune, "commune")
})

# Test 2

test_that("la classe d'origine est préservée", {
  df_commune <- data.frame(
    Code.du.département = c("44", "44"),
    Libellé.du.département = c("Loire-Atlantique", "Loire-Atlantique"),
    Code.de.la.collectivité.à.statut.particulier = c(NA, NA),
    Libellé.de.la.collectivité.à.statut.particulier = c(NA, NA),
    Code.de.la.commune = c("44109", "44109"),
    Libellé.de.la.commune = c("Nantes", "Nantes"),
    Nom.de.l.élu = c("Haouache", "Loas"),
    Prénom.de.l.élu = c("Zahia", "Lucie"),
    Code.sexe = c("F", "F"),
    Date.de.naissance = c("10/04/1980", "25/12/1975"),
    Code.de.la.catégorie.socio.professionnelle = c(1, 1),
    Libellé.de.la.catégorie.socio.professionnelle = c("Cadres", "Cadres"),
    Date.de.début.du.mandat = c("03/01/2020", "01/06/2022"),
    Libellé.de.la.fonction = c("Maire", "Maire"),
    Date.de.début.de.la.fonction = c("03/01/2020", "01/06/2022"),
    Code.nationalité = c("FR", "FR")
  )
  class(df_commune) <- "data.frame"
  commune <- creer_commune(df_commune)
  expect_equal(class(commune), c("commune", "data.frame"))
})
