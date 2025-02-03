# Test 1

test_that("trouver_l_elu_le_plus_age gère plusieurs élus avec la même date de naissance", {
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
    Date.de.naissance = c("01/01/1930", "01/01/1930"),
    Code.de.la.catégorie.socio.professionnelle = c(1, 2),
    Libellé.de.la.catégorie.socio.professionnelle = c("Cadres", "Employés"),
    Date.de.début.du.mandat = c("2020/01/01", "2021/03/15"),
    Libellé.de.la.fonction = c(NA, "adjoint"),
    Date.de.début.de.la.fonction = c("2020/01/01", "2021/03/15"),
    Code.nationalité = c("FR", "FR"),
    stringsAsFactors = FALSE
  )


  result <- trouver_l_elu_le_plus_age(df)

  expect_true(result$Nom.de.l.élu %in% c("Dupont", "Martin"))
  expect_equal(format(result$Date.de.naissance, "%d/%m/%Y"), "01/01/1930")
})

# Test 2

test_that("trouver_l_elu_le_plus_age gère les dates dans un format inattendu", {
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
    Date.de.naissance = c("01-01-1940", "01-01-1950"),
    Code.de.la.catégorie.socio.professionnelle = c(1, 2),
    Libellé.de.la.catégorie.socio.professionnelle = c("Cadres", "Employés"),
    Date.de.début.du.mandat = c("2020/01/01", "2021/03/15"),
    Libellé.de.la.fonction = c(NA, "adjoint"),
    Date.de.début.de.la.fonction = c("2020/01/01", "2021/03/15"),
    Code.nationalité = c("FR", "FR"),
    stringsAsFactors = FALSE
  )

  df$Date.de.naissance <- dmy(df$Date.de.naissance)
  expect_equal(format(df$Date.de.naissance, "%Y-%m-%d"), c("1940-01-01", "1950-01-01"))
})
