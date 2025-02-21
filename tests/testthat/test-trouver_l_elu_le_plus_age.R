# Test 1

test_that("trouver_l_elu_le_plus_age gère plusieurs élus avec la même date de naissance", {
  df <- data.frame(
    Code_du_département = c("44", "44"),
    Libellé_du_département = c("Loire-Atlantique", "Loire-Atlantique"),
    Code_de_la_collectivité_à_statut_particulier = c(NA, NA),
    Libellé_de_la_collectivité_à_statut_particulier = c(NA, NA),
    Code_de_la_commune = c("44109", "44109"),
    Libellé_de_la_commune = c("Nantes", "Nantes"),
    Nom_de_l_élu = c("Dupont", "Martin"),
    Prénom_de_l_élu = c("Jean", "Paul"),
    Code_sexe = c("M", "M"),
    Date_de_naissance = c("01/01/1930", "01/01/1930"),
    Code_de_la_catégorie_socio_professionnelle = c(1, 2),
    Libellé_de_la_catégorie_socio_professionnelle = c("Cadres", "Employés"),
    Date_de_début_du_mandat = c("2020/01/01", "2021/03/15"),
    Libellé_de_la_fonction = c(NA, "adjoint"),
    Date_de_début_de_la_fonction = c("2020/01/01", "2021/03/15"),
    Code_nationalité = c("FR", "FR"),
    stringsAsFactors = FALSE
  )


  result <- trouver_l_elu_le_plus_age(df)

  expect_true(result$Nom_de_l_élu %in% c("Dupont", "Martin"))
})

# Test 2

test_that("trouver_l_elu_le_plus_age gère les dates dans un format inattendu", {
  df <- data.frame(
    Code_du_département = c("44", "44"),
    Libellé_du_département = c("Loire-Atlantique", "Loire-Atlantique"),
    Code_de_la_collectivité_à_statut_particulier = c(NA, NA),
    Libellé_de_la_collectivité_à_statut_particulier = c(NA, NA),
    Code_de_la_commune = c("44109", "44109"),
    Libellé_de_la_commune = c("Nantes", "Nantes"),
    Nom_de_l_élu = c("Dupont", "Martin"),
    Prénom_de_l_élu = c("Jean", "Paul"),
    Code_sexe = c("M", "M"),
    Date_de_naissance = c("01-01-1940", "01-01-1950"),
    Code_de_la_catégorie_socio_professionnelle = c(1, 2),
    Libellé_de_la_catégorie_socio_professionnelle = c("Cadres", "Employés"),
    Date_de_début_du_mandat = c("2020/01/01", "2021/03/15"),
    Libellé_de_la_fonction = c(NA, "adjoint"),
    Date_de_début_de_la_fonction = c("2020/01/01", "2021/03/15"),
    Code_nationalité = c("FR", "FR"),
    stringsAsFactors = FALSE
  )

  df$Date_de_naissance <- dmy(df$Date_de_naissance)
  expect_equal(format(df$Date_de_naissance, "%Y-%m-%d"), c("1940-01-01", "1950-01-01"))
})
