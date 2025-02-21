# Test 1

test_that("compter_nombre_d_adjoints est insensible à la casse", {
  df <- data.frame(
    Code_du_département = c("44", "44", "44"),
    Libellé_du_département = c("Loire-Atlantique", "Loire-Atlantique", "Loire-Atlantique"),
    Code_de_la_collectivité_à_statut_particulier = c(NA, NA, NA),
    Libellé_de_la_collectivité_à_statut_particulier = c(NA, NA, NA),
    Code_de_la_commune = c("44109", "44109", "44109"),
    Libellé_de_la_commune = c("Nantes", "Nantes", "Nantes"),
    Nom_de_l_élu = c("Dupont", "Martin", "Durand"),
    Prénom_de_l_élu = c("Jean", "Paul", "Sophie"),
    Code_sexe = c("M", "M", "F"),
    Date_de_naissance = c("1970/01/01", "1980/05/12", "1990/11/20"),
    Code_de_la_catégorie_socio_professionnelle = c(1, 2, 3),
    Libellé_de_la_catégorie_socio_professionnelle = c("Cadres", "Employés", "Professions libérales"),
    Date_de_début_du_mandat = c("2020/01/01", "2021/03/15", "2022/06/10"),
    Libellé_de_la_fonction = c("Adjoint", "Maire", "adjoint à la culture"),
    Date_de_début_de_la_fonction = c("2020/01/01", "2021/03/15", "2022/06/10"),
    Code_nationalité = c("FR", "FR", "FR")
  )

  expect_equal(compter_nombre_d_adjoints(df), 2)
})

# Test 2

test_that("compter_nombre_d_adjoints gère les NA dans Libellé_de_la_fonction", {
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
    Date_de_naissance = c("1970/01/01", "1980/05/12"),
    Code_de_la_catégorie_socio_professionnelle = c(1, 2),
    Libellé_de_la_catégorie_socio_professionnelle = c("Cadres", "Employés"),
    Date_de_début_du_mandat = c("2020/01/01", "2021/03/15"),
    Libellé_de_la_fonction = c(NA, "Adjoint"),
    Date_de_début_de_la_fonction = c("2020/01/01", "2021/03/15"),
    Code_nationalité = c("FR", "FR")
  )

  expect_equal(compter_nombre_d_adjoints(df), 1)
})
