# Test 1

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
    Date.de.naissance = c("1970/01/01", "1980/05/12", "1990/11/20"),
    Code.de.la.catégorie.socio.professionnelle = c(1, 2, 3),
    Libellé.de.la.catégorie.socio.professionnelle = c("Cadres", "Employés", "Professions libérales"),
    Date.de.début.du.mandat = c("2020/01/01", "2021/03/15", "2022/06/10"),
    Libellé.de.la.fonction = c("Adjoint", "Maire", "Adjoint à la culture"),
    Date.de.début.de.la.fonction = c("2020/01/01", "2021/03/15", "2022/06/10"),
    Code.nationalité = c("FR", "FR", "FR")
  )

  expect_equal(compter_nombre_d_adjoints(df), 2)
})

# Test 2

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
    Date.de.naissance = c("1970/01/01", "1980/05/12"),
    Code.de.la.catégorie.socio.professionnelle = c(1, 2),
    Libellé.de.la.catégorie.socio.professionnelle = c("Cadres", "Employés"),
    Date.de.début.du.mandat = c("2020/01/01", "2021/03/15"),
    Libellé.de.la.fonction = c(NA, "adjoint"),
    Date.de.début.de.la.fonction = c("2020/01/01", "2021/03/15"),
    Code.nationalité = c("FR", "FR")
  )

  expect_equal(compter_nombre_d_adjoints(df), 1)
})
