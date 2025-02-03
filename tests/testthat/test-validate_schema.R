# Test 1

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

# Test 2

test_that("validate_schema détecte un schéma incomplet", {
  df <- data.frame(
    Code.du.département = c("44"),
    Libellé.du.département = c("Loire-Atlantique")
  )

  expect_error(validate_schema(df), "identical")
})
