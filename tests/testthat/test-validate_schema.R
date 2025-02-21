# Test 1

test_that("validate_schema fonctionne avec un schéma valide", {
  schema <- c("Code_du_département", "Libellé_du_département", "Code_de_la_collectivité_à_statut_particulier",
              "Libellé_de_la_collectivité_à_statut_particulier", "Code_de_la_commune", "Libellé_de_la_commune",
              "Nom_de_l_élu", "Prénom_de_l_élu", "Code_sexe", "Date_de_naissance",
              "Code_de_la_catégorie_socio_professionnelle", "Libellé_de_la_catégorie_socio_professionnelle",
              "Date_de_début_du_mandat", "Libellé_de_la_fonction", "Date_de_début_de_la_fonction",
              "Code_nationalité")
  df <- data.frame(matrix(ncol = length(schema), nrow = 0))
  colnames(df) <- schema

  expect_silent(validate_schema(df))
})

# Test 2

test_that("validate_schema détecte un schéma incomplet", {
  df <- data.frame(
    Code_du_département = c("44"),
    Libellé_du_département = c("Loire-Atlantique")
  )

  expect_error(validate_schema(df), "identical")
})
