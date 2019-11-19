# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
devtools::use_data(x, mtcars)
save(ion_library, file="data/ion_library.RData")

make_media_recipe <- function(data, TE_conc = 1000, base_conc = 10){
  `%>%` <- magrittr::`%>%`
  data("compound_library")
  data("ion_library")
  data("env_data")
  TE = env_data %>%
    dplyr::left_join(ion_library) %>%
    dplyr::filter(type == "trace") %>%
    dplyr::left_join(compound_library, by=c("ion" = "ion_a")) %>%
    dplyr::mutate(mass_ratio = (atomic_mass_grams*coeff_a)/molecular_mass_grams,
                  compound_recipe_mass = (env_grams_per_L/mass_ratio)*TE_conc,
                  recipe_mass_a = env_grams_per_L*TE_conc,
                  recipe_mass_b = compound_recipe_mass - recipe_mass_a,
                  unit = "mg",
                  concentration = paste(TE_conc, "X"))
  base = env_data %>%
    dplyr::left_join(ion_library) %>%
    dplyr::filter(type == "salt") %>%
    dplyr::left_join(compound_library, by=c("ion" = "ion_a")) %>%
    dplyr::mutate(mass_ratio = (atomic_mass_grams*coeff_a)/molecular_mass_grams,
                  compound_recipe_mass = ((env_grams_per_L/mass_ratio)*base_conc)/1000,
                  recipe_mass_a = (env_grams_per_L*base_conc)/1000,
                  recipe_mass_b = compound_recipe_mass - recipe_mass_a,
                  unit = "g",
                  concentration = paste(base_conc, "X"))
  recipe = TE %>%
    dplyr::bind_rows(base) %>%
    dplyr::select(compound, compound_recipe_mass, unit, type, concentration)
  DT::datatable(recipe)
}

