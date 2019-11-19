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
#devtools::use_data(x, mtcars)
#save(ion_library, file="data/ion_library.RData")

make_media_recipe <- function(data, TE_conc = 1000, base_conc = 10){
  `%>%` <- magrittr::`%>%`
  data("compound_library")
  data("ion_library")
  TE = data %>%
    dplyr::left_join(ion_library) %>%
    dplyr::filter(type == "trace") %>%
    dplyr::left_join(compound_library, by=c("ion" = "ion_a")) %>%
    dplyr::mutate(mass_ratio = (atomic_mass_grams*coeff_a)/molecular_mass_grams,
                  compound_recipe_mass = (env_grams_per_L/mass_ratio)*1000*TE_conc,
                  recipe_mass_a = env_grams_per_L*1000*TE_conc,
                  recipe_mass_b = compound_recipe_mass - recipe_mass_a,
                  unit = "mg",
                  concentration = paste(TE_conc, "X"))
  base = data %>%
    dplyr::left_join(ion_library) %>%
    dplyr::filter(type == "salt") %>%
    dplyr::left_join(compound_library, by=c("ion" = "ion_a")) %>%
    dplyr::mutate(mass_ratio = (atomic_mass_grams*coeff_a)/molecular_mass_grams,
                  compound_recipe_mass = (env_grams_per_L/mass_ratio)*base_conc,
                  recipe_mass_a = env_grams_per_L*base_conc,
                  recipe_mass_b = compound_recipe_mass - recipe_mass_a,
                  unit = "g",
                  concentration = paste(base_conc, "X"))
  TErecipe = TE %>%
    dplyr::select(compound, compound_recipe_mass, unit, type, concentration)%>%
    dplyr::filter(!is.na(compound))
  baseRecipe = base %>%
    dplyr::select(compound, compound_recipe_mass, unit, type, concentration) %>%
    dplyr::filter(!is.na(compound))

 writeLines(
   c(
     "# Media Recipe",
     "<h3 style='color:red'>Trace Element Solution</h3>",
     "```{r, echo=F}",
     'knitr::kable(TErecipe) %>%
                kableExtra::kable_styling(c("hover","stripe"))',
     "```",
   "### Basal Media",
   "```{r, echo=F}",
   'knitr::kable(baseRecipe) %>%
                kableExtra::kable_styling(c("hover","stripe"))',
   "```"),
   "recipe.Rmd"
   )
 rmarkdown::render("recipe.Rmd")
 if (interactive()) browseURL("recipe.html")
}
