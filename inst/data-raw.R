compound_library<- readr::read_csv("inst/compound_library.csv")
ion_library<- readr::read_csv("inst/ion_library.csv")
env_data<- readr::read_csv("inst/env_data.csv")

usethis::use_data(compound_library, ion_library, env_data)

system.file("extdata", "inst/compound_library.csv", package = "testdat", mustWork = TRUE)
