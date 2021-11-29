
rmarkdown::render(input = "output/presentation/index.Rmd")
xaringanBuilder::build_pdf(input = "output/presentation/index.Rmd")

files = list.files("output/presentation/",
                   recursive = T)

file.copy(file.path("output/presentation/", files),
          file.path("docs/", files),
          overwrite = T)



