
dir.create("docs", FALSE)
file.create("docs/.nojekyll")

bookdown::render_book(input = "webpage", output_dir = "../docs")