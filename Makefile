
all: docs/news/index.html docs/news/new-packages.html docs/news/analysis.html

docs/news/index.html: news/index.Rmd
	Rscript -e 'rmarkdown::render("news/index.Rmd")'
	mv news/index.html docs/news

docs/news/new-packages.html: news/new-packages.Rmd news/data/new-packages.csv
	Rscript -e 'rmarkdown::render("news/new-packages.Rmd")'
	mv news/new-packages.html docs/news

docs/news/analysis.html: news/analysis.R
	Rscript -e 'rmarkdown::render("news/analysis.R")'
	mv news/analysis.html docs/news

