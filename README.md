# ESA_Shiny

This is a project that arose as a result of some ideas from [Goring et al., 2014](http://onlinelibrary.wiley.com/doi/10.1890/120370/pdf) on interdisciplinarity in Ecology (in particular) and, more generally, hiring pracitices in academia.

The project consists of three seperate, but related components.  The first uses `R/Scrape_ECOLOG.R` to access and download messages from the [ECOLOG listserv](https://www.mail-archive.com/ecolog-l@listserv.umd.edu/).  These messages are then put into a large R object and saved.

The second seeks to classify the messages into job ads, and then further into sub-categories.  This was initially developed to look specifically at interdisciplinary jobs in particular, and the rate of change in postings for interdisciplinary jobs over the years 2000- 2015.  Because of the large number of messages, we hand classify a subset of messages using a Shiny app (`server.R` and `ui.R`).  We have progressively updated the Shiny app to facilitate better classification, and to extend the research potential of this project.

Once a subset of messages have been classified we then use a `randomForest` model in R to classify, using a balanced approach (most job messages, and interdisciplinary messages in particular, are "rare events").  We build subject-specific models (*i.e.*, "Tenure Track" vs "not Tenure Track") to track importance factors for the individual classes, and then a composite model to actually classify all messages.  This is all done as part of the `Rmd` workflow for `ecolog_paper.Rmd`, and the model is updated whenever there are new `RDS` files in the `data` folder, indicating someone has taken the time to do some hand classification.

The results of this `randomForest` model and then analysed using a STAN model that is currently under development.

## Contributions
The core of this work was realized, coded & developed by Simon Goring
Significant contributions by Sarah Supp and Andria Dawson were made to improve the paper, some of the workflow, and, in particular the STAN models were coded by Andria Dawson.  David Inoyue contributed to the writing in `ecolog_paper.Rmd`.

Contributors are still welcome.  This is a paper in progress.
