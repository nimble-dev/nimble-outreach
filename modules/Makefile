.PHONY: example_workshop_overview module_BUGSmodels module_MCMC_basics

# all: clean example_workshop_overview module_BUGSmodels
all: clean example bugs mcmc

clean:
	rm -rf *.md *.html *.pdf cache/ figure/


%.html: %.Rmd
	./make_slides $(basename $(@))
	rm -f $(basename $(@)).md

example: example_workshop_overview.html 
bugs: module_BUGSmodels.html
mcmc: module_MCMC_basics
