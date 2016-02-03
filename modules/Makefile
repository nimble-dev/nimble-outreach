.PHONY: example_workshop_overview module_BUGSmodels

# all: clean example_workshop_overview module_BUGSmodels
all: clean example bugs 

clean:
	rm -rf *.md *.html *.pdf cache/ figure/


%.md: %.Rmd
	./make_slides $(basename $(@))

example: example_workshop_overview.md 
bugs: module_BUGSmodels.md
