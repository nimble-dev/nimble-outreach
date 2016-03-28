.PHONY: example_workshop_overview module_BUGSmodels module_MCMC_basics module_compile_R, module_MCMC_litters module_nimble_features module_user_dist module_user_sampler

# all: clean example_workshop_overview module_BUGSmodels
all: clean example bugs mcmc rc litters features userdist usersampler

clean:
	rm -rf *.md *.html *.pdf cache/ figure/


%.html: %.Rmd
	./make_slides $(basename $(@))
#	rm -f $(basename $(@)).md

example: example_workshop_overview.html 
bugs: module_BUGS_models.html
mcmc: module_MCMC_basics.html
rc: module_compile_R.html
litters: module_MCMC_litters.html
features: module_nimble_features.html
userdist: module_user_dist.html
usersampler: module_user_sampler.html
