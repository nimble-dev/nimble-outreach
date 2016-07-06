.PHONY: example_workshop_overview alt_param bugs_language build_model compile_R constants customizing_mcmc data_nodes indexing litters_example mcmc_concepts model_structure operating_model run_mcmc user_sampler vectorizing user_dist user_function nimbleFunctions

# all: clean example_workshop_overview build_model
all: clean example alt_param bugs_language build_model compile_R constants customizing_mcmc data_nodes exercises indexing litters_example mcmc_concepts model_structure nimbleFunctions operating_model run_jags run_mcem run_mcmc user_dist user_function user_sampler vectorizing 

clean:
	rm -rf *.md *.html *.pdf cache/ figure/

%.html: %.Rmd
	./make_slides $(basename $(@))
#	rm -f $(basename $(@)).md

example: example_workshop_overview.html 
alt_param: alt_param.html
bugs_language: bugs_language.html
build_model: build_model.html
compile_R: compile_R.html
constants: constants.html
customizing_mcmc: customizing_mcmc.html
data_nodes: data_nodes.html
exercises: exercises.html
extends_bugs: how_nimble_extends_bugs.html
indexing: indexing.html
litters_example: litters_example.html
mcmc_concepts: mcmc_concepts.html
model_structure: model_structure.html
nimbleFunctions: nimbleFunctions.html
operating_model: operating_model.html
run_jags: run_jags.html
run_mcem: run_mcem.html
run_mcmc: run_mcmc.html
user_dist: user_dist.html
user_function: user_function.html
user_sampler: user_sampler.html
vectorizing: vectorizing.html



