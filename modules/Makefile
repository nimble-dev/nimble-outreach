.PHONY: example_workshop_overview alt_param bugs_language build_model compile_R constants customizing_mcmc data_nodes indexing litters_example mcmc_concepts model_structure operating_model run_mcmc user_sampler vectorizing user_dist user_function nimbleFunctions

# all: clean example_workshop_overview build_model
all: clean example alt_param bugs_language build_model compile_R constants customizing_mcmc data_nodes indexing litters_example mcmc_concepts model_structure operating_model run_mcmc user_sampler vectorizing user_dist user_function nimbleFunctions

clean:
	rm -rf *.md *.html *.pdf cache/ figure/

%.html: %.Rmd
	./make_slides $(basename $(@))
#	rm -f $(basename $(@)).md

example: example_workshop_overview.html 
bugs_language: bugs_language.html
build_model: build_model.html
alt_param: alt_param.html
compile_R: compile_R.html
constants: constants.html
customizing_mcmc: customizing_mcmc.html
data_nodes: data_nodes.html
indexing: indexing.html
litters_example: litters_example.html
mcmc_concepts: mcmc_concepts.html
model_structure: model_structure.html
operating_model: operating_model.html
run_mcmc: run_mcmc.html
run_mcem: run_mcem.html
user_sampler: user_sampler.html
user_dist: user_dist.html
user_function: user_function.html
vectorizing: vectorizing.html
nimbleFunctions: nimbleFunctions.html
exercises: exercises.html
