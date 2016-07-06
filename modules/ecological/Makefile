.PHONY: bugs_language build_model customizing_mcmc  exercises how_use model_structure operating_model run_mcem run_mcmc thinking_graph user_dist user_sampler

# all: clean example_workshop_overview build_model
all: clean bugs_language build_model customizing_mcmc exercises how_use model_structure operating_model run_mcem run_mcmc thinking_graph user_dist user_sampler

clean:
	rm -rf *.md *.html *.pdf cache/ figure/

%.html: %.Rmd
	./make_slides $(basename $(@))
#	rm -f $(basename $(@)).md

bugs_language: bugs_language.html
build_model: build_model.html
customizing_mcmc: customizing_mcmc.html
exercises: exercises.html
how_use: how_you_could_use_nimble.html
model_structure: model_structure.html
operating_model: operating_model.html
run_mcem: run_mcem.html
run_mcmc: run_mcmc.html
thinking_graph: thinking_like_a_graph.html
user_dist: user_dist.html
user_sampler: user_sampler.html



