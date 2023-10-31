This repository contains the data and the code for the analysis of the second
eye-tracking experiment (referred to as "experiment 4" in the paper) described
in the paper "Semantic accessibility and interference in pronoun resolution".
See this osf repository for details: https://osf.io/xznw7/
If you just want to examine the results feel free to download doc/models_summary.html.
This file contains all the results. If you want to reproduce the analysis, read on.

There is a default.nix file in the src dir which will make all the relevant
packages available in your shell. You have to have nix installed
(https://nixos.org/download).

The file src/default.nix will also temporarily move your ~/.R/Makevars file and
restore it once you exit the shell. This is because, there were some issues with
the recommended Makevars settings and the brms version used for this project. To
make sure it works as intended, check if the file src/shellExitHook.sh is
executable. If you never set Makevars you can ignore this part.
After you did all that, in the project root run:

nix-shell src/default.nix --pure

(this can take a while on its first run)

To recreate the project run in the nix-shell, in the project root:

Rscript src/01-prepare.R
Rscript src/02-regions.R
Rscript src/03-models.R
Rscript src/04-models_summary.R
Rscript -e "rmarkdown::render(here::here('doc/models_summary.Rmd'), output_format = 'html_document')"

Note:

- combined data assumes that subj_cond can be "MIS" and "MATCH", data from the second experiment assumes that it can be "M" (match) and "MM" (mismatch)
