  mer <- targets::tar_mermaid(targets_only = TRUE,
                              outdated = FALSE,
                              legend = FALSE,
                              color = FALSE,
                              script = "model_framework_targets.R",
                              exclude = c("readme", contains("AWS")))
  cat(
    "```mermaid",
    mer[1],
    #'Objects([""Objects""]) --- Functions>""Functions""]',
    'subgraph Project Workflow',
    mer[3:length(mer)],
    'linkStyle 0 stroke-width:0px;',
    "```",
    sep = "\n"
  )
  
  mer <- targets::tar_mermaid(targets_only = TRUE,
                              outdated = FALSE,
                              legend = FALSE,
                              color = FALSE,
                              script = "model_framework_targets.R")