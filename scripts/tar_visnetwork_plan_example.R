source(h("_targets.R"))
targets::tar_visnetwork(names = static_targets,
                        targets_only = T)

targets::tar_mermaid(names = static_targets,
                     targets_only = T)
