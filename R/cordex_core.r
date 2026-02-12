## ///////////////////////////////////////////////////////////////////////////////
# IMPORTANT NOTE: 
# This script requires an up-to-date version of the ontologies:
# --- datasource (v>=1.0.0)
# --- ipcc_terms (v>=0.8)
## ///////////////////////////////////////////////////////////////////////////////

# setwd("/home/juaco/workspace/METACLIP/C3S-CORDEX-CMIP5-Single_levels")

source("R/helpers.r")

# Dataset master file --------------------------------------------------------------

master <- read.csv("master_files/cica_inventory_CORDEX-CORE_10122025.csv", stringsAsFactors = FALSE)

# Experiment list
exps <- master$experiment %>% unique()

# Domain list
domains <- master$domain %>% unique()

## /////////////////////////////////////////
## set output directory
output.dir <- "json_ld/CORDEX-CORE"
## ////////////////////////////////////////




  # RCM component metadata master file -------------------------------

  # dom_comps <- read.csv("master_files/cordex_model_components.csv") %>% names()
  # subset(., Domain == dom)

  # create unique rcm_id for lookup
  # dom_comps$rcm_id <- paste(dom_comps$Institution,
  #                           dom_comps$RCM,
  #                           dom_comps$version,
    #                           sep = "_")

## ///////////////////////////////////////////////////////////////////
## EXPERIMENT
## ///////////////////////////////////////////////////////////////////

for (i in 1:length(exps)) {
  # i=1
  exp <- exps[i]
  message("[",format(Sys.time(), "%H:%M:%S"),"] Processing ", exp, " experiment")

  master.subset <- subset(master, subset = experiment == exp)

  ## Full path of the output json file
  output.file <- file.path(output.dir, paste0("C3S-ATLAS-CORDEX-CMIP5-CORDEX-CORE_", exp, ".jsonld"))
  if (!dir.exists(output.dir)) {
    dir.create(output.dir, recursive = TRUE)
  }

  ## ///////////////////////////////////////////////////////////////////
  ## DOMAIN
  ## ///////////////////////////////////////////////////////////////////

  ## Grand Ensemble list building - lapply over domains
  grand.ensemble.list <- lapply(1:length(domains), function(j) {
    # j=7
    dom <- domains[j]
    message("[",format(Sys.time(), "%H:%M:%S"),"] Processing ", dom, " domain")
    exp.subset <- subset(master.subset, subset = domain == dom) # Preserve exp.subset name from previous scripts
  
    ## /////////////////////////////////////////////////////////////////////
    ## DATASET SUBSETS CREATION --------------------------------------------
    ## /////////////////////////////////////////////////////////////////////

    ## Regional CORDEX-CORE Ensemble list - lapply over of dataset subset graphs
    ds.subset.list <- lapply(1:nrow(exp.subset), function(k) {

      # k=1
      gcm <- exp.subset$gcm[k]
      rcm <-  exp.subset$rcm[k]
      mem <- exp.subset$ensemble[k]
      rcmv <- exp.subset$rcm_version[k]

      ## Metadata for RCM components and miscellaneous details
      ind_rcm <- paste(exp.subset$rcm_institution[k], rcm, rcmv, sep = "_")

      # Use exact match to avoid duplicated rows that break attribute filtering
      # metadata <- subset(eur11_comps, rcm_id == ind_rcm)
      # if (nrow(metadata) == 0) {
      #   stop(ind_rcm, " not found in RCM component metadata master file")
      # }
      # if (nrow(metadata) > 1) {
      #   stop("Multiple entries found for ", ind_rcm, " in RCM component metadata master file")
      # }
      # metadata <- metadata[1, , drop = FALSE]


      ## Initialize graph
      graph <- make_empty_graph(directed = TRUE)

      ## /////////////////////////////////////////////////////////////////
      ## DATASET ---------------------------------------------------------
      ## /////////////////////////////////////////////////////////////////

      ## Dataset
      dlabel <- paste("CORDEX-CORE", dom, gcm, exp, mem, rcm, sep = "_")
      ds <- paste0(dlabel, "_", randomName())
      message("[",format(Sys.time(), "%H:%M:%S"),"] Processing Dataset ", dlabel)

      # Dataset Individuals from CORDEX-CMIP5 vocabulary are currently ignored

      ## The GCM model run is annotated as a Dataset Property using ds:hasRun
      graph <- my_add_vertices(graph,
                               name = ds,
                               label = dlabel,
                               className = "ds:MultiDecadalSimulation",
                               attr = list("ds:modelRun" = mem))
      ## Project
      graph <- my_add_vertices(graph,
                               name = "ds:CORDEX-CORE",
                               label = "CORDEX-CORE",
                               className = "ds:Project")
      graph <- add_edges(graph,
                         c(getNodeIndexbyName(graph, ds),
                           getNodeIndexbyName(graph, "ds:CORDEX-CORE")),
                         label = "ds:hasProject")

      ## Data Provider
      graph <- my_add_vertices(graph,
                               name = "ds:C3S",
                               label = "C3S",
                               className = "ds:DataProvider")
      graph <- add_edges(graph,
                         c(getNodeIndexbyName(graph, ds),
                           getNodeIndexbyName(graph, "ds:C3S")),
                         label = "ds:hasDataProvider")

      ## SpatialExtent
      dom.name <- paste0("ipcc:", dom)
      graph <- my_add_vertices(graph,
                               name = dom.name,
                               label = dom,
                               className = "ds:CORDEXdomain")
      graph <- add_edges(graph,
                         c(getNodeIndexbyName(graph, ds),
                           getNodeIndexbyName(graph, dom.name)),
                         label = "ds:hasHorizontalExtent")

      ## Experiment
      exp.nodename <- set.rcp(exp)
      graph <- my_add_vertices(graph,
                               name = exp.nodename,
                               label = exp,
                               className = "ds:Experiment")
      graph <- add_edges(graph,
                         c(getNodeIndexbyName(graph, ds),
                           getNodeIndexbyName(graph, exp.nodename)),
                         label = "ds:hasExperiment")

      ## RCM info
      insts <- exp.subset$rcm_institution[k]

      rcmind <- paste(insts, rcm, dom, rcmv, sep = "-")
      entity.check(target_instance = rcmind,
                   vocab_file = models.voc,
                   #vocab_file = "/home/juaco/workspace/METACLIP/CORDEX-CMIP5/CORDEX-CMIP5-models.owx",
                   silent = TRUE)
      rcm.nodename <- paste0("cx5m:", rcmind)


      attr.list <- list("ds:versionTag" = rcmv,
                        ## Provisional annotation until harmonized metadata table is ready
                        "ds:simulationNotes" = "https://zenodo.org/records/6553526"
      ) %>% filter_attr_list()

      graph <- my_add_vertices(graph,
                               name = rcm.nodename,
                               label = rcm,
                               className = "ds:RCM",
                               attr = attr.list)
      graph <- add_edges(graph,
                         c(getNodeIndexbyName(graph, ds),
                           getNodeIndexbyName(graph, rcm.nodename)),
                         label = "ds:hasSimulationModel")

      ## RCM modelling center - Dataset production
      ## NOTE: Since datasource v1.0.0 the new object property "ds:wasDevelopedBy"
      ## allows to differentiate de ModellingCenter producing the Dataset
      ## from the Modelling Center responsible for GCM/RCM code development
      ## (usually the same Organization, but different Domain -> Range relationship)

      if (insts == "CLMcom-ETH") insts <- list("CLMcom", "ETH")

      for (l in 1:length(insts)) {
        inst <- insts[l]
        inst.nodename <- paste0("ipcc:", inst)
        graph <- my_add_vertices(graph,
                                 name = inst.nodename,
                                 label = inst,
                                 className = "ds:ModellingCenter")
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, ds),
                             getNodeIndexbyName(graph, inst.nodename)),
                           label = "ds:hasModellingCenter")

        ## RCM modelling center - Software development
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, rcm.nodename),
                             getNodeIndexbyName(graph, inst.nodename)),
                           label = "ds:wasDevelopedBy")
      }

      ## Driving GCM
      ## NOTA: Cambios en mayusculas minusculas, efectivos a partir de ipcc_terms v0.8

      gcm.nodename <- paste0("ipcc:", toupper(gcm))
      graph <- my_add_vertices(graph,
                               name = gcm.nodename,
                               label = toupper(gcm),
                               className = "ds:GCM")
      graph <- add_edges(graph,
                         c(getNodeIndexbyName(graph, rcm.nodename),
                           getNodeIndexbyName(graph, gcm.nodename)),
                         label = "ds:hasDrivingGCM")

      ## GCM Modelling center
      insts <- exp.subset$gcm_institution[k]
      if (insts == "CNRM-CERFACS") insts <- list("CNRM", "CERFACS")

      for (l in 1:length(insts)) {
        inst <- insts[l]
        inst.nodename <- paste0("ipcc:", inst)
        graph <- my_add_vertices(graph,
                                 name = inst.nodename,
                                 label = inst,
                                 className = "ds:ModellingCenter")
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, gcm.nodename),
                             getNodeIndexbyName(graph, inst.nodename)),
                           label = "ds:wasDevelopedBy")
      }

      #### ////////////////////////////////////////////////////////////////
      #### RCM COMPONENTS
      #### ////////////////////////////////////////////////////////////////
      #
      ### Atmospheric Model ------------------------------------------------
      #
      #label <- paste(ind_rcm, "AtmosphericModel", sep = "_")
      #comp.nodename <- paste("Component", label, sep = ".")
      #
      ## Data properties:
      #attr.list <- list("ds:toaLevels" = as.character(metadata$Atmos_nlevelsToA),
      #                  "ds:gridDescription" = as.character(metadata$Atmos_Grid),
      #                  "ds:atmosDynamicsCore" = as.character(metadata$Atmos_Dyn),
      #                  "ds:atmosConvectionScheme" = as.character(metadata$Atmos_CU),
      #                  "ds:atmosMicrophysicsScheme" = as.character(metadata$Atmos_MP),
      #                  "ds:atmosPblScheme" = as.character(metadata$Atmos_PBL),
      #                  "ds:atmosShortwaveRadiationScheme" = as.character(metadata$Atmos_SW),
      #                  "ds:atmosLongwaveRadiationScheme" = as.character(metadata$Atmos_LW),
      #                  "ds:atmosMisc"= as.character(metadata$Atmos_details)
      #                  ) %>% filter_attr_list()
      #
      #graph <- my_add_vertices(graph,
      #                         name = comp.nodename,
      #                         label = label,
      #                         className = "ds:AtmosModel",
      #                         attr = attr.list)
      #
      #graph <- add_edges(graph,
      #                   c(getNodeIndexbyName(graph, rcm.nodename),
      #                     getNodeIndexbyName(graph, comp.nodename)),
      #                         label = "ds:hasAtmosModelComponent")
      #
      ### Aerosol Model ------------------------------------------------
      #label <- paste(ind_rcm, "AerosolModel", sep = "_")
      #comp.nodename <- paste("Component", label, sep = ".")
      #
      ## Data properties:
      #attr.list <- list("ds:aerosolParameterizationDescription" = as.character(metadata$Aerosols_mode),
      #      "ds:aerosolRepresentationMode" = as.character(metadata$Aerosols_compName),
      #      "ds:aerosolMisc" = as.character(metadata$Aerosols_details)
      #      ) %>% filter_attr_list()
      #
      #graph <- my_add_vertices(graph,
      #                         name = comp.nodename,
      #                         label = label,
      #                         className = "ds:AerosolModel",
      #                         attr = attr.list)
      #
      #graph <- add_edges(graph,
      #                   c(getNodeIndexbyName(graph, rcm.nodename),
      #                     getNodeIndexbyName(graph, comp.nodename)),
      #                         label = "ds:hasAerosolModelComponent")
      #
      #
      ### Land Surface Model ------------------------------------------------
      #label <- paste(ind_rcm, "LandSurfaceModel", sep = "_")
      #comp.nodename <- paste("Component", label, sep = ".")
      #
      ## Data properties:
      #attr.list <- list("ds:lsmLevels" = as.character(metadata$Land_nLevels),
      #                  "ds:lsmMaxDepth" = as.character(metadata$Land_maxDepth),
      #                  "ds:lsmComponentName" = as.character(metadata$Land_compName),
      #                  "ds:lsmMisc" = as.character(metadata$Land_details)
      #                  ) %>% filter_attr_list()
      #
      #graph <- my_add_vertices(graph,
      #                         name = comp.nodename,
      #                         label = label,
      #                         className = "ds:LandSurfaceModel",
      #                         attr = attr.list)
      #
      #graph <- add_edges(graph,
      #                   c(getNodeIndexbyName(graph, rcm.nodename),
      #                     getNodeIndexbyName(graph, comp.nodename)),
      #                   label = "ds:hasLandSurfaceModelComponent")
      #
      ### Ocean Model ------------------------------------------------
      #
      #label <- paste(ind_rcm, "OceanModel", sep = "_")
      #comp.nodename <- paste("Component", label, sep = ".")
      #
      ## Data properties:
      #attr.list <- list("ds:oceanRepresentationMode" = as.character(metadata$Ocean_mode),
      #                  "ds:oceanComponentReference" = as.character(metadata$Ocean_compRef),
      #                  "ds:oceanMisc" = as.character(metadata$Ocean_details)
      #                  ) %>% filter_attr_list()
      #
      #graph <- my_add_vertices(graph,
      #                         name = comp.nodename,
      #                         label = label,
      #                         className = "ds:OceanModel",
      #                         attr = attr.list)
      #
      #graph <- add_edges(graph,
      #                   c(getNodeIndexbyName(graph, rcm.nodename),
      #                     getNodeIndexbyName(graph, comp.nodename)),
      #                          label = "ds:hasOceanModelComponent")
      #
      ### Lake Model ------------------------------------------------
      #
      #label <- paste(ind_rcm, "LakeModel", sep = "_")
      #comp.nodename <- paste("Component", label, sep = ".")
      ## Data properties:
      #attr.list <- list("ds:lakeRepresentation" = as.character(metadata$Lake_model)) %>% filter_attr_list()
      ## No lake model node is represented if metadata is missing
      #if (length(attr.list) > 0) {
      #  graph <- my_add_vertices(graph,
      #                           name = comp.nodename,
      #                           label = label,
      #                           className = "ds:LakeModel",
      #                           attr = attr.list)
      #  graph <- add_edges(graph,
      #                     c(getNodeIndexbyName(graph, rcm.nodename),
      #                       getNodeIndexbyName(graph, comp.nodename)),
      #                     label = "ds:hasLakeModelComponent")
      #}
      #
      #
      ### Urban model ------------------------------------------------
      #
      #label <- paste(ind_rcm, "UrbanModel", sep = "_")
      #comp.nodename <- paste("Component", label, sep = ".")
      ## Data properties:
      #attr.list <- list("ds:urbanLsmRepresentation" = as.character(metadata$Urban_model)) %>% filter_attr_list()
      ## No lake model node is represented if metadata is missing
      #if (length(attr.list) > 0) {
      #  graph <- my_add_vertices(graph,
      #                           name = comp.nodename,
      #                           label = label,
      #                           className = "ds:UrbanLandSurfaceModel",
      #                           attr = attr.list)
      #
      #  graph <- add_edges(graph,
      #                     c(getNodeIndexbyName(graph, rcm.nodename),
      #                       getNodeIndexbyName(graph, comp.nodename)),
      #                     label = "ds:hasUrbanLandSurfaceModelComponent")
      #}
      #
      ## /////////////////////////////////////////////////////////////////
      ## DATASET SUBSET --------------------------------------------------
      ## /////////////////////////////////////////////////////////////////

      dsubname <- paste0("DatasetSubset.", ds)
      # descr <- paste("This step entails extracting a logical subset of the",
      #                dlabel, "Dataset")
      # attr.list <- list("dc:description" = descr)
      graph <- my_add_vertices(graph,
                               name = dsubname,
                               label = "DatasetSubset",
                               className = "ds:DatasetSubset")
      #                         attr = attr.list)
      graph <- add_edges(graph,
                         c(getNodeIndexbyName(graph, ds),
                           getNodeIndexbyName(graph, dsubname)),
                         label = paste0("ds:hasDatasetSubset"))

      ## TemporalExtent ----------------------------------------------

      if (exp == "historical") {
        start <- "1970-01-01T00:00:00Z"
        end <- "2005-12-31T23:59:59Z"
        label <- "[1970-2006)"
      } else {
        start <- "2006-01-01T00:00:00Z"
        end <- "2100-12-31T23:59:59Z"
        label <- "[2006-2100)"
      }
      attr.list <- list("prov:startedAtTime" = start,
                        "prov:endedAtTime" = end)
      textentname <- paste("TemporalPeriod", randomName(), sep = ".")
      graph <- my_add_vertices(graph,
                               name = textentname,
                               label = label,
                               className = "ds:TemporalPeriod",
                               attr = attr.list)
      graph <- add_edges(graph,
                         c(getNodeIndexbyName(graph, dsubname),
                           getNodeIndexbyName(graph, textentname)),
                         label = paste0("ds:hasValidTemporalPeriod"))


      return(list("parentnodename" = dsubname, "graph" = graph))
    }) # end lapply over datasets

    ## /////////////////////////////////////////////////////////////////////
    ## DOMAIN ENSEMBLE BUILDING --------------------------------------------
    ## /////////////////////////////////////////////////////////////////////

    message("[", format(Sys.time(), "%H:%M:%S"),
            "] Building ", exp, " Ensemble from ",
            length(ds.subset.list), " datasets")

    ## Ensemble dataset
    ens <- metaclipR.Ensemble2(graph.list = ds.subset.list)

    #plot(ens$graph, vertex.size = .25, edge.label.cex = .2, vertex.label.cex = .7)
    return(list("parentnodename" = ens$parentnodename, "graph" = ens$graph))

  }) # end lapply over domains

  grand.ensemble <- metaclipR.Ensemble2(graph.list = grand.ensemble.list,
                                        grand.ensemble = TRUE,
                                        named.individual = "ds:C3S-CORDEX-GrandEnsemble")

  ## /////////////////////////////////////////////////////////////////////
  ## C3S DATASET CREATION-------------------------------------------------
  ## /////////////////////////////////////////////////////////////////////

  # individual Defined since datasource v1.0.0
  graph <- my_add_vertices(grand.ensemble$graph,
                           name = "ds:Copernicus-CORDEX-CMIP5",
                           label = "CICA-CORDEX-CMIP5",
                           className = "ds:Dataset")

  graph <- add_edges(graph,
                     c(getNodeIndexbyName(graph, "ds:Copernicus-CORDEX-CMIP5"),
                       getNodeIndexbyName(graph, grand.ensemble$parentnodename)),
                     label = paste0("prov:wasDerivedFrom"))

  ## /////////////////////////////////////////////////////////////////////
  ## EXPORT JSON-LD ------------------------------------------------------
  ## /////////////////////////////////////////////////////////////////////

  message("[",format(Sys.time(), "%H:%M:%S"),
          "] Exporting to ", output.file)
  ## Copy template to output file
  system(paste("cp inst/template.json", output.file))

  ## Serialize output and write to json -----------------------------

  metaclipR::graph2json(graph, output.file, template = TRUE)

  ## Prettify ------------------------------------------

  metaclipR::prettyJSON(output.file)

}   # end for experiments 

