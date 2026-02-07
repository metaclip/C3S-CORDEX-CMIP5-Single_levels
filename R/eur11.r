## ///////////////////////////////////////////////////////////////////////////////
# IMPORTANT NOTE: 
# This script requires an up-to-date version of the ontologies:
# --- datasource (v>=0.31)
# --- ipcc_terms (v>=0.8)
## ///////////////////////////////////////////////////////////////////////////////

source("R/helpers.r")

# Dataset master file --------------------------------------------------------------

master <- read.csv("master_files/cica_inventory_CORDEX-EUR-11_10122025.csv")

# Experiment list
exps <- master$experiment %>% unique()


# RCM component metadata master file -------------------------------

eur11_comps <- read.csv("master_files/cordex_model_components.csv") %>% subset(., Domain == "EUR-11")
# create unique rcm_id for lookup
eur11_comps$rcm_id <- paste(eur11_comps$Institution,
                            eur11_comps$RCM,
                            eur11_comps$version,
                            sep = "_")


## /////////////////////////////////////////
## set output directory
output.dir <- "json_ld/EUR-11"
## ////////////////////////////////////////



for (i in 1:length(exps)) {
  # i=1
  message("[",format(Sys.time(), "%H:%M:%S"),"] Processing ", exps[i], " experiment")

  exp <- exps[i]
  exp.subset <- subset(master, subset = experiment == exp)
        
  ## Full path of the output json file
  output.file <- file.path(output.dir, paste0("C3S-ATLAS-CORDEX-CMIP5-EUR11_", exp, ".jsonld"))
                              
  if (!dir.exists(output.dir)) {
      dir.create(output.dir, recursive = TRUE)
  }

  ## /////////////////////////////////////////////////////////////////////
  ## DATASET SUBSETS CREATION --------------------------------------------
  ## /////////////////////////////////////////////////////////////////////

  ## List of dataset subset graphs
  ds.subset.list <- lapply(1:nrow(exp.subset), function(j) {
  # ds.subset.list <- lapply(1:3, function(j) {
  # j=1
    gcm <- exp.subset$gcm[j]
    rcm <-  exp.subset$rcm[j]
    mem <- exp.subset$ensemble[j]
    rcmv <- exp.subset$rcm_version[j]


    ## Metadata for RCM components and miscellaneous details
    ind_rcm <- paste(exp.subset$rcm_institution[j], rcm, rcmv, sep = "_")
    # Use exact match to avoid duplicated rows that break attribute filtering
    metadata <- subset(eur11_comps, rcm_id == ind_rcm)
    if (nrow(metadata) == 0) {
      stop(ind_rcm, " not found in RCM component metadata master file")
    }
    if (nrow(metadata) > 1) {
      stop("Multiple entries found for ", ind_rcm, " in RCM component metadata master file")
    }
    metadata <- metadata[1, , drop = FALSE]


    ## Initialize graph
    graph <- make_empty_graph(directed = TRUE)

    ## ///////////////////////////////////////////////////////////////// 
    ## DATASET ---------------------------------------------------------
    ## /////////////////////////////////////////////////////////////////

    ## Dataset 
    dlabel <- paste("EUR-11", gcm, exp, mem, rcm, sep = "_")
    ds <- paste0(dlabel, randomName(), sep = ".")
    message("[",format(Sys.time(), "%H:%M:%S"),"] Processing Dataset ", ds)
    
    # Dataset Individuals from CORDEX-CMIP5 vocabulary are currently ignored
    
    ## The GCM model run is annotated as a Dataset Property using ds:hasRun
    graph <- my_add_vertices(graph,
                             name = ds,
                             label = dlabel,
                             className = "ds:MultiDecadalSimulation",
                             attr = list("ds:modelRun" = mem))
    ## Project
    graph <- my_add_vertices(graph,
                             name = "ds:EURO-CORDEX",
                             label = "EURO-CORDEX",
                             className = "ds:Project")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, ds),
                         getNodeIndexbyName(graph, "ds:EURO-CORDEX")),
                        label = "ds:hadProject")
          
    ## Data Provider
    graph <- my_add_vertices(graph,
                             name = "ds:C3S",
                             label = "C3S",
                             className = "ds:DataProvider")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, ds),
                         getNodeIndexbyName(graph, "ds:C3S")),
                       label = "ds:hadDataProvider")

    ## SpatialExtent
    graph <- my_add_vertices(graph,
                             name = "ds:EURO",
                             label = "Europe",
                             className = "ds:EURO")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, ds),
                         getNodeIndexbyName(graph, "ds:EURO")),
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
                        label = "ds:hadExperiment")

    ## RCM info 
    insts <- exp.subset$rcm_institution[j]
    dom <- exp.subset$domain[j]
    rcmv <- exp.subset$rcm_version[j]
    rcmind <- paste(insts, rcm, dom, rcmv, sep = "-")
    entity.check(target_instance = rcmind, 
                 vocab_file = models.voc,
                 #vocab_file = "/home/juaco/workspace/METACLIP/CORDEX-CMIP5/CORDEX-CMIP5-models.owx",
                 silent = TRUE)
    rcm.nodename <- paste0("cx5m:", rcmind)


    attr.list <- list("ds:versionTag" = rcmv,
                      "ds:simulationNotes" = as.character(metadata$Other_comments)
                      ) %>% filter_attr_list()
    
    graph <- my_add_vertices(graph,
                             name = rcm.nodename,
                             label = rcm,
                             className = "ds:RCM",
                             attr = attr.list)
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, ds),
                         getNodeIndexbyName(graph, rcm.nodename)),
                       label = "ds:hadSimulationModel")

    ## RCM modelling center - Dataset production
    ## NOTE: Since datasource v0.31 the new object property "ds:wasDevelopedBy"
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
                         label = "ds:hadModellingCenter")
              
      ## RCM modelling center - Software development
      graph <- add_edges(graph,
                         c(getNodeIndexbyName(graph, rcm.nodename),
                           getNodeIndexbyName(graph, inst.nodename)),
                         label = "ds:wasDevelopedBy")
    }
    
    ## Driving GCM    
    ## NOTA: Cambios en mayusculas minusculas, efectivos a partir de ipcc_terms v0.8
          
    gcm.nodename <- paste0("ipcc:", gcm)
    graph <- my_add_vertices(graph,
                             name = gcm.nodename,
                             label = gcm,
                             className = "ds:GCM")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, rcm.nodename),
                         getNodeIndexbyName(graph, gcm.nodename)),
                       label = "ds:hadDrivingGCM")

    ## GCM Modelling center
    insts <- exp.subset$gcm_institution[j]
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

    ### ////////////////////////////////////////////////////////////////
    ### RCM COMPONENTS
    ### ////////////////////////////////////////////////////////////////
       
    ## Atmospheric Model ------------------------------------------------

    label <- paste(ind_rcm, "AtmosphericModel", sep = "_")
    comp.nodename <- paste("Component", label, sep = ".")
    
    # Data properties:     
    attr.list <- list("ds:toaLevels" = as.character(metadata$Atmos_nlevelsToA),
                      "ds:gridDescription" = as.character(metadata$Atmos_Grid),
                      "ds:atmosDynamicsCore" = as.character(metadata$Atmos_Dyn),
                      "ds:atmosConvectionScheme" = as.character(metadata$Atmos_CU),
                      "ds:atmosMicrophysicsScheme" = as.character(metadata$Atmos_MP),
                      "ds:atmosPblScheme" = as.character(metadata$Atmos_PBL),
                      "ds:atmosShortwaveRadiationScheme" = as.character(metadata$Atmos_SW),
                      "ds:atmosLongwaveRadiationScheme" = as.character(metadata$Atmos_LW),
                      "ds:atmosMisc"= as.character(metadata$Atmos_details)
                      ) %>% filter_attr_list()

    graph <- my_add_vertices(graph,
                             name = comp.nodename,
                             label = label,
                             className = "ds:AtmosModel", 
                             attr = attr.list)

    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, rcm.nodename),
                         getNodeIndexbyName(graph, comp.nodename)),
                             label = "ds:hasAtmosModelComponent")

    ## Aerosol Model ------------------------------------------------
    label <- paste(ind_rcm, "AerosolModel", sep = "_")
    comp.nodename <- paste("Component", label, sep = ".")  

    # Data properties:  
    attr.list <- list("ds:aerosolParameterizationDescription" = as.character(metadata$Aerosols_mode),
          "ds:aerosolRepresentationMode" = as.character(metadata$Aerosols_compName),
          "ds:aerosolMisc" = as.character(metadata$Aerosols_details)
          ) %>% filter_attr_list()
      
    graph <- my_add_vertices(graph,
                             name = comp.nodename,
                             label = label,
                             className = "ds:AerosolModel", 
                             attr = attr.list)

    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, rcm.nodename),
                         getNodeIndexbyName(graph, comp.nodename)),
                             label = "ds:hasAerosolModelComponent")
         
           
    ## Land Surface Model ------------------------------------------------
    label <- paste(ind_rcm, "LandSurfaceModel", sep = "_")
    comp.nodename <- paste("Component", label, sep = ".")

    # Data properties:  
    attr.list <- list("ds:lsmLevels" = as.character(metadata$Land_nLevels),
                      "ds:lsmMaxDepth" = as.character(metadata$Land_maxDepth),
                      "ds:lsmComponentName" = as.character(metadata$Land_compName),
                      "ds:lsmMisc" = as.character(metadata$Land_details)
                      ) %>% filter_attr_list()     
      
    graph <- my_add_vertices(graph,
                             name = comp.nodename,
                             label = label,
                             className = "ds:LandSurfaceModel", 
                             attr = attr.list)

    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, rcm.nodename),
                         getNodeIndexbyName(graph, comp.nodename)),
                       label = "ds:hasLandSurfaceModelComponent")

    ## Ocean Model ------------------------------------------------
    
    label <- paste(ind_rcm, "OceanModel", sep = "_")
    comp.nodename <- paste("Component", label, sep = ".")  
    
    # Data properties:   
    attr.list <- list("ds:oceanRepresentationMode" = as.character(metadata$Ocean_mode),
                      "ds:oceanComponentReference" = as.character(metadata$Ocean_compRef),
                      "ds:oceanMisc" = as.character(metadata$Ocean_details)
                      ) %>% filter_attr_list()

    graph <- my_add_vertices(graph,
                             name = comp.nodename,
                             label = label,
                             className = "ds:OceanModel", 
                             attr = attr.list)

    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, rcm.nodename),
                         getNodeIndexbyName(graph, comp.nodename)),
                              label = "ds:hasOceanModelComponent")
                              
    ## Lake Model ------------------------------------------------
    
    label <- paste(ind_rcm, "LakeModel", sep = "_")
    comp.nodename <- paste("Component", label, sep = ".")
    # Data properties:  
    attr.list <- list("ds:lakeRepresentation" = as.character(metadata$Lake_model)) %>% filter_attr_list()
    # No lake model node is represented if metadata is missing
    if (length(attr.list) > 0) {
      graph <- my_add_vertices(graph,
                               name = comp.nodename,
                               label = label,
                               className = "ds:LakeModel", 
                               attr = attr.list)
      graph <- add_edges(graph,
                         c(getNodeIndexbyName(graph, rcm.nodename),
                           getNodeIndexbyName(graph, comp.nodename)),
                         label = "ds:hasLakeModelComponent")
    }

      
    ## Urban model ------------------------------------------------

    label <- paste(ind_rcm, "UrbanModel", sep = "_")
    comp.nodename <- paste("Component", label, sep = ".")
    # Data properties:  
    attr.list <- list("ds:urbanLsmRepresentation" = as.character(metadata$Urban_model)) %>% filter_attr_list()
    # No lake model node is represented if metadata is missing
    if (length(attr.list) > 0) {
      graph <- my_add_vertices(graph,
                               name = comp.nodename,
                               label = label,
                               className = "ds:UrbanLandSurfaceModel", 
                               attr = attr.list)

      graph <- add_edges(graph,
                         c(getNodeIndexbyName(graph, rcm.nodename),
                           getNodeIndexbyName(graph, comp.nodename)),
                         label = "ds:hasUrbanLandSurfaceModelComponent")
    }

    ## /////////////////////////////////////////////////////////////////
    ## DATASET SUBSET --------------------------------------------------
    ## /////////////////////////////////////////////////////////////////
      
    dsubname <- paste0("DatasetSubset.", ds)
    descr <- paste("This step entails extracting a logical subset of the",
                   dlabel, "Dataset")
    attr.list <- list("dc:description" = descr)
    graph <- my_add_vertices(graph,
                             name = dsubname,
                             label = "DatasetSubset",
                             className = "ds:DatasetSubset",
                             attr = attr.list)
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, ds),
                         getNodeIndexbyName(graph, dsubname)),
                       label = paste0("ds:hadDatasetSubset"))

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
  ## ENSEMBLE BUILDING ---------------------------------------------------
  ## /////////////////////////////////////////////////////////////////////
  
  message("[", format(Sys.time(), "%H:%M:%S"),
          "] Building ", exp, " Ensemble from ", 
          length(ds.subset.list), " datasets")
  
  ## Ensemble dataset
  ens <- metaclipR.Ensemble(graph.list = ds.subset.list,
                            disable.command = TRUE)

  #plot(ens$graph, vertex.size = .25, edge.label.cex = .2, vertex.label.cex = .7)

  ## /////////////////////////////////////////////////////////////////////
  ## C3S DATASET CREATION-------------------------------------------------
  ## /////////////////////////////////////////////////////////////////////
  
  # individual Defined since datasource v0.31
  graph <- my_add_vertices(ens$graph,
                           name = "ds:Copernicus-CORDEX-CMIP5",
                           label = "CICA-CORDEX-CMIP5",
                           className = "ds:Dataset")
  
  graph <- add_edges(graph,
                     c(getNodeIndexbyName(graph, "ds:Copernicus-CORDEX-CMIP5"),
                       getNodeIndexbyName(graph, ens$parentnodename)),
                     label = paste0("prov:hadPrimarySource"))

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

} # end for experiments 

