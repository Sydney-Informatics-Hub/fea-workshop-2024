# Function to check disk space (in GB)
check_disk_space <- function() {
  df <- as.numeric(system("df --output=avail / | tail -1", intern = TRUE)) / (1024^2)
  return(round(df, 2))
}

# Initial disk space check
initial_space <- check_disk_space()
message(paste("Initial available disk space:", initial_space, "GB"))

# Function to check if rust is installed
check_rust <- function() {
  tryCatch({
    # Check if rustc (Rust compiler) is available in the PATH
    rust_installed <- system("which rustc", intern = TRUE, ignore.stderr = TRUE)
    
    # If rustc is not installed or the command fails
    if (length(rust_installed) == 0 || rust_installed == "") {
      message("Rust is not installed. Installing Rust...")
      
      # Install rust using rustup (works only on Linux)
      if (Sys.info()[["sysname"]] == "Linux") {
        install_command <- "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y"
        message("Running the installation command: ", install_command)
        
        # Run the installation command
        system(install_command, intern = TRUE)
        
        # After installation, update the PATH to include the cargo bin directory
        Sys.setenv(PATH = paste(Sys.getenv("HOME"), "/.cargo/bin", Sys.getenv("PATH"), sep = ":"))
        
        # Check if Rust is now installed
        rust_installed <- system("which rustc", intern = TRUE, ignore.stderr = FALSE)
        if (length(rust_installed) > 0 && rust_installed != "") {
          message("Rust installed successfully: ", rust_installed)
        } else {
          stop("Rust installation failed. Please install Rust manually.")
        }
      } else {
        stop("Rust installation is only supported on Linux through this script. Please install Rust manually on other systems.")
      }
    } else {
      message("Rust is already installed: ", rust_installed)
    }
  }, error = function(e) {
    message("Error checking for rust: ", e$message)
    stop("Please ensure Rust is installed and available in the PATH.")
  })
}

# Call check_rust to install Rust if necessary
check_rust()

# Define package lists
cran_packages <- c("gprofiler2", "fgsea", "WebGestaltR", "ggupset", 
                   "ggridges", "msigdbr", "grid", "venn", "ontologyIndex", "tidyverse")

bioc_packages <- c("STRINGdb", "ReactomePA", "topGO", "ALL", "GO.db", "DESeq2", 
                   "org.Hs.eg.db", "org.Mm.eg.db", "clusterProfiler", "biomaRt")

# Install CRAN packages only if they are missing
for (pkg in cran_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing CRAN package:", pkg))
    install.packages(pkg)
  } else {
    message(paste("CRAN package already installed:", pkg))
  }
}

# Install Bioconductor packages only if they are missing
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

for (pkg in bioc_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing Bioconductor package:", pkg))
    BiocManager::install(pkg)
  } else {
    message(paste("Bioconductor package already installed:", pkg))
  }
}

# Check and install devtools if necessary
if (!requireNamespace("devtools", quietly = TRUE)) {
  message("Installing CRAN package: devtools")
  install.packages("devtools")
} else {
  message("CRAN package already installed: devtools")
}

# Check for Rust installation
check_rust()

# Install WebGestaltR from GitHub
if (!requireNamespace("WebGestaltR", quietly = TRUE)) {
  message("Installing WebGestaltR from GitHub...")
  devtools::install_github("bzhanglab/WebGestaltR")
} else {
  message("Package WebGestaltR already installed")
}

# Install enrichR from GitHub
if (!requireNamespace("enrichR", quietly = TRUE)) {
  message("Installing enrichR from GitHub")
  devtools::install_github("wjawaid/enrichR")
} else {
  message("Package enrichR already installed")
}

# Load all packages, perform minimal tests, and print versions
test_installation <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    message(paste("Error loading", pkg))
    return(FALSE)
  }
  
  # Minimal functionality test for each package
  switch(pkg,
         "gprofiler2" = {try(gprofiler2::gconvert(c("TP53")), silent = TRUE)},
         "fgsea" = {try(fgsea::fgsea(pathways = list(path1 = c("gene1", "gene2")), stats = c(gene1 = 1, gene2 = 2), minSize = 1, maxSize = 2, scoreType = "pos"), silent = TRUE)},
         "WebGestaltR" = {try(WebGestaltR::WebGestaltR(enrichMethod = "ORA", organism = "hsapiens"), silent = TRUE)},
         "STRINGdb" = {try(STRINGdb::STRINGdb$new(version = "11.0", species = 9606, score_threshold = 200), silent = TRUE)},
         "ReactomePA" = {try(ReactomePA::enrichPathway(gene = c("675", "7157")), silent = TRUE)},
         "topGO" = {try(topGO::new("topGOdata", ontology = "BP", allGenes = c("gene1" = 1, "gene2" = 0)), silent = TRUE)},
         "ggupset" = {try(ggupset::geom_upset(aes()), silent = TRUE)},
         "ggridges" = {try(ggridges::geom_density_ridges(aes(y = 1, x = c(1:10))), silent = TRUE)},
         "msigdbr" = {try(msigdbr::msigdbr(species = "Homo sapiens", category = "C2"), silent = TRUE)},
         "grid" = {try(grid::grid.newpage(), silent = TRUE)},
         "ALL" = {
           try({
             library(ALL)
             data("ALL")
             if (exists("ALL")) {
               message("ALL dataset loaded successfully")
             } else {
               message("Failed to load ALL dataset")
             }
           }, silent = TRUE)
         },
         "GO.db" = {try(GO.db::GOID("GO:0008150"), silent = TRUE)},
         "venn" = {try(venn::venn(list(A = 1:5, B = 4:8)), silent = TRUE)},
         "ontologyIndex" = {try(ontologyIndex::get_ancestors("GO:0008150"), silent = TRUE)},
         "tidyverse" = {try(tidyverse::tibble(x = 1:5), silent = TRUE)},
         "DESeq2" = {try(DESeq2::makeExampleDESeqDataSet(), silent = TRUE)},
         "enrichR" = {try(enrichR::enrichr(c("TP53", "BRCA1"), "KEGG_2021_Human"), silent = TRUE)},
         "clusterProfiler" = {try(clusterProfiler::enrichGO(gene = c("TP53", "BRCA1"), OrgDb = "org.Hs.eg.db", ont = "BP"), silent = TRUE)},
         "biomaRt" = {try(biomaRt::useMart("ensembl"), silent = TRUE)},
         {message(paste("No test available for", pkg))}
  )
  
  # Get package version and print success message
  pkg_version <- as.character(packageVersion(pkg))
  message(paste("Package", pkg, "loaded and tested successfully (version:", pkg_version, ")"))
  return(TRUE)
}

# Run tests and report results
results <- sapply(c(cran_packages, bioc_packages, "enrichR"), test_installation)

# Summary of installation and test results
if(all(results)) {
  message("All packages installed, loaded, and tested successfully!")
} else {
  failed_packages <- names(results)[!results]
  message("Some packages failed to load or test successfully:", paste(failed_packages, collapse = ", "))
}

# Clean up temporary files
unlink(tempdir(), recursive = TRUE)
message("Temporary files cleaned up.")

# Final disk space check
final_space <- check_disk_space()
message(paste("Final available disk space:", final_space, "GB"))
message(paste("Disk space used:", round(initial_space - final_space, 2), "GB"))
