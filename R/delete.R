#' Delete packages
#'
#' Delete packages from the server.
#'
#' @export
#' @rdname sync_ropensci
#' @param universe name of the universe
delete_from_server <- function(universe){
  # Some input validation
  monorepo_url <- sprintf('https://github.com/r-universe/%s', universe)
  cranlike_url <- sprintf('https://%s.r-universe.dev', universe)
  userpwd <- Sys.getenv("CRANLIKEPWD", NA)
  if(is.na(userpwd)) stop("No CRANLIKEPWD set, cannot deploy")

  # Clone and cd into the monorepo
  repo <- file.path(tempdir(), paste0(universe, '-universe'))
  unlink(repo, recursive = TRUE)
  sys::exec_internal("git", c("clone", monorepo_url, repo))
  pwd <- getwd()
  on.exit(setwd(pwd))
  setwd(repo)

  # Get current submodules
  out <- sys::exec_internal('git', c('config', '--file', '.gitmodules', '--get-regexp', '\\.path$'))
  submodules <- vapply(strsplit(sys::as_text(out$stdout), ' ', fixed = TRUE), `[[`, character(1), 2)
  submodules <- unique(c(submodules, list.files())) # Just in case...
  caterr("Current submodules:", paste(submodules, collapse = ', '), '\n\n')
  pkgs <- jsonlite::fromJSON(paste0(cranlike_url, '/api/ls'))
  deleted <- pkgs[!(pkgs %in% submodules)]
  if(length(deleted)){
    caterr("Removed packages:", paste(deleted, collapse = ', '), '\n\n')
    if(utils::askYesNo("are you sure you want to delete these from the repository?")){
      lapply(deleted, function(package){
        message("Deleting: ", package)
        h <- curl::new_handle(customrequest = 'DELETE', userpwd = userpwd)
        url <- paste0(cranlike_url, '/packages/', package)
        out <- parse_res(curl::curl_fetch_memory(url, handle = h))
        stopifnot(out$Package == package)
      })
    }
  } else {
    caterr("Everything is up to date.\n")
  }

  # Delete docs for packages that were removed
  if(universe == 'ropensci'){
    repos <- list_ropensci_docs_repos()
    packages <- c(submodules, 'ropensci-docs.github.io')
    deleted <- repos[!(tolower(repos) %in% tolower(packages))]
    missing <- packages[!(tolower(packages) %in% tolower(repos))]
    if(length(missing)){
      caterr("Missing ropensci-docs for packages: ", paste(missing, collapse = ', '), "\n")
    }
    if(length(deleted) > 15){
      stop("Found more than 15 deleted repos. Something may be wrong?")
    }
    if(length(deleted)){
      caterr("Found docs for removed packages: ", paste(deleted, collapse = ', '), "\n")
      caterr("whoami: ", gh::gh_whoami()$login, "\n")
      if(utils::askYesNo("are you sure you want to delete these?")){
        lapply(deleted, function(name){
          message("Deleting: ropensci-docs/", name)
          gh::gh(paste0('/repos/ropensci-docs/', name), .method = 'DELETE')
        })
      }
    } else {
      cat("Nothing to delete in ropensci-docs!\n")
    }
  }
}

list_ropensci_docs_repos <- function(){
  repos <- gh::gh('/users/ropensci-docs/repos?per_page=100', .limit = 1e6)
  vapply(repos, function(x){
    return(x$name)
  }, character(1))
}

parse_res <- function(res){
  text <- rawToChar(res$content)
  if(res$status >= 400)
    stop(text)
  jsonlite::fromJSON(text)
}

caterr <- function(...){
  base::cat(..., file = stderr())
}
