#' Delete packages
#'
#' Delete packages from the server.
#'
#' @export
#' @rdname sync_ropensci
#' @param monorepo_url git url of the monorepo
#' @param cranlike_url url to the cranlike server packages api
delete_from_server <- function(monorepo_url, cranlike_url){
  # Some input validation
  userpwd <- Sys.getenv("CRANLIKEPWD", NA)
  if(is.na(userpwd)) stop("No CRANLIKEPWD set, cannot deploy")
  if(basename(cranlike_url) != 'packages') stop("cranlike_url should end in /packages")

  # Clone and cd into the monorepo
  repo <- file.path(tempdir(), paste0(basename(monorepo_url), '-universe'))
  unlink(repo, recursive = TRUE)
  gert::git_clone(monorepo_url, path = repo)
  pwd <- getwd()
  on.exit(setwd(pwd))
  setwd(repo)

  # Get current submodules
  out <- sys::exec_internal('git', c('config', '--file', '.gitmodules', '--get-regexp', 'path'))
  submodules <- vapply(strsplit(sys::as_text(out$stdout), ' ', fixed = TRUE), `[[`, character(1), 2)
  caterr("Current submodules:", paste(submodules, collapse = ', '), '\n\n')
  pkgs <- jsonlite::fromJSON(cranlike_url)
  deleted <- pkgs[!(pkgs %in% submodules)]
  if(length(deleted)){
    caterr("Removed packages:", paste(deleted, collapse = ', '), '\n\n')
    if(utils::askYesNo("are you sure you want to delete these from the repository?")){
      lapply(deleted, function(package){
        message("Deleting: ", package)
        h <- curl::new_handle(customrequest = 'DELETE', userpwd = userpwd)
        url <- sprintf("%s/%s", cranlike_url, package)
        out <- parse_res(curl::curl_fetch_memory(url, handle = h))
        stopifnot(out$Package == package)
      })
    }
  }
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
