options(scipen = 999)

get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

find_r_executable <- function() {
  r_home <- Sys.getenv("R_HOME", unset = NA_character_)
  if (!is.na(r_home) && nzchar(r_home)) {
    exe <- file.path(r_home, "bin", "R")
    if (.Platform$OS.type == "windows") exe <- paste0(exe, ".exe")
    if (file.exists(exe)) return(exe)
  }

  r_bin <- Sys.which("R")
  if (nzchar(r_bin)) return(unname(r_bin))
  stop("Unable to locate the R executable. Set R_HOME or add R to PATH.", call. = FALSE)
}

run_cmd <- function(r_bin, args, wd) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(wd)
  status <- system2(r_bin, args = args, stdout = "", stderr = "", wait = TRUE)
  if (!identical(status, 0L)) {
    stop(paste("Command failed:", paste(c(r_bin, args), collapse = " ")), call. = FALSE)
  }
}

assert_check_ok <- function(build_dir) {
  log_file <- file.path(build_dir, "ZhenMeasure.Rcheck", "00check.log")
  if (!file.exists(log_file)) {
    stop("R CMD check log was not created.", call. = FALSE)
  }

  log_lines <- readLines(log_file, warn = FALSE, encoding = "UTF-8")
  if (!any(grepl("^Status: OK$", log_lines))) {
    stop(paste0("R CMD check did not finish with 'Status: OK'. See: ", log_file), call. = FALSE)
  }

  invisible(log_file)
}

script_dir <- get_script_dir()
pkg_dir <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = TRUE)
build_dir <- file.path(pkg_dir, "build-output")
if (!dir.exists(build_dir)) dir.create(build_dir, recursive = TRUE)

r_bin <- find_r_executable()

message("Running R CMD build on package sources...")
run_cmd(r_bin, c("CMD", "build", shQuote(pkg_dir), "--no-manual"), wd = build_dir)

tarball <- list.files(build_dir, pattern = "\\.tar\\.gz$", full.names = TRUE)
if (length(tarball) == 0) {
  stop("R CMD build did not produce a source tarball.", call. = FALSE)
}
tarball <- tarball[which.max(file.info(tarball)$mtime)]

message("Running R CMD check on built tarball...")
run_cmd(r_bin, c("CMD", "check", shQuote(tarball), "--no-manual"), wd = build_dir)
check_log <- assert_check_ok(build_dir)

message("Build and check completed successfully.")
message("Tarball: ", tarball)
message("Check log: ", check_log)
message("Build output directory: ", normalizePath(build_dir, winslash = "/", mustWork = TRUE))