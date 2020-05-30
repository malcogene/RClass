
# http://r-pkgs.had.co.nz/check.html    # importFrom limma plotMDS
#' @name AppUI
#' @title Launch interactive User Interface
#' @description  AppUI initiates in the web browser an interactive user interface of ...  This user interface enables users to easily perform nearly all standard ... functions in ... package.
#' @usage AppUI()
#' @param sfPower numerical value of ... for
#' @return A user interface will be shown in users' default web browser.
#' @import shiny        
#' @importFrom limma plotMDS
#' @export 
AppUI <- function() {
  currentfilePath <- dirname(rstudioapi::getSourceEditorContext()$path)
  pkgname <-(pkg <- strsplit(currentfilePath, '/'))[[1]][length(pkg[[1]])-1]; pkgname 
  shiny::runApp(system.file("shiny", package=pkgname))} #
