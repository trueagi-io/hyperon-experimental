(@doc catalog-list!
  (@desc "Lists contents of all module catalogs that support the \"list\" method")
  (@params (
    (@param "Name of the catalog to list or \"all\" to list all available")))
  (@return "Unit atom"))

(@doc catalog-update!
  (@desc "Update all contents of all managed catalogs to the latest version of all modules")
  (@params (
    (@param "Name of the catalog to update or \"all\" to update all")))
  (@return "Unit atom"))

(@doc catalog-clear!
  (@desc "Clears the contents of all managed catalogs")
  (@params (
    (@param "Name of the catalog to clear or \"all\" to clear all")))
  (@return "Unit atom"))
