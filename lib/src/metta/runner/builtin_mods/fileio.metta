(@doc file-read
  (@desc "Function takes path to the file in form of string and reads file as string")
  (@params (
    (@param "Filepath (string atom)")))
  (@return "Content of file (string atom), error if no file exists"))

(@doc file-write
  (@desc "Function takes path to the file (string atom) and content to be written (string atom), creates file and puts content into it")
  (@params (
    (@param "Filepath (string atom)")
    (@param "Content (string atom)")))
  (@return "Unit atom if write successfull, error otherwise (if there is no such folder)"))