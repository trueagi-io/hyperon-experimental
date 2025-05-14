(@doc file-open!
  (@desc "Function takes path to the file and open options (r, w, c, a) both in form of string, creates filehandle and returns it")
  (@params (
    (@param "Filepath (string atom)")
    (@param "Open options (string atom), r - read, w - write, c - create if file doesn't exist, a - append to file")))
  (@return "Filehandle or error if combination of path and open options is wrong (e.g. file doesn't exist and no 'c' in options; or 'rc' option provided, since 'c' demands for 'w')"))

(@doc file-read!
  (@desc "Function takes filehandle provided by file-open! and returns file content in form of string")
  (@params (
    (@param "Filehandle")))
  (@return "File's content"))

(@doc file-write!
  (@desc "Function takes filehandle provided by file-open!, content to be written (string atom) and puts content into file associated with filehandle")
  (@params (
    (@param "Filehandle")
    (@param "Content (string atom)")))
  (@return "Unit atom"))