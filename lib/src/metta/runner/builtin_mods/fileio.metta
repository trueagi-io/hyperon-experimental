(@doc file-open!
  (@desc "Function takes path to the file and open options (r, w, c, a, t) both in form of string, creates filehandle and
    returns it")
  (@params (
    (@param "Filepath (string atom)")
    (@param "Open options (string atom), r - read, w - write, c - create if file doesn't exist, a - append to file,
        t - truncate file")))
  (@return "Filehandle or error if combination of path and open options is wrong (e.g. file doesn't exist and no 'c'
    in options; or 'rc' option provided, since 'c' demands for 'w')"))

(@doc file-read-to-string!
  (@desc "Function takes filehandle provided by file-open! reads its content from current cursor place till the end of
    file and returns content in form of string.")
  (@params (
    (@param "Filehandle")))
  (@return "File's content"))

(@doc file-write!
  (@desc "Function takes filehandle provided by file-open!, content to be written (string atom) and puts content into
    file associated with filehandle")
  (@params (
    (@param "Filehandle")
    (@param "Content (string atom)")))
  (@return "Unit atom"))

(@doc file-seek!
  (@desc "Function takes filehandle provided by file-open! and desired cursor position (number) and sets cursor to
    provided position")
  (@params (
    (@param "Filehandle")
    (@param "Desired cursor position (number)")))
  (@return "Unit atom"))

(@doc file-read-exact!
  (@desc "Function takes filehandle provided by file-open! and desired number of bytes to read (number), reads content
  of file from current cursor position (number of read bytes <= input number of bytes to read) and returns it in form of
  string")
  (@params (
    (@param "Filehandle")
    (@param "Number of bytes to read")))
  (@return "File's content"))

(@doc file-get-size!
  (@desc "Function takes filehandle provided by file-open! and returns size of file")
  (@params (
    (@param "Filehandle")))
  (@return "Size of file"))