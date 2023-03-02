import std/[asyncdispatch, asyncfile]

proc main: Future[void] {. async .} =
  var file = openAsync("/dev/stdin", fmRead)
  echo await file.readLine()


var main_fut = main()
asyncCheck main_fut
waitFor main_fut
