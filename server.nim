
import ./message
import std/[asyncnet, asyncdispatch, streams]
import std/[options, posix]

# I don't know how to handle streams asynchronously
# IRC has a max message len of 512, so we'll use that

const
  MAXMESSAGESIZE* = 512
type
  Client = object
    server: bool
    host: string
    nick: string
    sock: AsyncSocket

var RUNNING {. global .} = true
setControlCHook(proc() {. noconv .}=
  RUNNING = true
)

var clients = newSeqOfCap[Client](64)
func `$`(sp: tuple[ip:string, p:Port]): string = "[" & sp[0] & ":" & $sp[1].int & "]"

proc handleConnection(sock: AsyncSocket, server: string): Future[void] {. async .} =
  let ident = $sock.getPeerAddr()
  var client = Client()

  while RUNNING:
    let data = await sock.recvLine
    if data.len == 0:
      # HUP
      break
    #echo cast[seq[char]](data)
    #echo data.repr
    var inp = data.newStringStream()
    let res = inp.parseMessage()
    case res.kind
    of Error:
      echo "ERROR:", ident, ":", res.error
      continue
    of Success:
      echo "COMMAND:", ident,":", res.value.command
    let msg = res.value
    case msg.command
    of "CAP":
      if msg.params[0] == "LS":
        await sock.send(":" & server & " 410 * " & msg.params[0] & " : invalid CAP subcommand")
    else:
      let emsg = ":" & server & " 400 : The server cannot recognise the command " &
                 msg.command & "\r\n"
      await sock.send(emsg)
  # after the client dies
  echo "DISCONNECT:" , ident
  sock.close()

proc handleConnections(host: string): Future[void] {. async .} =
  var server = newAsyncSocket()
  server.setSockOpt(OptReuseAddr, true)
  server.bindAddr(Port(6667), host)
  server.listen()
  defer:
    server.close()
  echo server.getLocalAddr()
  while RUNNING:
    let conn = await server.accept
    echo "CONNECTED:", $conn.getPeerAddr()
    # handle one command from the client
    asyncCheck handleConnection(conn, host)
  server.close()

proc main(host: string): Future[void] {. async .} =
  let fut = handleConnections(host) 
  asyncCheck fut
  waitFor fut
var main_fut = main("localhost")
asyncCheck main_fut
waitFor main_fut
