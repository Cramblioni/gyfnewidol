
# from the rfc1459 page 7, in pesudo-bnf
# <message>  ::= [":" <prefix> <SPACE>] <command> <params> <crlf>
# <prefix>   ::= <servername>
#              | <nick> [ "!" <user> ] [ "@" <host> ]
# <command>  ::= <letter> { <letter> }
#              | <number> <number> <number>
# <SPACE>    ::= ' ' { ' ' }
# <params>   ::= <SPACE> [":" <trailing> | <middle> <params>]
# <CRLF>     ::= "\r\n"
# // CRLF handled by `readMessage`
#
# <middle>   ::= <Any *non-empty* sequence of octets, excluding
#                    SPACE, NULL, CR, and LF. first cannot be ":">
# <trailing> ::= <Any, or *EMPTY*, sequence of octets excluding
#                    NULL, CR, and LF>
#
# <user>     ::= <nonwhite> { <nonwhite> }
# <letter>   ::= 'a' .. 'z' | 'A' .. 'Z'
# <number>   ::= '0' .. '9'
# <special>  ::= '-' | '[' | ']' | '\\' | '`' | '^' | '{' | '}'
#
# <target>   ::= <to> ["," <target>]
# <to>       ::= <channel>
#              | <user> "@" <servername>
#              | <nick>
#              | <mask>
# <channel>  ::= ("#" | "&") <chstring>
# // servername is an alias for host
# <host>     ::= <&RFC 952&>
# <nick>     ::= <letter> { <letter> | <number> | <special> }
# <mask>     ::= ("#" | "&") <chstring>
# <chstring> ::= {&{'\0' .. '\255'} - {' ', '\a', '\0', '\r', '\n', ','}&}

# Attempting to write this in a stream friendly way

import std/[streams, options]

const
  MIDDLE*   = {char.low .. char.high} - {' ', '\0', '\r', '\n'}
  TRAILING* = {char.low .. char.high} - {'\0', '\r', '\n'}
  NONWHITE* = {char.low .. char.high} - {' '}
  HOSTNAME* = {'a' .. 'z', 'A' .. 'Z', '0' .. '9', '-', '.'}
  LETTER*   = {'a' .. 'z', 'A' .. 'Z'}
  NUMBER*   = {'0' .. '9'}

type
  ResultKind* = enum
    Error, Success
  Result*[E, S] = object
    case kind: ResultKind
    of Error:
      error: E
    of Success:
      value: S

func success*[E, S](value: S): Result[E, S] =
  Result[E, S](kind: Success, value: value)
func error*[E, S](error: E): Result[E, S] =
  Result[E, S](kind: Error, error: error)

template TRY*[E, T](res: Result[E, T]): T =
  let tmp = res
  case tmp.kind
  of Success:
    tmp.value
  of Error:
    return error[E, typeof(result.value)](tmp.error)

template OTHERWISE*[E, S](first, second: Result[E, S]): Result[E, S] =
  let fst = first
  case fst.kind
  of Success: fst
  of Error: second

proc readMessage*(stream: Stream): string =
  ## like `readLine` but only does up to "\r\n"
  ## without returning it.
  let start = stream.getPosition()
  while not stream.atEnd and not (stream.readChar() == '\r' and stream.readChar() == '\n'):
    discard
  discard stream.readChar()
  let mlen = stream.getPosition() - start - 2
  stream.setPosition(start)
  stream.readStr(mlen)

type
  Message* = object
    prefix*: Option[string]
    command*: string
    params*: seq[string]
    hastrail*: bool

proc parseMessage*(stream: Stream): Result[string, Message] =
  let prefix = (if stream.peekChar() == ':':
      var prefix = newStringOfCap(24)
      discard stream.readChar()
      while stream.peekChar != ' ':
        prefix.add stream.readChar()
        if stream.atEnd():
          return error[string, Message]("Malformed Message Prefix")
      while stream.peekChar == ' ': discard stream.readChar()
      some(prefix)
    else:
      none(string)
  )
  let command = (if stream.atEnd():
      return error[string, Message]("Malformed Message Command [Premature termination]")
    elif stream.peekChar() in LETTER:
      # name
      var buff = ""
      while stream.peekChar() in LETTER:
        buff.add stream.readChar()
      buff
    elif stream.peekChar() in NUMBER:
      # code
      var code = newString(3)
      for i in 0 ..< 3:
        if stream.peekChar() notin NUMBER:
          return error[string, Message]("Malformed Message Command [Short code]")
        code[i] = stream.readChar
      code
    elif stream.peekChar() in LETTER:
      var buff = stream.readStr(1)
      while stream.peekChar() in LETTER:
        buff.add stream.readChar()
      buff
    else:
      return error[string, Message]("Malformed Message Command [Invalid initial character]")
  )

  var trail: bool = false
  template parseParam: string =
    # handling the fucking space
    if stream.readChar() != ' ':
      return error[string, Message]("malformed param")
    while stream.peekChar == ' ':
      discard stream.readChar()
    var buff = newStringofCap(4)
    if stream.peekChar() == ':': # trailing
      trail = true
      discard stream.readChar()
      while stream.peekChar() in TRAILING:
        buff.add stream.readChar()
    else: # middle
      while stream.peekChar() in MIDDLE:
        buff.add stream.readchar()
      if buff.len == 0:
        return error[string, Message]("Null Param")
    buff

  var params = newSeq[string]()
  params.add parseParam()
  while stream.peekStr(2) != "\r\n":
    params.add parseParam()
  discard stream.readStr(2)
  success[string, Message](Message( prefix: prefix, command: command,
                                    params: params, hastrail: trail))

func toString*(msg: Message): string =
  if msg.prefix.isSome():
    result &= ":" & msg.prefix.get() & " "
  result &= msg.command
  let ei = msg.params.len - 1
  for i, param in msg.params:
    if msg.hastrail and i == ei:
      result &= " :"
    else:
      result &= " "
    result &= param

proc scanHost(stream: Stream): string =
  result = newStringOfCap(63)
  while stream.peekChar in HOSTNAME:
    result.add stream.readChar()

when isMainModule:
  let inp = newStringStream(
    "PASS somepass\r\nNICK cramble\r\nUSER cramble cramble dave :trystan `cramble` vincent\r\n" &
    ":dave!john@niel PRIVMSG #channel :what the fuck\r\n")
  while not inp.atEnd():
    let fst = inp.parseMessage
    if fst.kind == Error:
      echo "ERROR:", fst.error
      break
    echo fst.value
    echo fst.value.toString

