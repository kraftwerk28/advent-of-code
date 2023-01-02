datatype packet = pint of int
                | plist of packet list

fun parsePacket (s: char list): packet option =
  case parseList s of
       (SOME l, rest) => l
       (NONE, rest) => parseNumber rest

(* fun parseList (#"["::xs: char list): packet option * char list =
  case parseList xs of (SOME first, rest) => (NONE, rest) | f => f
  | parseList chrs =
    case Int.fromString (String.implode chrs) of
         SOME n => (SOME n, [])
       | NONE => (NONE, chrs) *)
fun main _ = print ""
