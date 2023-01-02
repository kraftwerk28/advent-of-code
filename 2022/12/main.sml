(*
fun main(name, args) = print("Program name: " ^ name)
  print("kek")
  *)

fun takeWhile _ [] = []
  | takeWhile f (x::xs) =
  if f x
  then x :: takeWhile f xs
  else takeWhile f xs

fun getlines (): string list =
  case TextIO.inputLine TextIO.stdIn of
       NONE => []
     | SOME line => line :: getlines ()

(* fun indices (l: 'a list): ('a * int) list =
  let
    fun aux [] _ = []
      | aux (x::xs) i = (x, i) :: aux xs (i + 1)
  in aux l 0
  end *)

type pt = int * int
type height_map = (pt * char) list
type grid = { data: height_map, width: int, height: int }

fun findPos ((pos, i)::xs: height_map, c: char): pt =
  if i = c then pos else findPos (xs, c)
  | findPos ([], _) = raise Fail "Map doesn't contain the point"


fun neighbours ({ width, height, ... }: grid, (row, col): pt): pt list =
  let
    val a = if row = 0 then NONE else SOME (row - 1, col)
    val b = if col = 0 then NONE else SOME (row, col - 1)
    val c = if row + 1 >= height then NONE else SOME (row + 1, col)
    val d = if row + 1 >= height then NONE else SOME (row + 1, col)
  in List.mapPartial (fn x => x) [a, b, c, d]
  end

fun fromLines (lines: string list): grid =
  let
    fun procLine (line, rowi) =
      let
        val cols = indices (explode line)
      in List.map (fn (c, coli) => ((rowi, coli), c)) cols
      end
    val linesi = List.map procLine (indices lines)
  in {
    data = List.concat linesi,
    width = List.length (hd linesi),
    height = List.length linesi
  }
  end


(* type Grid = (int * int * char) list *)

(* fun findPath () *)

(* fun getxy (pos: int * int, map): string = "" *)

  (*
fun listToStr l =
let
  List.foldl (fn acc cur => acc 
fun aux [] = "]"
  | aux x::xs = "
fun listToStr [] = "]"
  | listToStr x::xs  *)

fun main args =
let
  val rawlines = getlines ()
  val grid = fromLines rawlines
  (* val a::_ = takeWhile (fn x => x < 30) [1, 2, 100, 4] *)
  (* val b = List.zip 1 *)
  (* val lines = getlines () *)
  (* val ind = indices [10, 5, 15, 11] *)
in
  print ""
  (* print (List.foldl (fn (acc, cur) => acc ^ "," ^ cur) "" lines);
  print "\n" *)
end
