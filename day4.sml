   

fun parseBounds s : int*int*int*int =
   let
      fun scanLine getc stream =
         case Int.scan StringCvt.DEC getc stream of
            NONE => NONE
          | SOME (a, stream') =>
         case getc stream' of
            NONE => NONE
          | SOME (_, stream'') =>
         case Int.scan StringCvt.DEC getc stream'' of
            NONE => NONE
          | SOME (b, stream''') =>
         case getc stream''' of
            NONE => NONE
          | SOME (_, stream'''') =>
         case Int.scan StringCvt.DEC getc stream'''' of
            NONE => NONE
          | SOME (c, stream''''') =>
         case getc stream''''' of
            NONE => NONE
          | SOME (_, stream'''''') =>
         case Int.scan StringCvt.DEC getc stream'''''' of
            NONE => NONE
          | SOME (d, stream''''''') => SOME ((a,b,c,d),stream''''''')
   in
      case StringCvt.scanString scanLine s of
         NONE => (0,0,1,1)
       | SOME bounds => bounds
   end


fun readLines (filename : string) : string list =
   let 
      fun doline stream : string list =
         case TextIO.inputLine stream of
            SOME line => line :: doline stream
          | NONE => nil
   in
      doline (TextIO.openIn filename)
   end


fun readInput (filename : string) : (int*int*int*int) list =
   map parseBounds (readLines filename)


fun reduce (unit, opn, nil) =
    unit
  | reduce (unit, opn, h::t) =
    opn (h, reduce (unit, opn, t))


fun contained (a,b,c,d) : int = 
   if a>=c andalso b<=d then 1 else
   if a<=c andalso b>=d then 1 else 0


fun part1 () : int =
   let 
      val input = readInput "input4.txt" 
   in
      reduce (0, op +, (map contained input))
   end


fun overlap (a,b,c,d) : int = 
   if a<=d andalso c<=b then 1 else 0


fun part2 () : int =
   let 
      val input = readInput "input4.txt" 
   in
      reduce (0, op +, (map overlap input))
   end