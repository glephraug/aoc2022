
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

fun contained s : int = 
   let 
      val (a,b,c,d) = parseBounds s
   in
      if a>=c andalso b<=d then 1 else
      if a<=c andalso b>=d then 1 else 0
   end

fun doline stream sum : int =
   let 
      val line = TextIO.inputLine stream 
   in
      case line of
         SOME text => doline stream ((contained text) + sum)
       | NONE => sum
   end

fun part1 () : unit =
   let 
      val ins = TextIO.openIn "input4.txt" 
   in
      TextIO.print (Int.toString (doline ins 0))
   end



