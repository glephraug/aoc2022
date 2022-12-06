

fun makeStacks 0 = nil
  | makeStacks i = nil :: (makeStacks (i-1))


fun pushStack 0 v (h :: t) : char list list = (v :: h) :: t
  | pushStack i v (h :: t) = h :: (pushStack (i-1) v t)
  | pushStack _ _ _ = nil


fun popStack 0 ((h::t) :: s) : char * (char list list) = (h, (t :: s))
  | popStack i (h :: t) = 
  	   let val (v,s) = popStack (i-1) t in
          (v,h::s)
       end
  | popStack _ s = (#"0",s)


fun moveStack from to 0 s = s
  | moveStack from to count s =
      let 
      	 val (v, s') = popStack from s
         val s'' = pushStack to v s' 
      in
         moveStack from to (count-1) s''
      end

(*
[V]         [T]         [J]        
[Q]         [M] [P]     [Q]     [J]
[W] [B]     [N] [Q]     [C]     [T]
[M] [C]     [F] [N]     [G] [W] [G]
[B] [W] [J] [H] [L]     [R] [B] [C]
[N] [R] [R] [W] [W] [W] [D] [N] [F]
[Z] [Z] [Q] [S] [F] [P] [B] [Q] [L]
[C] [H] [F] [Z] [G] [L] [V] [Z] [H]
 1   2   3   4   5   6   7   8   9 
*)


val initialStacks =
   let val strings = ["VQWMBNZC", "BCWRZH", "JRQF", "TMNFHWSZ", "PQNLWFG", "WPL", "JQCGRDBV", "WBNQZ", "JTGCFLH"] in
      map String.explode strings
   end


fun ignore 0 l = l
  | ignore i nil = nil
  | ignore i (h::t) = ignore (i-1) t


fun ignoreChars 0 getc stream = SOME stream
  | ignoreChars i getc stream =
	   case getc stream of
	      NONE => NONE
	    | SOME (c,stream') => ignoreChars (i-1) getc stream'


fun scanLabeledInt i getc stream =
	case ignoreChars i getc stream of
	   NONE => NONE
	 | SOME stream' =>
	Int.scan StringCvt.DEC getc stream'


fun parseInstruction s : int*int*int =
   let
      fun scanLine getc stream =
         case scanLabeledInt 5 getc stream of
            NONE => NONE
          | SOME (move, stream') =>
         case scanLabeledInt 6 getc stream' of
            NONE => NONE
          | SOME (from, stream'') =>
         case scanLabeledInt 4 getc stream'' of
            NONE => NONE
          | SOME (to, stream''') =>
         SOME ((move,from-1,to-1),stream''')
   in
      case StringCvt.scanString scanLine s of
         NONE => (0,0,0)
       | SOME instruction => instruction
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


fun readInput (filename : string) : (int*int*int) list =
   let 
      val instructions = ignore 10 (readLines filename)
   in
      map parseInstruction instructions
   end


fun reduce_left (unit, opn, nil) =
    unit
  | reduce_left (unit, opn, h::t) =
  	reduce_left (opn (h,unit), opn, t)


fun part1 () =
   let 
      val input = readInput "input5.txt" 
      fun execute ((move,from,to),stacks) =
         moveStack from to move stacks
   in
      reduce_left (initialStacks, execute, input)
   end


fun pushStack9001 0 v (h :: t) : char list list = (v @ h) :: t
  | pushStack9001 i v (h :: t) = h :: (pushStack9001 (i-1) v t)
  | pushStack9001 _ _ _ = nil


fun popStack9001 0 0 s : char list * char list list = (nil, s)
  | popStack9001 0 c s = 
       let 
          val (v,s') = popStack 0 s
          val (vs,s'') = popStack9001 0 (c-1) s'
       in
          (v::vs,s'')
       end
  | popStack9001 i c (h :: t) = 
  	   let val (vs,s) = popStack9001 (i-1) c t in
          (vs,h::s)
       end
  | popStack9001 _ _ s = (nil,s)


fun moveStack9001 from to count s : char list list =
      let 
      	 val (vs, s') = popStack9001 from count s
      in
         pushStack9001 to vs s'
      end


fun part2 () =
   let 
      val input = readInput "input5.txt" 
      fun execute ((move,from,to),stacks) =
         moveStack9001 from to move stacks
   in
      reduce_left (initialStacks, execute, input)
   end
