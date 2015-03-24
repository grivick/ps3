type 'a stream = Stream of 'a * (unit -> 'a stream)

(* NOTE: You may add the rec keyword as you wish for this file. *)


let rec take n stream = 
 if n>0 then
  match stream with
  Stream (x, y) -> x::(take (n-1) (y()))
 else [];;

let repeat   x = 
	let rec str= Stream (x, fun ()->str) in
	str;;

let rec map    f s = 
 match s with
 Stream (x, y)-> Stream ((f x), (fun ()-> map f (y())))
 ;;

let rec diag     s = 
 match s with
 Stream (x, y)-> match x with
  Stream (x, z) -> Stream (x,fun ()-> diag (y())))
  ;;
let rec suffixes s =
 match s with
 Stream (x, y)->Stream (s, (fun ()->(suffixes (y()))))
 ;;

let rec interleave s s' = 
 match s with
 Stream (x,y)-> Stream (x, fun ()->interleave s' (y()))
 ;;

let fibs         () =
	let rec fibbi nO nT=
	  Stream (nO, fun ()-> fibbi nT (nO+nT)) in
	  fibbi 1 1
	;;
	
let pi           () = 
 let part n cur= 
   4.*.(-1.**(float_of_int n))/.(float_of_int ((2*n)+1))+.cur in
   let rec parti n cur = Stream((part n cur), fun ()->parti (n+1) (part n cur)) in parti 0 0;;

 let revAdd lst v1 v2=
  List.rev(v2::(v1::(List.rev lst)));;
 
let look_and_say () = 
   let rec speaking inp num acc cur=
    match inp with
    h::t-> if h=cur then speaking t (num+1) acc cur else
    if num<>0 then speaking t 1 (revAdd acc num cur) h else
      speaking t 1 acc h
    |[]->if num <> 0 then Stream ((revAdd acc num cur), 
      fun () -> speaking (revAdd acc num cur) 0 [] 0)
      else Stream ([1], fun ()->speaking [1] 0 [] 0)
    in speaking [] 0 [] 0;;


