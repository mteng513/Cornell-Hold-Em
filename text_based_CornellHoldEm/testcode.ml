	let remove_elt e l =
    	let rec go l acc = match l with
	    	| [] -> List.rev acc
	    	| x::xs when e = x -> go xs acc
	    	| x::xs -> go xs (x::acc)
    	in go l [] ;;

	let remove_duplicates l =
	    let rec go l acc = match l with
		    | [] -> List.rev acc
		    | x :: xs -> go (remove_elt x xs) (x::acc)
	    in go l [] ;;




	"gengine_flush1" >:: (fun _ -> assert_equal
    	0
    	(Game_Engine.flush [(Ten, Gries); (Three, Gries); (Four, Gries); (Five, Clarkson); (Six, Gries); (Jack, Dijkstra)]));
	"gengine_flush2" >:: (fun _ -> assert_equal
    	(16120643)
    	(Game_Engine.flush [(Ten, Gries); (Three, Gries); (Four, Gries); (Five, Clarkson); (Six, Gries); (Jack, Gries)]));


	"gengine_straight1" >:: (fun _ -> assert_equal
	15000006
	(Game_Engine.straight [(Two, Gries); (Three, Gries); (Four, Gries); (Five, Clarkson); (Six, Gries); (Jack, Dijkstra)]));

	"gengine_straight_flush1"  >:: (fun _ -> assert_equal
	20000006
	(Game_Engine.straight [(Two, Gries); (Three, Gries); (Four, Gries); (Five, Gries); (Six, Gries); (Jack, Dijkstra)]));

	"gengine_straight_flush1"  >:: (fun _ -> assert_equal
	1000000000
	(Game_Engine.straight [(Ace, Gries); (King, Gries); (Queen, Gries); (Jack, Gries); (Ten, Gries); (Jack, Dijkstra)]));

completed scoring formulas and changes in functions for all hand types, reformatted all scoring functions. Three of a kind and Two of a Kind not currently working, as they only return 3 and 2 cards respectively instead of 5



(* 
module E_Tester = Game_Engine

module Engine_test = struct

	let tests = "test suite" >::: [
		"gengine_flush1" >:: (fun _ -> assert_equal
	    	0
	    	(E_Tester.flush [(Ten, Gries); (Three, Gries); (Four, Gries); (Five, Clarkson); (Six, Gries); (Jack, Dijkstra)]));
		"gengine_flush2" >:: (fun _ -> assert_equal
	    	(16120643)
	    	(E_Tester.flush [(Ten, Gries); (Three, Gries); (Four, Gries); (Five, Clarkson); (Six, Gries); (Jack, Gries)]));


		"gengine_straight1" >:: (fun _ -> assert_equal
		15000006
		(E_Tester.straight [(Two, Gries); (Three, Gries); (Four, Gries); (Five, Clarkson); (Six, Gries); (Jack, Dijkstra)]));

		"gengine_straight_flush1"  >:: (fun _ -> assert_equal
		20000006
		(E_Tester.straight [(Two, Gries); (Three, Gries); (Four, Gries); (Five, Gries); (Six, Gries); (Jack, Dijkstra)]));

		"gengine_straight_flush1"  >:: (fun _ -> assert_equal
		1000000000
		(E_Tester.straight [(Ace, Gries); (King, Gries); (Queen, Gries); (Jack, Gries); (Ten, Gries); (Jack, Dijkstra)]));
	
	]

end

module E_Test = Engine_test

let tests = [] @ E_test.tests *)



	