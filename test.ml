(* Main test file for the Texas Hold'em Suite *)
open OUnit2
open Gengine
open Player
open Opponent
open GUpoker

module Test_Game_Engine = struct

	module GEngine = Game_Engine


	let tests = "test suite" >::: [
	
		"gengine_flush1" >:: (fun _ -> assert_equal 0
	  	(GEngine.flush [(Ten, Gries); (Three, Gries); (Four, Gries); 
	   		(Five, Clarkson); (Six, Gries); (Jack, Dijkstra)]));
		
		"gengine_flush2" >:: (fun _ -> assert_equal (16120643)
	    (GEngine.flush [(Ten, Gries); (Three, Gries); (Four, Gries); 
	    	(Five, Clarkson); (Six, Gries); (Jack, Gries)]));

		"gengine_straight1" >:: (fun _ -> assert_equal 15000006
		(GEngine.straight [(Two, Gries); (Three, Gries); (Four, Gries); 
			(Five, Clarkson); (Six, Gries); (Jack, Dijkstra)]));

		"gengine_straight_flush1"  >:: (fun _ -> assert_equal 20000006
			(GEngine.straight [(Two, Gries); (Three, Gries); (Four, Gries); 
				(Five, Gries); (Six, Gries); (Jack, Dijkstra)]));

		"gengine_straight_flush2"  >:: (fun _ -> assert_equal 1000000000
		(GEngine.straight [(Ace, Gries); (King, Gries); (Queen, Gries); 
			(Jack, Gries); (Ten, Gries); (Jack, Dijkstra)]));]

end

module Test_Engine = Test_Game_Engine


let tests = Test_Engine.tests 

let _ = run_test_tt_main tests