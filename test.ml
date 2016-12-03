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

		"gengine_flush2" >:: (fun _ -> assert_equal 0
	  	(GEngine.flush [(Ten, Gries); (Three, Gries); (Four, Clarkson); 
	   		(Five, Clarkson); (Six, George); (Jack, Dijkstra)]));
		
		"gengine_flush3" >:: (fun _ -> assert_equal (16120643)
	    (GEngine.flush [(Ten, Gries); (Three, Gries); (Four, Gries); 
	    	(Five, Clarkson); (Six, Gries); (Jack, Gries)]));

		"gengine_straight0" >:: (fun _ -> assert_equal 0
		(GEngine.straight [(Two, Gries); (Three, Gries); (Four, Gries); 
			(Five, Clarkson); (Seven, Gries); (Jack, Dijkstra)]));

		"gengine_straight1" >:: (fun _ -> assert_equal 15000006
		(GEngine.straight [(Two, Gries); (Three, Gries); (Four, Gries); 
			(Five, Clarkson); (Six, Gries); (Jack, Dijkstra)]));

		"gengine_straight_flush1"  >:: (fun _ -> assert_equal 20000006
			(GEngine.straight [(Two, Gries); (Three, Gries); (Four, Gries); 
				(Five, Gries); (Six, Gries); (Jack, Dijkstra)]));

		"gengine_straight_flush2"  >:: (fun _ -> assert_equal 1000000000
		(GEngine.straight [(Ace, Gries); (King, Gries); (Queen, Gries); 
			(Jack, Gries); (Ten, Gries); (Jack, Dijkstra)]));

		"gengine_straight_flush3"  >:: (fun _ -> assert_equal 0
		(GEngine.straight [(Ace, Gries); (King, Gries); (Queen, Gries); 
			(Jack, Gries); (Nine, Gries); (Jack, Dijkstra)]));

		"high_card 0" >:: (fun _ -> assert_equal 22 (GEngine.high_card
			[(Two, Gries); (Two, Clarkson)]));

		"high_card 1" >:: (fun _ -> assert_equal 32 (GEngine.high_card
			[(Two, Gries); (Three, Clarkson)]));

		"high_card 2" >:: (fun _ -> assert_equal 73 (GEngine.high_card
			[(Three, Gries); (Seven, Clarkson)]));

		"high_card 3" >:: (fun _ -> assert_equal 1413 (GEngine.high_card
			[(Three, Gries); (Four, Clarkson); (Six, Gries); (Seven, Clarkson);
			(Nine, Dijkstra)]));

		"high_card 4" >:: (fun _ -> assert_equal 1414 (GEngine.high_card
			[(Four, Gries); (Four, Clarkson); (Six, Gries); (Seven, Clarkson);
			(Nine, Dijkstra)]));

		"high_card 5" >:: (fun _ -> assert_equal 1514 (GEngine.high_card
			[(Four, Gries); (Four, Clarkson); (Six, Gries); (Seven, Clarkson);
			(Ten, Dijkstra)]));

		"high_card 6" >:: (fun _ -> assert_equal 1614 (GEngine.high_card
			[(Four, Gries); (Four, Clarkson); (Six, Gries); (Seven, Clarkson);
			(Jack, Dijkstra)]));

		"high_card 7" >:: (fun _ -> assert_equal 1914 (GEngine.high_card
			[(Four, Gries); (Four, Clarkson); (Six, Gries); (Seven, Clarkson);
			(Ace, Dijkstra)]));

		"Full House 0" >:: (fun _ -> assert_equal 0 (GEngine.full_house
			[(Four, Gries); (Four, Clarkson); (Six, Gries); (Seven, Clarkson);
			(Ace, Dijkstra)]));

		"Full House 1" >:: (fun _ -> assert_equal 0 (GEngine.full_house
			[(Four, Gries); (Four, Clarkson); (Six, Gries); (Six, Clarkson);
			(Ace, Dijkstra)]));

		"Full House 2" >:: (fun _ -> assert_equal 0 (GEngine.full_house
			[(Four, Gries); (Four, Clarkson); (Four, Gries); (Seven, Clarkson);
			(Ace, Dijkstra)]));

		"Full House 3" >:: (fun _ -> assert_equal 0 (GEngine.full_house
			[(Four, Gries); (Four, Clarkson); (Four, Gries); (Four, Clarkson);
			(Ace, Dijkstra)]));

		"Full House 4" >:: (fun _ -> assert_equal 0 (GEngine.full_house
			[(Four, Gries); (Four, Clarkson); (Four, Gries); (Ace, Clarkson);
			(Ace, Dijkstra)]));



		]

end

module Test_Engine = Test_Game_Engine


let tests = Test_Engine.tests 

let _ = run_test_tt_main tests