(* Main test file for the Texas Hold'em Suite *)
open OUnit2
open Gengine
open Player
open Opponent
open GUpoker


module Test_Game_Engine = struct

	module GEngine = Game_Engine

	let test_state = {GEngine.current_st = START; 
		cards_in_play = [(Ace, Gries); (Two, Gries);
			(Three, Clarkson); (Queen, Gries); (King, Gries)];
		pot = 9438294;
		current_bet = 0; n_players = 0; c_player = 0; 
		hands = [[(Ten, Gries);(Jack, Gries)];
			[(King, Clarkson);(Ace, Clarkson)];[];[];[];[];[];[]];
		bets = [|0;0;0;0;0;0;0;0|]; 
		players_in = [|true;true;true;false;false;false;false;false|];
		scores = [|0;0;0;0;0;0;0;0|]}

	let test_state_after = {GEngine.current_st = START; 
		cards_in_play = [(Ace, Gries); (Two, Gries);
			(Three, Clarkson); (Queen, Gries); (King, Gries)]; 
		pot = 9438294;
	 	current_bet = 0; n_players = 0; c_player = 0; 
	 	hands = [[(Ten, Gries); (Jack, Gries)]; 
	 		[(King, Clarkson); (Ace, Clarkson)];[]; []; []; []; []; []];
 		bets = [|0; 0; 0; 0; 0; 0; 0; 0|];
 		players_in = [|true; true; true; false; false; false; false; false|];
 		scores = [|1000000000; 1401312; 2322; 0; 0; 0; 0; 0|]};;


	let tests = "test suite" >::: [

		"Royal Flush 0" >:: (fun _ -> assert_equal 1000000000 (Game_Engine.straight
			[(Ace, Gries); (Queen, Gries); (Jack, Gries); (Ace, Clarkson); 
			 (King, Gries); (Ten, Gries); (Two, Clarkson)]));

		"Royal Flush 1" >:: (fun _ -> assert_equal 16154320 (Game_Engine.flush
			[(Ace, Gries); (Queen, Gries); (Jack, Gries); (Ace, Clarkson); 
			 (King, Gries); (Ten, Gries); (Two, Clarkson)]));

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

		"Full House 4" >:: (fun _ -> assert_equal 18004014 (GEngine.full_house
			[(Four, Gries); (Four, Clarkson); (Four, Gries); (Ace, Clarkson);
			(Ace, Dijkstra)]));

		"Two Pair 0" >:: (fun _ -> assert_equal 0 (Game_Engine.two_pair 
			[(Ace,Gries); (Two, Gries); (Three, Clarkson); (Queen, Gries);
			 (King, Gries);(Jack, Clarkson);(Ace, Clarkson)]));

		"Two Pair 1" >:: (fun _ -> assert_equal 1401312 (Game_Engine.two_pair 
			[(Ace,Gries); (Two, Gries); (Three, Clarkson); (Queen, Gries);
			 (King, Gries);(King, Clarkson);(Ace, Clarkson)]));

		"Two Pair 2" >:: (fun _ -> assert_equal 0 (Game_Engine.two_pair 
			[(Ace,Gries); (Two, Gries); (Three, Clarkson); (Queen, Gries);
			 (King, Gries);(Ace, Clarkson);(Ace, Clarkson)]));

		"Two Pair 3" >:: (fun _ -> assert_equal 1401213 (Game_Engine.two_pair 
			[(Ace,Gries); (Two, Gries); (Three, Clarkson); (Queen, Gries);
 			(King, Gries);(Queen, Clarkson);(Ace, Clarkson)]));

		"Two Pair 4" >:: (fun _ -> assert_equal 0 (Game_Engine.two_pair 
			[(Ace, Gries); (Two, Gries); (Three, Clarkson); (Ace, George);
			(Ace, Dijkstra); (Ace, Clarkson)]));

		"Pair 0" >:: (fun _ -> assert_equal 0 (Game_Engine.pair
			[(Ace, Gries); (Two, Gries); (Three, Clarkson); (King, George);
			(Seven, Dijkstra); (Ten, Clarkson)]));

		"Pair 1" >:: (fun _ -> assert_equal 22442 (Game_Engine.pair
			[(Ace, Gries); (Two, Gries); (Three, Clarkson); (Queen, Gries); (King, Gries);
	        (King, Clarkson); (Ace, Clarkson)]));

		"Pair 2" >:: (fun _ -> assert_equal 21023 (Game_Engine.pair
			[(Ace, Gries); (Two, Gries); (Three, Clarkson); (Queen, Gries); (King, Gries);
	        (King, Clarkson); (Two, Clarkson)]));

		"Pair 3" >:: (fun _ -> assert_equal 4426 (Game_Engine.pair
			[(Two, George); (Two, Gries); (Three, Clarkson); (Queen, Gries);
			(King, Gries); (Six, Dijkstra); (Two, Clarkson)]));

		"Pair 4" >:: (fun _ -> assert_equal 4427 (Game_Engine.pair
			[(Two, George); (Two, Gries); (Three, Clarkson); (Queen, Gries);
 			 (King, Gries); (Seven, Dijkstra); (Two, Clarkson)]));

		"Pair 5" >:: (fun _ -> assert_equal 20732 (Game_Engine.pair
			[(Two, George); (Two, Gries); (Three, Clarkson); (Queen, Gries);
			 (King, Gries); (King, Clarkson); (Two, Clarkson)]));

		"Three Kind 0" >:: (fun _ -> assert_equal 2001313 (Game_Engine.three_kind
			[(Two, George); (Two, Gries); (Three, Clarkson); (Queen, Gries);
			 (King, Gries); (King, Clarkson); (Two, Clarkson)]));

		"Three Kind 1" >:: (fun _ -> assert_equal 2001313 (Game_Engine.three_kind
			[(Two, George); (Two, Gries); (Two, Clarkson); (Queen, Gries);
			 (King, Gries); (King, Clarkson); (Two, Dijkstra)]));

		"Three Kind 2" >:: (fun _ -> assert_equal 3001302 (Game_Engine.three_kind
			[(Two, George); (Two, Gries); (Three, Clarkson); (Three, Gries);
			 (King, Gries); (Three, Dijkstra); (Two, Clarkson)]));

		"Three Kind 3" >:: (fun _ -> assert_equal 0 (Game_Engine.three_kind
			[(Four, George); (Five, Gries); (Three, Clarkson); (Seven, Gries);
			 (King, Gries); (Three, Dijkstra); (Two, Clarkson)]));

		"Three Kind 4" >:: (fun _ -> assert_equal 4001307 (Game_Engine.three_kind
			[(Four, George); (Four, Gries); (Four, Clarkson); (Seven, Gries);
			 (King, Gries); (Four, Dijkstra); (Two, Clarkson)]));

		"Three Kind 5" >:: (fun _ -> assert_equal 14001307 (Game_Engine.three_kind
			[(Ace, George); (Ace, Gries); (Four, Clarkson); (Seven, Gries);
			 (King, Gries); (Ace, Dijkstra); (Two, Clarkson)]));

		"Four Kind 0" >:: (fun _ -> assert_equal 19004013 (Game_Engine.four_kind
			[(Four, George); (Four, Gries); (Four, Clarkson); (Seven, Gries);
			 (King, Gries); (Four, Dijkstra); (Two, Clarkson)]));

		"Four Kind 1" >:: (fun _ -> assert_equal 0 (Game_Engine.four_kind
			[(Four, George); (Three, Gries); (Four, Clarkson); (Seven, Gries);
			 (King, Gries); (Four, Dijkstra); (Two, Clarkson)]));

		"Four Kind 2" >:: (fun _ -> assert_equal 19014013 (Game_Engine.four_kind
			[(Ace, George); (Ace, Gries); (Four, Clarkson); (Seven, Gries);
			 (King, Gries); (Ace, Dijkstra); (Ace, Clarkson)]));

		(* Order of cards shouldn't change score *)
		"Four Kind 3" >:: (fun _ -> assert_equal 19014013 (Game_Engine.four_kind
			[(Ace, Dijkstra); (Ace, George); (Ace, Gries); (Ace, Clarkson); 
			 (Seven, Gries); (King, Gries); (Two, Clarkson)]));

		"Score Calc 0" >:: (fun _ -> assert_equal 1000000000 (Game_Engine.score_calculation
			[(Ace, Gries); (Queen, Gries); (Jack, Gries); (Ace, Clarkson); 
			 (King, Gries); (Ten, Gries); (Two, Clarkson)]));

		"Score Calc 1" >:: (fun _ -> assert_equal 2224 (Game_Engine.score_calculation
			[(Three, Gries); (Two, Gries); (Jack, George); (Ace, Clarkson); 
			 (Seven, George); (Ten, Dijkstra); (Four, Clarkson)]));

		(* SCORE *)
		"Score 0" >:: (fun _ -> assert_equal () (Game_Engine.score test_state));

		"Score 1" >:: (fun _ -> assert_equal test_state_after (Game_Engine.score test_state; test_state));

		]

end

module Test_Engine = Test_Game_Engine


let tests = Test_Engine.tests 

let _ = run_test_tt_main tests