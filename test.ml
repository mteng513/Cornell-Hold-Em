(* Main test file for the Texas Hold'em Suite *)
open OUnit2
open Gengine
open Player
open Opponent
open GUpoker

(* This is our OUnit test file for the Entire System. Many aspects of 
 * this system need to be play tested, particularly the opponent and
 * betting mechanisms. Most unit tests have to do with scoring and
 * updating state properly, as these are the things that make the most
 * sense to run OUnit tests on. We test individual scoring functionality
 * then the overall scoring capability and then test that the scoring
 * is properly reflected in the state. We also test some basic state
 * transitions. Most tests tell what they're testing in the name.
 * We've defined some basic states for testing purposes at the top. *)

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
		players_in = [|true; true; true; false; false; false; false; false|];
		scores = [|0;0;0;0;0;0;0;0|];
		chips =[|9999999;9999999;9999999;9999999;9999999;9999999;9999999;9999999|]}

	let test_state_after = {GEngine.current_st = START; 
		cards_in_play = [(Ace, Gries); (Two, Gries);
			(Three, Clarkson); (Queen, Gries); (King, Gries)]; 
		pot = 9438294;
	 	current_bet = 0; n_players = 0; c_player = 0; 
	 	hands = [[(Ten, Gries); (Jack, Gries)]; 
	 		[(King, Clarkson); (Ace, Clarkson)];[]; []; []; []; []; []];
 		bets = [|0; 0; 0; 0; 0; 0; 0; 0|];
 		players_in = [|true; true; true; false; false; false; false; false|];
 		scores = [|1000000000; 1401312; 2322; 0; 0; 0; 0; 0|];
 		chips =[|9999999;9999999;9999999;9999999;9999999;9999999;9999999;9999999|]}

 	let test_b = {GEngine.current_st = START;                                                                                                                                                       
   	cards_in_play = [(Nine, George); (Ace, Dijkstra);
   	(Three, Clarkson); (Eight, Dijkstra); (Jack, Gries)];
   	pot = 100000; 
   	current_bet = 0; n_players = 0; c_player = 0;
   	hands = [[(Ten, Gries); (Seven, Gries)];
   		[(King, Clarkson); (Ace, Clarkson)]];
   	bets = [|0; 0; 0; 0; 0; 0; 0; 0|];
   	players_in = [|true; true; false; false; false; false; false; false|];
   	scores = [|0; 0; 0; 0; 0; 0; 0; 0|];
   	chips =[|9999999;9999999;9999999;9999999;9999999;9999999;9999999;9999999|]}

   	let test_b_after = {GEngine.current_st = START; 
   		cards_in_play = [(Nine, George); (Ace, Dijkstra);
   			(Three, Clarkson); (Eight, Dijkstra); (Jack, Gries)];
 		pot = 100000; 
 		current_bet = 0; n_players = 0; c_player = 0;
 		hands = [[(Ten, Gries); (Seven, Gries)]; 
 			[(King, Clarkson); (Ace, Clarkson)]];
 		bets = [|0; 0; 0; 0; 0; 0; 0; 0|];
 		players_in = [|true; true; false; false; false; false; false; false|];
 		scores = [|15000011; 22419; 0; 0; 0; 0; 0; 0|];
 		chips =[|9999999;9999999;9999999;9999999;9999999;9999999;9999999;9999999|]}

 	let test_c = {GEngine.current_st = START;                                                                                                                                                       
   	cards_in_play = [(Jack, Gries); (Queen, Gries);
   	(Three, Clarkson); (Eight, Dijkstra); (King, Gries)];
   	pot = 100000; 
   	current_bet = 0; n_players = 0; c_player = 0;
   	hands = [[(Ace, Gries); (Ten, Gries)];
   		[(Three, Gries); (Three, George)]];
   	bets = [|0; 0; 0; 0; 0; 0; 0; 0|];
   	players_in = [|true; true; false; false; false; false; false; false|];
   	scores = [|0; 0; 0; 0; 0; 0; 0; 0|];
   	chips =[|9999999;9999999;9999999;9999999;9999999;9999999;9999999;9999999|]}

  let test_c_after = {GEngine.current_st = START;                                                                                                                                                       
   	cards_in_play = [(Jack, Gries); (Queen, Gries);
   	(Three, Clarkson); (Eight, Dijkstra); (King, Gries)];
   	pot = 100000; 
   	current_bet = 0; n_players = 0; c_player = 0;
   	hands = [[(Ace, Gries); (Ten, Gries)];
   		[(Three, Gries); (Three, George)]];
   	bets = [|0; 0; 0; 0; 0; 0; 0; 0|];
   	players_in = [|true; true; false; false; false; false; false; false|];
   	scores = [|1000000000; 3001312; 0; 0; 0; 0; 0; 0|];
   	chips =[|9999999;9999999;9999999;9999999;9999999;9999999;9999999;9999999|]}

  let test_d = {GEngine.current_st = START;                                                                                                                                                       
   	cards_in_play = [(Jack, Gries); (Queen, Gries);
   	(Three, Clarkson); (Eight, Dijkstra); (King, Gries)];
   	pot = 100000; 
   	current_bet = 0; n_players = 0; c_player = 0;
   	hands = [[(Ace, Gries); (Ten, Gries)];
   		[(Three, Gries); (Three, George)]; 
   		[(Nine, Gries); (Four, Gries)]];
   	bets = [|0; 0; 0; 0; 0; 0; 0; 0|];
   	players_in = [|true; true; true; false; false; false; false; false|];
   	scores = [|0; 0; 0; 0; 0; 0; 0; 0|];
   	chips =[|9999999;9999999;9999999;9999999;9999999;9999999;9999999;9999999|]}

  let test_d_after = {GEngine.current_st = START;                                                                                                                                                       
   	cards_in_play = [(Jack, Gries); (Queen, Gries);
   	(Three, Clarkson); (Eight, Dijkstra); (King, Gries)];
   	pot = 100000; 
   	current_bet = 0; n_players = 0; c_player = 0;
   	hands = [[(Ace, Gries); (Ten, Gries)];
   		[(Three, Gries); (Three, George)];
   		[(Nine, Gries); (Four, Gries)]];
   	bets = [|0; 0; 0; 0; 0; 0; 0; 0|];
   	players_in = [|true; true; true; false; false; false; false; false|];
   	scores = [|1000000000; 3001312; 16143194; 0; 0; 0; 0; 0|];
   	chips =[|9999999;9999999;9999999;9999999;9999999;9999999;9999999;9999999|]}

  let test_e = {GEngine.current_st = START;                                                                                                                                                       
   	cards_in_play = [(Jack, Gries); (Queen, Gries);
   	(Three, Clarkson); (Eight, Dijkstra); (King, Gries)];
   	pot = 100000; 
   	current_bet = 0; n_players = 0; c_player = 0;
   	hands = [[(Ace, Gries); (Ten, Gries)];
   		[(Three, Gries); (Three, George)]; 
   		[(Nine, Gries); (Four, Gries)];
   		[(Jack, Dijkstra); (Eight, George)]];
   	bets = [|0; 0; 0; 0; 0; 0; 0; 0|];
   	players_in = [|true; true; true; true; false; false; false; false|];
   	scores = [|0; 0; 0; 0; 0; 0; 0; 0|];
   	chips =[|9999999;9999999;9999999;9999999;9999999;9999999;9999999;9999999|]}

  let test_e_after = {GEngine.current_st = START;                                                                                                                                                       
   	cards_in_play = [(Jack, Gries); (Queen, Gries);
   	(Three, Clarkson); (Eight, Dijkstra); (King, Gries)];
   	pot = 100000; 
   	current_bet = 0; n_players = 0; c_player = 0;
   	hands = [[(Ace, Gries); (Ten, Gries)];
   		[(Three, Gries); (Three, George)];
   		[(Nine, Gries); (Four, Gries)];
   		[(Jack, Dijkstra); (Eight, George)]];
   	bets = [|0; 0; 0; 0; 0; 0; 0; 0|];
   	players_in = [|true; true; true; true; false; false; false; false|];
   	scores = [|1000000000; 3001312; 16143194; 1100813; 0; 0; 0; 0|];
   	chips =[|9999999;9999999;9999999;9999999;9999999;9999999;9999999;9999999|]}

  let test_f = {GEngine.current_st = START;                                                                                                                                                       
   	cards_in_play = [(Jack, Gries); (Queen, Gries);
   	(Three, Clarkson); (Eight, Dijkstra); (King, Gries)];
   	pot = 100000; 
   	current_bet = 0; n_players = 0; c_player = 0;
   	hands = [[(Ace, Gries); (Ten, Gries)];
   		[(Three, Gries); (Three, George)]; 
   		[(Nine, Gries); (Four, Gries)];
   		[(Jack, Dijkstra); (Eight, George)];
   		[(Queen, Dijkstra); (Two, George)]];
   	bets = [|0; 0; 0; 0; 0; 0; 0; 0|];
   	players_in = [|true; true; true; true; true; false; false; false|];
   	scores = [|0; 0; 0; 0; 0; 0; 0; 0|];
   	chips =[|9999999;9999999;9999999;9999999;9999999;9999999;9999999;9999999|]}

  let test_f_after = {GEngine.current_st = START;                                                                                                                                                       
   	cards_in_play = [(Jack, Gries); (Queen, Gries);
   	(Three, Clarkson); (Eight, Dijkstra); (King, Gries)];
   	pot = 100000; 
   	current_bet = 0; n_players = 0; c_player = 0;
   	hands = [[(Ace, Gries); (Ten, Gries)];
   		[(Three, Gries); (Three, George)];
   		[(Nine, Gries); (Four, Gries)];
   		[(Jack, Dijkstra); (Eight, George)];
   		[(Queen, Dijkstra); (Two, George)]];
   	bets = [|0; 0; 0; 0; 0; 0; 0; 0|];
   	players_in = [|true; true; true; true; true; false; false; false|];
   	scores = [|1000000000; 3001312; 16143194; 1100813; 19418; 0; 0; 0|];
   	chips =[|9999999;9999999;9999999;9999999;9999999;9999999;9999999;9999999|]}

  let test_g = {GEngine.current_st = START;                                                                                                                                                       
   	cards_in_play = [(Jack, Gries); (Queen, Gries);
   	(Three, Clarkson); (Eight, Dijkstra); (King, Gries)];
   	pot = 100000; 
   	current_bet = 0; n_players = 0; c_player = 0;
   	hands = [[(Ace, Gries); (Ten, Gries)];
   		[(Three, Gries); (Three, George)]; 
   		[(Nine, Gries); (Four, Gries)];
   		[(Jack, Dijkstra); (Eight, George)];
   		[(Queen, Dijkstra); (Two, George)];
   		[(Nine, Dijkstra); (Four, George)]];
   	bets = [|0; 0; 0; 0; 0; 0; 0; 0|];
   	players_in = [|true; true; true; true; true; true; false; false|];
   	scores = [|0; 0; 0; 0; 0; 0; 0; 0|];
   	chips =[|9999999;9999999;9999999;9999999;9999999;9999999;9999999;9999999|]}

  let test_g_after = {GEngine.current_st = START;                                                                                                                                                       
   	cards_in_play = [(Jack, Gries); (Queen, Gries);
   	(Three, Clarkson); (Eight, Dijkstra); (King, Gries)];
   	pot = 100000; 
   	current_bet = 0; n_players = 0; c_player = 0;
   	hands = [[(Ace, Gries); (Ten, Gries)];
   		[(Three, Gries); (Three, George)];
   		[(Nine, Gries); (Four, Gries)];
   		[(Jack, Dijkstra); (Eight, George)];
   		[(Queen, Dijkstra); (Two, George)];
   		[(Nine, Dijkstra); (Four, George)]];
   	bets = [|0; 0; 0; 0; 0; 0; 0; 0|];
   	players_in = [|true; true; true; true; true; true; false; false|];
   	scores = [|1000000000; 3001312; 16143194; 1100813; 19418; 2218; 0; 0|];
   	chips =[|9999999;9999999;9999999;9999999;9999999;9999999;9999999;9999999|]}

  (* End of state definitions for later tests. Unit tests begin here. *)

	let tests = "test suite" >::: [

		"Royal Flush 0" >:: (fun _ -> assert_equal 1000000000 
			(Game_Engine.straight
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

		"Two Pair: Only 1 pair" >:: (fun _ -> assert_equal 0 
			(Game_Engine.two_pair 
			[(Ace,Gries); (Two, Gries); (Three, Clarkson); (Queen, Gries);
			 (King, Gries);(Jack, Clarkson);(Ace, Clarkson)]));

		"Two Pair: Ace, King" >:: (fun _ -> assert_equal 1401312 
			(Game_Engine.two_pair 
			[(Ace,Gries); (Two, Gries); (Three, Clarkson); (Queen, Gries);
			 (King, Gries);(King, Clarkson);(Ace, Clarkson)]));

		"Two Pair: Only 1 pair (3 of a kind)" >:: (fun _ -> assert_equal 0 
			(Game_Engine.two_pair 
			[(Ace,Gries); (Two, Gries); (Three, Clarkson); (Queen, Gries);
			 (King, Gries);(Ace, Clarkson);(Ace, Clarkson)]));

		"Two Pair: Ace, Queen" >:: (fun _ -> assert_equal 1401213 
			(Game_Engine.two_pair 
			[(Ace,Gries); (Two, Gries); (Three, Clarkson); (Queen, Gries);
 			(King, Gries);(Queen, Clarkson);(Ace, Clarkson)]));

		"Two Pair: 4 Aces (4 of a kind)" >:: (fun _ -> assert_equal 0 
			(Game_Engine.two_pair 
			[(Ace, Gries); (Two, Gries); (Three, Clarkson); (Ace, George);
			(Ace, Dijkstra); (Ace, Clarkson)]));

		"Pair: None" >:: (fun _ -> assert_equal 0 
			(Game_Engine.pair
			[(Ace, Gries); (Two, Gries); (Three, Clarkson); (King, George);
			(Seven, Dijkstra); (Ten, Clarkson)]));

		"Pair: Ace" >:: (fun _ -> assert_equal 22442 
			(Game_Engine.pair
			[(Ace, Gries); (Two, Gries); (Three, Clarkson); 
			(Queen, Gries); (King, Gries);
	        (King, Clarkson); (Ace, Clarkson)]));

		"Pair: King (and Two)" >:: (fun _ -> assert_equal 21023 
			(Game_Engine.pair
			[(Ace, Gries); (Two, Gries); (Three, Clarkson); (Queen, Gries); 
				(King, Gries); (King, Clarkson); (Two, Clarkson)]));

		"Pair: Twos (3 of a kind) has Six" >:: (fun _ -> assert_equal 4426 
			(Game_Engine.pair
			[(Two, George); (Two, Gries); (Three, Clarkson); (Queen, Gries);
			(King, Gries); (Six, Dijkstra); (Two, Clarkson)]));

		"Pair: Twos (3 of a kind) has Seven" >:: (fun _ -> assert_equal 4427 
			(Game_Engine.pair
			[(Two, George); (Two, Gries); (Three, Clarkson); (Queen, Gries);
 			 (King, Gries); (Seven, Dijkstra); (Two, Clarkson)]));

		"Pair: King (and Two (3 of a kind))" >:: (fun _ -> assert_equal 20732 
			(Game_Engine.pair
			[(Two, George); (Two, Gries); (Three, Clarkson); (Queen, Gries);
			 (King, Gries); (King, Clarkson); (Two, Clarkson)]));

		"Three Kind: Two" >:: (fun _ -> assert_equal 2001313 
			(Game_Engine.three_kind
			[(Two, George); (Two, Gries); (Three, Clarkson); (Queen, Gries);
			 (King, Gries); (King, Clarkson); (Two, Clarkson)]));

		"Three Kind: Two (4 of a kind)" >:: (fun _ -> assert_equal 2001313 
			(Game_Engine.three_kind
			[(Two, George); (Two, Gries); (Two, Clarkson); (Queen, Gries);
			 (King, Gries); (King, Clarkson); (Two, Dijkstra)]));

		"Three Kind: Two and Three" >:: (fun _ -> assert_equal 3001302 
			(Game_Engine.three_kind
			[(Two, George); (Two, Gries); (Three, Clarkson); (Three, Gries);
			 (King, Gries); (Three, Dijkstra); (Two, Clarkson)]));

		"Three Kind: None" >:: (fun _ -> assert_equal 0 
			(Game_Engine.three_kind
			[(Four, George); (Five, Gries); (Three, Clarkson); (Seven, Gries);
			 (King, Gries); (Three, Dijkstra); (Two, Clarkson)]));

		"Three Kind: Four (4 of a kind)" >:: (fun _ -> assert_equal 4001307 
			(Game_Engine.three_kind
			[(Four, George); (Four, Gries); (Four, Clarkson); (Seven, Gries);
			 (King, Gries); (Four, Dijkstra); (Two, Clarkson)]));

		"Three Kind: Ace" >:: (fun _ -> assert_equal 14001307 
			(Game_Engine.three_kind
			[(Ace, George); (Ace, Gries); (Four, Clarkson); (Seven, Gries);
			 (King, Gries); (Ace, Dijkstra); (Two, Clarkson)]));

		"Four Kind: Four" >:: (fun _ -> assert_equal 19004013 
			(Game_Engine.four_kind
			[(Four, George); (Four, Gries); (Four, Clarkson); (Seven, Gries);
			 (King, Gries); (Four, Dijkstra); (Two, Clarkson)]));

		"Four Kind: None" >:: (fun _ -> assert_equal 0 
			(Game_Engine.four_kind
			[(Four, George); (Three, Gries); (Four, Clarkson); (Seven, Gries);
			 (King, Gries); (Four, Dijkstra); (Two, Clarkson)]));

		"Four Kind: Ace" >:: (fun _ -> assert_equal 19014013 
			(Game_Engine.four_kind
			[(Ace, George); (Ace, Gries); (Four, Clarkson); (Seven, Gries);
			 (King, Gries); (Ace, Dijkstra); (Ace, Clarkson)]));

		(* Order of cards shouldn't change score *)
		"Four Kind: Ace (chaning order shouldn't change score)" 
			>:: (fun _ -> assert_equal 19014013 
			(Game_Engine.four_kind
			[(Ace, Dijkstra); (Ace, George); (Ace, Gries); (Ace, Clarkson); 
			 (Seven, Gries); (King, Gries); (Two, Clarkson)])); 

		(* END OF INDIVIDUAL SCORE FUNCTION TESTS. 
		 * Now, we run the same tests on the overarching "score calculation" 
		 * function, making sure we get the same thing. *)

		"Score Calc: Royal Flush" 
			>:: (fun _ -> assert_equal 1000000000 
			(Game_Engine.score_calculation
			[(Ace, Gries); (Queen, Gries); (Jack, Gries); (Ace, Clarkson); 
			 (King, Gries); (Ten, Gries); (Two, Clarkson)]));

		"Score Calc: Royal Flush (Ordering changes)" 
			>:: (fun _ -> assert_equal 1000000000 
			(Game_Engine.score_calculation
			[(Ten, Gries); (Jack, Gries); (Queen, Gries); (Two, Clarkson); 
			 (King, Gries); (Ace, Gries); (Five, Clarkson)]));

		"Score Calc: High Card (no hand)" >:: (fun _ -> assert_equal 2224 
			(Game_Engine.score_calculation
			[(Three, Gries); (Two, Gries); (Jack, George); (Ace, Clarkson); 
			 (Seven, George); (Ten, Dijkstra); (Four, Clarkson)]));

		"Score Calc: High Card (higher tiebreaker)" >:: (fun _ -> assert_equal 2225
			(Game_Engine.score_calculation
			[(Four, Gries); (Three, Gries); (Jack, George); (Ace, Clarkson); 
			 (Seven, George); (Ten, Dijkstra); (Five, Clarkson)]));

		"Score Calc: Regular flush" >:: (fun _ -> assert_equal (16120643)
	    (GEngine.score_calculation [(Ten, Gries); (Three, Gries); (Four, Gries); 
	    	(Five, Clarkson); (Six, Gries); (Jack, Gries)]));

		"Score Calc: Regular flush with a Pair" >:: 
		(fun _ -> assert_equal (16120643)
	    (GEngine.score_calculation [(Ten, Gries); (Three, Gries); (Four, Gries); 
	    	(Four, Clarkson); (Six, Gries); (Jack, Gries)]));

		"Score Calc: Regular flush with 2 Pair" >:: 
			(fun _ -> assert_equal (16120643)
	    (GEngine.score_calculation [(Ten, Gries); (Three, Gries); (Four, Gries);
	    	(Four, Clarkson); (Six, Gries); (Jack, Gries); (Three, Dijkstra)]));

			"Score Calc: Regular flush with triple" >:: 
			(fun _ -> assert_equal (16120643)
	    (GEngine.score_calculation [(Ten, Gries); (Three, Gries); (Four, Gries); 
	    	(Four, Clarkson); (Six, Gries); (Jack, Gries); (Four, Dijkstra)]));

		"Score Calc: Higher flush" >:: (fun _ -> assert_equal (16120654)
	    (GEngine.score_calculation [(Ten, Gries); (Four, Gries); (Five, Gries); 
	    	(Five, Clarkson); (Six, Gries); (Jack, Gries)]));

		"Score Calc: A straight flush"  >:: (fun _ -> assert_equal 20000006
			(GEngine.straight [(Two, Gries); (Three, Gries); (Four, Gries); 
				(Five, Gries); (Six, Gries); (Jack, Dijkstra)]));

		"Score Calc: A higher straight flush"  >:: (fun _ -> assert_equal 20000007
			(GEngine.straight [(Three, Gries); (Four, Gries); (Five, Gries); 
				(Six, Gries); (Seven, Gries); (Jack, Dijkstra)]));

		"Score Calc: Regular Straight" >:: (fun _ -> assert_equal 15000006
		(GEngine.score_calculation [(Two, Gries); (Three, Gries); (Four, Gries); 
			(Five, Clarkson); (Six, Gries); (Jack, Dijkstra)]));

		"Score Calc: Regular Straight with pair" >:: 
		(fun _ -> assert_equal 15000006
		(GEngine.score_calculation [(Two, Gries); (Three, Gries); (Four, Gries); 
			(Five, Clarkson); (Six, Gries); (Jack, Dijkstra); (Two, Dijkstra)]));

		"Score Calc: Regular Straight with 2pair" >:: 
		(fun _ -> assert_equal 15000006
		(GEngine.score_calculation [(Two, Gries); (Three, Gries); (Four, Gries); 
			(Five, Clarkson); (Six, Gries); (Six, Dijkstra); (Two, Dijkstra)]));

		"Score Calc: Regular Straight with triple" >:: 
		(fun _ -> assert_equal 15000006
		(GEngine.score_calculation [(Two, Gries); (Three, Gries); (Four, Gries); 
			(Five, Clarkson); (Six, Gries); (Six, Dijkstra); (Six, George)]));

		"Score Calc: Regular Straight (Sanity check for ordering)" >:: 
		(fun _ -> assert_equal 15000006
		(GEngine.score_calculation [(Three, Gries); (Four, Gries); (Two, Gries); 
			(Jack, Clarkson); (Six, Gries); (Five, Dijkstra)]));

		"Score Calc: Higher Straight" >:: (fun _ -> assert_equal 15000007
		(GEngine.score_calculation [(Three, Gries); (Four, Gries); (Five, Gries); 
			(Six, Clarkson); (Seven, Gries); (Jack, Dijkstra)]));

		"Score Calc: Complex High Card" >:: (fun _ -> assert_equal 1413 
			(GEngine.score_calculation [(Three, Gries); (Four, Clarkson); 
				(Six, Gries); (Seven, Clarkson); (Nine, Dijkstra)]));

		"Score Calc: Complex lower High Card" >:: (fun _ -> assert_equal 1412 
			(GEngine.score_calculation [(Two, Gries); (Four, Clarkson); (Six, Gries); 
				(Seven, Clarkson); (Nine, Dijkstra)]));

		"Score Calc: Full House" >:: (fun _ -> assert_equal 18004014 
			(GEngine.score_calculation [(Four, Gries); (Four, Clarkson); 
				(Four, Gries); (Ace, Clarkson); (Ace, Dijkstra)]));

		"Score Calc: Full House with 2 triples" >:: (fun _ -> assert_equal 18014004
			(GEngine.score_calculation [(Four, Gries); (Four, Clarkson); 
				(Four, Gries);(Ace, Clarkson); (Ace, Dijkstra); (Ace, George)]));

		"Score Calc: Lower Full House" >:: (fun _ -> assert_equal 18004010 
			(GEngine.score_calculation [(Four, Gries); (Four, Clarkson); 
				(Four, Gries);(Ten, Clarkson); (Ten, Dijkstra)]));

		"Score Calc: Higher Full House" >:: (fun _ -> assert_equal 18005014 
			(GEngine.score_calculation [(Five, Gries); (Five, Clarkson); 
				(Five, Gries); (Ace, Clarkson); (Ace, Dijkstra)]));

		"Score Calc: Two Pair: Ace, King" >:: (fun _ -> assert_equal 1401312 
			(Game_Engine.score_calculation [(Ace,Gries); (Two, Gries); 
				(Three, Clarkson); (Queen, Gries); (King, Gries);(King, Clarkson);
				(Ace, Clarkson)]));

		"Score Calc: Three Pair: Ace, King, three" >:: 
			(fun _ -> assert_equal 1401312 
			(Game_Engine.score_calculation [(Ace,Gries); (Three, Gries); 
				(Three, Clarkson); (Queen, Gries); (King, Gries);(King, Clarkson);
				(Ace, Clarkson)]));

		"Score Calc: Two Pair: Ace, King (Sanity check for ordering)" >:: 
			(fun _ -> assert_equal 1401312 (Game_Engine.score_calculation 
				[(Queen,Gries); (Ace, Gries);(King, Clarkson); (Ace, Gries); 
				(King, Gries);(Three, Clarkson);(Two, Clarkson)]));

		"Score Calc: Two Pair: Ace, King with lower tie breaker" >:: 
			(fun _ -> assert_equal 1401311 (Game_Engine.score_calculation 
				[(Ace,Gries); (Two, Gries); (Three, Clarkson); (Jack, Gries); 
				(King, Gries);(King, Clarkson);(Ace, Clarkson)]));

		"Score Calc: Pair Ace" >:: (fun _ -> assert_equal 22431 
			(Game_Engine.score_calculation
			[(Ace, Gries); (Two, Gries); (Three, Clarkson);(Queen, Gries); 
			(King, Gries); (Jack, Clarkson); (Ace, Clarkson)]));

		"Score Calc: Pair Ace with lower tie breaker" >:: 
			(fun _ -> assert_equal 22430 (Game_Engine.score_calculation
			[(Ace, Gries); (Two, Gries); (Three, Clarkson);(Queen, Gries); 
			(King, Gries); (Ten, Clarkson); (Ace, Clarkson)]));

		"Score Calc Pair King" >:: (fun _ -> assert_equal 21025 
			(Game_Engine.score_calculation
			[(Ace, Gries); (Two, Gries); (Three, Clarkson); 
			(Queen, Gries); (King, Gries); (King, Clarkson); (Five, Clarkson)]));

		"Score Calc Pair King with lower tie breaker" >:: 
		  (fun _ -> assert_equal 21024 (Game_Engine.score_calculation
			[(Ace, Gries); (Two, Gries); (Three, Clarkson); 
			(Queen, Gries); (King, Gries); (King, Clarkson); (Four, Clarkson)]));

		"Score Calc: Three Kind Two" >:: (fun _ -> assert_equal 2001312 
			(Game_Engine.score_calculation
			[(Two, George); (Two, Gries); (Three, Clarkson); (Queen, Gries);
			 (King, Gries); (Four, Clarkson); (Two, Clarkson)]));

		"Score Calc: Three Kind Two with lower tie breaker" >:: 
			(fun _ -> assert_equal 2001311 (Game_Engine.score_calculation
			[(Two, George); (Two, Gries); (Three, Clarkson); (Jack, Gries);
			 (King, Gries); (Four, Clarkson); (Two, Clarkson)]));

		"Score Calc: Three Kind Four" >:: (fun _ -> assert_equal 4001307 
			(Game_Engine.score_calculation
			[(Four, George); (Four, Gries); (Four, Clarkson); (Seven, Gries);
			 (King, Gries); (Three, Dijkstra); (Two, Clarkson)]));

		"Score Calc: Three Kind Ace" >:: (fun _ -> assert_equal 14001307 
			(Game_Engine.score_calculation
			[(Ace, George); (Ace, Gries); (Four, Clarkson); (Seven, Gries);
			 (King, Gries); (Ace, Dijkstra); (Two, Clarkson)]));

		"Score Calc: Four Kind Four" >:: (fun _ -> assert_equal 19004013 
			(Game_Engine.score_calculation
			[(Four, George); (Four, Gries); (Four, Clarkson); (Seven, Gries);
			 (King, Gries); (Four, Dijkstra); (Two, Clarkson)]));

		"Score Calc: Four Kind Four with full house" >:: 
			(fun _ -> assert_equal 19004013 (Game_Engine.score_calculation
			[(Four, George); (Four, Gries); (Four, Clarkson); (Seven, Gries);
			 (King, Gries); (Four, Dijkstra); (King, Clarkson)]));

		"Score Calc: Four Kind Four with Triple" >:: 
			(fun _ -> assert_equal 19004013 (Game_Engine.score_calculation
			[(Four, George); (Four, Gries); (Four, Clarkson); (King, Dijkstra);
			 (King, Gries); (Four, Dijkstra); (King, Clarkson)]));

		"Score Calc: Four Kind Four (Sanity check for order changes)" >:: 
			(fun _ -> assert_equal 19004013 (Game_Engine.score_calculation
			[(Four, George); (King, Gries); (Two, Clarkson); (Seven, Gries);
			 (Four, Gries); (Four, Dijkstra); (Four, Clarkson)]));

		"Score Calc: Four Kind Ace" >:: (fun _ -> assert_equal 19014013 
			(Game_Engine.score_calculation
			[(Ace, George); (Ace, Gries); (Four, Clarkson); (Seven, Gries);
			 (King, Gries); (Ace, Dijkstra); (Ace, Clarkson)]));

		"Score Calc: Four Kind Ace lower tie breaker" >:: 
			(fun _ -> assert_equal 19014012 (Game_Engine.score_calculation
			[(Ace, George); (Ace, Gries); (Four, Clarkson); (Seven, Gries);
			 (Queen, Gries); (Ace, Dijkstra); (Ace, Clarkson)]));

		"Four Kind: Ace (Sanity check for changed order)" 
			>:: (fun _ -> assert_equal 19014013 
			(Game_Engine.score_calculation
			[(Ace, Dijkstra); (Ace, George); (Ace, Gries); (Ace, Clarkson); 
			 (Seven, Gries); (King, Gries); (Two, Clarkson)])); 

		(* END OF SCORE CALCULATION TESTS.
		 * Now, we run a few tests to make sure state is properly updated
		 * when scoring. The tests get increasingly more complex. *)

		"Score 0a: Check that returns unit" >:: (fun _ -> assert_equal () 
			(Game_Engine.score test_state));

		"Score 0b: Royal Flush vs 2 pair" 
			>:: (fun _ -> assert_equal test_state_after 
			(Game_Engine.score test_state; test_state));

		"Score 1a: Check that returns unit" >:: (fun _ -> assert_equal () 
			(Game_Engine.score test_b));

		"Score 1b: Straight vs pair" >:: (fun _ -> assert_equal test_b_after
			(Game_Engine.score test_b; test_b));

		"Score 2a: Check that returns unit" >:: (fun _ -> assert_equal () 
			(Game_Engine.score test_b));

		"Score 2b: Royal Flush vs triple" >:: (fun _ -> assert_equal test_c_after
			(Game_Engine.score test_c; test_c));

		"Score 3: Royal Flush vs triple vs flush" >:: 
			(fun _ -> assert_equal test_d_after (Game_Engine.score test_d; test_d));

		"Score 4: Royal Flush vs triple vs flush vs 2pair" >:: 
			(fun _ -> assert_equal test_e_after (Game_Engine.score test_e; test_e));

		"Score 5: Royal Flush vs triple vs flush vs 2pair vs pair" >:: 
			(fun _ -> assert_equal test_f_after (Game_Engine.score test_f; test_f));

		"Score 6: Royal Flush vs triple vs flush vs 2pair vs pair vs high card" >:: 
			(fun _ -> assert_equal test_g_after (Game_Engine.score test_g; test_g));

		]

end

module Test_Engine = Test_Game_Engine

let tests = Test_Engine.tests 

let _ = run_test_tt_main tests