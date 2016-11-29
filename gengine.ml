(* ml file for the Game Engine *)

module Game_Engine = struct

	type suit = Clarkson | Gries | Dijkstra | George

	type rank = Two | Three | Four | Five | Six | Seven
		| Eight | Nine | Ten | Jack | Queen | King | Ace

	type card = rank * suit

	type deck = card list

	type hand = card list

	type current_state = START | DEAL | BET_ZERO | BET_ONE | BET_TWO | BET_THREE | SCORE
		| END

	type global_state = {
		mutable current_st : current_state;
		mutable cards_in_play : card list;
		mutable pot : int;
		mutable current_bet : int;
		mutable n_players : int;
		mutable c_player : int;
		mutable hands : (int * hand) list; 
		mutable bets : int array;
		mutable players_in : bool array;
	}

	exception GameEnded

	(* End of type declarations. We will now declare some references that
	 * will need to be updated as we go. *)

	let deck = ref [] 
	let state = ref {current_st = START; cards_in_play = []; pot = 0;
		current_bet = 0; n_players = 0; c_player = 0; hands = []; 
		bets = [||]; players_in = [||]}

	let suit_print suit =
		match suit with 
			| Clarkson -> "Clarkson "
			| Gries -> "Gries "
			| Dijkstra -> "Dijkstra "
			| George -> "George "

	let print_helper ele = 
		match ele with  
			| (Two, suit) -> "Two of " ^ suit_print suit
			| (Three, suit) -> "Three of " ^ suit_print suit
			| (Four, suit) -> "Four of " ^ suit_print suit
			| (Five, suit) -> "Five of " ^ suit_print suit
			| (Six, suit) -> "Six of " ^ suit_print suit
			| (Seven, suit) -> "Seven of " ^ suit_print suit
			| (Eight, suit) -> "Eight of " ^ suit_print suit
			| (Nine, suit) -> "Nine of " ^ suit_print suit
			| (Ten, suit) -> "Ten of " ^ suit_print suit
			| (Jack, suit) -> "Jack of " ^ suit_print suit
			| (Queen, suit) -> "Queen of " ^ suit_print suit
			| (King, suit) -> "King of " ^ suit_print suit
			| (Ace, suit) -> "Ace of " ^ suit_print suit

	let rec formatted_print lst = 
		match lst with 
			| [] -> ""
			| h::t -> (print_helper h) ^ formatted_print t

	let printc lst =
		print_endline (formatted_print lst)

	(* Deck reset function. Sets the deck back to normal state. *)
	let reset_deck () = deck := 
		[(Two, Clarkson); (Three, Clarkson); (Four, Clarkson); 
		(Five, Clarkson); (Six, Clarkson); (Seven, Clarkson); 
		(Eight, Clarkson); (Nine, Clarkson); (Ten, Clarkson); (Jack, Clarkson);
		(Queen, Clarkson); (King, Clarkson); (Ace, Clarkson); 
		(Two, Gries); (Three, Gries); (Four, Gries); (Five, Gries);
		(Six, Gries); (Seven, Gries); (Eight, Gries); (Nine, Gries);
		(Ten, Gries); (Jack, Gries); (Queen, Gries); (King, Gries);
		(Ace, Gries);
		(Two, Dijkstra); (Three, Dijkstra); (Four, Dijkstra); (Five, Dijkstra);
		(Six, Dijkstra); (Seven, Dijkstra); (Eight, Dijkstra); 
		(Nine, Dijkstra); (Ten, Dijkstra); (Jack, Dijkstra); (Queen, Dijkstra);
		(King, Dijkstra); (Ace, Dijkstra);
		(Two, George); (Three, George); (Four, George); (Five, George); 
		(Six, George); (Seven, George); (Eight, George); (Nine, George);
		(Ten, George); (Jack, George); (Queen, George); (King, George); 
		(Ace, George);
		]

	(* Pushes an element onto a ref list *)
	let push lst ele = 
		lst := ele::!lst

	(* Pops an element off of a ref list *)
	let pop lst = 
		match !lst with
			| [] -> failwith "no more card in the deck"
			| h::t -> lst := t; h 

	(* Helper for shuffle. Takes in a deck, and pushes the elements
	 * of the deck onto two lists psuedo randomly. *)
	let rec shuffle_helper deck lst1 lst2 lst3 lst4 lst5 lst6 lst7 lst8 = 
		match !deck with
			| [] -> ()
			| h::t -> let roll = Random.int 5200 in 
				(if (roll mod 8 = 0) then push lst2 h 
				else if ((roll mod 8) = 1) then push lst3 h
				else if ((roll mod 8) = 2) then push lst4 h
				else if ((roll mod 8) = 3) then push lst7 h
				else if ((roll mod 8) = 4) then push lst5 h
				else if ((roll mod 8) = 5) then push lst6 h
				else if ((roll mod 8) = 6) then push lst8 h
				else push lst1 h); let tt = ref t in
				shuffle_helper tt lst2 lst3 lst4 lst1 lst8 lst5 lst6 lst7

	let cut () = 
		let a_list = ref [] in
		let b_list = ref [] in 
		(for i = 0 to 51 do 
			(if ((Random.int 3000) mod 2 = 0) then push a_list (List.nth !deck i)
			else push b_list (List.nth !deck i))
		done); 
		deck := (!b_list) @ (!a_list)

	(* Shuffling mechanism. Creates two ref lists to be randomly
	 * pushed onto, punts to the helper, then sets the deck equal to
	 * the newly shuffeled deck. *)
	let shuffle () = 
		let a_list = ref [] in 
		let b_list = ref [] in 
		let c_list = ref [] in 
		let d_list = ref [] in 
		let e_list = ref [] in 
		let f_list = ref [] in 
		let g_list = ref [] in 
		let h_list = ref [] in 
    shuffle_helper deck a_list b_list c_list d_list e_list 
    	f_list g_list h_list; 
    deck := (!b_list) @ (!d_list) @ (!c_list) @ (!a_list) @ (!h_list) @
    	(!f_list) @ (!e_list) @ (!g_list);
    cut ()

    (* SORT HAND *)
	(* !state.cards_in_play @ player hand *)
	let sort_cards (h:hand) : hand = 
		List.sort Pervasives.compare h

	(* [flush hand]
	 * Returns the sorted card list, in ascending order of rank,
	 * containing the flush if there is one (may exceed 5 cards if 6 or all cards have same suit).
	 * Otherwise, returns the empty list.
	 *)
	let flush (hand: hand) = 
		let h = sort_cards hand in 
		if (List.(h |> filter (fun x -> snd x = snd(List.hd h))|> length) >= 5) then 
			List.(h |> filter (fun x -> snd x = snd(List.hd h)))
		else if (List.(h |> filter (fun x -> snd x = snd(List.nth h 1)) |> length) >= 5) then
			List.(h |> filter (fun x -> snd x = snd(List.nth h 1)))
		else if (List.(h |> filter (fun x -> snd x = snd(List.nth h 2)) |> length) >= 5) then
			List.(h |> filter (fun x -> snd x = snd(List.nth h 2)))
		else 
			[]

	(* BEGIN STRAIGHT CALC *)
	let make_enum_rank rank = 
		match rank with 
		| Two -> 2
		| Three -> 3
		| Four -> 4
		| Five -> 5
		| Six -> 6
		| Seven -> 7
		| Eight -> 8
		| Nine -> 9
		| Ten -> 10
		| Jack -> 11
		| Queen -> 12
		| King -> 13
		| Ace -> 14

	(* Returns sorted an int list in ascending order*)
	let rec make_enum_hand (h: hand) acc = 
		match h with 
		|[] -> List.sort Pervasives.compare acc
		|(rank, suit)::t -> make_enum_hand t (make_enum_rank rank::acc)

	(* [straight hand]
	 * 
	 *
	 *)
	let straight (hand: hand) = 
		(* If either are false then we know we don't have a flush *)
		(List.mem_assoc Five hand) (List.mem_assoc Ten hand) 
		(* Ace 2 3 4 5 valid, 10 Jack Queen King Ace valid, if latter case + flush then royal flush *)
		
		(* If not [] then we have a straight flush, otherwise just a straight *)


    (* This function takes in the current_st g_state (POTENTIALLY NEEDS MORE INPUTS)
     * and updates the winning players scores with the points they won from the
 	 * pot. Returns a unit *)
	let score g_state = 
		failwith "Unimplemented"

	let transition_state g_state = 
		ignore (g_state.c_player <- 0);
		match g_state.current_st with
			| START -> g_state.current_st <- DEAL 
			| DEAL -> g_state.current_st <- BET_ZERO
			| BET_ZERO -> g_state.current_st <- BET_ONE
			| BET_ONE -> g_state.current_st <- BET_TWO
			| BET_TWO -> g_state.current_st <- BET_THREE
			| BET_THREE -> g_state.current_st <- SCORE
			| SCORE -> if g_state.n_players = 1 then 
				g_state.current_st <- END else g_state.current_st <- DEAL
			| END -> failwith "Bad"

	(* increments an int ref *)
	let mutable_incr i = 
		i := !i + 1

	let get_state () = state 

	(* Signals bets to the players. 
	 * Takes in the current_st g_state, makes necessary updates to g_state,
	 * and then returns unit*)
	let rec signal_bet g_state (* current_player *) = 
		print_endline ("Place your bet. 
			The current bet is " ^ (string_of_int g_state.current_bet));
		let bet = read_int () in
		(* must make every other player match the bet, raise, or fold *)
		match bet with
		(* if player raises, bet and pot both increase *)
		| bet when bet > g_state.current_bet -> 
							g_state.current_bet <- bet + g_state.current_bet;
							g_state.pot <- g_state.pot+bet (* -g_state.c_player_bet *)
		(* if a player matches, the bet is added to the pot (can be 0) *)
		| bet when bet = g_state.current_bet -> g_state.pot <- g_state.pot+bet (* -g_state.bets c_player_bet *)
		(* if a player decides not to bet, they fold *)
		| bet when bet = 0 -> () (* this means the player folds *)
		(* if negative # or # less than bet, retry *)
		| _ -> print_endline "invalid input received. try again"; 
							signal_bet g_state

	(* takes in array a, element e, and the index accumulator index_acc.
	 * returns the index of element e in array a. 
	 * The index_acc should start at 0, or at whatever point you want to start *)
	let rec index_in_array a e index_acc =
		if a.(index_acc) = e 
		then index_acc 
		else index_in_array a e (index_acc + 1)
	
	(* takes in an array bets and returns the greatest array element value *)
	let rec max_of_array bets =
		let len = Array.length bets in
		match bets with
		| [||] -> 0
		| bets when len = 1 -> Array.get bets 0
		| bets when len > 0 -> 
			let x = Array.get bets 0 in
			let y = max_of_array (Array.sub bets 1 (len - 1)) in
			if x > y then x else y
		| _ -> failwith "wtf is going on"


	(* gets the index of the max element in an int array. Takes in the 
	 * int array bets and returns index of the greatest element in the array *)
	let rec index_of_max (bets: int array) : int =
		let max_element = max_of_array bets in
		index_in_array bets max_element 0 

	(* Called in after deal and each bet stage to signal bets to
	 * all the players. *)
	let switch g_state = 
		match g_state.current_st with 
			| BET_ZERO | BET_ONE | BET_TWO | BET_THREE ->
				(* I think this needs to be a while loop until you reach the player who set the highest bet
				We also need to make sure we don't go through players that have folded *)
				(while not (g_state.c_player = index_of_max g_state.bets) do 
					signal_bet g_state; 
					g_state.c_player <- g_state.c_player + 1; done);

				g_state.c_player <- 0;
				transition_state g_state;
			| _ -> failwith "Bad state"

	(* deal takes in the current_st g_state and the current deck ref (deck) and
	 * updates the inputted state with hands *)
	let deal g_state deck = () 
		let deal g_state deck = 
		(* step 1: make sure deck has enough cards *)
		(if List.length !deck < (g_state.n_players+5) 
		then (reset_deck (); shuffle ())
		else ());
		(* step 2: deal cards to all players using pop *)
		(for i = 1 to g_state.n_players do
			g_state.hands <- 
				(g_state.n_players - i, [pop deck; pop deck])::(g_state.hands);
		done)(* ;
		(*step 3: MAYBE?!?!?! change state to BET_ONE*) 
		transition_state g_state *)

	let flop g_state = 
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		printc (g_state.cards_in_play)

	let turn g_state = 
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		printc (g_state.cards_in_play)

	let river g_state = 
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		printc (g_state.cards_in_play)

	let game_loop g_state = 
		(* Transition to the deal stage. Deal. *)
		transition_state g_state;
		deal g_state deck; 

		(* Bet after dealing. BET_ZERO state, transition to flop *)
		switch g_state;
		transition_state g_state;

		(* FOR NOW: Deal flop, then bet (taken care of in flop). Move on to turn *)
		flop g_state;
		switch g_state;

		(* Drop the turn, bet. *)
		turn g_state;
		switch g_state;

		(* Drop the river, bet. *)
		river g_state;
		switch g_state;

		(* Now, we are in the score stage. *)
		score g_state;
		transition_state g_state;

		(* At this point, we want to do a few thing while not touching the 
		 * state since the outter loop will need it to determine whether or 
		 * not the game has ended. Score needs to allocate chips to whoever won
		 * them. *)
		 reset_deck (); shuffle (); shuffle (); shuffle (); shuffle (); shuffle () 

	let init () = 
		try 
			(* Initial state is set to Start. There are no players, no cards,
       * no bets. Read in number of players. *)
      print_endline ("Welcome to Cornell Hold'Em! Enter the number of 
      players you'd like to play with.");
      let players = read_int () in
      !state.n_players <- players;

      (while (!state.current_st <> END) do game_loop !state done); 
      raise GameEnded

		with 
			| GameEnded -> ()
			| _ -> failwith "This is bad"

end 