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

	(* [suit_to_string suit] takes in a suit [suit] and returns its
	 * string representation formatted for printing *)
	let suit_to_string (suit: suit) : string =
		match suit with 
			| Clarkson -> "Clarkson "
			| Gries -> "Gries "
			| Dijkstra -> "Dijkstra "
			| George -> "George "

	(* [card_to_string ele] takes in a card [ele] and returns its
	 * string representation formatted for printing *)
	let card_to_string (ele: card) : string = 
		match ele with  
			| (Two, suit) -> "Two of " ^ suit_to_string suit
			| (Three, suit) -> "Three of " ^ suit_to_string suit
			| (Four, suit) -> "Four of " ^ suit_to_string suit
			| (Five, suit) -> "Five of " ^ suit_to_string suit
			| (Six, suit) -> "Six of " ^ suit_to_string suit
			| (Seven, suit) -> "Seven of " ^ suit_to_string suit
			| (Eight, suit) -> "Eight of " ^ suit_to_string suit
			| (Nine, suit) -> "Nine of " ^ suit_to_string suit
			| (Ten, suit) -> "Ten of " ^ suit_to_string suit
			| (Jack, suit) -> "Jack of " ^ suit_to_string suit
			| (Queen, suit) -> "Queen of " ^ suit_to_string suit
			| (King, suit) -> "King of " ^ suit_to_string suit
			| (Ace, suit) -> "Ace of " ^ suit_to_string suit

	(* [formatted_print lst] takes in a card list [lst] and returns a
	 * formatted string containing info on all of the cards in the list *)
	let rec formatted_print (lst: card list) : string = 
		match lst with 
			| [] -> ""
			| h::t -> (card_to_string h) ^ formatted_print t

	(* [printc lst] take in a card list [lst] and converts it into a formatted
	 * string using formatted_print and then prints the string. Returns unit *)
	let printc lst =
		print_endline (formatted_print lst)

	(* [reset_deck] resets the deck back to normal, full, ordered 52-card 
	 * state, just like a freshly opened deck of cards. Returns a unit *)
	let reset_deck () : unit = deck := 
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

	(* [push lst ele] prepends an element [ele] onto a list ref [lst]. 
	 * Returns unit*)
	let push (lst: 'a list ref) (ele: 'a): unit = 
		lst := ele::!lst

	(* [pop lst] Pops an element off of a 'a list ref [lst] and returns that
	 * popped element of type 'a *)
	let pop (lst: 'a list ref) : 'a = 
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
	
	(* [sort_card hand] takes in a hand [hand] and returns a hand, sorted using
	 * List.sort Pervasives.compare on the hand *)
	let sort_cards (h:hand) : hand = 
		List.sort Pervasives.compare h

	(* [filter_suit hand i] takes in a hand [hand] and an int [i] and filters
	 * out all cards from [hand] of number [i]. Returns the filtered hand *)
	let filter_suit (hand: hand) (i: int) : hand = 
	List.(hand |> filter (fun x -> snd x = snd(List.nth hand i)))

	(* [flush hand] takes in a hand [hand],
	 * Returns the sorted card list, in ascending order of rank,
	 * containing the flush if there is one (may exceed 5 cards if 6 or all cards have same suit).
	 * Otherwise, returns the empty list.
	 *)
	let flush_hand (hand: hand) : hand = 
		let h = sort_cards hand in 
		if ((filter_suit h 0) |> List.length) >= 5 then 
			filter_suit h 0
		else if ((filter_suit h 1) |> List.length) >= 5 then 
			filter_suit h 1
		else if ((filter_suit h 2) |> List.length) >= 5 then 
			filter_suit h 2
		else 
			[]

	let flush (hand: hand) : int =
		match flush_hand hand with
		| a, b, c, d, e -> e * 1000000000000 + d * 10000000000 + c * 100000000 + b * 1000000 + a * 10000
		| _ -> 0


	(* BEGIN STRAIGHT CALC *)
	(* [make_enum_rank rank] takes in [rank] of type rank and returns the int
	 * that corresponds to that rank *)
	let make_enum_rank (rank: rank) : int = 
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

	(* [make_enum_hand hand acc] Returns sorted an int list in ascending order.
	 * Takes in hand [h] and the int list accumulator acc, generally designed
	 * to take in an empty list that accumulates ints. Returns that int list *)
	let rec make_enum_hand (h: hand) (acc: int list) : int list = 
		match h with 
		|[] -> List.sort Pervasives.compare acc
		|(rank, suit)::t -> make_enum_hand t (make_enum_rank rank::acc)

	(* [straight_hand hand pat acc] returns the sorted straight hand, takes in: 
	 * [hand] of type hand which is a 7 card hand including the dealt cards and 
	 * the cards in play, [pat] of type int that indicates which pattern of a
	 * straight the hand is, and an accumulator [acc] of type hand that 
	 * recursively accumulates the cards in order from least to greatest *)
	let rec straight_hand (hand: hand) (pat: int) (acc: hand): hand =
		let len = List.length acc in
		match pat with
		| 1 when len < 5 -> 
					straight_hand hand pat ((List.nth hand (6 - len)) :: acc)
		| 2 when len < 5 -> 
					straight_hand hand pat ((List.nth hand (6 - len)) :: acc)
		| 3 when len < 5 -> 
					straight_hand hand pat ((List.nth hand (4 - len)) :: acc)
		| 4 -> (List.nth hand 0)::(List.nth hand 1)::(List.nth hand 2)
				::(List.nth hand 3)::(List.nth hand 6)::[]
		| _ -> acc
	
	(* [are_consec a b c d e] returns a bool and takes in 5 ints, [a], [b],  
	 * [c], [d], and [e] and checks if they are consecutive, meaning 
	 * consecutively 1 more than the last. If so, then returns true, 
	 * else returns false *)
	let are_consec (a:int) (b:int) (c:int) (d:int) (e:int) : bool =
		if (a+1=b) && (b+1=c) && (c+1=d) && (d+1=e) then true else false  
	
	let remove_dup_rank e l =
  		let rec removing l acc = match l with
    		| [] -> List.rev acc
	    	| h::t when fst e = fst h -> removing t acc
	    	| h::t -> removing t (h::acc)
	  	in removing l []
	
	let remove_dups_ranks l =
	    let rec removing l acc = match l with
	        | [] -> List.rev acc
	        | h::t -> removing (remove_dup_rank h t) (h::acc)
	    in removing l []
	
	let remove_elt e l =
    	let rec go l acc = match l with
	    	| [] -> List.rev acc
	    	| x::xs when e = x -> go xs acc
	    	| x::xs -> go xs (x::acc)
    	in go l []

	let remove_duplicates l =
	    let rec go l acc = match l with
		    | [] -> List.rev acc
		    | x :: xs -> go (remove_elt x xs) (x::acc)
	    in go l []

	let rec remove_nonconsec l e acc =
		match l with
		| [] -> acc
		| h::[] -> acc @ [h]
		| h1::h2::t -> if h1 = h2 - 1 || e = h1 -1 then remove_nonconsec (h2::t) h1 (acc @ [h1])
								else remove_nonconsec (h2::t) h1 acc

	(* [straight hand] takes in a [hand] of type hand and returns the 
	 * hand of the 5 cards making up the straight, sorted lowest to 
	 * highest *)
	let straight (hand: hand) : int = 
		let len = List.length hand - 1
		let contains_ace = make_enum_rank (List.nth hand len) = 14 in
		let card_values = remove_duplicates (make_enum_hand hand []) in
		let consec_values = remove_nonconsec card_values in
		match consec_values with
		| _::_::10::11::12::13::14::[] 
					when flush hand > 0 -> 100000000000000000000 (* straight_hand hand 0 [] *)
		| _::10::11::12::13::14::[] 
					when flush hand > 0 -> 100000000000000000000 (* straight_hand hand 0 [] *)
		| 10::11::12::13::14::[] when flush hand > 0 -> 100000000000000000000 (* straight_hand hand 0 [] *)
		| _::_::a::b::c::d::e::[] when are_consec a b c d e -> 10000000000 * e (* straight_hand hand 1 [] *)
		| _::a::b::c::d::e::[] when are_consec a b c d e -> 10000000000 * e (* straight_hand hand 1 [] *)
		| a::b::c::d::e::[] when are_consec a b c d e -> 10000000000 * e (* straight_hand hand 1 [] *)
		| 2::3::4::5::_::_::_::[] when contains_ace -> 50000000000 (* when List.nth hand len -> straight_hand hand 4 [] *)
		| 2::3::4::5::_::_::[] when contains_ace -> 50000000000(* when List.nth hand len -> straight_hand hand 4 [] *)
		| 2::3::4::5::[] when contains_ace -> 50000000000 (* when List.nth hand len -> straight_hand hand 4 [] *)
		| _ -> []

	(* BEGIN 4Kind 3Kind PAIR CALC *)
	let filter_rank hand i = 
		List.(hand |> filter (fun x -> fst x = fst(List.nth hand i)))

	(* [four_kind hand] *)
	let rec four_kind (hand: hand) = 
		let h = List.rev (sort_cards hand) in
		if (List.length h < 4) then 
			[]
		else 
			match h with 
			|[]-> []
			|h'::t-> 
				if (filter_rank h 0 |> List.length) = 4 then filter_rank h 0 
				else four_kind t



		(* [three_kind hand] *)
	let rec three_kind (hand: hand) = 
		let h = List.rev (sort_cards hand) in
		if (List.length h < 3) then 
			[]
		else 
			match h with 
			|[]-> []
			|_::t-> 
				if (filter_rank h 0 |> List.length) = 3 then filter_rank h 0 
				else three_kind t

	(* [pair hand] *)
	let rec pair (hand: hand) = 
		let h = List.rev (sort_cards hand) in
		if (List.length h < 2) then 
			[]
		else 
			match h with 
			|[]-> []
			|h'::t-> 
				if (filter_rank h 0 |> List.length) = 2 then filter_rank h 0 
				else pair t

	(* run pair then pair again  *)
	let two_pair (hand: hand) = 
		let h = List.rev (sort_cards hand) in
		if not (pair h = []) then 
			pair h @ pair (List.(h |> filter (fun x -> fst x != fst(List.hd h))))
		else 
			[]

	(* full_house [hand] *)
	let full_house (hand: hand) = 
		let h = List.rev (sort_cards hand) in 
		if not (three_kind h = []) then 
			three_kind h @ pair (List.(h |> filter (fun x -> fst x != fst(List.hd h))))
		else 
			[]



    (* [score g_state] takes in the global_state [g_state] (POTENTIALLY NEEDS MORE INPUTS)
     * and updates the winning players scores with the points they won from the
 	 * pot. Returns a unit *)
	let score (g_state : global_state) : unit = 
		match g_state.players_in with
		| [|a; b; c; d; e; f; g; h|] -> () 
		| _ -> ()

		

		(* case 2:  *)

	(* [transition_state g_state] takes in the global_state [g_state] and 
	 * transitions its current_st property to the next current_st of type
	 * current_state. Returns unit*)
	let transition_state (g_state : global_state) : unit = 
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

	(* [mutable_incr i] increments an int ref [i], returns unit*)
	let mutable_incr (i: int ref) : unit = 
		i := !i + 1

	(* [get_state ()] returns the global_state reference, state *)
	let get_state () : global_state ref = state 

	(* [signal_bet g_state] signals bets to the players. 
	 * Takes in the current_st [g_state], makes necessary updates to g_state,
	 * and then returns unit *)
	let rec signal_bet (g_state : global_state) (* current_player *) = 
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

	(* [index_in_array a e index_acc] takes in array [a], element [e], and the
	 * index accumulator int [index_acc]. returns the index of element e in 
	 * array a. The index_acc should start at 0/the desired starting point *)
	let rec index_in_array (a: 'a array) (e: 'a) (index_acc: int) : int =
		if a.(index_acc) = e 
		then index_acc 
		else index_in_array a e (index_acc + 1)
	
	(* [max_of_array bets] takes in an array [bets] and returns the greatest
	 * array element value *)
	let rec max_of_array (bets: 'a array) : 'a =
		let len = Array.length bets in
		match bets with
		| [||] -> 0
		| bets when len = 1 -> Array.get bets 0
		| bets when len > 0 -> 
			let x = Array.get bets 0 in
			let y = max_of_array (Array.sub bets 1 (len - 1)) in
			if x > y then x else y
		| _ -> failwith "wtf is going on"


	(* [index_of_max bets] gets the index of the max element in an int array. 
	 * Takes in the int array [bets] and returns int index of the greatest 
	 * element in the array *)
	let rec index_of_max (bets: int array) : int =
		let max_element = max_of_array bets in
		index_in_array bets max_element 0 

	(* [switch g_state] is called in after deal and each bet stage to signal bets to
	 * all the players. Takes in global_state [g_state], returns unit*)
	let switch (g_state : global_state) : unit = 
		match g_state.current_st with 
			| BET_ZERO | BET_ONE | BET_TWO | BET_THREE ->
				(while not (g_state.c_player = index_of_max g_state.bets) do 
					signal_bet g_state; 
					g_state.c_player <- g_state.c_player + 1; done);

				g_state.c_player <- 0;
				transition_state g_state;
			| _ -> failwith "Bad state"

	(* [deal g_state deck] takes in the current_st [g_state] and the current 
	 * deck ref [deck] and updates the state with the hands. returns unit *)
	let deal (g_state : global_state) (deck: deck ref) : unit = (* ()
		let deal g_state deck =  *)
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

	(* [flop g_state] takes in the global_state [g_state] and updates it with
	 * the flop stage of the game. it draws and plays the 3 cards revealed in 
	 * the flop stage. Returns unit *)
	let flop (g_state : global_state) : unit = 
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		printc (g_state.cards_in_play)

	(* [turn g_state] takes in the global_state [g_state] and updates it with
	 * the turn stage of the game. it draws and plays the card revealed in 
	 * the turn stage. Returns unit *)
	let turn (g_state : global_state) : unit = 
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		printc (g_state.cards_in_play)

	(* [river g_state] takes in the global_state [g_state] and updates it with
	 * the river stage of the game. it draws and plays the card revealed in 
	 * the river stage. Returns unit *)
	let river (g_state : global_state) : unit = 
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		printc (g_state.cards_in_play)

	(* [game_loop g_state] takes in the global_state [g_state] and updates it
	 * as the game goes along. Terminates when a player has won the game
	 *  *)
	let rec game_loop (g_state : global_state) : unit = 
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