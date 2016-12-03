(* Ml file for opponent *)
open Player
open Gengine

module Opponent = struct 

	open Game_Engine
	open Player

  type difficulty = One | Two | Three 

  (* Returns true, bet amt for SUITED cards *)
  let suit_helper (c1 : rank) (c2 : rank) = 
  	match (c1, c2) with 
  		| (l, r) when (l = r) ->
  			(match l with 
  				| Ace -> (true, (Random.int 800))
  				| King -> (true, (Random.int 775))
  				| Queen -> (true, (Random.int 760))
  				| Jack -> (true, (Random.int 725))
  				| Ten -> (true, (Random.int 675))
  				| Nine -> (true, (Random.int 550))
  				| Eight -> (true, (Random.int 475))
  				| Seven -> (true, (Random.int 400))
  				| Six -> (true, (Random.int 350))
  				| Five -> (true, (Random.int 300))
  				| Four -> (true, (Random.int 200))
  				| Three -> (true, (Random.int 125))
  				| Two -> (true, (Random.int 100)))

  		| l, r when (l <> r) -> 
  			(match (l, r) with 
  				| (King, Ace) -> (true, (Random.int 600))
  				| (Queen, Ace) -> (true, (Random.int 400))
  				| (Jack, Ace) -> (true, (Random.int 200))
  				| (Ten, Ace) -> (true, (Random.int 160))
  				| (Nine, Ace) -> (true, (Random.int 100))
  				| (Eight, Ace) -> (true, (Random.int 150))
  				| (Queen, King) -> (true, (Random.int 400))
  				| (Jack, King) -> (true, (Random.int 300))
  				| (Ten, King) -> (true, (Random.int 275))
  				| (Nine, King) -> (true, (Random.int 180))
  				| (Jack, Queen) -> (true, (Random.int 300))
  				| (Ten, Queen) -> (true, (Random.int 300))
  				| (Ten, Jack) -> (true, (Random.int 350))
  				| (Nine, Ten) -> (true, (Random.int 300))
  				| (Eight, Ten) -> (true, (Random.int 275))
  				| (Seven, Ten) -> (true, (Random.int 250))
  				| (Six, Ten) -> (true, (Random.int 200))
  				| (Eight, Nine) -> (true, (Random.int 260))
  				| (Seven, Nine) -> (true, (Random.int 180))
  				| (Six, Nine) -> (true, (Random.int 140))
  				| (Seven, Eight) -> (true, (Random.int 120))
  				| (Six, Eight) -> (true, (Random.int 100))
  				| (Six, Seven) -> (true, (Random.int 80))
  				| (Five, Seven) -> (true, (Random.int 60))
  				| (Five, Six) -> (true, (Random.int 50))
  				| (Four, Five) -> (true, Random.int 30)
  				| _ -> (true, 20))

  		| _ -> failwith "Bad cards"

  (* Returns true, bet amt based on the rank of the two UNSUITED cards. *)
  let unsuit_helper (c1 : rank) (c2 : rank) = 
  	match (c1, c2) with 
  		| (l, r) when (l = r) -> 
  			(match l with 
  				| Ace -> (true, (Random.int 400))
  				| King -> (true, (Random.int 375))
  				| Queen -> (true, (Random.int 360))
  				| Jack -> (true, (Random.int 325))
  				| Ten -> (true, (Random.int 375))
  				| Nine -> (true, (Random.int 350))
  				| Eight -> (true, (Random.int 375))
  				| Seven -> (true, (Random.int 300))
  				| Six -> (true, (Random.int 250))
  				| Five -> (true, (Random.int 200))
  				| Four -> (true, (Random.int 100))
  				| Three -> (false, 0)
  				| Two -> (false, 0))

  		| (l, r) when (l <> r) ->
  			(match (l, r) with 
  				| (King, Ace) -> (true, (Random.int 300))
  				| (Queen, Ace) -> (true, (Random.int 200))
  				| (Jack, Ace) -> (true, (Random.int 100))
  				| (Ten, Ace) -> (true, (Random.int 80))
  				| (Nine, Ace) -> (true, (Random.int 50))
  				| (Eight, Ace) -> (true, (Random.int 30))
  				| (Queen, King) -> (true, (Random.int 200))
  				| (Jack, King) -> (true, (Random.int 175))
  				| (Ten, King) -> (true, (Random.int 150))
  				| (Nine, King) -> (true, (Random.int 90))
  				| (Jack, Queen) -> (true, (Random.int 150))
  				| (Ten, Queen) -> (true, (Random.int 150))
  				| (Ten, Jack) -> (true, (Random.int 250))
  				| (Nine, Ten) -> (true, (Random.int 150))
  				| (Eight, Ten) -> (true, (Random.int 130))
  				| (Seven, Ten) -> (true, (Random.int 110))
  				| (Six, Ten) -> (true, (Random.int 100))
  				| (Eight, Nine) -> (true, (Random.int 130))
  				| (Seven, Nine) -> (true, (Random.int 115))
  				| _ -> (false, 0))

  		| _ -> failwith "Bad cards"
  	
 	(* Helper function for the first round of betting that separates
 	 * into whether the opponent has suited or unsuited cards, then
 	 * punts to the respective helper. *)
  let b0_helper2 card1 card2 = 
  	match card1, card2 with
  		| (c1, s1), (c2, s2) when (s1 = s2) -> 
  			(match (suit_helper c1 c2) with 
  				| (true, amt) -> (if Random.int 2700 mod 5 = 0 then check () 
  					else ignore (bet amt))
  				| _ -> check ())

  		| (c1, s1), (c2, s2) when (s1 <> s2) -> 
  			(match (unsuit_helper c1 c2) with 
  				| (true, amt) -> (if Random.int 2700 mod 3 = 0 then check () 
  					else ignore (bet amt))
  				| _ -> (if Random.int 25 mod 2 = 0 then fold () else check ())) 

  		| _ -> ignore (bet (Random.int 100))

  (* Main helper function for the first betting round, with two cards. *)
  let bet_zero_helper (hand : card list) = 
  	match hand with 
  		| (c1::c2::[]) -> b0_helper2 c1 c2
  		| _ -> failwith "Failure: Bet zero"

  (* Computes the max of 4 numbers. *)
  let four_max a b c d = 
  	let max = ref a in 
    if (b > !max) then 
        max := b else  
    if (c > !max) then 
        max := c else 
    if (d > !max) then 
        max := d
    else (failwith "Failure: four_max");
 		!max

 	(* Returns true if there is a flush or there is a high possibility 
 	 * of a flush. *)
  let flush lst = 
  	let c1 = ref 0 in let c2 = ref 0 in let c3 = ref 0 in let c4 = ref 0 in 
  	(for i = 0 to (List.length lst) do 
  		match (snd (List.nth lst i)) with 
  			| Clarkson -> c1 := !c1 + 1
  			| Gries -> c2 := !c2 + 1
  			| Dijkstra -> c3 := !c3 + 1
  			| George -> c4 := !c4 + 1
  		done);
  	four_max !c1 !c2 !c3 !c4  
 
  (* General helper for bets after the first. *)
  let bet_helper cardlst = 
  	if ((straight cardlst) > 0) then ignore (bet (Random.int 750))
  	else if ((flush cardlst) > 4) then ignore (bet (Random.int 500));

  	check ()

  (* Sorts cards by RANK. *)
  let sort_cards (h:hand) : hand = 
		List.sort Pervasives.compare h


	(* Returns maximum bet applicable given the cards in the hand. *)
	let max_bet cardlst = 
		if ((straight cardlst) > 0) then 750
  	else if ((flush cardlst) > 4) then 500
  	else 50 

	let twofifty_bet () = failwith "TODO"

	let sixhun_bet () = failwith "TODO"

	let thous_bet () = failwith "TODO"

	let bluff_bet () = failwith "TODO"

  (* Secondary decision function for the second round of betting. *)
  let decide_two () = 
  	let st = get_state () in 
  	let player_index = !st.c_player in 
  	let hands = !st.hands in
  	let current_bet = !st.current_bet in 

  	if current_bet < 100 then call () 
  	else if current_bet < 250 then twofifty_bet () 
  	else if current_bet < 600 then sixhun_bet () 
  	else if current_bet < 1000 then thous_bet () 
  	else  bluff_bet ()

	(* Main decision function for the opponent. *)
  let decide () = 
  	let st = get_state () in 
  	let player_index = !st.c_player in 
  	let hands = !st.hands in 

  	match (List.nth hands player_index) with 
  		| ((c1::c2::[])) -> let hand = (c1::c2::[]) in 

  		(match !st.current_st with
  			| BET_ZERO -> bet_zero_helper (sort_cards hand)
  			| BET_ONE -> bet_helper (sort_cards (hand @ !st.cards_in_play))
  			| BET_TWO -> bet_helper (sort_cards (hand @ !st.cards_in_play))
  			| BET_THREE -> bet_helper (sort_cards (hand @ !st.cards_in_play))
  			| _ -> ())

  		| _ -> failwith "Failure: bad hand"




end 