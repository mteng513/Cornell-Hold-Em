(* Ml file for opponent *)
open Types

module Opponent = struct 

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
  let b0_helper2 st card1 card2 = 
    match card1, card2 with
      | (c1, s1), (c2, s2) when (s1 = s2) -> 
        (match (suit_helper c1 c2) with 
          | (true, amt) -> (if Random.int 2700 mod 5 = 0 then 0 
            else (amt))
          | _ -> 0)

      | (c1, s1), (c2, s2) when (s1 <> s2) -> 
        (match (unsuit_helper c1 c2) with 
          | (true, amt) -> (if Random.int 2700 mod 3 = 0 then 0 
            else (amt))
          | _ -> (0)) 

      | _ -> Random.int 100

  (* Main helper function for the first betting round, with two cards. *)
  let bet_zero_helper st (hand : card list) = 
    match hand with 
      | (c1::c2::[]) -> b0_helper2 st c1 c2
      | _ -> failwith "Failure: Bet zero"

  (* Computes the max of 4 numbers. *)
  let four_max a b c d = 
    max (max a b) (max c d)

  (* Returns true if there is a flush or there is a high possibility 
   * of a flush. *)
  let flush lst = 
    let c1 = ref 0 in let c2 = ref 0 in let c3 = ref 0 in let c4 = ref 0 in 
    (for i = 0 to (List.length lst) - 1 do 
      match (snd (List.nth lst i)) with 
        | Clarkson -> c1 := !c1 + 1
        | Gries -> c2 := !c2 + 1
        | Dijkstra -> c3 := !c3 + 1
        | George -> c4 := !c4 + 1
      done);
    four_max !c1 !c2 !c3 !c4  
 
  (* General helper for bets after the first. *)
  let bet_helper st current_score cardlst = 

    (* Royal Flush. Bet Everything *)
    if (current_score >= 100000000) then (Random.int 750)

    (* Straight flush. Bet everything *)
    else if (current_score >= 20000005) then (Random.int 750)

    (* Four of a kind. Bet a good amount *)
    else if (current_score >= 19002003) then (Random.int 500)

    (* Full House. Bet a decent amount. *)
    else if (current_score >= 18002003) then (Random.int 300)

    (* Manual check for flush (if we are close, we should bet) *)
    else if ((flush cardlst) >= 4) then (Random.int 200)

    (* Straight. Bet a decent amount. *)
    else if (current_score >= 15000002) then (Random.int 150)

    (* 3 of a kind. Bet a decent amount *)
    else if (current_score >= 2000000) then (Random.int 100)

    (* 2 pair. Bet a little. *)
    else if (current_score >= 200000) then (Random.int 50)

    (* Pair. Bet a tiny bit. *)
    else if (current_score >= 2000) then (Random.int 20)

    else 0

    (* Sorts cards by RANK. *)
  let sort_cards (h:hand) : hand = 
    List.sort Pervasives.compare h



  let twofifty_bet st current_score cards = 

    (* Royal Flush. Bet Everything *)
    if (current_score >= 100000000) then 9999999999999 

    (* Straight flush. Bet everything *)
    else if (current_score >= 20000005) then 9999999999999 

    (* Four of a kind. Bet a good amount *)
    else if (current_score >= 19002003) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* Full House. Bet a decent amount. *)
    else if (current_score >= 18002003) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* Manual check for flush (if we are close, we should bet) *)
    else if ((flush cards) >= 4) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* Straight. Bet a decent amount. *)
    else if (current_score >= 15000002) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* 3 of a kind. Bet a decent amount *)
    else if (current_score >= 2000000) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* 2 pair. Bet a little. *)
    else if (current_score >= 200000) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* Pair. Bet a tiny bit. *)
    else if (current_score >= 2000) then (st.current_bet - (Array.get st.bets st.c_player)) 

    else 0

  let sixhun_bet st current_score cards = 

    (* Royal Flush. Bet Everything *)
    if (current_score >= 100000000) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* Straight flush. Bet everything *)
    else if (current_score >= 20000005) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* Four of a kind. Bet a good amount *)
    else if (current_score >= 19002003) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* Full House. Bet a decent amount. *)
    else if (current_score >= 18002003) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* Manual check for flush (if we are close, we should bet) *)
    else if ((flush cards) >= 4) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* Straight. Bet a decent amount. *)
    else if (current_score >= 15000002) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* 3 of a kind. Bet a decent amount *)
    else if (current_score >= 2000000) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* 2 pair. Bet a little. *)
    else if (current_score >= 200000) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* Pair. Bet a tiny bit. *)
    else if (current_score >= 2000) then 0

    else 0

  let thous_bet st current_score cards = 

    (* Royal Flush. Bet Everything *)
    if (current_score >= 100000000) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* Straight flush. Bet everything *)
    else if (current_score >= 20000005) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* Four of a kind. Bet a good amount *)
    else if (current_score >= 19002003) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* Full House. Bet a decent amount. *)
    else if (current_score >= 18002003) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* Manual check for flush (if we are close, we should bet) *)
    else if ((flush cards) >= 4) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* Straight. Bet a decent amount. *)
    else if (current_score >= 15000002) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* 3 of a kind. Bet a decent amount *)
    else if (current_score >= 2000000) then (st.current_bet - (Array.get st.bets st.c_player)) 

    (* 2 pair. Bet a little. *)
    else if (current_score >= 200000) then 0

    (* Pair. Bet a tiny bit. *)
    else if (current_score >= 2000) then 0

    else 0

  let bluff_bet st = (st.current_bet - (Array.get st.bets st.c_player)) 

  (* Secondary decision function for the second round of betting. *)
  let decide_two st score cards = 
    let current_bet = st.current_bet in 

    if current_bet < 100 then current_bet 
    else if current_bet < 250 then (twofifty_bet st score cards) 
    else if current_bet < 600 then sixhun_bet st score cards 
    else if current_bet < 1000 then thous_bet st score cards  
    else  bluff_bet st

  (* Main decision function for the opponent. *)
  let decide st score = 
    Random.self_init();
    let player_index = st.c_player in 
    let hands = st.hands in 
    match (List.nth hands player_index) with 
      | ((c1::c2::[])) -> let hand = (c1::c2::[]) in 

    let playcards = (sort_cards (hand @ st.cards_in_play)) in

    if (st.current_bet > 0) then decide_two st score playcards else 
      (match st.current_st with
        | BET_ZERO -> (bet_zero_helper st (sort_cards hand))
        | BET_ONE -> bet_helper st score playcards
        | BET_TWO -> bet_helper st score playcards
        | BET_THREE -> bet_helper st score playcards
        | _ -> 0)

      | _ -> failwith "Failure: bad hand"




end 