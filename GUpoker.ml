open GMain
open GdkKeysyms
open GtkMisc
open Gengine
open Graphics

(* Ml file for the GUpoker *)
module GU_Poker = struct

  (*Make 2D Color Array out of png file*)
  let imagearray (img: string) : int array array =
  Png.load_as_rgb24 img [] |> Graphic_image.array_of_image;

  let clarksonAce = imagearray "CHoldEmImgs/Classic/c01.png"
  let clarkson2 = imagearray "CHoldEmImgs/Classic/c02.png"
  let clarkson3 = imagearray "CHoldEmImgs/Classic/c03.png"
  let clarkson4 = imagearray "CHoldEmImgs/Classic/c04.png"
  let clarkson5 = imagearray "CHoldEmImgs/Classic/c05.png"
  let clarkson6 = imagearray "CHoldEmImgs/Classic/c06.png"
  let clarkson7 = imagearray "CHoldEmImgs/Classic/c07.png"
  let clarkson8 = imagearray "CHoldEmImgs/Classic/c08.png"
  let clarkson9 = imagearray "CHoldEmImgs/Classic/c09.png"
  let clarkson10 = imagearray "CHoldEmImgs/Classic/c10.png"
  let clarksonJack = imagearray "CHoldEmImgs/Classic/c11.png"
  let clarksonQueen = imagearray "CHoldEmImgs/Classic/c12.png"
  let clarksonKing = imagearray "CHoldEmImgs/Classic/c13.png"

  let griesAce = imagearray "CHoldEmImgs/Classic/d01.png"
  let gries2 = imagearray "CHoldEmImgs/Classic/d02.png"
  let gries3 = imagearray "CHoldEmImgs/Classic/d03.png"
  let gries4 = imagearray "CHoldEmImgs/Classic/d04.png"
  let gries5 = imagearray "CHoldEmImgs/Classic/d05.png"
  let gries6 = imagearray "CHoldEmImgs/Classic/d06.png"
  let gries7 = imagearray "CHoldEmImgs/Classic/d07.png"
  let gries8 = imagearray "CHoldEmImgs/Classic/d08.png"
  let gries9 = imagearray "CHoldEmImgs/Classic/d09.png"
  let gries10 = imagearray "CHoldEmImgs/Classic/d10.png"
  let griesJack = imagearray "CHoldEmImgs/Classic/d11.png"
  let griesQueen = imagearray "CHoldEmImgs/Classic/d12.png"
  let griesKing = imagearray "CHoldEmImgs/Classic/d13.png"

  let hAce = imagearray "CHoldEmImgs/Classic/h01.png"
  let h2 = imagearray "CHoldEmImgs/Classic/h02.png"
  let h3 = imagearray "CHoldEmImgs/Classic/h03.png"
  let h4 = imagearray "CHoldEmImgs/Classic/h04.png"
  let h5 = imagearray "CHoldEmImgs/Classic/h05.png"
  let h6 = imagearray "CHoldEmImgs/Classic/h06.png"
  let h7 = imagearray "CHoldEmImgs/Classic/h07.png"
  let h8 = imagearray "CHoldEmImgs/Classic/h08.png"
  let h9 = imagearray "CHoldEmImgs/Classic/h09.png"
  let h10 = imagearray "CHoldEmImgs/Classic/h10.png"
  let hJack = imagearray "CHoldEmImgs/Classic/h11.png"
  let hQueen = imagearray "CHoldEmImgs/Classic/h12.png"
  let hKing = imagearray "CHoldEmImgs/Classic/h13.png"

  let georgeAce = imagearray "CHoldEmImgs/Classic/s01.png"
  let george2 = imagearray "CHoldEmImgs/Classic/s02.png"
  let george3 = imagearray "CHoldEmImgs/Classic/s03.png"
  let george4 = imagearray "CHoldEmImgs/Classic/s04.png"
  let george5 = imagearray "CHoldEmImgs/Classic/s05.png"
  let george6 = imagearray "CHoldEmImgs/Classic/s06.png"
  let george7 = imagearray "CHoldEmImgs/Classic/s07.png"
  let george8 = imagearray "CHoldEmImgs/Classic/s08.png"
  let george9 = imagearray "CHoldEmImgs/Classic/s09.png"
  let george10 = imagearray "CHoldEmImgs/Classic/s10.png"
  let georgeJack = imagearray "CHoldEmImgs/Classic/s11.png"
  let georgeQueen = imagearray "CHoldEmImgs/Classic/s12.png"
  let georgeKing = imagearray "CHoldEmImgs/Classic/s13.png"

  let locale = GtkMain.Main.init ()

  (* Draw difficulty box *)
  let draw_difficulty () =
    let difwindow = GWindow.window ~width:640 ~height:480
                              ~title:"Choose Difficulty" () in
    let difvbox = GPack.vbox ~packing:difwindow#add () in
    difwindow#connect#destroy ~callback:Main.quit;

    (* Menu bar *)
    let menubar = GMenu.menu_bar ~packing:vbox#pack () in
    let factory = new GMenu.factory menubar in
    let accel_group = factory#accel_group in
    let file_menu = factory#add_submenu "File" in

    (* File menu *)
    let factory = new GMenu.factory file_menu ~accel_group in
    factory#add_item "Quit" ~key:_Q ~callback: Main.quit;


    let diff1 = GButton.button ~label:"Easy"
          ~packing:vbox#add () in
    diff1#connect#clicked ~callback: (fun () -> draw_n_players ();
    prerr_endline "Medium is the only difficult supported at this time");
    let diff2 = GButton.button ~label:"Medium"
          ~packing:vbox#add () in
    diff2#connect#clicked ~callback: (fun () -> draw_n_players ());
    let diff3 = GButton.button ~label: "Hard"
          ~packing:vbox#add () in
    diff3#connect#clicked ~callback: (fun () -> draw_n_players ();
    prerr_endline "Medium is the only difficult supported at this time");


  (* Draw number of players box that will allow the
   * user to enter the number of players *)
  let draw_n_players () =

    let opp1 = GButton.button ~label:"vs 1 CPU"
          ~packing:vbox#add () in
    opp1#connect#clicked ~callback: (fun () -> init_game 1);
    let opp2 = GButton.button ~label:"vs 2 CPUs"
          ~packing:vbox#add () in
    opp2#connect#clicked ~callback: (fun () -> init_game 2);
    let opp3 = GButton.button ~label: "vs 3 CPUs"
          ~packing:vbox#add () in
    opp3#connect#clicked ~callback: (fun () -> init_game 3);
    let opp4 = GButton.button ~label:"vs 4 CPUs"
          ~packing:vbox#add () in
    opp4#connect#clicked ~callback: (fun () -> init_game 4);
    let opp5 = GButton.button ~label:"vs 5 CPUs"
          ~packing:vbox#add () in
    opp5#connect#clicked ~callback: (fun () -> init_game 5);
    let opp6 = GButton.button ~label: "vs 6 CPUs"
          ~packing:vbox#add () in
    opp6#connect#clicked ~callback: (fun () -> init_game 6);
    let opp7 = GButton.button ~label:"vs 7 CPUs"
          ~packing:vbox#add () in
    opp7#connect#clicked ~callback: (fun () -> init_game 7);

  (* Sends the number of players to the GUI, indicating
   * that the user has started the game *)
  let init_game i =
   (* Engine.init (); *)
    player_home ();

  let rec choose_bet () =
    let x = ref 0 in
    let difwindow = GWindow.window ~width:640 ~height:480
                              ~title:"Set Bet" () in
    let difvbox = GPack.vbox ~packing:difwindow#add () in
    difwindow#connect#destroy ~callback:Main.quit;

    (* Menu bar *)
    let menubar = GMenu.menu_bar ~packing:difvbox#pack () in
    let factory = new GMenu.factory menubar in
    let accel_group = factory#accel_group in
    let file_menu = factory#add_submenu "File" in

      (* File menu *)
    let factory = new GMenu.factory file_menu ~accel_group in
    factory#add_item "Quit" ~key:_Q ~callback: Main.quit;

    let opp1 = GButton.button ~label:"add 1"
          ~packing:difvbox#add () in
    opp1#connect#clicked ~callback: (fun () -> x := !x + 1);
    let opp2 = GButton.button ~label:"add 5"
          ~packing:difvbox#add () in
    opp2#connect#clicked ~callback: (fun () -> x := !x + 5);
    let opp3 = GButton.button ~label:"add 10"
          ~packing:difvbox#add () in
    opp3#connect#clicked ~callback: (fun () -> x := !x + 10);
    let opp4 = GButton.button ~label:"add 50"
          ~packing:difvbox#add () in
    opp4#connect#clicked ~callback: (fun () -> x := !x + 50);
    let opp5 = GButton.button ~label:"add 100"
          ~packing:difvbox#add () in
    opp5#connect#clicked ~callback: (fun () -> x := !x + 100);
    let opp6 = GButton.button ~label:"all in"
          ~packing:difvbox#add () in
    opp6#connect#clicked ~callback: (fun () -> x := !x + 100000);
    let opp7 = GButton.button ~label:"send bet"
          ~packing:difvbox#add () in
    opp7#connect#clicked ~callback: (fun () -> prerr_endline ("YOU BET " ^ string_of_int !x ^ "DOLLARS MY DUDE");
    difwindow#destroy ());

    difwindow#add_accel_group accel_group;
    difwindow#show ();
    Main.main ()


  (* Draws players hand *)
  let draw_cards lt rt=
    (*Link Images of two cards side by side- with cash amount as label*)
    let card_image ca = match ca with
    | (Ace, Clarkson) -> clarksonAce
    | (Two, Clarkson) -> clarkson2
    | (Three, Clarkson) -> clarkson3
    | (Four, Clarkson)  -> clarkson4
    | (Five, Clarkson) -> clarkson5
    | (Six, Clarkson) -> clarkson6
    | (Seven, Clarkson) -> clarkson7
    | (Eight, Clarkson) -> clarkson8
    | (Nine, Clarkson) -> clarkson9
    | (Ten, Clarkson) -> clarkson10
    | (Jack, Clarkson) -> clarksonJack
    | (Queen, Clarkson) -> clarksonQueen
    | (King, Clarkson) -> clarksonKing
    | (Ace, Gries) -> griesAce
    | (Two, Gries) -> gries2
    | (Three, Gries) -> gries3
    | (Four, Gries) -> gries4
    | (Five, Gries) -> gries5
    | (Six, Gries) -> gries6
    | (Seven, Gries) -> gries7
    | (Eight, Gries) -> gries8
    | (Nine, Gries) -> gries9
    | (Ten, Gries) -> gries10
    | (Jack, Gries) -> griesJack
    | (Queen, Gries) -> griesQueen
    | (King, Gries) -> griesKing
    | (Ace, Dijkstra) -> hAce
    | (Two, Dijkstra) -> h2
    | (Three, Dijkstra) -> h3
    | (Four, Dijkstra) -> h4
    | (Five, Dijkstra) -> h5
    | (Six, Dijkstra) -> h6
    | (Seven, Dijkstra) -> h7
    | (Eight, Dijkstra) -> h8
    | (Nine, Dijkstra) -> h9
    | (Ten, Dijkstra) -> h10
    | (Jack, Dijkstra) -> hJack
    | (Queen, Dijkstra) -> hQueen
    | (King, Dijkstra) -> hKing
    | (Ace, George) -> georgeAce
    | (Two, George) -> george2
    | (Three, George) -> george3
    | (Four, George) -> george4
    | (Five, George) -> george5
    | (Six, George) -> george6
    | (Seven, George) -> george7
    | (Eight, George) -> george8
    | (Nine, George) -> george9
    | (Ten, George) -> george10
    | (Jack, George) -> georgeJack
    | (Queen, George) -> georgeQueen
    | (King, George) -> georgeKing
  in let leftcard = card_image lt in let rightcard = card_image rt in
  Graphics.open_graph " 308x213";
  Graphics.draw_image (Graphics.make_image lt) 0 0;
  Graphics.draw_image (Graphics.make_image rt) 154 0

  (* Draws table happenings. Takes in the new amount in
   * the pot, current number of opponents, cards in play,
        . *)
let draw_table amt opp cards =
    Graphics.open_graph " 800x600";
    Graphics.draw_image (Graphics.make_image table1) 125 260;
    Graphics.draw_string ("CURRENT POT : " ^
      (string_of_int amt) ^ "   OPPONENTS LEFT= " ^ (string_of_int opp));
    match cards with
    | h1::h2::h3::h4::h5::[] ->
    Graphics.draw_image (Graphics.make_image h1) 0 25;
    Graphics.draw_image (Graphics.make_image h2) 160 25;
    Graphics.draw_image (Graphics.make_image h3) 320 25;
    Graphics.draw_image (Graphics.make_image h4) 480 25;
    Graphics.draw_image (Graphics.make_image h5) 640 25
    | h1::h2::h3::h4::[] -> Graphics.draw_image (Graphics.make_image h1) 0 25;
    Graphics.draw_image (Graphics.make_image h2) 160 25;
    Graphics.draw_image (Graphics.make_image h3) 320 25;
    Graphics.draw_image (Graphics.make_image h4) 480 25
    | h1::h2::h3::[] -> Graphics.draw_image (Graphics.make_image h1) 0 25;
    Graphics.draw_image (Graphics.make_image h2) 160 25;
    Graphics.draw_image (Graphics.make_image h3) 320 25
    | _ -> ()

  let player_home () =
    let window = GWindow.window ~width:640 ~height:480
                                ~title:"Cornell Hold Em" () in
    let hbox = GPack.hbox ~packing:window#add () in
    window#connect#destroy ~callback:Main.quit;

    (* Menu bar *)
    let menubar = GMenu.menu_bar ~packing:hbox#pack () in
    let factory = new GMenu.factory menubar in
    let accel_group = factory#accel_group in
    let file_menu = factory#add_submenu "File" in

    (* File menu *)
    let factory = new GMenu.factory file_menu ~accel_group in
    factory#add_item "Quit" ~key:_Q ~callback: Main.quit;

    let bet = GButton.button ~label:"BET"
          ~packing:hbox#add () in
    bet#connect#clicked ~callback: (fun () -> prerr_endline "(*input box and bet function*))");
    let fold = GButton.button ~label:"FOLD"
          ~packing:hbox#add () in
    fold#connect#clicked ~callback: (fun () -> prerr_endline "(*fold function*))");
    let call = GButton.button ~label: "CALL"
          ~packing:hbox#add () in
    call#connect#clicked ~callback: (fun () -> prerr_endline "(*tf is call*))");
    let check = GButton.button ~label:"CHECK"
          ~packing:hbox#add () in
    check#connect#clicked ~callback: (fun () -> prerr_endline "draw_cards ())");
    let get_state = GButton.button ~label:"GET STATE"
          ~packing:hbox#add () in
    get_state#connect#clicked ~callback: (fun () -> prerr_endline "(*state getter*))");
    let show_table = GButton.button ~label: "SHOW TABLE"
          ~packing:hbox#add () in
    show_table#connect#clicked ~callback: (fun () -> prerr_endline "(*show image of table*))");

    window#add_accel_group accel_group;
    window#show ();
    Main.main ()


  (* Indicates the player has folded *)
  let draw_fold () =
    failwith "Unimplemented"

  (* Draws the Flop *)
  let draw_flop () =
    failwith "Unimplemented"

  (* Draws the turn *)
  let draw_turn () =
    failwith "Unimplemented"

  (* Draws the river *)
  let draw_river () =
    failwith "Unimplemented"

  (* Draws the win box if you win *)
  let draw_w () =
    failwith "Unimplemented"

  (* Draws lose box if you lose *)
  let draw_l () =
    failwith "Unimplemented"

  (* Draws win if you win the whole game *)
  let draw_win () =
    failwith "Unimplemented"

  (* Draws lose if you lose the whole game *)
  let draw_lose () =
    failwith "Unimplemented"

  (* Draws main window for the game *)
  let draw_start () =

    let window = GWindow.window ~width:640 ~height:480
                                ~title:"Cornell Hold Em" () in
    let vbox = GPack.vbox ~packing:window#add () in
    window#connect#destroy ~callback:Main.quit;

    (* Menu bar *)
    let menubar = GMenu.menu_bar ~packing:vbox#pack () in
    let factory = new GMenu.factory menubar in
    let accel_group = factory#accel_group in
    let file_menu = factory#add_submenu "File" in

    (* File menu *)
    let factory = new GMenu.factory file_menu ~accel_group in
    factory#add_item "Quit" ~key:_Q ~callback: Main.quit;

    let startbutton = GButton.button ~label:"Start"
          ~packing:vbox#add () in
    startbutton#connect#clicked ~callback: (fun () -> draw_difficulty ());

    window#add_accel_group accel_group;
    window#show ();
    Main.main ()


end