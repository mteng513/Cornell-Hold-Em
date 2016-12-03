open GMain
open GdkKeysyms
open GtkMisc

(* Ml file for the GUpoker *)
module GU_Poker = struct

  let clarksonAce = GMisc.image
    ~file:"../CHoldEmImgs/Classic/c01.png" ()
  let clarkson2 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/c02.png" ()
  let clarkson3 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/c03.png" ()
  let clarkson4 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/c04.png" ()
  let clarkson5 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/c05.png" ()
  let clarkson6 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/c06.png" ()
  let clarkson7 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/c07.png" ()
  let clarkson8 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/c08.png" ()
  let clarkson9 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/c09.png" ()
  let clarkson10 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/c10.png" ()
  let clarksonJack = GMisc.image
    ~file:"../CHoldEmImgs/Classic/c11.png" ()
  let clarksonQueen = GMisc.image
    ~file:"../CHoldEmImgs/Classic/c12.png" ()
  let clarksonKing = GMisc.image
    ~file:"../CHoldEmImgs/Classic/c13.png" ()

  let griesAce = GMisc.image
    ~file:"../CHoldEmImgs/Classic/d01.png" ()
  let gries2 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/d02.png" ()
  let gries3 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/d03.png" ()
  let gries4 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/d04.png" ()
  let gries5 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/d05.png" ()
  let gries6 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/d06.png" ()
  let gries7 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/d07.png" ()
  let gries8 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/d08.png" ()
  let gries9 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/d09.png" ()
  let gries10 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/d10.png" ()
  let griesJack = GMisc.image
    ~file:"../CHoldEmImgs/Classic/d11.png" ()
  let griesQueen = GMisc.image
    ~file:"../CHoldEmImgs/Classic/d12.png" ()
  let griesKing = GMisc.image
    ~file:"../CHoldEmImgs/Classic/d13.png" ()

  let hAce = GMisc.image
    ~file:"../CHoldEmImgs/Classic/h01.png" ()
  let h2 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/h02.png" ()
  let h3 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/h03.png" ()
  let h4 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/h04.png" ()
  let h5 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/h05.png" ()
  let h6 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/h06.png" ()
  let h7 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/h07.png" ()
  let h8 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/h08.png" ()
  let h9 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/h09.png" ()
  let h10 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/h10.png" ()
  let hJack = GMisc.image
    ~file:"../CHoldEmImgs/Classic/h11.png" ()
  let hQueen = GMisc.image
    ~file:"../CHoldEmImgs/Classic/h12.png" ()
  let hKing = GMisc.image
    ~file:"../CHoldEmImgs/Classic/h13.png" ()

  let georgeAce = GMisc.image
    ~file:"../CHoldEmImgs/Classic/s01.png" ()
  let george2 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/s02.png" ()
  let george3 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/s03.png" ()
  let george4 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/s04.png" ()
  let george5 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/s05.png" ()
  let george6 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/s06.png" ()
  let george7 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/s07.png" ()
  let george8 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/s08.png" ()
  let george9 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/s09.png" ()
  let george10 = GMisc.image
    ~file:"../CHoldEmImgs/Classic/s10.png" ()
  let georgeJack = GMisc.image
    ~file:"../CHoldEmImgs/Classic/s11.png" ()
  let georgeQueen = GMisc.image
    ~file:"../CHoldEmImgs/Classic/s12.png" ()
  let georgeKing = GMisc.image
    ~file:"../CHoldEmImgs/Classic/s13.png" ()

  let locale = GtkMain.Main.init ()

  (* Draw difficulty box *)
  let draw_difficulty () = ()
    (* let difwindow = GWindow.window ~width:320 ~height:240
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
    prerr_endline "Medium is the only difficult supported at this time"); *)


  (* Draw number of players box that will allow the
   * user to enter the number of players *)
  let draw_n_players () =
    ()
    (* let opp1 = GButton.button ~label:"vs 1 CPU"
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
    opp7#connect#clicked ~callback: (fun () -> init_game 7);  *)

  (* Sends the number of players to the GUI, indicating
   * that the user has started the game *)
  let init_game i =
    failwith "Unimplemented"
    (* let game_state = {current_st = START; cards_in_play =  }
    player_home (); *)

  let player_home () = failwith "Unimplemented"
    (* let bet = GButton.button ~label:"BET"
          ~packing:vbox#add () in
    bet#connect#clicked ~callback: (fun () -> ()(*input box and bet function*));
    let fold = GButton.button ~label:"FOLD"
          ~packing:vbox#add () in
    fold#connect#clicked ~callback: (fun () -> ()(*fold function*));
    let call = GButton.button ~label: "CALL"
          ~packing:vbox#add () in
    call#connect#clicked ~callback: (fun () -> ()(*tf is call*));
    let check = GButton.button ~label:"CHECK"
          ~packing:vbox#add () in
    check#connect#clicked ~callback: (fun () -> draw_cards ());
    let get_state = GButton.button ~label:"GET STATE"
          ~packing:vbox#add () in
    get_state#connect#clicked ~callback: (fun () -> ()(*state getter*));
    let show_table = GButton.button ~label: "SHOW TABLE"
          ~packing:vbox#add () in
    show_table#connect#clicked ~callback: (fun () -> ()(*show image of table*));  *)

  (* Draws players hand *)
  let draw_cards () =
    failwith "Unimplemented"
    (*Link Images of two cards side by side- with cash amount as label*)


  (* Draws cash box for player. Takes in the amount of
   * cash the player currently has. *)
  let draw_balance amt =
    failwith "Unimplemented"

  (* Draws pot amount. Takes in the new amount in
   * the pot. *)
  let draw_pot amt =
    failwith "Unimplemented"

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



    let window = GWindow.window ~width:320 ~height:240
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