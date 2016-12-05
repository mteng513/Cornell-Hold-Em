open GMain
open GdkKeysyms
open GMisc
open Gdk
open Graphics

module Image = Gdk.Image

let locale = GtkMain.Main.init ()

let imagearray (img: string) : int array array =
    Png.load_as_rgb24 img [] |> Graphic_image.array_of_image

 let clarksonAce = imagearray "CHoldEmImgs/Classic/c01.png"
 let clarkson2 = imagearray "CHoldEmImgs/Classic/c02.png"
 let clarkson3 = imagearray "CHoldEmImgs/Classic/c03.png"
 let clarkson4 = imagearray "CHoldEmImgs/Classic/c04.png"
 let clarkson5 = imagearray "CHoldEmImgs/Classic/c05.png"
 let clarksonKing = imagearray "CHoldEmImgs/Classic/c13.png"
 let table = imagearray "CHoldEmImgs/2-player-poker-table.png"
 let table1 = imagearray "CHoldEmImgs/1v1-poker-table.png"

  (* Draws table happenings. Takes in the new amount in
   * the pot, current number of opponents, cards in play,
        . *)
let draw_table amt opp cards s =
    if s = "close" then Graphics.close_graph () else
    Graphics.open_graph " 800x600";
    Graphics.draw_image (Graphics.make_image table1) 125 260;
    Graphics.draw_string ("CURRENT POT : " ^ (string_of_int amt) ^ "   OPPONENTS LEFT= " ^ (string_of_int opp));
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

let draw_cards lt rt wealth s =
    if s = "close" then Graphics.close_graph () else
  Graphics.open_graph " 308x250";
  Graphics.draw_image (Graphics.make_image lt) 0 35;
  Graphics.draw_image (Graphics.make_image rt) 154 35;
  Graphics.draw_string ("YOU HAVE" ^ (string_of_int wealth) ^ "DOLLAS MY DUDE")

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

let init_game i =
    let window = GWindow.window ~width:1280 ~height:480
                                ~title:"User Central" () in
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
    bet#connect#clicked ~callback: (fun () -> choose_bet ());
    let fold = GButton.button ~label:"FOLD"
          ~packing:hbox#add () in
    fold#connect#clicked ~callback: (fun () -> Graphics.close_graph ());
    let call = GButton.button ~label: "CALL"
          ~packing:hbox#add () in
    call#connect#clicked ~callback: (fun () -> draw_cards [|[||]|] [|[||]|] 0 "close");
    let check = GButton.button ~label:"CHECK"
          ~packing:hbox#add () in
    check#connect#clicked ~callback: (fun () -> prerr_endline "check nigga");
    let get_state = GButton.button ~label:"GET STATE"
          ~packing:hbox#add () in
    get_state#connect#clicked ~callback: (fun () -> draw_cards clarksonAce clarksonKing 9001 "");
    let show_table = GButton.button ~label: "SHOW TABLE"
          ~packing:hbox#add () in
    show_table#connect#clicked ~callback: (fun () -> draw_table 2000 i [clarkson2; clarkson3; clarkson4; clarkson5] "");

    window#add_accel_group accel_group;
    window#show ();
    Main.main ()

let draw_n_players () =
    let difwindow = GWindow.window ~width:640 ~height:480
                              ~title:"Choose Number of Opponents" () in
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

    let opp1 = GButton.button ~label:"vs 1 CPU"
          ~packing:difvbox#add () in
    opp1#connect#clicked ~callback: (fun () -> difwindow#destroy (); init_game 1);
    let opp2 = GButton.button ~label:"vs 2 CPUs"
          ~packing:difvbox#add () in
    opp2#connect#clicked ~callback: (fun () -> difwindow#destroy (); init_game 2);
    let opp3 = GButton.button ~label: "vs 3 CPUs"
          ~packing:difvbox#add () in
    opp3#connect#clicked ~callback: (fun () -> difwindow#destroy (); init_game 3);
    let opp4 = GButton.button ~label:"vs 4 CPUs"
          ~packing:difvbox#add () in
    opp4#connect#clicked ~callback: (fun () -> difwindow#destroy (); init_game 4);
    let opp5 = GButton.button ~label:"vs 5 CPUs"
          ~packing:difvbox#add () in
    opp5#connect#clicked ~callback: (fun () -> difwindow#destroy (); init_game 5);
    let opp6 = GButton.button ~label: "vs 6 CPUs"
          ~packing:difvbox#add () in
    opp6#connect#clicked ~callback: (fun () -> difwindow#destroy (); init_game 6);
    let opp7 = GButton.button ~label:"vs 7 CPUs"
          ~packing:difvbox#add () in
    opp7#connect#clicked ~callback: (fun () -> difwindow#destroy (); init_game 7);

    difwindow#add_accel_group accel_group;
    difwindow#show ();
    Main.main ()

let draw_difficulty () =
    let difwindow = GWindow.window ~width:640 ~height:480
                              ~title:"Choose Difficulty" () in
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

    let diff1 = GButton.button ~label:"Easy"
      ~packing:difvbox#add () in
    diff1#connect#clicked ~callback: (fun () -> difwindow#destroy (); draw_n_players ();
    prerr_endline "Medium is the only difficult supported at this time");
    let diff2 = GButton.button ~label:"Medium"
      ~packing:difvbox#add () in
    diff2#connect#clicked ~callback: (fun () -> difwindow#destroy (); draw_n_players ());
    let diff3 = GButton.button ~label: "Hard"
      ~packing:difvbox#add () in
    diff3#connect#clicked ~callback: (fun () -> difwindow#destroy (); draw_n_players ();
    prerr_endline "Medium is the only difficult supported at this time");

    difwindow#add_accel_group accel_group;
    difwindow#show ();
    Main.main ()

let main () =
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
    startbutton#connect#clicked ~callback: (fun () -> window#destroy ();
    draw_difficulty ());

    window#add_accel_group accel_group;
    window#show ();
    Main.main ()

let () = main ()