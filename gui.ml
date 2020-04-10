open Curses

let get_row () =
  let (y, x) = getyx (stdscr ()) in
  y

let get_col () =
  let (y, x) = getyx (stdscr ()) in
  x

let center row title =
  begin
  let len = String.length title in
  let (y, x) = getmaxyx (stdscr ()) in
  let indent = (x - len) / 2 in
  mvaddstr row indent title |> ignore;
  refresh () |> ignore;
  end

let init () =
  begin
    initscr () |> ignore;
    center 0 "Wahrheitstabellen-Loeser v1" |> ignore;
    move 2 0 |> ignore;
    refresh () |> ignore;
  end

let await_end () =
  begin
  ignore (getch ());
  ignore (endwin ());
  end

let eval str =
  (match Base.String.strip ~drop:(fun char -> Char.equal char ' ' || Char.equal char (char_of_int 0)) str with
  | "exit" ->  Caml.exit 0;
  | "help" -> addstr "Geben sie" |> ignore
  | str -> addstr str |> ignore);
  move (get_row () + 1) 0 |> ignore

let rec repl () =
  begin
    addstr "> " |> ignore;
    let str = String.make 100 ' ' in
    getstr str |> ignore;
    let str = String.trim str in
    eval str;
    repl ();
  end


let () =
  init ();
  repl ();
  await_end ();
