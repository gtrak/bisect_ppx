module R = ReportUtils

let escape_line tab_size line offset points =
  let ofs = ref offset in
  let pts = ref points in
  let marker_if_any _content =
    match !pts with
    | (o, _n) :: tl when o = !ofs ->
        pts := tl
    | _ -> () in
  String.iter
    (fun ch ->
      let s =
        match ch with
        | '<' -> "&lt;"
        | '>' -> "&gt;"
        | '&' -> "&amp;"
        | '\t' -> String.make tab_size ' '
        | c -> Printf.sprintf "%c" c
      in
      marker_if_any s;
      incr ofs)
    line

let visited_or_unvisited = function
  | true, false -> "Visited"
  | false, true -> "Unvisited"
  | true, true -> "Some-Visted"
  | false, false -> "Unknown"

let output_lines verbose tab_size in_file out_channel resolver visited points =
  verbose (Printf.sprintf "Processing file '%s'..." in_file);
  match resolver in_file with
  | None -> verbose "... file not found"
  | Some resolved_in_file ->
    let cmp_content = Hashtbl.find points in_file
                      |> Bisect.Common.read_points' in
    verbose (Printf.sprintf "... file has %d points" (List.length cmp_content));
    let len = Array.length visited in
    let stats = ReportStat.make () in
    let pts = ref (List.map
                     (fun p ->
                        let nb =
                          if p.Bisect.Common.identifier < len then
                            visited.(p.Bisect.Common.identifier)
                          else
                            0 in
                        ReportStat.update stats (nb > 0);
                        (p.Bisect.Common.offset, nb))
                     cmp_content) in
    let in_channel = open_in resolved_in_file in
    (try
       let lines, _line_count =
         let rec read number acc =
           let start_ofs = pos_in in_channel in
           try
             let line = input_line in_channel in
             let end_ofs = pos_in in_channel in
             let before, after = R.split (fun (o, _) -> o < end_ofs) !pts in
             pts := after;
             let line' = escape_line tab_size line start_ofs before in
             let visited, unvisited =
               List.fold_left
                 (fun (v, u) (_, nb) -> ((v || (nb > 0)), (u || (nb = 0))))
                 (false, false)
                 before
             in
             read (number + 1) ((number, line', visited, unvisited)::acc)
           with End_of_file -> List.rev acc, number - 1
         in
         read 1 []
       in
       (* Line highlights. *)
       lines
       |> List.map (fun (_, _, visited, unvisited) ->
           Printf.sprintf "%s" (visited_or_unvisited (visited, unvisited)))
       |> String.concat ","
       |> Printf.sprintf "%s,\"%s\"\n" resolved_in_file
       |> output_string out_channel
       |> ignore
     with e ->
       close_in_noerr in_channel;
       raise e);
    close_in_noerr in_channel

let output ~verbose ~out_file ~tab_size ~resolver ~data ~points =
  let out_channel = open_out out_file in
  (try
     let header = "module name,line information\n" in
     output_string out_channel header;
     Hashtbl.iter (fun in_file visited ->
         output_lines verbose tab_size in_file out_channel resolver visited
           points)
       data
   with e ->
     close_out_noerr out_channel;
     raise e);
  close_out_noerr out_channel;
