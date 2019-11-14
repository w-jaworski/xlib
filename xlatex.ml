(* Copyright (c) 2006-2017, Wojciech Jaworski
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS "AS IS" AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
*)

let escape_string s =
  let l = Xlist.rev_map (Xunicode.utf8_chars_of_utf8_string s) (function
        "_" -> "\\_"
      | "#" -> "\\#"
      | "%" -> "\\%"
      | "&" -> "\\&"
      | "\"" -> "''"
      | "\\" -> "b/"
      | "{" -> "\\{"
      | "}" -> "\\}"
      | "µ" -> "{}$\\mu${}"
      | "⟨" -> "{}$\\langle${}"
      | "⟩" -> "{}$\\rangle${}"
      | c -> c) in
  String.concat "" (List.rev l)

let tikz_header =
  String.concat "\n" [
    "\\documentclass{article}";
    "\\usepackage{a4wide}";
    "\\usepackage{amsmath}";
    "\\usepackage{amssymb}";
    "\\usepackage[T1]{fontenc}";
    "\\usepackage[utf8]{inputenc}";
    "\\usepackage[polish]{babel}";
    "\\usepackage{tikz}";
    "\\usepackage{tikz-qtree}";
    "\\usetikzlibrary{conceptgraph}";
    "\\parindent 0pt";
    "\\parskip 4pt";
    "\\begin{document}\n\n"]

let a0poster_header papersize use_conceptgraph =
  String.concat "\n" ([
      "\\documentclass{article}";
      (*     "\\documentclass[portrait,final]{a0poster}"; *)
      "\\usepackage[" ^ papersize ^ ", left=2.5cm, right=2.5cm, top=3.5cm, bottom=3.5cm, headsep=1.2cm]{geometry}";
      (*     "\\newgeometry{tmargin=3cm, bmargin=3cm, lmargin=0.5cm, rmargin=0.5cm}"; *)
      (* "\\usepackage{a4wide}"; *)
      "\\usepackage{amsmath}";
      "\\usepackage{amssymb}";
      "\\usepackage{longtable}";
      "\\usepackage{cmll}";
      "\\usepackage[T1]{fontenc}";
      "\\usepackage[utf8]{inputenc}";
      "\\usepackage[polish]{babel}";
      "\\usepackage{tikz}";
      "\\usepackage{tikz-qtree}"] @
      (if use_conceptgraph then ["\\usetikzlibrary{conceptgraph}"] else []) @
      ["\\parindent 0pt";
       "\\parskip 4pt";
       "\\begin{document}\n\n"])

let trailer = "\\end{document}"

let latex_file_out latex_path latex_filename paper_size use_conceptgraph f =
  File.file_out (latex_path ^ latex_filename ^ ".tex") (fun file ->
      Printf.fprintf file "%s" (a0poster_header paper_size use_conceptgraph);
      f file;
      Printf.fprintf file "%s" trailer)

let process_pdflatex_output chan =
  try
    while true do
      let line = input_line chan in
      if String.length line < 7 then () else
        match String.sub line 0 7 with
          "Output " -> print_endline ("pdflatex: " ^ line)
        | _ -> ()
    done
  with End_of_file -> ();
    ()

let process_pdflatex_status = function
    Unix.WEXITED n -> Printf.printf "pdflatex: terminated with return code %d\n%!" n
  | Unix.WSIGNALED n -> Printf.printf "pdflatex: killed by a signal %d\n%!" n
  | Unix.WSTOPPED n -> Printf.printf "pdflatex: stopped by a signal %d\n%!" n

let latex_compile_and_clean latex_path latex_filename =
  let dir = Sys.getcwd () in
  Sys.chdir latex_path;
  (*   ignore (Sys.command ("pdflatex -halt-on-error " ^ latex_filename ^ ".tex"));    *)
  let chan = Unix.open_process_in ("pdflatex -halt-on-error " ^ latex_filename ^ ".tex") in
  process_pdflatex_output chan;
  let status = Unix.close_process_in chan in
  process_pdflatex_status status;
  if status = Unix.WEXITED 0 then (
    ignore (Sys.command ("rm " ^ latex_filename ^ ".tex"));
    ignore (Sys.command ("rm " ^ latex_filename ^ ".aux"));
    ignore (Sys.command ("rm " ^ latex_filename ^ ".log")));
  Sys.chdir dir
