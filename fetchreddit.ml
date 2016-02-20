(*
#use "topfind";;
#require "cohttp.lwt";;
#require "lwt";;
#require "cohttp";;
this is useful if you wanted to run ocaml program
from toplevel.
 *)

open Lwt
open Cohttp
open Cohttp_lwt_unix

let body =
  Client.get(Uri.of_string "https://www.quora.com/What-needs-to-be-done-to-make-agriculture-profitable-for-more-farmers-in-India") >>= fun (resp, body) ->
  body |> Cohttp_lwt_body.to_string >|= fun body ->
  body

let res =
  Client.call `GET (Uri.of_string "https://www.google.co.in/")
  >>= fun (resp, body) ->
  let code = (resp |> Response.status |> Code.code_of_status) in
  let head = (resp |> Response.headers |> Header.to_string) in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" head;
  body |> Cohttp_lwt_body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    body |> Cohttp_lwt_body.to_string >|= (fun body ->
      (Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s"
         uri meth headers body))
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () =
  let body = Lwt_main.run body in
  print_endline ("Received body\n" ^ body)
