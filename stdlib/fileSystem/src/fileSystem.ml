(* FreeSpec
 * Copyright (C) 2018–2019 ANSSI
 *
 * Contributors:
 * 2019 Vincent Tourneur <vincent.tourneur@inria.fr>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *)

open Exec_plugin.Coqstr
open Exec_plugin.Coqnum
open Exec_plugin.Extends
open Exec_plugin.Coqunit
open Unix
open Exec_plugin.Query
open Exec_plugin.Utils

let path = ["FreeSpec"; "Stdlib"; "FileSystem"; "FileSystem"]

type mode_constructor = ReadOnly | WriteOnly | ReadWrite

module Ind = struct
  module Mode =
    Inductive.Make(struct
        type constructor = mode_constructor
        let type_name = "mode"
        let modlist = path
        let names = [("ReadOnly", ReadOnly); ("WriteOnly", WriteOnly); ("ReadWrite", ReadWrite)]
      end)
end

let coqmode_to_open_flagl m =
  let (m, args) = app_full m in
  match (Ind.Mode.constructor_of m, args) with
  | (Some ReadOnly, []) -> [O_RDONLY]
  | (Some WriteOnly, []) -> [O_WRONLY]
  | (Some ReadWrite, []) -> [O_RDWR]
  | _ -> raise (UnsupportedTerm "not a constructor of [FileSystem.mode]")

let install_interface =
  let open_ = function
    | [m; str] -> int_to_coqz (Obj.magic (openfile (string_of_coqstr str) (coqmode_to_open_flagl m) 0o640))
    | _ -> assert false in
  let getSize = function
    | [fd] -> int_to_coqz (fstat (Obj.magic (int_of_coqz fd))).st_size
    | _ -> assert false in
  let read = function
    | [n; fd] -> let buff = Bytes.create (int_of_coqz n) in
               ignore (read (Obj.magic (int_of_coqz fd)) buff 0 (int_of_coqz n));
               bytes_to_coqstr buff
    | _ -> assert false in
  let write = function
    | [str; fd] -> let buff = bytes_of_coqstr str in
                 ignore (write (Obj.magic (int_of_coqz fd)) buff 0 (Bytes.length buff));
                 coqtt
    | _ -> assert false in
  let close = function
    | [fd] -> close (Obj.magic (int_of_coqz fd));
               coqtt
    | _ -> assert false in
  register_interface path [("Open", open_); ("GetSize", getSize); ("Read", read); ("Write", write); ("Close", close)]
