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

let path = ["FreeSpec"; "Stdlib"; "FileSystem"; "FileSystem"]

let install_interface =
  let open_ = function
    | [str] -> int_to_coqz (Obj.magic (openfile (string_of_coqstr str) [O_RDONLY] 0o640))
    | _ -> assert false in
  let read = let buff = Bytes.create 10 in function
    | [fd] -> ignore (read (Obj.magic (int_of_coqz fd)) buff 0 10);
               bytes_to_coqstr buff
    | _ -> assert false in
  let close = function
    | [fd] -> close (Obj.magic (int_of_coqz fd));
               coqtt
    | _ -> assert false in
  register_interface path [("Open", open_); ("Read", read); ("Close", close)]
