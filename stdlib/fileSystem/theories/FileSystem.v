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

Require Import FreeSpec.Exec.
Require Export Coq.Strings.String.
Require Import FreeSpec.Program.
Require Import BinNums.

Module FileSystem.
  Inductive i: Type -> Type :=
  | Open: string -> i Z
  | Read: Z -> Z -> i string
  | Close: Z -> i unit.

  Definition open {ix} `{Use i ix} (str: string)
    : Program ix Z :=
    request (Open str).

  Definition read {ix} `{Use i ix} (n fd: Z)
    : Program ix string :=
    request (Read n fd).

  Definition close {ix} `{Use i ix} (fd: Z)
    : Program ix unit :=
    request (Close fd).
End FileSystem.

Declare ML Module "stdlib_fileSystem_plugin".
