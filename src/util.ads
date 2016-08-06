-----------------------------------------------------------------
--                                                             --
-- UTIL Specification                                          --
--                                                             --
-- Copyright (c) 2016, John Leimon                             --
--                                                             --
-- Permission to use, copy, modify, and/or distribute          --
-- this software for any purpose with or without fee           --
-- is hereby granted, provided that the above copyright        --
-- notice and this permission notice appear in all copies.     --
--                                                             --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR             --
-- DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE       --
-- INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY         --
-- AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE         --
-- FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL         --
-- DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS       --
-- OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF            --
-- CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING      --
-- OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF      --
-- THIS SOFTWARE.                                              --
-----------------------------------------------------------------
with Ada.Containers.Indefinite_Vectors; use Ada.Containers;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;                       use Ada.Text_IO;
with Interfaces;                        use Interfaces;
with Primatives;                        use Primatives;

package Util is
   
   package Unsigned_8_IO is new Ada.Text_IO.Modular_IO(Unsigned_8);

   function Split_String(Text : String; Seperators : String) return String_Vectors.Vector;

   function To_Hex(Input : Unsigned_8) return String;
   function To_Hex(Input : Unsigned_16) return String;
   function To_Hex(Input : Unsigned_32) return String;

   procedure Put_Hex(Input : Unsigned_8);
   -- Send value to STDOUT in hexidecimal --

   procedure Put_Hex(Input : Unsigned_16);
   -- Send value to STDOUT in hexidecimal --

   procedure Dump(Input : Unsigned_8_Array);
   -- Send a byte array to STDOUT as hex --

end Util;
