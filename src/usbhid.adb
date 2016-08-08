-----------------------------------------------------------------
--                                                             --
-- USBHID                                                      --
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
with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings;
with Primatives;           use Primatives;
with System;
with Util;                 use Util;

package body USBHID is

   -------------------------------
   -- WRITE_DATA_PREPEND_LENGTH --
   -------------------------------

   function Write_Data_Prepend_Length (Device : System.Address;
                                       Data   : Unsigned_8_Array) return Int is
      Length : constant Natural := Data'Length;
   begin
      declare
         Data_With_Length : Unsigned_8_Array(1 .. Length + 1);
      begin
         Data_With_Length(1)                          := Unsigned_8(Length);
         Data_With_Length(2 .. Data_With_Length'Last) := Data;
         return USBHID.Write(Device, Data_With_Length'Address, Data_With_Length'Length);
      end;
   exception
      when others =>
         raise Write_Error;
   end Write_Data_Prepend_Length;

end USBHID;
