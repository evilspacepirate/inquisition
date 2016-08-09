-----------------------------------------------------------------
--                                                             --
-- DEVICE Specification                                        --
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
with Configuration;                     use Configuration;
with Interfaces;                        use Interfaces;
with Primatives;                        use Primatives;

package Device is

   Maximum_Transmission_Unit      : constant := 2048;

   Error_Opening_Device           : exception;
   Communications_Error           : exception;
   Invalid_DataLink_Config        : exception;
   Connection_Already_Established : exception;
   Connection_Not_Established     : exception;

   type Connection_State_Type is (Connected, Not_Connected);

   procedure Connect(Config : Datalink_Configuration);

   procedure Disconnect;

   procedure Shutdown;
   -- Clean up any loose ends --

   procedure Send_Data(Data : Unsigned_8_Array);

   function Get_Data return Unsigned_8_Array;

   function Connected return Boolean;

end Device;
