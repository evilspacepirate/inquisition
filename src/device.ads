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

   Error_Opening_Device           : exception;
   Communications_Error           : exception;
   Invalid_Protocol               : exception;
   Invalid_DataLink_Config        : exception;
   Connection_Already_Established : exception;
   Connection_Not_Established     : exception;

   type Connection_State_Type is (Connected, Not_Connected);

   protected type Values_Buffer is
      procedure Add(Item : in Name_Value_Pair);
      procedure Remove(Items : out Name_Value_Pair_Vectors.Vector);
   private
      Elements : Name_Value_Pair_Vectors.Vector;
   end Values_Buffer;

   protected type Requests_Buffer is
      procedure Add(Item : in Unsigned_16);
      procedure Remove(Items : out Unsigned_16_Vectors.Vector);
   private
      Elements : Unsigned_16_Vectors.Vector;
   end Requests_Buffer;

   procedure Connect(Protocol : Protocol_Type;
                     Config   : Datalink_Configuration);

   procedure Disconnect;

   procedure Set_Value(Parameter_ID : Unsigned_16;
                       Value        : Unsigned_32);

   procedure Request_Values(Parameter_IDs : Unsigned_16_Vectors.Vector);

   procedure Shutdown;
   -- Clean up any loose ends --

   procedure Send_Data(Data : Unsigned_8_Array);

   function Connected return Boolean;

   Requests    : Requests_Buffer;
   Set_Values  : Values_Buffer;

end Device;
