-----------------------------------------------------------------
--                                                             --
-- NVP Specification                                           --
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
with Interfaces;                        use Interfaces;
with Primatives;                        use Primatives;

package NVP_Protocol is

   type Message_ID_Type is new Unsigned_8;

   Write_Data_ID : constant Message_ID_Type := 0;
   Read_Data_ID  : constant Message_ID_Type := 1;

   function Envelope_Message_Data(Message_ID : Message_ID_Type;
                                  Data       : Unsigned_8_Array) return Unsigned_8_Array;
   -- Creates an NVP message (No routing information).        --
   -- Adds the NVP message header to the beginning of the     --
   -- message and adds the CRC byte to the end of the message --

   function Envelope_Message_Data(Message_ID  : Message_ID_Type;
                                  Source      : Unsigned_8;
                                  Destination : Unsigned_8;
                                  Data        : Unsigned_8_Array) return Unsigned_8_Array;
   -- Creates an NVP message with routing information.        --
   -- Adds the NVP message header to the beginning of the     --
   -- message and adds the CRC byte to the end of the message --
   
   function Compute_CRC (Data : Unsigned_8_Array) return Unsigned_8;

   function Create_Set_Value_Packet (Parameter_ID : Unsigned_16;
                                     Value        : Unsigned_32) return Unsigned_8_Array;
   
   function Create_Set_Value_Packet (Parameter_ID : Unsigned_16;
                                     Value        : Unsigned_32;
                                     Source       : Unsigned_8;
                                     Destination  : Unsigned_8) return Unsigned_8_Array;
   
   function Create_Get_Value_Packet (Parameter_IDs : Unsigned_16_Vectors.Vector) return Unsigned_8_Array;
   
   function Create_Get_Value_Packet (Parameter_IDs : Unsigned_16_Vectors.Vector;
                                     Source        : Unsigned_8;
                                     Destination   : Unsigned_8) return Unsigned_8_Array;

end NVP_Protocol;
