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
with Device;
with Interfaces;                        use Interfaces;
with Primatives;                        use Primatives;

package NVP_Protocol is

   type Message_ID_Type is new Unsigned_8;

   Header_Size              : constant := 1;
   Header_Size_With_Routing : constant := 3;
   CRC_Data_Start_Index     : constant := 3;
   Valid_CRC_Value          : constant := 0;

   Start_Of_Message_1       : constant Unsigned_8 := 16#55#;
   Start_Of_Message_2       : constant Unsigned_8 := 16#AA#;

   Write_Data_ID            : constant Message_ID_Type := 0;
   Read_Data_ID             : constant Message_ID_Type := 1;

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
   
   function Create_Request_Value_Packet (Parameter_IDs : Unsigned_16_Vectors.Vector) return Unsigned_8_Array;
   
   function Create_Request_Value_Packet (Parameter_IDs : Unsigned_16_Vectors.Vector;
                                         Source        : Unsigned_8;
                                         Destination   : Unsigned_8) return Unsigned_8_Array;

   function Interpret_Data (Input : Unsigned_8) return Unsigned_8_Array;
   -- Reads bytes off an input stream and returns a byte array containing a --
   -- complete message if one is found                                      --

   function Interpret_Data_With_Routing (Input : Unsigned_8) return Unsigned_8_Array;
   -- Reads bytes off an input stream and returns a byte array containing a --
   -- complete message if one is found                                      --

   function Get_Packet_ID_With_Routing (Input : Unsigned_8_Array) return Message_ID_Type;
   -- Get the packet ID of a packet containing routing fields in its header. This --
   -- function does not validate the correctness of a packet format.              --

   function Get_Packet_ID (Input : Unsigned_8_Array) return Message_ID_Type;
   -- Get the packet ID of a packet that does not contain routing fields in its header. --
   -- This function does not validate the correctness of a packet format.               --

   function Get_Packet_Data_With_Routing (Input : Unsigned_8_Array) return Unsigned_8_Array;
   -- Get the data field of a packet containing routing fields in its header. This --
   -- function does not validate the correctness of a packet format.  --

   function Get_Packet_Data (Input : Unsigned_8_Array) return Unsigned_8_Array;
   -- Get the data field of a packet that does not contain routing fields in its header. This --
   -- function does not validate the correctness of a packet format.  --

   private

   type Interpreter_State is (Sync_Byte_1,
                              Sync_Byte_2,
                              Length_Low_Byte,
                              Length_High_Byte,
                              Source,
                              Destination,
                              Message_ID,
                              Message_Data,
                              CRC);

   type Interpreter_Data is record
      State                        : Interpreter_State;
      Message_Data_Buffer          : Unsigned_8_Array(1 .. Device.Maximum_Transmission_Unit);
      Byte_Index                   : Natural;
      Packet_Length                : Natural;
      Message_Data_Bytes_Remaining : Natural;
   end record;

   Interpreter : Interpreter_Data := (State                        => Sync_Byte_1,
                                      Byte_Index                   => 1,
                                      Message_Data_Buffer          => (others => 0),
                                      Packet_Length                => 0,
                                      Message_Data_Bytes_Remaining => 0);

end NVP_Protocol;
