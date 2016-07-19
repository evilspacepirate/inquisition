-----------------------------------------------------------------
--                                                             --
-- NVP                                                         --
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
with Primatives; use Primatives;
with Util;       use Util;

package body NVP_Protocol is

   Message_ID_Field_Length       : constant := 1;
   CRC_Field_Length              : constant := 1;
   Length_Field_Length           : constant := 2;
   Start_Of_Message_Field_Length : constant := 2;
   Source_Field_Length           : constant := 1;
   Destination_Field_Length      : constant := 1;

   Start_Of_Message_Byte_1       : constant Unsigned_8 := 16#55#;
   Start_Of_Message_Byte_2       : constant Unsigned_8 := 16#AA#;

   -----------------
   -- COMPUTE_CRC --
   -----------------
   
   function Compute_CRC (Data : Unsigned_8_Array) return Unsigned_8 is
      type Unsigned_8_Array is array (Unsigned_8 range 0 .. 255) of Unsigned_8;

      Lookup_Table : Unsigned_8_Array := (
                                         0,   7,  14,   9,  28,  27,  18,  21,  56,  63,  54,  49,  36,  35,  42,  45,
                                       112, 119, 126, 121, 108, 107,  98, 101,  72,  79,  70,  65,  84,  83,  90,  93,
                                       224, 231, 238, 233, 252, 251, 242, 245, 216, 223, 214, 209, 196, 195, 202, 205,
                                       144, 151, 158, 153, 140, 139, 130, 133, 168, 175, 166, 161, 180, 179, 186, 189,
                                       199, 192, 201, 206, 219, 220, 213, 210, 255, 248, 241, 246, 227, 228, 237, 234,
                                       183, 176, 185, 190, 171, 172, 165, 162, 143, 136, 129, 134, 147, 148, 157, 154,
                                        39,  32,  41,  46,  59,  60,  53,  50,  31,  24,  17,  22,   3,   4,  13,  10,
                                        87,  80,  89,  94,  75,  76,  69,  66, 111, 104,  97, 102, 115, 116, 125, 122,
                                       137, 142, 135, 128, 149, 146, 155, 156, 177, 182, 191, 184, 173, 170, 163, 164,
                                       249, 254, 247, 240, 229, 226, 235, 236, 193, 198, 207, 200, 221, 218, 211, 212,
                                       105, 110, 103,  96, 117, 114, 123, 124,  81,  86,  95,  88,  77,  74,  67,  68,
                                        25,  30,  23,  16,   5,   2,  11,  12,  33,  38,  47,  40,  61,  58,  51,  52,
                                        78,  73,  64,  71,  82,  85,  92,  91, 118, 113, 120, 127, 106, 109, 100,  99,
                                        62,  57,  48,  55,  34,  37,  44,  43,   6,   1,   8,  15,  26,  29,  20,  19,
                                       174, 169, 160, 167, 178, 181, 188, 187, 150, 145, 152, 159, 138, 141, 132, 131,
                                       222, 217, 208, 215, 194, 197, 204, 203, 230, 225, 232, 239, 250, 253, 244, 243);
      Value : Unsigned_8 := 0;
   begin
     for Index in Natural range Data'First .. Data'Last loop
       Value := Lookup_Table(Value xor Data(Index));
     end loop;
     return Value;
   end Compute_CRC;

   ---------------------------
   -- ENVELOPE_MESSAGE_DATA --
   ---------------------------

   function Envelope_Message_Data(Message_ID : Message_ID_Type;
                                  Data       : Unsigned_8_Array) return Unsigned_8_Array is
      Packet : Unsigned_8_Array(1 .. Data'Length +
                                     Message_ID_Field_Length +
                                     CRC_Field_Length +
                                     Length_Field_Length + 
                                     Start_Of_Message_Field_Length);
      Length_High_Part : Unsigned_8;
      Length_Low_Part  : Unsigned_8;
   begin
      Length_Low_Part  := Unsigned_8(Unsigned_16(Data'Length + Message_ID_Field_Length) and 16#FF#);
      Length_High_Part := Unsigned_8(Shift_Right(Unsigned_16(Data'Length + Message_ID_Field_Length), 8));

      Packet(1)                    := Start_Of_Message_Byte_1;
      Packet(2)                    := Start_Of_Message_Byte_2;
      Packet(3)                    := Length_Low_Part;
      Packet(4)                    := Length_High_Part;
      Packet(5)                    := Unsigned_8(Message_ID);
      Packet(6 .. 5 + Data'Length) := Data;
      Packet(Packet'Last)          := Compute_CRC(Packet(3 .. Packet'Last - 1));

      return Packet;
   end Envelope_Message_Data;

   ---------------------------
   -- ENVELOPE_MESSAGE_DATA --
   ---------------------------

   function Envelope_Message_Data(Message_ID  : Message_ID_Type;
                                  Source      : Unsigned_8;
                                  Destination : Unsigned_8;
                                  Data        : Unsigned_8_Array) return Unsigned_8_Array is
      Packet : Unsigned_8_Array(1 .. Data'Length +
                                     Message_ID_Field_Length +
                                     CRC_Field_Length +
                                     Source_Field_Length +
                                     Destination_Field_Length +
                                     Length_Field_Length + 
                                     Start_Of_Message_Field_Length);
      Length_High_Part : Unsigned_8;
      Length_Low_Part  : Unsigned_8;
      Message_Length   : Natural := Data'Length +
                                    Message_ID_Field_Length;
   begin
      Length_Low_Part := Unsigned_8(Unsigned_16(Data'Length + 
                                                Message_ID_Field_Length +
                                                Source_Field_Length +
                                                Destination_Field_Length) and 16#FF#);
      Length_High_Part := Unsigned_8(Shift_Right(Unsigned_16(Data'Length + 
                                                Message_ID_Field_Length +
                                                Source_Field_Length +
                                                Destination_Field_Length), 8));

      Packet(1)                    := Start_Of_Message_Byte_1;
      Packet(2)                    := Start_Of_Message_Byte_2;
      Packet(3)                    := Length_Low_Part;
      Packet(4)                    := Length_High_Part;
      Packet(5)                    := Source;
      Packet(6)                    := Destination;
      Packet(7)                    := Unsigned_8(Message_ID);
      Packet(8 .. 7 + Data'Length) := Data;
      Packet(Packet'Last)          := Compute_CRC(Packet(3 .. Packet'Last - 1));

      return Packet;
   end Envelope_Message_Data;

   -----------------------------
   -- CREATE_SET_VALUE_PACKET --
   -----------------------------

   function Create_Set_Value_Packet (Parameter_ID : Unsigned_16;
                                     Value        : Unsigned_32) return Unsigned_8_Array is
      Packet : Unsigned_8_Array(1 .. 6);
   begin
      Packet(1) := Unsigned_8(Unsigned_16(Parameter_ID) and 16#FF#);
      Packet(2) := Unsigned_8(Shift_Right(Unsigned_16(Parameter_ID), 8));
      Packet(3) := Unsigned_8(Unsigned_32(Value) and 16#FF#);
      Packet(4) := Unsigned_8(Shift_Right(Unsigned_32(Value),  8) and 16#FF#);
      Packet(5) := Unsigned_8(Shift_Right(Unsigned_32(Value), 16) and 16#FF#);
      Packet(6) := Unsigned_8(Shift_Right(Unsigned_32(Value), 24) and 16#FF#);

      return Envelope_Message_Data(Write_Data_ID, Packet);
   end;

   -----------------------------
   -- CREATE_SET_VALUE_PACKET --
   -----------------------------
   
   function Create_Set_Value_Packet (Parameter_ID : Unsigned_16;
                                     Value        : Unsigned_32;
                                     Source       : Unsigned_8;
                                     Destination  : Unsigned_8) return Unsigned_8_Array is
      Packet : Unsigned_8_Array(1 .. 6);                   
   begin                                                           
      Packet(1) := Unsigned_8(Unsigned_16(Parameter_ID) and 16#FF#); 
      Packet(2) := Unsigned_8(Shift_Right(Unsigned_16(Parameter_ID), 8));
      Packet(3) := Unsigned_8(Unsigned_32(Value) and 16#FF#);
      Packet(4) := Unsigned_8(Shift_Right(Unsigned_32(Value),  8) and 16#FF#);
      Packet(5) := Unsigned_8(Shift_Right(Unsigned_32(Value), 16) and 16#FF#);
      Packet(6) := Unsigned_8(Shift_Right(Unsigned_32(Value), 24) and 16#FF#);

      return Envelope_Message_Data(Write_Data_ID,
                                   Source,
                                   Destination,
                                   Packet);
   end;

   -----------------------------
   -- CREATE_GET_VALUE_PACKET --
   -----------------------------
   
   function Create_Get_Value_Packet (Parameter_IDs : Unsigned_16_Vectors.Vector) return Unsigned_8_Array is
      use Unsigned_16_Vectors;
      Packet : Unsigned_8_Array(1 .. Natural(Length(Parameter_IDs) * 2));
   begin

      put_line("len = " & natural'image(natural(Length(Parameter_IDs))));
      for Index in Natural range 0 .. Natural(Length(Parameter_IDs)) - 1 loop
      put_line("index -> " & natural'image(index));
         Packet(2 * Index + 1) := Unsigned_8(Unsigned_16(Parameter_IDs.Element(Index)) and 16#FF#);
         Packet(2 * Index + 2) := Unsigned_8(Shift_Right(Unsigned_16(Parameter_IDs.Element(Index)), 8));
      end loop;

      return Envelope_Message_Data(Read_Data_ID, Packet);

   end Create_Get_Value_Packet;

   -----------------------------
   -- CREATE_GET_VALUE_PACKET --
   -----------------------------
   
   function Create_Get_Value_Packet (Parameter_IDs : Unsigned_16_Vectors.Vector;
                                     Source        : Unsigned_8;
                                     Destination   : Unsigned_8) return Unsigned_8_Array is
      use Unsigned_16_Vectors;
      Packet : Unsigned_8_Array(1 .. Natural(Unsigned_16_Vectors.Length(Parameter_IDs) * 2));
   begin
      
      for Index in Natural range 0 .. Natural(Length(Parameter_IDs) - 1) loop
         Packet(2 * Index + 1) := Unsigned_8(Unsigned_16(Parameter_IDs.Element(Index)) and 16#FF#);
         Packet(2 * Index + 2) := Unsigned_8(Shift_Right(Unsigned_16(Parameter_IDs.Element(Index)), 8));
      end loop;
      
      return Envelope_Message_Data(Read_Data_ID,
                                   Source,
                                   Destination,
                                   Packet);
   end Create_Get_Value_Packet;

end NVP_Protocol;
