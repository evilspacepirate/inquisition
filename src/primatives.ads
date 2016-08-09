-----------------------------------------------------------------
--                                                             --
-- PRIMATIVES Specification                                    --
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
with Ada.Calendar;                      use Ada.Calendar;
with Ada.Containers.Indefinite_Vectors; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Interfaces;                        use Interfaces;

package Primatives is

   type Unsigned_8_Array is array (Natural range <>) of Unsigned_8;

   type Name_Value_Pair is record
      Name  : Unsigned_16;
      Value : Unsigned_32;
   end record;

   Name_Value_Pair_Size : constant := 6; -- Bytes --

   function Unsigned_16_Hashed(Value : Unsigned_16) return Hash_Type is (Hash_Type(Value));

   package UnStr renames Ada.Strings.Unbounded;
   package String_Vectors is new Indefinite_Vectors (Natural, UnStr.Unbounded_String);
   package Unsigned_8_Vectors is new Indefinite_Vectors (Natural, Unsigned_8);
   use Unsigned_8_Vectors;
   package Unsigned_16_Vectors is new Indefinite_Vectors (Natural, Unsigned_16);
   package Name_Value_Pair_Vectors is new Indefinite_Vectors (Natural, Name_Value_Pair);
   package Natural_Vectors is new Indefinite_Vectors(Natural, Natural);
   use Natural_Vectors;
   package Name_Index_Maps is new Hashed_Maps(Unsigned_16, Natural_Vectors.Vector, Unsigned_16_Hashed, "=");

   type Message_Record is record
      Message    : Unsigned_8_Vectors.Vector;
      Time_Stamp : Time;
   end record;

   package Message_Record_Vectors is new Indefinite_Vectors(Natural, Message_Record);

   type Name_Value_Pair_Record is record
      Values     : Name_Value_Pair_Vectors.Vector;
      Time_Stamp : Time;
   end record;

   package Name_Value_Pair_Record_Vectors is new Indefinite_Vectors(Natural, Name_Value_Pair_Record);

   protected type Name_Value_Pair_Record_Buffer is
      procedure Add(Value : in Name_Value_Pair_Record);
      procedure Remove (Values : out Name_Value_Pair_Record_Vectors.Vector);
      procedure Clear;
   private
      Elements : Name_Value_Pair_Record_Vectors.Vector;
   end Name_Value_Pair_Record_Buffer;

   protected type Requests_Buffer is
      procedure Add(Request : in Unsigned_16);
      procedure Set(Requests : in Unsigned_16_Vectors.Vector);
      procedure Clear;
      procedure Remove (Values : out Unsigned_16_Vectors.Vector);
      function Get_Requests return Unsigned_16_Vectors.Vector;
   private
      Elements : Unsigned_16_Vectors.Vector;
   end Requests_Buffer;

   protected type Message_Records_Buffer is
      procedure Add (Message : Message_Record);
      procedure Remove (Values : out Message_Record_Vectors.Vector);
      procedure Clear;
   private
      Elements : Message_Record_Vectors.Vector;
   end Message_Records_Buffer;

   function Unsigned_8_Array_To_Vector (Input : Unsigned_8_Array) return Unsigned_8_Vectors.Vector;

end Primatives;
