-----------------------------------------------------------------
--                                                             --
-- PRIMATIVES                                                  --
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
with ada.text_io; use ada.text_io;
with util;        use util;
package body Primatives is

   -----------------------------------
   -- NAME_VALUE_PAIR_RECORD_BUFFER --
   -----------------------------------

   protected body Name_Value_Pair_Record_Buffer is

      ---------------------------------------
      -- NAME_VALUE_PAIR_RECORD_BUFFER.ADD --
      ---------------------------------------

      procedure Add(Value : in Name_Value_Pair_Record) is
      begin
         Elements.Append(Value);
      end Add;

      -----------------------------------------
      -- NAME_VALUE_PAIR_RECORD_BUFFER.CLEAR --
      -----------------------------------------

      procedure Clear is
      begin
         Elements.Clear;
      end Clear;

      ------------------------------------------
      -- NAME_VALUE_PAIR_RECORD_BUFFER.REMOVE --
      ------------------------------------------

      procedure Remove (Values : out Name_Value_Pair_Record_Vectors.Vector) is
      begin
         Values := Elements;
         Elements.Clear;
      end Remove;

   end Name_Value_Pair_Record_Buffer;

   ---------------------
   -- REQUESTS_BUFFER --
   ---------------------

   protected body Requests_Buffer is

      -------------------------
      -- REQUESTS_BUFFER.ADD --
      -------------------------

      procedure Add(Request : in Unsigned_16) is
      begin
         Elements.Append(Request);
      end Add;

      -------------------------
      -- REQUESTS_BUFFER.SET --
      -------------------------

      procedure Set (Requests : in Unsigned_16_Vectors.Vector) is
      begin
         Elements := Requests;
      end Set;

      ---------------------------
      -- REQUESTS_BUFFER.CLEAR --
      ---------------------------

      procedure Clear is
      begin
         Elements.Clear;
      end Clear;

      ----------------------------
      -- REQUESTS_BUFFER.REMOVE --
      ----------------------------

      procedure Remove (Values : out Unsigned_16_Vectors.Vector) is
      begin
         Values := Elements;
         Elements.Clear;
      end Remove;

      ----------------------------------
      -- REQUESTS_BUFFER.GET_REQUESTS --
      ----------------------------------

      function Get_Requests return Unsigned_16_Vectors.Vector is
      begin
         return Elements;
      end Get_Requests;

   end Requests_Buffer;

   protected body Message_Records_Buffer is

      --------------------------------
      -- MESSAGE_RECORDS_BUFFER.ADD --
      --------------------------------

      procedure Add (Message : Message_Record) is
      begin
         Elements.Append(Message);
      end Add;

      -----------------------------------
      -- MESSAGE_RECORDS_BUFFER.REMOVE --
      -----------------------------------

      procedure Remove (Values : out Message_Record_Vectors.Vector) is
      begin
          Values := Elements;
          Elements.Clear;
      end Remove;

      ----------------------------------
      -- MESSAGE_RECORDS_BUFFER.CLEAR --
      ----------------------------------

      procedure Clear is
      begin
         Elements.Clear;
      end Clear;

   end Message_Records_Buffer;

   --------------------------------
   -- UNSIGNED_8_ARRAY_TO_VECTOR --
   --------------------------------

   function Unsigned_8_Array_To_Vector (Input : Unsigned_8_Array) return Unsigned_8_Vectors.Vector is
      Output : Unsigned_8_Vectors.Vector;
   begin
      for Index in Natural range Input'First .. Input'Last loop
         Output.Append(Input(Index));
      end loop;
      return Output;
   end Unsigned_8_Array_To_Vector;

end Primatives;
