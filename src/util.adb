--                                                             --
-- UTIL                                                        --
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

package body Util is

   ------------------
   -- STRING_SPLIT --
   ------------------

   function Split_String(Text : String; Seperators : String) return String_Vectors.Vector is

      use String_Vectors;

      function Is_A_Seperator(Char : Character; Seperators : String) return boolean is
      begin
        for Index in Natural range Seperators'First .. Seperators'Last loop
           if Seperators(Index) = Char then
              return True;
           end if;
        end loop;
        return False;
      end Is_A_Seperator;

      type Parser_State is (No_String, In_String);

      State        : Parser_State := No_String;
      String_Start : Natural      := Text'First;
      Result       : Vector;
   begin
     for Index in Natural range Text'First .. Text'Last loop
        if Is_A_Seperator(Text(Index), Seperators) then
           case State is
              when No_String =>
                 null;
              when In_String =>
                 State := No_String;
                 Result.Append(UnStr.To_Unbounded_String(Text(String_Start .. Index - 1)));
           end case;
        else
           case State is
              when No_String =>
                 String_Start := Index;
                 State        := In_String;
              when In_String =>
                 null;
           end case;
        end if;
     end loop;

     if State = In_String then
        Result.Append(UnStr.To_Unbounded_String(Text(String_Start .. Text'Last)));
     end if;

     return Result;
   end Split_String;

   ------------
   -- TO_HEX --
   ------------

   function To_Hex(Input : Unsigned_8) return String is
      Hex    : String(1 .. 6);
      Output : String(1 .. 2);
      Start  : Integer;
      Length : Integer;
   begin
      Unsigned_8_IO.Put(To => Hex, Item => Input, Base => 16);
      for i in 1 .. 6 loop
         if Hex(i) = '#' then
            Start := i + 1;
            exit;
         end if;
      end loop;
      Output := "00";
      Length := 6 - Start;
      Output(3 - Length .. 2) := Hex(6 - Length .. 5);
      return Output;
   end To_Hex;

   ------------
   -- TO_HEX --
   ------------

   function To_Hex(Input : Unsigned_16) return String is
      High_Byte : Unsigned_8 := Unsigned_8(Shift_Right(Input, 8));
      Low_Byte  : Unsigned_8 := Unsigned_8(Input and 16#FF#);
   begin
      return To_Hex(High_Byte) & To_Hex(Low_Byte);
   end To_Hex;

   ------------
   -- TO_HEX --
   ------------

   function To_Hex(Input : Unsigned_32) return String is
     Highest_Byte : Unsigned_8 := Unsigned_8(Shift_Right(Input, 24));
     High_Byte    : Unsigned_8 := Unsigned_8(Shift_Right(Input, 16));
     Low_Byte     : Unsigned_8 := Unsigned_8(Shift_Right(Input,  8));
     Lowest_Byte  : Unsigned_8 := Unsigned_8(Input and 16#FF#);
   begin
     return To_Hex(Highest_Byte) &
            To_Hex(High_Byte) &
            To_Hex(Low_Byte) &
            To_Hex(Lowest_Byte);
   end To_Hex;

   ------------
   -- TO_HEX --
   ------------

   function To_Hex(Input : Unsigned_8_Vectors.Vector) return String is
      use Unsigned_8_Vectors;
      Length : constant Natural := Natural(Unsigned_8_Vectors.Length(Input));
   begin
      case Length is
         when 0 =>
            return "";
         when 1 =>
            declare
               Output : String(1 .. 2);
            begin
               Output := To_Hex(Element(Input, 0));
               return Output;
            end;
         when others =>
            declare
               Output : String (1 .. 3 * Length);
            begin
               for Index in Natural range 0 .. Length - 1 loop
                  Output(1 + 3 * Index .. 2 + 3 * Index) := To_Hex(Element(Input, Index));
                  Output(3 + 3 * Index) := ' ';
               end loop;
               return Output;
            end;
      end case;
   end To_Hex;
   
   -------------
   -- PUT_HEX --
   -------------
   
   procedure Put_Hex(Input : Unsigned_8) is
      Hex    : String(1 .. 6);
      Output : String(1 .. 2);
      Start  : Integer;
      Length : Integer;
   begin
     Unsigned_8_IO.Put(To => Hex, Item => Input, Base => 16);
     for Index in 1 .. 6 loop
        if Hex(Index) = '#' then
           Start := Index + 1;
           exit;
        end if;
     end loop;
     Output := "00";
     Length := 6 - Start;
     Output(3 - Length .. 2) := Hex(6 - Length .. 5);
     Put(Output);
   end Put_Hex;

   -------------
   -- PUT_HEX --
   -------------

   procedure Put_Hex(Input : Unsigned_16) is
      High_Byte : Unsigned_8 := Unsigned_8(Shift_Right(Input, 8));
      Low_Byte  : Unsigned_8 := Unsigned_8(Input and 16#FF#);
   begin
      Put_Hex(High_Byte);
      Put_Hex(Low_Byte);
   end Put_Hex;

   ----------
   -- DUMP --
   ----------

   procedure Dump(Input : Unsigned_8_Array) is
   begin
      Put(Natural'Image(Input'Length) & " bytes: ");
      for Index in Input'Range loop
         Put_Hex(Input(Index));
         Put(" ");
      end loop;
   end Dump;

end Util;
