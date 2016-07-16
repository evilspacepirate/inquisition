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

end Util;
