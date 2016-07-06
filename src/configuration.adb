-----------------------------------------------------------------
--                                                             --
-- CONFIGURATION                                               --
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
with Ada.Text_IO;                       use Ada.Text_io;
with Ada.Strings.Fixed;                 use Ada.Strings.fixed;
with Ada.Strings.Unbounded;
with Ada.Directories;                   use Ada.Directories;
with Ada.Exceptions;                    use Ada.Exceptions;
with Interfaces;                        use Interfaces;

package body Configuration is
   
   use Adaptable_Parameter_Record_Vectors;

   Delimeter_Symbol          : constant Character := '|';

   Tab_Character             : constant Character := Character'Val(9);
   Line_Feed_Character       : constant Character := Character'Val(10);
   Carriage_Return_Character : constant Character := Character'Val(13);
   CRLF                      : constant String    := Carriage_Return_Character & Line_Feed_Character;

   Serial_Datalink_Tag       : constant String    := "Datalink Serial";
   TCP_IPv4_Datalink_Tag     : constant String    := "Datalink TCPv4";
   USB_HID_Datalink_Tag      : constant String    := "Datalink USBHID";
   Configuration_Type_Tag    : constant String    := "IQ_Config_Format";
   Sampling_Tag              : constant String    := "Sampling:";
   Sample_Period_Tag         : constant String    := "Sample Period:";
   Default_Set_Value_Tag     : constant String    := "Default Set Value:";

   ------------------------------
   -- DUMP_ADAPTABLE_PARAMETER --
   ------------------------------

   procedure Dump_Adaptable_Parameter(Parameter : Adaptable_Parameter_Record) is
   begin
      put_line("Friendly_Name     : " & UnStr.To_String(Parameter.Friendly_Name));
      put_line("Units_Name        : " & UnStr.To_String(Parameter.Units_Name));
      put_line("Unique_Identifier : " & Unsigned_16'image(Parameter.Unique_Identifier));
      put_line("Default_Set_Value : " & Unsigned_32'image(Parameter.Default_Set_Value));
      put_line("Is_Sampling       : " & Boolean'image(Parameter.Is_Sampling));
      put_line("Is_Readable       : " & Boolean'image(Parameter.Is_Readable));
      put_line("Is_Writable       : " & Boolean'image(Parameter.Is_Writable));
      put_line("Sample_Period     : " & Duration'image(Parameter.Sample_Period));
   end Dump_Adaptable_Parameter;

   ---------------------------------
   -- GET_CONFIGURATION_FILE_NAME --
   ---------------------------------
   
   function Get_Configuration_File_Name return String is
      Search     : Search_Type;
      Dir_Entry  : Directory_Entry_Type;
      File_Count : Natural := 0;
   begin
      Start_Search(Search, ".", "*.iq");

      while(More_Entries(Search)) loop
         Get_Next_Entry(Search, Dir_Entry);
         File_Count := File_Count + 1;
      end loop;

      if File_Count = 1 then
         return Simple_Name(Dir_Entry);
      else
         return "";
      end if;
   end Get_Configuration_File_Name;
   
   -------------------------
   -- HEX_DIGIT_TO_NUMBER --
   -------------------------

   function Hex_Digit_To_Number (Digit : Character) return Natural is
   begin
      case Digit is
         when '0' =>
            return 0;
         when '1' =>
            return 1;
         when '2' =>
            return 2;
         when '3' =>
            return 3;
         when '4' =>
            return 4;
         when '5' =>
            return 5;
         when '6' =>
            return 6;
         when '7' =>
            return 7;
         when '8' =>
            return 8;
         when '9' =>
            return 9;
         when 'A' | 'a' =>
            return 16#A#;
         when 'B' | 'b' =>
            return 16#B#;
         when 'C' | 'c' =>
            return 16#C#;
         when 'D' | 'd' =>
            return 16#D#;
         when 'E' | 'e' =>
            return 16#E#;
         when 'F' | 'f' =>
            return 16#F#;
         when others =>
            raise Conversion_Failure;
      end case;
   end Hex_Digit_To_Number;

   ------------------------
   -- SPLIT_ON_DELIMETER --
   ------------------------

   procedure Split_On_Delimeter(Text      : in String;
                                Delimeter : in Character;
                                Tokens    : out String_Vectors.Vector) is
      Delimeter_Count : Natural := 0;
   begin

      -- Count number of delimeters --
      for Index in Natural range Text'First .. Text'Last loop
         if Text(Index) = Delimeter then
            Delimeter_Count := Delimeter_Count + 1;
         end if;
      end loop;

      if Delimeter_Count = 0 then
         return;
      end if;

      -- Index all delimeters --
      declare
         type Delimeter_Array is array(Natural range <>) of Natural;
         Delimeter_Indexes : Delimeter_Array(1 .. Delimeter_Count);
         Delimeter_Number  : Natural := 1;
      begin
         for Index in Natural range Text'First .. Text'Last loop
            if Text(Index) = Delimeter then
               Delimeter_Indexes(Delimeter_Number) := Index;
               Delimeter_Number                    := Delimeter_Number + 1;
            end if;
         end loop;

         for Index in Natural range 1 .. Delimeter_Count + 1 loop
            if Index = 1 then
               Tokens.Append(UnStr.To_Unbounded_String(Text(Text'First .. Delimeter_Indexes(Index) - 1)));
            elsif Index = Delimeter_Count + 1 then
               Tokens.Append(UnStr.To_Unbounded_String(Text(Delimeter_Indexes(Index - 1) + 1 .. Text'Last)));
            else
               Tokens.Append(UnStr.To_Unbounded_String(Text(Delimeter_Indexes(Index - 1) + 1 .. Delimeter_Indexes(Index) - 1)));
            end if;
         end loop;
      end;

   end Split_On_Delimeter;

   ----------------------
   -- STRIP_WHITESPACE --
   ----------------------

   function Strip_Whitespace (Text : String) return String is
      White_Characters : Natural := 0;
   begin
      -- Count number of whitespace characters in the string --
      for Index in Natural range Text'First .. Text'Last loop
         if Text(Index) = ' ' or
            Text(Index) = Tab_Character
         then
            White_Characters := White_Characters + 1;
         end if;
      end loop;

      -- Copy over all of the non-whitespace values in the result string --
      declare
         Result      : String(Text'First .. Text'Last - White_Characters);
         Write_Index : Natural := Text'First;
      begin
         for Index in Natural range Text'First .. Text'Last loop
            if Text(Index) /= ' ' and
               Text(Index) /= Tab_Character
            then
               Result(Write_Index) := Text(Index);
               Write_Index := Write_Index + 1;
            end if;
         end loop;
         return Result;
      end;
   end Strip_Whitespace;
   
   ------------------------
   -- STRING_TO_DURATION --
   ------------------------

   function String_To_Duration (Text : String) return Duration is
      type Unit_Type is (Not_Specified,
                         Seconds,
                         Milliseconds,
                         Microseconds);
      Units         : Unit_Type := Not_Specified;
      Stripped_Text : String    := Strip_Whitespace(Text);
   begin

      if Text'Length = 0 then
         return 0.0;
      end if;

      -- Identify units being used (if any) --

      begin
         if Stripped_Text(Stripped_Text'Last - 1 .. Stripped_Text'Last) = "ms" then
            Units := Milliseconds;
            goto Perform_Conversion;
         end if;
      exception
         when others =>
            null;
      end;
      
      begin
         if Stripped_Text(Stripped_Text'Last - 1 .. Stripped_Text'Last) = "us" then
            Units := Microseconds;
            goto Perform_Conversion;
         end if;
      exception
         when others =>
            null;
      end;
      
      begin
         if Stripped_Text(Stripped_Text'Last) = 's' then
            Units := Seconds;
            goto Perform_Conversion;
         end if;
      exception
         when others =>
            null;
      end;

   <<Perform_Conversion>>

      case Units is
         when Not_Specified =>
            -- Seconds Unit Type is Default --
            return Duration'Value(Stripped_Text(Stripped_Text'First .. Stripped_Text'Last));
         when Seconds =>
            return Duration'Value(Stripped_Text(Stripped_Text'First .. Stripped_Text'Last - 1));
         when Milliseconds =>
            return Duration'Value(Stripped_Text(Stripped_Text'First .. Stripped_Text'Last - 2)) / 1_000.0;
         when Microseconds =>
            return Duration'Value(Stripped_Text(Stripped_Text'First .. Stripped_Text'Last - 2)) / 1_000_000.0;
      end case;

   end String_To_Duration;
   
   -------------------------------
   -- STRING_TO_READ_WRITE_MODE --
   -------------------------------

   procedure String_To_Read_Write_Mode (Text : in  String; 
                                        Read : out Boolean;
                                        Write : out Boolean) is
   begin
      case Text'Length is
         when 1 =>
            case Text(Text'First) is
               when 'R' | 'r' =>
                  Read  := True;
                  Write := False;
               when 'W' | 'w' =>
                  Read  := False;
                  Write := True;
               when others =>
                  raise Conversion_Failure;
            end case;
         when 2 =>
            if Text = "RW" or
               Text = "rW" or
               Text = "Rw" or
               Text = "rw"
            then
               Read  := True;
               Write := True;
            else
               raise Conversion_Failure;
            end if;
         when others =>
            raise Conversion_Failure;
      end case;
   end String_To_Read_Write_Mode;
   
   ---------------------------
   -- STRING_TO_UNSIGNED_16 --
   ---------------------------

   function String_To_Unsigned_16 (Text : String) return Unsigned_16 is
      type Number_Format is (Hexidecimal, Decimal);
      Format       : Number_Format;
      Value        : Unsigned_16;
      Trimmed_Text : String := Trim_Outside_Whitespace(Text);
   begin
      begin
         -- Is the number format in hexidecimal? --
         if Trimmed_Text(Trimmed_Text'First .. Trimmed_Text'First + 1) = "0x" then
            Format := Hexidecimal;
         else 
            Format := Decimal;
         end if;
      exception
         when others =>
            Format := Decimal;
      end;

      case Format is
         when Hexidecimal =>
            declare
               Nibble_0 : Unsigned_16 := 0;
               Nibble_1 : Unsigned_16 := 0;
               Nibble_2 : Unsigned_16 := 0;
               Nibble_3 : Unsigned_16 := 0;
            begin
               case Trimmed_Text'Length is
                   when 3 =>
                      Nibble_0 := Unsigned_16(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 2)));
                   when 4 =>
                      Nibble_1 := Unsigned_16(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 2)));
                      Nibble_0 := Unsigned_16(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 3)));
                   when 5 =>
                      Nibble_2 := Unsigned_16(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 2)));
                      Nibble_1 := Unsigned_16(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 3)));
                      Nibble_0 := Unsigned_16(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 4)));
                   when 6 =>
                      Nibble_3 := Unsigned_16(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 2)));
                      Nibble_2 := Unsigned_16(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 3)));
                      Nibble_1 := Unsigned_16(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 4)));
                      Nibble_0 := Unsigned_16(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 5)));
                   when others =>
                      raise Conversion_Failure;
               end case;

               Value := Shift_Left(Nibble_3, 12) or 
                        Shift_Left(Nibble_2, 8)  or 
                        Shift_Left(Nibble_1, 4)  or 
                        Nibble_0;
            end;
         when Decimal =>
            Value := Unsigned_16'Value(Trimmed_Text);
      end case;

      return Value;
   exception
      when others =>
          raise Conversion_Failure;
   end String_To_Unsigned_16; 

   ---------------------------
   -- STRING_TO_UNSIGNED_32 --
   ---------------------------

   function String_To_Unsigned_32 (Text : String) return Unsigned_32 is
      type Number_Format is (Hexidecimal, Decimal);
      Format       : Number_Format;
      Value        : Unsigned_32;
      Trimmed_Text : String := Trim_Outside_Whitespace(Text);
   begin

      begin
         -- Is the number format in hexidecimal? --
         if Trimmed_Text(Trimmed_Text'First .. Trimmed_Text'First + 1) = "0x" then
            Format := Hexidecimal;
         else 
            Format := Decimal;
         end if;
      exception
         when others =>
            Format := Decimal;
      end;
      
      case Format is
         when Hexidecimal =>
            declare
               Nibble_0 : Unsigned_32 := 0;
               Nibble_1 : Unsigned_32 := 0;
               Nibble_2 : Unsigned_32 := 0;
               Nibble_3 : Unsigned_32 := 0;
               Nibble_4 : Unsigned_32 := 0;
               Nibble_5 : Unsigned_32 := 0;
               Nibble_6 : Unsigned_32 := 0;
               Nibble_7 : Unsigned_32 := 0;
            begin
               case Trimmed_Text'Length is
                   when 3 =>
                      Nibble_0 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 2)));
                   when 4 =>
                      Nibble_1 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 2)));
                      Nibble_0 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 3)));
                   when 5 =>
                      Nibble_2 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 2)));
                      Nibble_1 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 3)));
                      Nibble_0 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 4)));
                   when 6 =>
                      Nibble_3 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 2)));
                      Nibble_2 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 3)));
                      Nibble_1 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 4)));
                      Nibble_0 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 5)));
                   when 7 =>
                      Nibble_4 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 2)));
                      Nibble_3 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 3)));
                      Nibble_2 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 4)));
                      Nibble_1 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 5)));
                      Nibble_0 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 6)));
                   when 8 =>
                      Nibble_5 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 2)));
                      Nibble_4 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 3)));
                      Nibble_3 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 4)));
                      Nibble_2 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 5)));
                      Nibble_1 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 6)));
                      Nibble_0 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 7)));
                   when 9 =>
                      Nibble_6 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 2)));
                      Nibble_5 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 3)));
                      Nibble_4 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 4)));
                      Nibble_3 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 5)));
                      Nibble_2 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 6)));
                      Nibble_1 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 7)));
                      Nibble_0 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 8)));
                   when 10 =>
                      Nibble_7 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 2)));
                      Nibble_6 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 3)));
                      Nibble_5 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 4)));
                      Nibble_4 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 5)));
                      Nibble_3 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 6)));
                      Nibble_2 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 7)));
                      Nibble_1 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 8)));
                      Nibble_0 := Unsigned_32(Hex_Digit_To_Number(Trimmed_Text(Trimmed_Text'First + 9)));
                   when others =>
                      raise Conversion_Failure;
               end case;

               Value := Shift_Left(Nibble_7, 28) or 
                        Shift_Left(Nibble_6, 24) or 
                        Shift_Left(Nibble_5, 20) or 
                        Shift_Left(Nibble_4, 16) or 
                        Shift_Left(Nibble_3, 12) or 
                        Shift_Left(Nibble_2,  8) or 
                        Shift_Left(Nibble_1,  4) or 
                        Nibble_0;
            end;
         when Decimal =>
            Value := Unsigned_32'Value(Trimmed_Text);
      end case;

      return Value;
   exception
      when others =>
          raise Conversion_Failure;
   end String_To_Unsigned_32; 
   
   -----------------------------
   -- TRIM_OUTSIDE_WHITESPACE --
   -----------------------------

   function Trim_Outside_Whitespace (Text : String) return String is
      Text_Begin : Natural := Text'First;
      Text_End   : Natural := Text'Last;
   begin

      for Index in Natural range Text'First .. Text'Last loop
         if Text(Index) = ' ' then
            Text_Begin := Index + 1;
         else
            exit;
         end if;
      end loop;

      for Index in reverse Natural range Text'First .. Text'Last loop
         if Text(Index) = ' ' then
            Text_End := Index - 1;
         else
            exit;
         end if;
      end loop;

      return Text(Text_Begin .. Text_End);

   end Trim_Outside_Whitespace;
   
   ----------------------------------------
   -- GET_CONFIG_FILE_FORMAT_FROM_STRING --
   ----------------------------------------

   function Get_Config_File_Format_From_String (Line : String) return Config_File_Format is
      Whitespace_Index : Natural;
      Type_Number      : Natural;
   begin

      if Index(Line, Configuration_Type_Tag) = 0 then
         return Undefined;
      end if;

      Whitespace_Index := Index(Line, " ");
      Type_Number      := Natural'Value(Line(Whitespace_Index .. Line'last));

      if Type_Number = 1 then
         return One;
      else 
         return Undefined;
      end if;
   
   exception
      when others =>
         return Undefined;
   end Get_Config_File_Format_From_String;
   
   ---------------------------------------------
   -- GET_USB_HID_DATALINK_CONFIG_FROM_STRING --
   ---------------------------------------------

   function Get_USB_HID_Datalink_Config_From_String(Config_Text : String) return Datalink_Configuration is
      type Parser_State is (Datalink_Text,
                            Whitespace_1,
                            USBHID_Text,
                            Whitespace_2,
                            Vendor_ID,
                            Whitespace_3);
      State                  : Parser_State := Datalink_Text;
      Configuration          : Datalink_Configuration(USB_HID);
      No_Configuration       : Datalink_Configuration(None);
      Vendor_ID_Start_Index  : Natural;
      Vendor_ID_End_Index    : Natural;
      Product_ID_Start_Index : Natural;
   begin

      begin
         if Config_Text(Config_Text'First .. Config_Text'First + USB_HID_Datalink_Tag'Length - 1) /= USB_HID_Datalink_Tag then
            return No_Configuration;
         end if;
      exception
         when others =>
            return No_Configuration;
      end;
      
      for Index in Natural range Config_Text'First .. Config_Text'Last loop
         case State is
            when Datalink_Text =>
               if Config_Text(Index) = ' ' then
                  State := Whitespace_1;
               end if;
            when Whitespace_1 =>
               if Config_Text(Index) /= ' ' then
                  State := USBHID_Text;
               end if;
            when USBHID_Text =>
               if Config_Text(Index) = ' ' then
                  State := Whitespace_2;
               end if;
            when Whitespace_2 =>
               if Config_Text(Index) /= ' ' then
                  State                 := Vendor_ID;
                  Vendor_ID_Start_Index := Index;
               end if;
            when Vendor_ID =>
               if Config_Text(Index) = ' ' then
                  State               := Whitespace_3;
                  Vendor_ID_End_Index := Index - 1;
               end if;
            when Whitespace_3 =>
               if Config_Text(Index) /= ' ' then
                  Product_ID_Start_Index := Index;
                  exit;
               end if;
         end case;
      end loop;
      
      begin
         Configuration.Vendor_ID  := String_To_Unsigned_16(Config_Text(Vendor_ID_Start_Index .. Vendor_ID_End_Index));
      exception
         when others =>
            raise Conversion_Failure_Vendor_ID;
      end;

      begin
         Configuration.Product_ID := String_To_Unsigned_16(Config_Text(Product_ID_Start_Index .. Config_Text'Last));
      exception
         when others =>
            raise Conversion_Failure_Product_ID;
      end;

      return Configuration;

   end Get_USB_HID_Datalink_Config_From_String;

   ----------------------------------------------
   -- GET_TCP_IPV4_DATALINK_CONFIG_FROM_STRING --
   ----------------------------------------------
   
   function Get_TCP_IPv4_Datalink_Config_From_String(Config_Text : String) return Datalink_Configuration is
      type Parser_State is (Datalink_Text,
                            Whitespace_1,
                            TCP_Text,
                            Whitespace_2,
                            Octet_1,
                            Octet_2,
                            Octet_3,
                            Octet_4);
      Configuration       : Datalink_Configuration(TCP_IPv4);
      No_Configuration    : Datalink_Configuration(None);
      Octet_1_Start_Index : Natural      := 0;
      Octet_1_End_Index   : Natural      := 0;
      Octet_2_Start_Index : Natural      := 0;
      Octet_2_End_Index   : Natural      := 0;
      Octet_3_Start_Index : Natural      := 0;
      Octet_3_End_Index   : Natural      := 0;
      Octet_4_Start_Index : Natural      := 0;
      Octet_4_End_Index   : Natural      := 0;
      Port_Start_Index    : Natural      := 0;
      State               : Parser_State := Datalink_Text;
   begin

      begin
         if Config_Text(Config_Text'First .. Config_Text'First + TCP_IPv4_Datalink_Tag'Length - 1) /= TCP_IPv4_Datalink_Tag then
            return No_Configuration;
         end if;
      exception
         when others =>
            return No_Configuration;
      end;

      for Index in Natural range Config_Text'First .. Config_Text'Last loop
         case State is
            when Datalink_Text =>
              if Config_Text(Index) = ' ' then 
                 State := Whitespace_1;
              end if;
            when Whitespace_1 =>
              if Config_Text(Index) /= ' ' then 
                 State := TCP_Text;
              end if;
            when TCP_Text =>
              if Config_Text(Index) = ' ' then 
                 State := Whitespace_2;
              end if;
            when Whitespace_2 =>
              if Config_Text(Index) /= ' ' then 
                 Octet_1_Start_Index := Index;
                 State               := Octet_1;
              end if;
            when Octet_1 =>
              if Config_Text(Index) = '.' then
                 Octet_1_End_Index   := Index - 1;
                 Octet_2_Start_Index := Index + 1;
                 State               := Octet_2;
              end if;
            when Octet_2 =>
              if Config_Text(Index) = '.' then
                 Octet_2_End_Index   := Index - 1;
                 Octet_3_Start_Index := Index + 1;
                 State               := Octet_3;
              end if;
            when Octet_3 =>
              if Config_Text(Index) = '.' then
                 Octet_3_End_Index   := Index - 1;
                 Octet_4_Start_Index := Index + 1;
                 State               := Octet_4;
              end if;
            when Octet_4 =>
              if Config_Text(Index) = ':' then
                 Octet_4_End_Index := Index - 1;
                 Port_Start_Index  := Index + 1;
                 exit;
              end if;
         end case;
      end loop;

      begin
         Configuration.Address_Octet_1 := Unsigned_8'Value(Config_Text(Octet_1_Start_Index .. Octet_1_End_Index));
         Configuration.Address_Octet_2 := Unsigned_8'Value(Config_Text(Octet_2_Start_Index .. Octet_2_End_Index));
         Configuration.Address_Octet_3 := Unsigned_8'Value(Config_Text(Octet_3_Start_Index .. Octet_3_End_Index));
         Configuration.Address_Octet_4 := Unsigned_8'Value(Config_Text(Octet_4_Start_Index .. Octet_4_End_Index));
      exception
         when others =>
            raise Conversion_Failure_IPv4_Address;
      end;

      begin
         Configuration.Port := Unsigned_16'Value(Config_Text(Port_Start_Index .. Config_Text'Last));
      exception
         when others =>
            raise Conversion_Failure_TCP_Port;
      end;
      
      return Configuration;
   
   end Get_TCP_IPv4_Datalink_Config_From_String;

  --------------------------------------------
   -- GET_SERIAL_DATALINK_CONFIG_FROM_STRING --
   --------------------------------------------

   function Get_Serial_Datalink_Config_From_String(Config_Text : String) return Datalink_Configuration is
      type Parser_State is (Datalink_Text,
                            Whitespace_1,
                            Serial_Text,
                            Whitespace_2,
                            Baud,
                            Whitespace_3,
                            Parity,
                            Whitespace_4,
                            Stop_Bits,
                            Whitespace_5,
                            Device_Name);
      Configuration           : Datalink_Configuration(Serial);
      No_Configuration        : Datalink_Configuration(None);
      Baud_Start_Index        : Natural      := 0;
      Baud_End_Index          : Natural      := 0;
      Parity_Start_Index      : Natural      := 0;
      Parity_End_Index        : Natural      := 0;
      Stop_Bits_Start_Index   : Natural      := 0;
      Stop_Bits_End_Index     : Natural      := 0;
      Device_Name_Start_Index : Natural      := 0;
      Device_Name_End_Index   : Natural      := 0;
      State                   : Parser_State := Datalink_Text;
   begin
      
      begin
         if Config_Text(Config_Text'First .. Config_Text'First + Serial_Datalink_Tag'Length - 1) /= Serial_Datalink_Tag then
            return No_Configuration;
         end if;
      exception
         when others =>
            return No_Configuration;
      end;

      begin
      for Index in Natural range Config_Text'First .. Config_Text'Last loop
         case State is
            when Datalink_Text =>
              if Config_Text(Index) = ' ' then 
                 State := Whitespace_1;
              end if;
            when Whitespace_1 =>
              if Config_Text(Index) /= ' ' then 
                 State := Serial_Text;
              end if;
            when Serial_Text =>
              if Config_Text(Index) = ' ' then 
                 State := Whitespace_2;
              end if;
            when Whitespace_2 =>
              if Config_Text(Index) /= ' ' then 
                 Baud_Start_Index := Index;
                 State            := Baud;
              end if;
            when Baud =>
              if Config_Text(Index) = ' ' then 
                 Baud_End_Index := Index - 1;
                 State          := Whitespace_3;
              end if;
            when Whitespace_3 =>
              if Config_Text(Index) /= ' ' then 
                 Parity_Start_Index := Index;
                 State              := Parity;
              end if;
            when Parity =>
              if Config_Text(Index) = ' ' then 
                 Parity_End_Index := Index - 1;
                 State            := Whitespace_4;
              end if;
            when Whitespace_4 =>
              if Config_Text(Index) /= ' ' then 
                 Stop_Bits_Start_Index := Index;
                 State                 := Stop_Bits;
              end if;
            when Stop_Bits =>
              if Config_Text(Index) = ' ' then 
                 Stop_Bits_End_Index := Index - 1;
                 State               := Whitespace_5;
              end if;
            when Whitespace_5 =>
              if Config_Text(Index) = '"' then 
                 Device_Name_Start_Index := Index + 1;
                 State                   := Device_Name;
              end if;
            when Device_Name =>
              if Config_Text(Index) = '"' then
                 Device_Name_End_Index := Index - 1;
                 exit;
              end if;
         end case;
      end loop;
      exception
         when others =>
            raise Syntax_Error;
      end;

      begin
         Configuration.Baud_Rate := Natural'Value(Config_Text(Baud_Start_Index .. Baud_End_Index));
      exception
         when others =>
            raise Conversion_Failure_Baud;
      end;

      begin
         Configuration.Parity := Parity_Type'Value(Config_Text(Parity_Start_Index .. Parity_End_Index));
      exception
         when others =>
            raise Conversion_Failure_Parity;
      end;
      
      declare
         Stop_Bits : Natural := Natural'Value(Config_Text(Stop_Bits_Start_Index .. Stop_Bits_End_Index));
      begin
         case Stop_Bits is
            when 0 =>
               Configuration.Stop_Bits := None;
            when 1 =>
               Configuration.Stop_Bits := One;
            when 2 =>
               Configuration.Stop_Bits := Two;
            when others =>
               raise Conversion_Failure_Stop_Bits;
         end case;
      exception
         when others =>
            raise Conversion_Failure_Stop_Bits;
      end;

      begin
         Configuration.Device_Name := UnStr.To_Unbounded_String(Config_Text(Device_Name_Start_Index .. Device_Name_End_Index));
      exception
         when others =>
            raise Conversion_Failure_Device_Name;
      end;

      return Configuration;

   end Get_Serial_Datalink_Config_From_String;

   -------------------------------------
   -- GET_DATALINK_CONFIG_FROM_STRING -- 
   -------------------------------------
   
   function Get_Datalink_Config_From_String(Config_Text : String) return Datalink_Configuration is
      Serial_Config   : Datalink_Configuration := Get_Serial_Datalink_Config_From_String(Config_Text);
      TCP_IPv4_Config : Datalink_Configuration := Get_TCP_IPv4_Datalink_Config_From_String(Config_Text);
      USBHID_Config   : Datalink_Configuration := Get_USB_HID_Datalink_Config_From_String(Config_Text);
      No_Config       : Datalink_Configuration(None);
   begin
      declare
      begin
         if Serial_Config.Datalink /= None then
            return Serial_Config;
         end if;

         if TCP_IPv4_Config.Datalink /= None then
            return TCP_IPv4_Config;
         end if;

         if USBHID_Config.Datalink /= None then
            return USBHID_Config;
         end if;

         return No_Config;
      end;
   end Get_Datalink_Config_From_String;
   
   -----------------------------------
   -- STRING_TO_ADAPTABLE_PARAMETER --
   -----------------------------------

   procedure String_To_Adaptable_Parameter (Record_Text     : in  String;
                                            Parameter       : out Adaptable_Parameter_Record;
                                            Parameter_Valid : out Boolean) is
      Tokens : String_Vectors.Vector;
   begin

      Parameter_Valid := False;

      Split_On_Delimeter(Record_Text, Delimeter_Symbol, Tokens);

      case Tokens.Length is
         when 5 =>
           -- Expect adaptable parameter with mode: Write Only (W) --
           declare
              Name_String  : String  := Trim_Outside_Whitespace(UnStr.To_String(Tokens.Element(0)));
              Units_String : String  := Trim_Outside_Whitespace(UnStr.To_String(Tokens.Element(1)));
              UID_String   : String  := Trim_Outside_Whitespace(UnStr.To_String(Tokens.Element(2)));
              Mode_String  : String  := Trim_Outside_Whitespace(UnStr.To_String(Tokens.Element(3)));
           begin

              -- Decode Read Write Mode --
              begin
                 String_To_Read_Write_Mode(Mode_String, Parameter.Is_Readable, Parameter.Is_Writable);
              exception
                 when Conversion_Failure =>
                    raise Conversion_Failure_Read_Write_Mode;
              end;

              if Parameter.Is_Readable = True or
                 Parameter.Is_Writable = False then
                 -- Invalid number of tokens for this read/write mode --
                 raise Syntax_Error;
              end if;

              Parameter.Is_Sampling   := False;
              Parameter.Friendly_Name := UnStr.To_Unbounded_String(Name_String);
              Parameter.Units_Name    := UnStr.To_Unbounded_String(Units_String);

              begin
                 Parameter.Unique_Identifier := String_To_Unsigned_16(UID_String);
              exception
                 when others =>
                    raise Conversion_Failure_UID;
              end;

              declare
                 Token : String := Trim_Outside_Whitespace(UnStr.To_String(Tokens.Element(4)));
              begin
                 if Token(Token'First .. Token'First + Default_Set_Value_Tag'Length - 1) = Default_Set_Value_Tag then
                    Parameter.Default_Set_Value := String_To_Unsigned_32(Token(Token'First + Default_Set_Value_Tag'Length .. Token'Last));
                 else
                    raise Conversion_Failure_Default_Set_Value;
                 end if;
              exception
                 when others =>
                    raise Conversion_Failure_Default_Set_Value;
              end;
           end;
         when 6 =>
           -- Expect adaptable parameter with mode: Read Only (R) --
           declare
              Name_String             : String  := Trim_Outside_Whitespace(UnStr.To_String(Tokens.Element(0)));
              Units_String            : String  := Trim_Outside_Whitespace(UnStr.To_String(Tokens.Element(1)));
              UID_String              : String  := Trim_Outside_Whitespace(UnStr.To_String(Tokens.Element(2)));
              Mode_String             : String  := Trim_Outside_Whitespace(UnStr.To_String(Tokens.Element(3)));
              Is_Sampling_Defined     : Boolean := False;
              Sampling_Period_Defined : Boolean := False;
           begin
              
              -- Decode Read Write Mode --
              begin
                 String_To_Read_Write_Mode(Mode_String, Parameter.Is_Readable, Parameter.Is_Writable);
              exception
                 when Conversion_Failure =>
                    raise Conversion_Failure_Read_Write_Mode;
              end;

              if Parameter.Is_Readable = False or
                 Parameter.Is_Writable = True then
                 -- Invalid number of tokens for this read/write mode --
                 raise Syntax_Error;
              end if;

              Parameter.Friendly_Name := UnStr.To_Unbounded_String(Name_String);
              Parameter.Units_Name    := UnStr.To_Unbounded_String(Units_String);
              
              begin
                 Parameter.Unique_Identifier := String_To_Unsigned_16(UID_String);
              exception
                 when others =>
                    raise Conversion_Failure_UID;
              end;

              for Token_Index in Natural range 4 .. 5 loop
                 declare
                    Token : String := Trim_Outside_Whitespace(UnStr.To_String(Tokens.Element(Token_Index)));
                 begin
                    begin
                       if Token(Token'First .. Token'First + Sampling_Tag'Length - 1) = Sampling_Tag then
                          Parameter.Is_Sampling := Boolean'Value(Token(Token'First + Sampling_Tag'Length .. Token'Last));
                          Is_Sampling_Defined   := True;
                       end if;
                    exception
                       when Conversion_Failure =>
                          raise Conversion_Failure_Is_Sampling;
                       when others =>
                          null;
                    end;
                    begin
                       if Token(Token'First .. Token'First + Sample_Period_Tag'Length - 1) = Sample_Period_Tag then
                          Parameter.Sample_Period := String_To_Duration(Token(Token'First + Sample_Period_Tag'Length .. Token'Last));
                          Sampling_Period_Defined := True;
                       end if;
                    exception
                       when Constraint_Error =>
                          raise Conversion_Failure_Sample_Period;
                       when others =>
                          null;
                    end;
                 end;
              end loop;

              if not Is_Sampling_Defined then
                 raise Parameter_Missing_Is_Sampling;
              end if;

              if not Sampling_Period_Defined then
                 raise Parameter_Missing_Sampling_Period;
              end if;
           end;
         when 7 =>
           -- Expect adaptable parameter with mode: Read/Write (RW) --
           declare
              Name_String               : String  := Trim_Outside_Whitespace(UnStr.To_String(Tokens.Element(0)));
              Units_String              : String  := Trim_Outside_Whitespace(UnStr.To_String(Tokens.Element(1)));
              UID_String                : String  := Trim_Outside_Whitespace(UnStr.To_String(Tokens.Element(2)));
              Mode_String               : String  := Trim_Outside_Whitespace(UnStr.To_String(Tokens.Element(3)));
              Default_Set_Value_Defined : Boolean := False;
              Is_Sampling_Defined       : Boolean := False;
              Sampling_Period_Defined   : Boolean := False;
           begin
              
              -- Decode Read Write Mode --
              begin
                 String_To_Read_Write_Mode(Mode_String, Parameter.Is_Readable, Parameter.Is_Writable);
              exception
                 when Conversion_Failure =>
                    raise Conversion_Failure_Read_Write_Mode;
              end;

              if Parameter.Is_Readable = False or
                 Parameter.Is_Writable = False then
                 -- Invalid number of tokens for this read/write mode --
                 raise Syntax_Error;
              end if;

              Parameter.Friendly_Name := UnStr.To_Unbounded_String(Name_String);
              Parameter.Units_Name    := UnStr.To_Unbounded_String(Units_String);
              
              begin
                 Parameter.Unique_Identifier := String_To_Unsigned_16(UID_String);
              exception
                 when others =>
                    raise Conversion_Failure_UID;
              end;

              for Token_Index in Natural range 4 .. 6 loop
                 declare
                    Token : String := Trim_Outside_Whitespace(UnStr.To_String(Tokens.Element(Token_Index)));
                 begin
                    begin
                       if Token(Token'First .. Token'First + Sampling_Tag'Length - 1) = Sampling_Tag then
                          begin
                             Parameter.Is_Sampling := Boolean'Value(Token(Token'First + Sampling_Tag'Length .. Token'Last));
                          exception
                             when Constraint_Error =>
                                raise Conversion_Failure_Is_Sampling;
                          end;
                          Is_Sampling_Defined   := True;
                       end if;
                    exception
                       when Constraint_Error =>
                          raise Syntax_Error;
                       when others =>
                          null;
                    end;
                    begin
                       if Token(Token'First .. Token'First + Sample_Period_Tag'Length - 1) = Sample_Period_Tag then
                          Parameter.Sample_Period := String_To_Duration(Token(Token'First + Sample_Period_Tag'Length .. Token'Last));
                          Sampling_Period_Defined := True;
                       end if;
                    exception
                       when Constraint_Error =>
                          raise Conversion_Failure_Sample_Period;
                       when others =>
                          null;
                    end;
                    begin
                       if Token(Token'First .. Token'First + Default_Set_Value_Tag'Length - 1) = Default_Set_Value_Tag then
                          Parameter.Default_Set_Value := String_To_Unsigned_32(Token(Token'First + Default_Set_Value_Tag'Length .. Token'Last));
                          Default_Set_Value_Defined   := True;
                       end if;
                    exception
                       when Conversion_Failure =>
                          raise Conversion_Failure_Default_Set_Value;
                       when others =>
                          null;
                    end;
                 end;
              end loop;

              if not Is_Sampling_Defined then
                 raise Parameter_Missing_Is_Sampling;
              end if;

              if not Sampling_Period_Defined then
                 raise Parameter_Missing_Sampling_Period;
              end if;

              if not Default_Set_Value_Defined then
                 raise Parameter_Missing_Default_Set_Value;
              end if;
           end;
         when others =>
           -- Malformed parameter record string --
           return;
      end case;

      Parameter_Valid := True;

   end String_To_Adaptable_Parameter;

   --------------------------------------
   -- GET_CONFIG_FILE_FORMAT_FROM_FILE --
   --------------------------------------

   function Get_Config_File_Format_From_File(File_Name : String) return Config_File_Format is
      Config_File : File_Type;
      File_Format : Config_File_Format := Undefined;
   begin
      Open(Config_File, In_File, File_Name);

      while not End_Of_File(Config_File) loop
         declare
            Line                     : String := Get_Line(Config_File);
            Current_Line_File_Format : Config_File_Format;
         begin
            -- Is this a non-comment line? --
            if Line(Line'First) /= '#' then
               Current_Line_File_Format := Get_Config_File_Format_From_String(Line);

               case Current_Line_File_Format is
                  when One =>
                     if File_Format = Undefined then
                        File_Format := One;
                     else
                        Close(Config_File);
                        raise Too_Many_File_Formats_Specified;
                     end if;
                  when Undefined =>
                     null;
               end case;
            end if;
         exception
            when Constraint_Error =>
               null;
         end;
      end loop;
      Close(Config_File);
      return File_Format;
   end Get_Config_File_Format_From_File;

   --------------------------
   -- GET_CONFIG_FROM_FILE --
   --------------------------

   procedure Get_Config_From_File(File_Name            : in String;
                                  Adaptable_Parameters : out Adaptable_Parameter_Record_Vectors.Vector;
                                  Config               : out Datalink_Configuration;
                                  Error_Text           : out UnStr.Unbounded_String;
                                  Config_Valid         : out Boolean) is
      Config_File     : File_Type;
      File_Format     : Config_File_Format;
      Line_Number     : Natural := 1;
      Config_Defined  : Boolean := False;
   begin

      Config_Valid := False;

      -- Identify configuration file format --

      File_Format := Get_Config_File_Format_From_File(File_Name);

      if File_Format = Undefined then
         Error_Text := UnStr.To_Unbounded_String("Error: No configuration file format defined. Add 'IQ_Config_Format 1' to config file.");
         return;
      end if;

      -- Parse configuration data from file --

      Open(Config_File, In_File, File_Name);

      while not End_Of_File(Config_File) loop
         declare
            Line            : String  := Get_Line(Config_File);
         begin
            -- Is this a non-comment line? --
            if Line(Line'First) /= '#' then

               -- Parse line as a datalink configuration line --
               declare
                  Line_Config : Datalink_Configuration := Get_Datalink_Config_From_String(Line);
               begin
                  -- Only one datalink configuration declaration per         --
                  -- configuration file is allowed. If two or more are found --
                  -- generate an error.                                      --
                  if Line_Config.Datalink /= None then
                     if Config_Defined then
                        Close(Config_File);
                        Config_Valid := False;
                        UnStr.Append(Error_Text, UnStr.To_Unbounded_String("Error: More than one Datalink declaration specified for configuration file: " & File_Name & CRLF));
                        UnStr.Append(Error_Text, UnStr.To_Unbounded_String("Second declaration found on line:" & Natural'image(Line_Number) & CRLF));
                        return;
                     else
                        -- Valid datalink configuration found --
                        Config         := Line_Config;
                        Config_Defined := True;
                        Config_Valid   := True;
                        goto Next_Line;
                     end if;
                  end if;
               exception
                  when Syntax_Error =>
                     Close(Config_File);
                     Config_Valid := False;
                     UnStr.Append(Error_Text, UnStr.To_Unbounded_String("Error: Syntax error: Line:" & Natural'image(Line_Number) & " in configuration file: " & File_Name & CRLF));
                     return;
                  when Conversion_Failure =>
                     Close(Config_File);
                     Config_Valid := False;
                     UnStr.Append(Error_Text, UnStr.To_Unbounded_String("Error: Conversion Failure: Line:" & Natural'image(Line_Number) & " in configuration file: " & File_Name & CRLF));
                     return;
                  when Conversion_Failure_Stop_Bits =>
                     Close(Config_File);
                     Config_Valid := False;
                     UnStr.Append(Error_Text, UnStr.To_Unbounded_String("Error: Conversion Failure: Invalid value for Stop Bits on Line:" & Natural'image(Line_Number) & " in configuration file: " & File_Name & CRLF));
                     return;
                  when Conversion_Failure_Baud =>
                     Close(Config_File);
                     Config_Valid := False;
                     UnStr.Append(Error_Text, UnStr.To_Unbounded_String("Error: Conversion Failure: Invalid value for Baud Rate on Line:" & Natural'image(Line_Number) & " in configuration file: " & File_Name & CRLF));
                     return;
                  when Conversion_Failure_Device_Name =>
                     Close(Config_File);
                     Config_Valid := False;
                     UnStr.Append(Error_Text, UnStr.To_Unbounded_String("Error: Conversion Failure: Invalid value for Device Name on Line:" & Natural'image(Line_Number) & " in configuration file: " & File_Name & CRLF));
                     return;
                  when Conversion_Failure_Parity =>
                     Close(Config_File);
                     Config_Valid := False;
                     UnStr.Append(Error_Text, UnStr.To_Unbounded_String("Error: Conversion Failure: Invalid value for Parity on Line:" & Natural'image(Line_Number) & " in configuration file: " & File_Name & CRLF));
                     return;
                  when Conversion_Failure_IPv4_Address =>
                     Close(Config_File);
                     Config_Valid := False;
                     UnStr.Append(Error_Text, UnStr.To_Unbounded_String("Error: Conversion Failure: Invalid value for IPv4 Address on Line:" & Natural'image(Line_Number) & " in configuration file: " & File_Name & CRLF));
                     return;
                  when Conversion_Failure_TCP_Port =>
                     Close(Config_File);
                     Config_Valid := False;
                     UnStr.Append(Error_Text, UnStr.To_Unbounded_String("Error: Conversion Failure: Invalid value for TCP Port on Line:" & Natural'image(Line_Number) & " in configuration file: " & File_Name & CRLF));
                     return;
                  when Conversion_Failure_Vendor_ID =>
                     Close(Config_File);
                     Config_Valid := False;
                     UnStr.Append(Error_Text, UnStr.To_Unbounded_String("Error: Conversion Failure: Invalid value for USB Vendor ID on Line:" & Natural'image(Line_Number) & " in configuration file: " & File_Name & CRLF));
                     return;
                  when Conversion_Failure_Product_ID =>
                     Close(Config_File);
                     Config_Valid := False;
                     UnStr.Append(Error_Text, UnStr.To_Unbounded_String("Error: Conversion Failure: Invalid value for USB Product ID on Line:" & Natural'image(Line_Number) & " in configuration file: " & File_Name & CRLF));
               end;

               -- Parse line as a adaptable parameter record declaration --
               declare
                  Parameter       : Adaptable_Parameter_Record;
                  Parameter_Valid : Boolean;
               begin
                  String_To_Adaptable_Parameter(Line, Parameter, Parameter_Valid);

                  if Parameter_Valid then
                     Adaptable_Parameters.Append(Parameter);
                  end if;

               exception
                  when Syntax_Error =>
                     UnStr.Append(Error_Text, UnStr.To_Unbounded_String(File_Name & ":" & Natural'image(Line_Number) & " Error: Adaptable parameter declaration syntax error. Parameter not added to control panel." & CRLF));
                  when Parameter_Missing_Default_Set_Value =>
                     UnStr.Append(Error_Text, UnStr.To_Unbounded_String(File_Name & ":" & Natural'image(Line_Number) & " Error: Writable adaptable parameter declaration missing '" & Default_Set_Value_Tag & "' field. Parameter not added to control panel." & CRLF));
                  when Parameter_Missing_UID =>
                     UnStr.Append(Error_Text, UnStr.To_Unbounded_String(File_Name & ":" & Natural'image(Line_Number) & " Error: Adaptable parameter declaration missing 'UID' field. Parameter not added to control panel." & CRLF));
                  when Parameter_Missing_Sampling_Period =>
                     UnStr.Append(Error_Text, UnStr.To_Unbounded_String(File_Name & ":" & Natural'image(Line_Number) & " Error: Readable adaptable parameter declaration missing '" & Sample_Period_Tag & "' field. Parameter not added to control panel." & CRLF));
                  when Parameter_Missing_Is_Sampling =>
                     UnStr.Append(Error_Text, UnStr.To_Unbounded_String(File_Name & ":" & Natural'image(Line_Number) & " Error: Readable adaptable parameter declaration missing '" & Sampling_Tag & "' field. Parameter not added to control panel." & CRLF));
                  when Conversion_Failure_Is_Sampling =>
                     UnStr.Append(Error_Text, UnStr.To_Unbounded_String(File_Name & ":" & Natural'image(Line_Number) & " Error: Invalid value for '" & Sampling_Tag & "' field in adaptable parameter declaration. Parameter not added to control panel." & CRLF));
                  when Conversion_Failure_Read_Write_Mode =>
                     UnStr.Append(Error_Text, UnStr.To_Unbounded_String(File_Name & ":" & Natural'image(Line_Number) & " Error: Invalid value for 'Read/Write Mode' field in adaptable parameter declaration. Parameter not added to control panel." & CRLF));
                  when Conversion_Failure_Sample_Period =>
                     UnStr.Append(Error_Text, UnStr.To_Unbounded_String(File_Name & ":" & Natural'image(Line_Number) & " Error: Invalid value for '" & Sample_Period_Tag & "' field in adaptable parameter declaration. Parameter not added to control panel." & CRLF));
                  when Conversion_Failure_UID =>
                     UnStr.Append(Error_Text, UnStr.To_Unbounded_String(File_Name & ":" & Natural'image(Line_Number) & " Error: Invalid value for 'UID' field in adaptable parameter declaration. Parameter not added to control panel." & CRLF));
                  when Conversion_Failure_Default_Set_Value =>
                     UnStr.Append(Error_Text, UnStr.To_Unbounded_String(File_Name & ":" & Natural'image(Line_Number) & " Error: Invalid value for '" & Default_Set_Value_Tag & "' field in adaptable parameter declaration. Parameter not added to control panel." & CRLF));
               end;
            end if;
         exception
            when Constraint_Error =>
               null;
         end;
      <<Next_Line>>
         Line_Number := Line_Number + 1;
      end loop;
      Close(Config_File);
   exception
      when Too_Many_File_Formats_Specified =>
         Error_Text := UnStr.To_Unbounded_String("Error: More than one IQ_Config_Format declaration specified for configuration file: " & File_Name);
   end Get_Config_From_File;

end Configuration;
