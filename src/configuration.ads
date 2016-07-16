-----------------------------------------------------------------
--                                                             --
-- CONFIGURATION Specification                                 --
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
with Ada.Characters.Latin_1;
with Ada.Containers.Indefinite_Vectors; use Ada.Containers;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Interfaces;                        use Interfaces;
with Util;                              use Util;

package Configuration is

   use Ada.Characters;

   type Byte is mod 2**8;
   type Word is mod 2**16;
   type Double_Word is mod 2**32;
   
   Too_Many_File_Formats_Specified            : exception;
   Syntax_Error                               : exception;
   Parameter_Missing_Default_Set_Value        : exception;
   Parameter_Missing_UID                      : exception;
   Parameter_Missing_Sampling_Period          : exception;
   Parameter_Missing_Is_Sampling              : exception;
   Conversion_Failure                         : exception;
   Conversion_Failure_UID                     : exception;
   Conversion_Failure_Default_Set_Value       : exception;
   Conversion_Failure_Is_Sampling             : exception;
   Conversion_Failure_Read_Write_Mode         : exception;
   Conversion_Failure_Sample_Period           : exception;
   Conversion_Failure_Stop_Bits               : exception;
   Conversion_Failure_Baud                    : exception;
   Conversion_Failure_Device_Name             : exception;
   Conversion_Failure_Parity                  : exception;
   Conversion_Failure_IPv4_Address            : exception;
   Conversion_Failure_TCP_Port                : exception;
   Conversion_Failure_Vendor_ID               : exception;
   Conversion_Failure_Product_ID              : exception;
   Conversion_Failure_Address                 : exception;
   IQ_Protocol_Declaration_Syntax_Error       : exception;
   NVP_Routing_Declaration_Syntax_Error       : exception;
   Protocol_Declaration_Address_Size_Mismatch : exception;
   
   type Protocol_Type is (NVP, NVP_With_Routing, IQ, None);
   type Address_Size_Type is (Byte_Sized, Word_Sized, Double_Word_Sized, None);
   type Parity_Type is (Even, Odd, None);
   type Stop_Bits_Type is (Two, One, None);
   type Datalink_Type is (None, TCP_IPv4, Serial, USB_HID);
   type Config_File_Format is (One, Undefined);

   type Address_Type is record
      Address : Unsigned_32;
      Size    : Address_Size_Type;
   end record;

   type Protocol_Configuration (Protocol : Protocol_Type := None) is record
      case Protocol is
         when NVP_With_Routing | IQ =>
            Source       : Address_Type;
            Destination  : Address_Type;
         when NVP | None =>
            null;
      end case;
   end record;

   type Datalink_Configuration (Datalink : Datalink_Type := None) is record
      case Datalink is
         when TCP_IPv4 =>
            Address_Octet_1 : Unsigned_8;
            Address_Octet_2 : Unsigned_8;
            Address_Octet_3 : Unsigned_8;
            Address_Octet_4 : Unsigned_8;
            Port            : Unsigned_16;
         when Serial =>
            Baud_Rate       : Natural;
            Stop_Bits       : Stop_Bits_Type;
            Parity          : Parity_Type;
            Device_Name     : UnStr.Unbounded_String;
         when USB_HID =>
            Vendor_ID       : Unsigned_16;
            Product_ID      : Unsigned_16;
         when None =>
            null;
      end case;
   end record;

   type Adaptable_Parameter_Record is record
      Friendly_Name       : UnStr.Unbounded_String;
      Units_Name          : UnStr.Unbounded_String;
      Unique_Identifier   : Unsigned_16;
      Default_Set_Value   : Unsigned_32;
      Is_Sampling         : Boolean;
      Is_Readable         : Boolean;
      Is_Writable         : Boolean;
      Sample_Period       : UnStr.Unbounded_String;
   end record;

   CRLF                   : constant String := Latin_1.CR & Latin_1.LF;

   package Adaptable_Parameter_Record_Vectors is new Indefinite_Vectors (Natural, Adaptable_Parameter_Record);

   function Datalink_Configuration_To_String(Config : Datalink_Configuration) return String;

   procedure Dump_Adaptable_Parameter(Parameter : Adaptable_Parameter_Record);

   function Get_Configuration_File_Name return String;
   -- Look in the current directory for files with names ending in ".iq". --
   -- If only one file exists that filename is returned.                   --
   -- If more than one file is found then an empty string is returned.     --

   function Hex_Digit_To_Number (Digit : Character) return Natural;

   procedure Split_On_Delimeter(Text      : in String;
                                Delimeter : in Character;
                                Tokens    : out String_Vectors.Vector);

   function Strip_Whitespace (Text : String) return String;
   -- Remove all tab and space characters from a string --

   function String_To_Duration (Text : String) return Duration;
   -- Supports the following units: 
   --   Seconds      (s)
   --   Milliseconds (ms)
   --   Microseconds (us)

   procedure String_To_Read_Write_Mode (Text  : in  String; 
                                        Read  : out Boolean;
                                        Write : out Boolean);
   -- When a string 'Text' of one or two characters is input:             --
   -- Read is true if one of the characters is an 'R' (case insensitive),
   -- and false otherwise.
   -- Write is true if one of the characters is an 'W' (case insensitive). --

   function String_To_Unsigned_16 (Text : String) return Unsigned_16;
   -- Convert a decimal or hexidecimal string to an unsigned_16 type --
   -- Hexidecimal numbers must be prefixed with '0x'                 --

   function String_To_Unsigned_32 (Text : String) return Unsigned_32;
   -- Convert a decimal or hexidecimal string to an unsigned_32 type --
   -- Hexidecimal numbers must be prefixed with '0x'                 --

   function Trim_Outside_Whitespace (Text : String) return String;
   -- Remove space characters from the begining and end of a string --

   function Get_Config_File_Format_From_String (Line : String) return Config_File_Format;
   -- Configuration File Format Line Format --
   -- IQ_Config_Format <Format Number>      --
   --                                       --
   -- Example:                              --
   -- IQ_Config_Format 1                    --

   function Get_USB_HID_Datalink_Config_From_String(Config_Text : String) return Datalink_Configuration;
   -- USB HID Datalink Configuration Line Format --
   -- Datalink USBHID <Vendor ID> <Product ID>   --
   --                                            --
   -- Example:                                   --
   -- Datalink USBHID 0x1001 0xA008              --

   function Get_TCP_IPv4_Datalink_Config_From_String(Config_Text : String) return Datalink_Configuration;
   -- TCP IPv4 Datalink Configuration Line Format --
   -- Datalink TCPv4 <IPv4 Address> : <Port>      --
   --                                             --
   -- Example:                                    --
   -- Datalink TCPv4 1.0.0.5 : 32000              --

   function Get_Serial_Datalink_Config_From_String(Config_Text : String) return Datalink_Configuration;
   -- Serial Datalink Configuration Line Format                      --
   -- Datalink Serial <Baud Rate> <Parity> <Stop Bits> <Device Name> --
   --                                                                --
   -- Example:                                                       --
   -- Datalink Serial 115200 None 1 "/dev/ttyUSB0"                   --

   function Get_Datalink_Config_From_String(Config_Text : String) return Datalink_Configuration;

   function Get_Protocol_Config_From_String(Protocol_Text : String) return Protocol_Configuration;
   -- Protocol Configuration Line Format                  --
   --                                                     --
   -- NVP Protocol Format:                                --
   -- Protocol NVP                                        --
   --                                                     --
   -- NVP Protocol with Routing Format:                   --
   -- Protocol NVP Routing <IQ Address> <Target Address>  --
   --                                                     --
   -- Example with 8-bit address size:                    --
   -- Protocol NVP Routing 0x00 0xC3                      --
   -- Example with 16-bit address size:                   --
   -- Protocol NVP Routing 0x0001 0x10C3                  --
   -- Example with 32-bit address size:                   --
   -- Protocol NVP Routing 0xF0000001 0x57001000          --
   --                                                     --
   -- IQ Protocol Format:                                 --
   -- Protocol IQ <IQ Address> <Target Address>           --
   --                                                     --
   -- Example with 8-bit address size:                    --
   -- Protocol IQ 0x00 0xC3                               --
   -- Example with 16-bit address size:                   --
   -- Protocol IQ 0x0001 0x10C3                           --
   -- Example with 32-bit address size:                   --
   -- Protocol IQ 0xF0000001 0x57001000                   --

   function Get_Protocol_Config_From_File(File_Name : String) return Protocol_Configuration;

   procedure String_To_Adaptable_Parameter (Record_Text     : in  String;
                                            Parameter       : out Adaptable_Parameter_Record;
                                            Parameter_Valid : out Boolean);
   -- Adaptable Parameter Line Formats:                                                                                                              --
   -- <Adaptable Parameter Name> | <Units Name> | <UID> | W  | Default Set Value: <Value>                                                            --
   -- <Adaptable Parameter Name> | <Units Name> | <UID> | R  | Sampling: <Is Sampling> | Sample Period: <Sample Period>                              --
   -- <Adaptable Parameter Name> | <Units Name> | <UID> | RW | Sampling: <Is Sampling> | Sample Period: <Sample Period> | Default Set Value: <Value> --
   --                                                                                                                                                --
   -- Examples:                                                                                                                                      --
   -- Reboot System  |       | 0xFF01 | W  | Default Set Value: 1                                                                                    --
   -- System Voltage | Volts | 0x0001 | R  | Sampling: True  | Sample Period: 100 ms                                                                 --
   -- Test Mode      |       | 0xFF00 | RW | Sampling: False | Sample Period:   1 s | Default Set Value: 1                                           --

   function Get_Config_File_Format_From_File(File_Name : String) return Config_File_Format;

   procedure Get_Config_From_File(File_Name            : in String;
                                  Adaptable_Parameters : out Adaptable_Parameter_Record_Vectors.Vector;
                                  Config               : out Datalink_Configuration;
                                  Error_Text           : out UnStr.Unbounded_String;
                                  Config_Valid         : out Boolean);

   function To_Hex_String(Input : Unsigned_8) return String;

   function To_Hex_String(Input : Unsigned_16) return String;

   function To_Address_Type(Text : String) return Address_Type;
   -- Convert an 8-bit, 16-bit, or 32-bit hexidecimal address string to an Address_Type --

end Configuration;
