-----------------------------------------------------------------
--                                                             --
-- USBHID Specification                                        --
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
with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings;
with System;

package USBHID is

   -- Ada binding to the C USB HIDAPI by Alan Ott --

   subtype Size_t is Unsigned_Long;
   subtype Wint_t is Unsigned;

   type Device_Info is record
      Path                : Interfaces.C.Strings.Chars_Ptr;
      Vendor_ID           : aliased Unsigned_Short;
      Product_ID          : aliased Unsigned_Short;
      Serial_Number       : access  Wchar_t;
      Release_Number      : aliased Unsigned_Short;
      Manufacturer_String : access  WChar_t;
      Product_String      : access  WChar_t;
      Usage_Page          : aliased Unsigned_Short;
      Usage               : aliased Unsigned_Short;
      Interface_Number    : aliased Int;
      Next                : access  Device_Info;
   end record;

   function Initialize return Int;

   function Free return Int;
   -- This function frees all of the static data associated with   --
   -- HIDAPI. It should be called at the end of execution to avoid --
   -- memory leaks.                                                --
   -- Return  0: Success                                           --
   -- Return -1: Error                                             --

   function Enumerate (Vendor_ID  : Unsigned_Short;
                       Product_ID : Unsigned_Short) return access Device_Info;
   -- This function returns a linked list of all the HID devices   --
   -- attached to the system which match vendor_id and product_id. --

   procedure Free_Enumeration (devs : access Device_Info);
   -- This function frees a linked list created by Enumerate --

   function Open (Vendor_ID     : Unsigned_16;
                  Product_ID    : Unsigned_16;
                  Serial_Number : System.Address) return System.Address;
   -- Return Null: Failure --

   function Open_Path (Path : Interfaces.C.Strings.Chars_Ptr) return System.Address;
   -- Return Null: Failure --

   function Write (Device : System.Address;
                   Data   : System.Address;
                   Length : Size_t) return Int;
   -- Return  N: 'N' bytes written --
   -- Return -1: Error             --

   function Read_Timeout (Dev          : System.Address;
                          Data         : System.Address;
                          Length       : Size_t;
                          Milliseconds : Int) return Int;
   -- Read an Input report from a HID device with timeout.         --
   -- Return  N: 'N' bytes read                                    --
   -- Return  0: No packet was available before the timeout period --
   -- Return -1: Error                                             --

   function Read (Device : System.Address;
                  Data   : access Unsigned_Char;
                  Length : Size_t) return Int;
   -- Read an Input report from a HID device. --
   -- Return  N: 'N' bytes read --
   -- Return -1: Error          --

   function Set_Nonblocking (Device   : System.Address;
                             Nonblock : Int) return int;
   -- Set the device handle to be non-blocking. --
   -- Return  0: Success                        --
   -- Return -1: Error                          --

   function Send_Feature_Report (Device : System.Address;
                                 Data   : access Unsigned_Char;
                                 Length : Size_t) return Int;

   function Get_Feature_Report (Device : System.Address;
                                Data   : access Unsigned_Char;
                                Length : Size_t) return Int;

   procedure Close (Device : System.Address);

   function Get_Manufacturer_String (Device : System.Address;
                                     String : access Wchar_t;
                                     Maxlen : Size_t) return Int;

   function Get_Product_String (Device : System.Address;
                                String : access Wchar_t;
                                Maxlen : Size_t) return Int;

   function Get_Serial_Number_String (Device : System.Address;
                                      String : access Wchar_t;
                                      Maxlen : Size_t) return Int;

   function Get_Indexed_String (Device       : System.Address;
                                String_Index : Int;
                                String       : access Wchar_t;
                                Maxlen       : Size_t) return Int;

   function Error (device : System.Address) return access wchar_t;

   pragma Convention (C_Pass_By_Copy, Device_Info);
   pragma Import (C, Initialize,               "hid_init");
   pragma Import (C, Free,                     "hid_exit");
   pragma Import (C, Enumerate,                "hid_enumerate");
   pragma Import (C, Free_Enumeration,         "hid_free_enumeration");
   pragma Import (C, Open,                     "hid_open");
   pragma Import (C, Open_Path,                "hid_open_path");
   pragma Import (C, Write,                    "hid_write");
   pragma Import (C, Read_Timeout,             "hid_read_timeout");
   pragma Import (C, Read,                     "hid_read");
   pragma Import (C, Set_Nonblocking,          "hid_set_nonblocking");
   pragma Import (C, Send_Feature_Report,      "hid_send_feature_report");
   pragma Import (C, Get_Feature_Report,       "hid_get_feature_report");
   pragma Import (C, Close,                    "hid_close");
   pragma Import (C, Get_Manufacturer_String,  "hid_get_manufacturer_string");
   pragma Import (C, Get_Product_String,       "hid_get_product_string");
   pragma Import (C, Get_Serial_Number_String, "hid_get_serial_number_string");
   pragma Import (C, Get_Indexed_String,       "hid_get_indexed_string");
   pragma Import (C, Error,                    "hid_error");

end USBHID;
