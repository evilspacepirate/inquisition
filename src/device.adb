-----------------------------------------------------------------
--                                                             --
-- DEVICE                                                      --
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
with Configuration; use Configuration;
with Interfaces.C;  use Interfaces.C;
with Util;          use Util;
with USBHID;
with System;        use System;

package body Device is

   Read_Timeout_In_ms : constant := 100;

   Active_Config      : Datalink_Configuration;
   USBHID_Device      : System.Address;

   Connection_State   : Connection_State_Type := Not_Connected;

   ---------------
   -- SEND_DATA --
   ---------------

   procedure Send_Data (Data : Unsigned_8_Array) is
   begin
      case Active_Config.Datalink is
         when TCP_IPv4 =>
            -- TODO Not supported yet TODO --
            raise Invalid_DataLink_Config;
         when Serial =>
            -- TODO Not supported yet TODO --
            raise Invalid_DataLink_Config;
         when USB_HID =>
            declare
               Result : Int;
            begin
               Result := USBHID.Write_Data_Prepend_Length(USBHID_Device, Data);
            end;
         when None =>
            raise Invalid_DataLink_Config;
      end case;
   end Send_Data;

   --------------
   -- GET_DATA --
   --------------

   function Get_Data return Unsigned_8_Array is
      Read_Buffer : Unsigned_8_Array(1 .. Maximum_Transmission_Unit);
      No_Data     : Unsigned_8_Array(1 .. 0);
      Result      : Int;
   begin
      case Active_Config.Datalink is
         when USB_HID =>
            Result := USBHID.Read_Timeout(USBHID_Device,
                                          Read_Buffer'Address,
                                          Maximum_Transmission_Unit,
                                          Read_Timeout_In_ms);

            if Result = 0 then
               return No_Data;
            elsif Result = -1 then
               raise Communications_Error;
            else
               return Read_Buffer(1 .. Natural(Result));
            end if;
         when others =>
            -- TODO --
            raise Invalid_DataLink_Config;
      end case;
   end Get_Data;

   -------------
   -- CONNECT --
   -------------

   procedure Connect (Config : Datalink_Configuration) is
   begin
      if Connection_State = Connected then
         raise Connection_Already_Established;
      end if;

      case Config.Datalink is
         when TCP_IPv4 =>
            -- TODO Not supported yet TODO --
            raise Invalid_DataLink_Config;
         when Serial =>
            -- TODO Not supported yet TODO --
            raise Invalid_DataLink_Config;
         when USB_HID =>
            USBHID_Device := USBHID.Open(Config.Vendor_ID,
                                         Config.Product_ID,
                                         System.Null_Address);
            if USBHID_Device = System.Null_Address then
               raise Error_Opening_Device;
            end if;

            declare
               Result : Int;
            begin
               Result :=  USBHID.Set_NonBlocking(USBHID_Device, 1);
            end;

         when None =>
            raise Invalid_DataLink_Config;
      end case;

      Active_Config    := Config;
      Connection_State := Connected;

   end;

   ----------------
   -- DISCONNECT --
   ----------------

   procedure Disconnect is
   begin
      if Connection_State = Not_Connected then
         raise Connection_Not_Established;
      end if;

      case Active_Config.Datalink is
         when TCP_IPv4 =>
            -- TODO --
            null;
         when Serial =>
            -- TODO --
            null;
         when USB_HID =>
            USBHID.Close(USBHID_Device);
         when None =>
            Null;
      end case;
      Connection_State := Not_Connected;
   end Disconnect;

   --------------
   -- SHUTDOWN --
   --------------

   procedure Shutdown is
      Return_Value : Int;
   begin

      if Connection_State = Connected then
         Disconnect;
      end if;

      Return_Value := USBHID.Free;

   end Shutdown;

   ---------------
   -- CONNECTED --
   ---------------

   function Connected return Boolean is
   begin
      case Connection_State is
         when Connected =>
            return True;
         when Not_Connected =>
            return False;
      end case;
   end Connected;

begin

   declare
      Return_Value : Int;
   begin
      Return_Value := USBHID.Initialize;
   end;

end Device;
