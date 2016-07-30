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
with NVP_Protocol;  use NVP_Protocol;
with Util;          use Util;
with USBHID;
with System;        use System;

package body Device is

   Active_Protocol  : Protocol_Configuration;
   Active_Config    : Datalink_Configuration;

   USBHID_Device    : System.Address;

   Connection_State : Connection_State_Type := Not_Connected;

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
            null;
            -- TODO --
         when None =>
            raise Invalid_DataLink_Config;
      end case;
   end Send_Data;

   -------------
   -- CONNECT --
   -------------

   procedure Connect (Protocol : Protocol_Configuration;
                      Config   : Datalink_Configuration) is
   begin

      if Connection_State = Connected then
         raise Connection_Already_Established;
      end if;

      case Active_Config.Datalink is
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
         when None =>
            raise Invalid_DataLink_Config;
      end case;

      Active_Protocol  := Protocol;
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

   ---------------
   -- SET_VALUE --
   ---------------

   procedure Set_Value(Parameter_ID : Unsigned_16;
                       Value        : Unsigned_32) is
   begin
      if Connection_State = Not_Connected then
         raise Connection_Not_Established;
      end if;

      case Active_Protocol.Protocol is
         when NVP =>
            -- TODO --
            Null;
         when NVP_With_Routing =>
            -- TODO --
            Null;
         when IQ =>
            -- TODO --
            raise Invalid_Protocol;
         when None =>
            raise Invalid_Protocol;
      end case;
   end Set_Value;

   --------------------
   -- REQUEST_VALUES --
   --------------------

   procedure Request_Values(Parameter_IDs : Unsigned_16_Vectors.Vector) is
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
            -- TODO --
            null;
         when None =>
            raise Connection_Not_Established;
      end case;
   end Request_Values;

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

   -------------------
   -- VALUES_BUFFER --
   -------------------

   protected body Values_Buffer is

      -----------------------
      -- VALUES_BUFFER.ADD --
      -----------------------

      procedure Add(Item : in Name_Value_Pair) is
      begin
         Elements.Append(Item);
      end Add;

      --------------------------
      -- VALUES_BUFFER.REMOVE --
      --------------------------

      procedure Remove(Items : out Name_Value_Pair_Vectors.Vector) is
      begin
         Items := Elements;
         Elements.Clear;
      end Remove;

   end Values_Buffer;

   ---------------------
   -- REQUESTS_BUFFER --
   ---------------------

   protected body Requests_Buffer is

      -------------------------
      -- REQUESTS_BUFFER.ADD --
      -------------------------

      procedure Add(Item : in Unsigned_16) is
      begin
         Elements.Append(Item);
      end Add;

      ----------------------------
      -- REQUESTs_BUFFER.REMOVE --
      ----------------------------

      procedure Remove(Items : out Unsigned_16_Vectors.Vector) is
      begin
         Items := Elements;
         Elements.Clear;
      end Remove;

   end Requests_Buffer;

begin

   declare
      Return_Value : Int;
   begin
      Return_Value := USBHID.Initialize;
   end;

end Device;
