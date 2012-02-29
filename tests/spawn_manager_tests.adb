--
--  Process Spawn Manager
--
--  Copyright (C) 2012 Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2012 secunet Security Networks AG
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 2
--  of the License, or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
--  USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit,  or  you  link  this  unit  with  other  files  to  produce  an
--  executable   this  unit  does  not  by  itself  cause  the  resulting
--  executable to  be  covered by the  GNU General  Public License.  This
--  exception does  not  however  invalidate  any  other reasons why  the
--  executable file might be covered by the GNU Public License.
--

with Ada.Streams;
with Ada.Strings.Unbounded;

with Anet.Sockets;

with Spawn.Types;

package body Spawn_Manager_Tests is

   use Ada.Strings.Unbounded;
   use Ahven;
   use Spawn;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Spawn manager tests");
      T.Add_Test_Routine
        (Routine => Send_Receive'Access,
         Name    => "Send and receive data");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Send_Receive
   is
      use Ada.Streams;

      S   : Anet.Sockets.Socket_Type;
      Req : constant Types.Data_Type
        := (Command => To_Unbounded_String ("/bin/true"),
            others  => <>);

      Invalid : constant Stream_Element_Array (1 .. 1) := (others => 12);
   begin
      S.Create (Family => Anet.Sockets.Family_Unix,
                Mode   => Anet.Sockets.Stream_Socket);
      S.Connect (Dst => (Family => Anet.Sockets.Family_Unix,
                         Path   => To_Unbounded_String
                           ("/tmp/spawn_manager_0")));

      S.Send (Item => Types.Serialize (Data => Req));
      declare
         Data     : Stream_Element_Array (1 .. 128);
         Last_Idx : Stream_Element_Offset;
         Sender   : Anet.Sockets.Socket_Addr_Type;
         Response : Types.Data_Type;
      begin
         S.Receive (Src  => Sender,
                    Item => Data,
                    Last => Last_Idx);

         Response := Types.Deserialize
           (Buffer => Data (Data'First .. Last_Idx));
         Assert (Condition => Response.Success,
                 Message   => "Cmd not successful");
      end;

      S.Send (Item => Invalid);
      declare
         Data     : Stream_Element_Array (1 .. 128);
         Last_Idx : Stream_Element_Offset;
         Sender   : Anet.Sockets.Socket_Addr_Type;
         Response : Types.Data_Type;
      begin
         S.Receive (Src  => Sender,
                    Item => Data,
                    Last => Last_Idx);

         Response := Types.Deserialize
           (Buffer => Data (Data'First .. Last_Idx));
         Assert (Condition => not Response.Success,
                 Message   => "Failure expected");
      end;

      S.Close;
   end Send_Receive;

end Spawn_Manager_Tests;
