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

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Streams;
with Ada.Exceptions;

with GNAT.OS_Lib;

with Anet.OS;
with Anet.Sockets;

with Spawn.Types;
with Spawn.Utils;
with Spawn.Logger;

procedure Spawn_Manager
is
   use Ada.Strings.Unbounded;

   package L renames Spawn.Logger;

   function S
     (Source : Unbounded_String)
      return String
      renames Ada.Strings.Unbounded.To_String;

   Shell : constant String := "/bin/bash";
   --  Shell used to execute commands.

   Sock_Listen, Sock_Comm : Anet.Sockets.Socket_Type;

   procedure Send_Reply (Success : Boolean);
   --  Send reply message indicating success or failure.

   procedure Send_Reply (Success : Boolean)
   is
   begin
      Sock_Comm.Send (Item => Spawn.Types.Serialize
                      (Data => (Success => Success,
                                others  => <>)));
      pragma Debug (L.Log ("Manager - reply sent [" & Success'Img & "]"));
   end Send_Reply;

begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
      return;
   end if;

   if not Anet.Sockets.Is_Valid_Unix
     (Path => Ada.Command_Line.Argument (1))
   then
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
      return;
   end if;

   Spawn.Utils.Expand_Search_Path (Cmd_Path => Ada.Command_Line.Command_Name);
   Spawn.Utils.Clear_Signal_Mask;

   Sock_Listen.Create (Family => Anet.Sockets.Family_Unix,
                       Mode   => Anet.Sockets.Stream_Socket);
   Sock_Listen.Bind_Unix
     (Path => Anet.Sockets.Unix_Path_Type (Ada.Command_Line.Argument (1)));
   Sock_Listen.Listen_Unix;
   pragma Debug (L.Log ("Manager - listening on socket "
     & Ada.Command_Line.Argument (1)));

   Sock_Listen.Accept_Socket (New_Socket => Sock_Comm);
   pragma Debug (L.Log ("Manager - connection established"));

   Main :
   loop
      declare
         use type Ada.Streams.Stream_Element_Offset;

         Buffer   : Ada.Streams.Stream_Element_Array (1 .. 8192);
         Last_Idx : Ada.Streams.Stream_Element_Offset;
         Req      : Spawn.Types.Data_Type;
         Sender   : Anet.Sockets.Socket_Addr_Type;
      begin
         pragma Debug (L.Log ("Manager - waiting for data"));
         Sock_Comm.Receive (Src  => Sender,
                            Item => Buffer,
                            Last => Last_Idx);

         pragma Debug (L.Log ("Manager - received" & Last_Idx'Img & " bytes"));
         exit Main when Last_Idx = 0;

         Req := Spawn.Types.Deserialize
           (Buffer => Buffer (Buffer'First .. Last_Idx));

         pragma Debug (L.Log ("Manager - command request received:"));
         pragma Debug (L.Log ("Manager - cmd  [" & S (Req.Command) & "]"));
         pragma Debug (L.Log ("Manager - dir  [" & S (Req.Dir) & "]"));

         declare
            Args   : GNAT.OS_Lib.Argument_List (1 .. 4);
            Dir    : constant String := To_String (Req.Dir);
            Status : Boolean;
         begin
            if Dir'Length /= 0
              and then Dir /= Ada.Directories.Current_Directory
            then
               Ada.Directories.Set_Directory (Directory => Dir);
            end if;

            Args (1) := new String'("-o");
            Args (2) := new String'("pipefail");
            Args (3) := new String'("-c");
            Args (4) := new String'(To_String (Req.Command));

            GNAT.OS_Lib.Spawn
              (Program_Name => Shell,
               Args         => Args,
               Success      => Status);

            for A in Args'Range loop
               GNAT.OS_Lib.Free (X => Args (A));
            end loop;

            Send_Reply (Success => Status);
         end;

      exception
         when E : others =>
            pragma Debug (L.Log ("Manager - exception:"));
            pragma Debug (L.Log (Ada.Exceptions.Exception_Information (E)));
            Send_Reply (Success => False);
      end;
   end loop Main;

   pragma Debug (L.Log ("Manager - shutting down"));
   Anet.OS.Delete_File (Filename => Ada.Command_Line.Argument (1));
   Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Success);
end Spawn_Manager;
