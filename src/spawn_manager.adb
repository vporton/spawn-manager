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

with GNAT.OS_Lib;

with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;

with Spawn.Types;
with Spawn.Utils;

procedure Spawn_Manager
is
   Shell : constant String := "/bin/bash";
   --  Shell used to execute commands.

   use Ada.Strings.Unbounded;
   use ZMQ;

   Ctx : Contexts.Context;
   S   : Sockets.Socket;

   procedure Send_Reply (Success : Boolean);
   --  Send reply message indicating success or failure.

   procedure Send_Reply (Success : Boolean)
   is
      Reply  : Messages.Message;
      Result : constant Spawn.Types.Data_Type
        := (Success => Success,
            others  => <>);
   begin
      Reply.Initialize
        (Data => Spawn.Types.Serialize (Data => Result));

      S.Send (Msg => Reply);
      Reply.Finalize;
   end Send_Reply;

begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
      return;
   end if;

   Spawn.Utils.Expand_Search_Path (Cmd_Path => Ada.Command_Line.Command_Name);

   Ctx.Initialize (App_Threads => 1);
   S.Initialize (With_Context => Ctx,
                 Kind         => Sockets.REP);
   S.Bind (Address => Ada.Command_Line.Argument (1));

   Main :
   loop
      declare
         Request : Messages.Message;
         Data    : Spawn.Types.Data_Type;
      begin
         Request.Initialize;
         S.recv (Msg   => Request,
                 Flags => 0);

         Data := Spawn.Types.Deserialize (Buffer => Request.getData);
         Request.Finalize;

         declare
            Args   : GNAT.OS_Lib.Argument_List (1 .. 4);
            Dir    : constant String := To_String (Data.Dir);
            Status : Boolean;
         begin
            exit Main when Data.Do_Quit;

            if Dir'Length /= 0
              and then Dir /= Ada.Directories.Current_Directory
            then
               Ada.Directories.Set_Directory (Directory => Dir);
            end if;

            Args (1) := new String'("-o");
            Args (2) := new String'("pipefail");
            Args (3) := new String'("-c");
            Args (4) := new String'(To_String (Data.Command));

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
         when others => Send_Reply (Success => False);
      end;
   end loop Main;

   S.Close;
   Ctx.Finalize;
   Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Success);
end Spawn_Manager;
