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

with Ada.Text_IO;
with Ada.Calendar;

with GNAT.OS_Lib;

with Spawn.Pool;

procedure Performance
is
   use type Ada.Calendar.Time;

   Loops   : constant        := 1000;
   Cmd     : constant String := "true";
   Runtime : Duration        := Duration (0);
   Start   : Ada.Calendar.Time;
   Status  : Boolean;
begin
   Spawn.Pool.Init;

   Ada.Text_IO.Put_Line ("* Calling '" & Cmd & "'" & Loops'Img & " times");

   for I in  1 .. Loops loop
      Start := Ada.Calendar.Clock;
      Spawn.Pool.Execute (Command => Cmd);
      Runtime := Runtime + (Ada.Calendar.Clock - Start);
   end loop;
   Ada.Text_IO.Put_Line ("* IPC 0MQ   :" & Duration'Image (Runtime / Loops));

   Runtime := Duration (0);

   declare
      Args : GNAT.OS_Lib.Argument_List (1 .. 4);
   begin
      Args (1) := new String'("-o");
      Args (2) := new String'("pipefail");
      Args (3) := new String'("-c");
      Args (4) := new String'(Cmd);

      for I in  1 .. Loops loop
         Start := Ada.Calendar.Clock;
         GNAT.OS_Lib.Spawn
           (Program_Name => "/bin/bash",
            Args         => Args,
            Success      => Status);
         Runtime := Runtime + (Ada.Calendar.Clock - Start);
      end loop;

      for A in Args'Range loop
         GNAT.OS_Lib.Free (X => Args (A));
      end loop;
   end;
   Ada.Text_IO.Put_Line ("* GNAT Spawn:" & Duration'Image (Runtime / Loops));

   Spawn.Pool.Cleanup;
end Performance;
