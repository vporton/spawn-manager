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

with Ada.Directories;
with Ada.Environment_Variables;

with GNAT.OS_Lib;

package body Spawn.Utils is

   -------------------------------------------------------------------------

   procedure Expand_Search_Path (Cmd_Path : String)
   is
      package ENV renames Ada.Environment_Variables;
   begin
      if Cmd_Path = Ada.Directories.Base_Name (Name => Cmd_Path) then

         --  Command already in PATH.

         return;
      end if;

      declare
         Search_Path : constant String := GNAT.OS_Lib.Normalize_Pathname
           (Name => Ada.Directories.Containing_Directory (Name => Cmd_Path));
      begin
         ENV.Set (Name  => "PATH",
                  Value => Search_Path & ":" & ENV.Value ("PATH"));
      end;
   end Expand_Search_Path;

   -------------------------------------------------------------------------

   function Locate_Exec_On_Path (Name : String) return String
   is
      use type GNAT.OS_Lib.String_Access;

      Exec : GNAT.OS_Lib.String_Access;
   begin
      Exec := GNAT.OS_Lib.Locate_Exec_On_Path (Exec_Name => Name);
      if Exec = null then
         raise Exec_Not_Found with "Could not locate '" & Name & "' in PATH";
      end if;

      declare
         Path : constant String := Exec.all;
      begin
         GNAT.OS_Lib.Free (Exec);
         return Path;
      end;
   end Locate_Exec_On_Path;

   -------------------------------------------------------------------------

   procedure Wait_For_Socket
     (Path     : String;
      Timespan : Duration)
   is
   begin
      for L in 1 .. Positive (100 * Timespan) loop
         if Ada.Directories.Exists (Name => Path) then
            return;
         end if;
         delay Timespan / 100;
      end loop;

      raise Socket_Error with "Socket '" & Path & "' not available";
   end Wait_For_Socket;

end Spawn.Utils;
