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
with Ada.Numerics.Discrete_Random;

with GNAT.OS_Lib;

with Interfaces.C.Strings;

with sys_stat_h;
with bits_stat_h;
with bits_types_h;

package body Spawn.Utils is

   S_IFMT   : constant := 61440; --  These bits determine the file type.
   S_IFSOCK : constant := 49152; --  File type socket.

   subtype Chars is Character range 'a' .. 'z';
   package Random_Chars is new Ada.Numerics.Discrete_Random
     (Result_Subtype => Chars);
   Generator : Random_Chars.Generator;

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

   function Random_String (Len : Positive) return String
   is
      Result : String (1 .. Len);
   begin
      for I in Result'Range loop
         Result (I) := Random_Chars.Random (Gen => Generator);
      end loop;

      return Result;
   end Random_String;

   -------------------------------------------------------------------------

   procedure Wait_For_Socket
     (Path     : String;
      Timespan : Duration)
   is
      use Interfaces.C;

      function Is_Socket (Mode : bits_types_h.uu_mode_t) return Boolean;
      --  Return True if given mode designates a socket.

      function Is_Socket (Mode : bits_types_h.uu_mode_t) return Boolean
      is
      begin
         return (Mode and S_IFMT) = S_IFSOCK;
      end Is_Socket;

      C_Path : Strings.chars_ptr := Strings.New_String (Str => Path);
      C_Stat : aliased bits_stat_h.stat;
      C_Res  : int;
   begin
      for L in 1 .. Positive (100 * Timespan) loop
         C_Res := sys_stat_h.stat
           (uu_file => C_Path,
            uu_buf  => C_Stat'Access);

         exit when C_Res = 0;
         delay Timespan / 100;
      end loop;
      Strings.Free (Item => C_Path);

      if C_Res /= 0 then
         raise Socket_Error with "Socket '" & Path & "' not available";
      end if;

      if not Is_Socket (Mode => C_Stat.st_mode) then
         raise Socket_Error with "File '" & Path & "' is not a socket";
      end if;
   end Wait_For_Socket;

begin
   Random_Chars.Reset (Gen => Generator);
end Spawn.Utils;
