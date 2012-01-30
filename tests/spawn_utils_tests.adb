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
with Ada.Exceptions;

with Spawn.Utils;

package body Spawn_Utils_Tests is

   use Ahven;
   use Spawn.Utils;

   -------------------------------------------------------------------------

   procedure Get_Random_Strings
   is
   begin
      Assert (Condition => Random_String (Len => 4)'Length = 4,
              Message   => "Length incorrect");
      Assert (Condition => Random_String (Len => 4) /=
                Random_String (Len => 4),
              Message   => "Strings match");
   end Get_Random_Strings;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Spawn utils tests");
      T.Add_Test_Routine
        (Routine => Verify_Wait_For_Socket'Access,
         Name    => "Wait for sockets");
      T.Add_Test_Routine
        (Routine => Get_Random_Strings'Access,
         Name    => "Get random strings");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Verify_Wait_For_Socket
   is
      T_Path : constant String := "/tmp/spawntest-" & Random_String (Len => 8);
   begin
      begin
         Wait_For_Socket (Path     => "/nonexistent/nonexistent",
                          Timespan => 0.1);
         Fail (Message => "Exception expected");

      exception
         when Socket_Error => null;
      end;

      begin
         Ada.Directories.Create_Path (New_Directory => T_Path);
         Wait_For_Socket (Path     => T_Path,
                          Timespan => 0.1);
         Fail (Message => "Exception expected");

      exception
         when E : Socket_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message
                    (X => E) = "File '" & T_Path & "' is not a socket",
                    Message   => "Invalid exception message: "
                    & Ada.Exceptions.Exception_Message (X => E));
      end;

      Ada.Directories.Delete_Directory (Directory => T_Path);
   end Verify_Wait_For_Socket;

end Spawn_Utils_Tests;
