--
--  Process Spawn Manager
--
--  Copyright (C) 2017 Victor Porton <porton@narod.ru>
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

with Interfaces.C;
with Interfaces.C.Strings;

--  FIXME

package body Spawn.Spawner is

   use Interfaces.C, Interfaces.C.Strings;

   function fork return Process_Id;
   pragma Import (C, fork);

   type C_Cmd_Args is array (Integer range <>) of
     chars_ptr;
   pragma Convention (C, C_Cmd_Args);

   function execv (File : char_array; Args : C_Cmd_Args)
                   return int;
   pragma Import (C, execv);

   procedure Kill (Pid : Process_Id; Sig_Num : Integer);
   pragma Import (C, Kill);

   procedure Close
     (Descriptor : in out Process_Descriptor)
   is
   begin
      --  TODO: Close file descriptors
      Kill (Descriptor.Pid, 15); --  TODO: SIGTERM portably
      --  TODO: SIGKILL after a timeout
   end Close;

   function Get_Pid
     (Descriptor : Process_Descriptor)
      return       Process_Id
   is
   begin
      return Descriptor.Pid;
   end Get_Pid;

   procedure Non_Blocking_Spawn
     (Descriptor  : out Process_Descriptor'Class;
      Command     : String)
   is
      Pid : constant Process_Id := fork;
   begin
      case Pid is
         when 0 =>
            --  FIXME: Shall we support shell semantics of use plain execvp()?
            declare
               Cmd : aliased char_array := To_C (Command);
               R : constant int :=
                 execv (To_C ("/bin/sh"),
                        (1 => To_Chars_Ptr (Cmd'Unchecked_Access)));
               pragma Unreferenced (R);
            begin
               raise Command_Failed with "Cannot exec shell"; --  TODO
            end;
         when -1 =>
            raise Command_Failed with "Cannot fork"; --  TODO
         when others =>
            Descriptor.Pid := Pid;
      end case;
   end Non_Blocking_Spawn;

end Spawn.Spawner;
