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

with GNAT.OS_Lib;

package Spawn.Spawner is

   Command_Failed : exception;

   --  TODO: Non-portable!
   type Process_Id is new Integer;
   Invalid_Pid : constant Process_Id := -1;

   --  TODO: Make private
   type Process_Descriptor is tagged record
      Pid              : Process_Id := Invalid_Pid;
      Input_Fd         : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
      Output_Fd        : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
--  Error_Fd         : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
   end record;

   procedure Non_Blocking_Spawn
     (Descriptor  : out Process_Descriptor'Class;
      Command     : String);

end Spawn.Spawner;
