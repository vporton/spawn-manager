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

with Interfaces.C;

--  with GNAT.OS_Lib;

with Spawn.OS;
with Spawn.World_Internals;
with Spawn.Logger;
with Spawn.Pool;

package body Spawn.Signals is

   package L renames Spawn.Logger;

   World : Spawn.World_Internals.World_Internals_Type renames
     Spawn.World_Internals.World;

   -------------------------------------------------------------------------

   protected body Signal_Handler_Type
   is
      ----------------------------------------------------------------------

      procedure Handle_Signal
      is
         C : aliased Interfaces.C.char_array (1 .. 1) := (1 => 'a');
      begin
         pragma Debug (L.Log_File ("Signal received"));
         declare
            use type Spawn.OS.ssize_t;
            Res : constant Spawn.OS.ssize_t
              := Spawn.OS.write (World.Self_Communication_Write, C, 1);
         begin
            if Res = -1 then
               raise Spawn.Pool.Pool_Error with
                 "Cannot write to self-communincation pipe";
            end if;
         end;
      end Handle_Signal;

   end Signal_Handler_Type;

end Spawn.Signals;
