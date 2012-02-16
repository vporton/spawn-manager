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

package Spawn.Types is

   type Data_Type is record
      Success : Boolean := False;
      Do_Quit : Boolean := False;
      Command : Ada.Strings.Unbounded.Unbounded_String;
      Dir     : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   Shutdown_Token : constant Data_Type;

   function Serialize
     (Data : Data_Type)
      return Ada.Streams.Stream_Element_Array;
   --  Serialize given data type.

   function Deserialize
     (Buffer : Ada.Streams.Stream_Element_Array)
      return Data_Type;
   --  Deserialize given data type.

   Deserialize_Error : exception;

private

   Shutdown_Token : constant Data_Type := (Do_Quit => True,
                                           others  => <>);

end Spawn.Types;
