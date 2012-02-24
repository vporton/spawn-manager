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

package Spawn.Utils is

   function Random_String (Len : Positive) return String;
   --  Return a random string of given length.

   procedure Expand_Search_Path (Cmd_Path : String);
   --  Expand the search path for binaries (i.e. PATH) with the directory of
   --  the given command.

   procedure Wait_For_Socket
     (Path     : String;
      Timespan : Duration);
   --  This procedure waits max. the given timespan until the socket specified
   --  by path is available. An exception is raised if the socket is not
   --  present after the timespan has passed.

   function Locate_Exec_On_Path (Name : String) return String;
   --  Find given executable in environment $PATH. If the executable is not
   --  found an Exec_Not_Found exception is raised.

   Socket_Error   : exception;
   Exec_Not_Found : exception;

end Spawn.Utils;
