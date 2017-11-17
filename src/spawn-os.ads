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

--  Internals

with Interfaces.C; use Interfaces.C;

package Spawn.OS is

   subtype ssize_t  is size_t; -- C ssize_t type

   --  TODO:
--     type file_id is new integer;

   function write (file : int; b : in out char_array; length : size_t)
                   return ssize_t;
   pragma Import (C, write);

   function close (FD : int) return int;
   pragma Import (C, close);

   type Two_FDs is array (0 .. 1) of int;
   pragma Convention (C, Two_FDs);

   function pipe (FDs : out Two_FDs) return int;
   pragma Import (C, pipe);

end Spawn.OS;
