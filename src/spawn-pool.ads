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
with Ada.Streams;

package Spawn.Pool is

   procedure Init
     (Manager_Count : Positive := 1;
      Socket_Dir    : String   := "/tmp");
   --  Init pool with given number of spawn managers. The Socket_Dir argument
   --  specifies the directory used to store spawn_manager communication
   --  sockets.

   procedure Execute
     (Command   : String;
      Directory : String  := Ada.Directories.Current_Directory;
      Timeout   : Integer := -1);
   --  Execute command in given directory. The Timeout parameter specifies the
   --  time in milliseconds after the command times out (the default is no
   --  timeout (-1)). If a timeout occurs, a Command_Failed exception is raised
   --  to indicate failure.

   procedure Cleanup;
   --  Cleanup spawn pool.

   Command_Failed : exception;

private

   function Send_Receive
     (Request : Ada.Streams.Stream_Element_Array)
      return Ada.Streams.Stream_Element_Array;
   --  Send given data as request to spawn manager. Return data of received
   --  reply.

end Spawn.Pool;
