with Ada.Directories;

package Spawn.Pool is

   procedure Init (Manager_Count : Positive := 1);
   --  Init pool with given number of spawn managers.

   procedure Execute
     (Command   : String;
      Directory : String := Ada.Directories.Current_Directory);
   --  Execute command in given directory.

   procedure Cleanup;
   --  Cleanup spawn pool.

   Command_Failed : exception;

end Spawn.Pool;
