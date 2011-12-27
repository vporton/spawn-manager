package Spawn.Pool is

   procedure Init (Manager_Count : Positive := 1);
   --  Init pool with given number of spawn managers.

   procedure Execute (Command : String);
   --  Execute given command.

   procedure Cleanup;
   --  Cleanup spawn pool.

   Command_Failed : exception;

end Spawn.Pool;
