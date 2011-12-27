package Spawn.Pool is

   procedure Init;
   --  Init spawn pool.

   procedure Execute (Command : String);
   --  Execute given command.

   procedure Cleanup;
   --  Cleanup spawn pool.

   Command_Failed : exception;

end Spawn.Pool;
