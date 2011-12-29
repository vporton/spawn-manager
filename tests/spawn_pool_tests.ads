with Ahven.Framework;

package Spawn_Pool_Tests is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out Testcase);
   --  Initialize testcase.

   procedure Execute_Bin_True;
   --  Execute /bin/true.

   procedure Execute_Bin_False;
   --  Execute /bin/false, should raise an exception.

   procedure Execute_Complex_Command;
   --  Execute complex command.

   procedure Parallel_Execution;
   --  Verify parallel command execution.

   procedure Pool_Depleted;
   --  Verify exception handling if pool is depleted.

end Spawn_Pool_Tests;
