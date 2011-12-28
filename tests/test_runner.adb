with Ahven.Text_Runner;
with Ahven.Framework;

with Spawn.Pool;

with Spawn_Pool_Tests;
with Spawn_Types_Tests;

procedure Test_Runner is
   use Ahven.Framework;

   S : constant Test_Suite_Access
     := Create_Suite (Suite_Name => "0MQ/Spawn tests");
begin
   Spawn.Pool.Init;

   Add_Test (Suite => S.all,
             T     => new Spawn_Pool_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Spawn_Types_Tests.Testcase);

   Ahven.Text_Runner.Run (Suite => S);
   Release_Suite (T => S);

   Spawn.Pool.Cleanup;
end Test_Runner;
