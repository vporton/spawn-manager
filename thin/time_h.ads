with Interfaces.C; use Interfaces.C;
with bits_types_h;

package time_h is

   subtype time_t is bits_types_h.uu_time_t;  -- /usr/include/time.h:76

   type timespec is record
      tv_sec : aliased bits_types_h.uu_time_t;  -- /usr/include/time.h:122
      tv_nsec : aliased long;  -- /usr/include/time.h:123
   end record;
   pragma Convention (C_Pass_By_Copy, timespec);  -- /usr/include/time.h:120

end time_h;
