with Interfaces.C; use Interfaces.C;
with bits_types_h;
with time_h;

package bits_stat_h is

   --  unsupported macro: st_atime st_atim.tv_sec
   --  unsupported macro: st_mtime st_mtim.tv_sec
   --  unsupported macro: st_ctime st_ctim.tv_sec

   UTIME_NOW : constant := ((2 ** 30) - 1);  --  /usr/include/bits/stat.h:206
   UTIME_OMIT : constant := ((2 ** 30) - 2);  --  /usr/include/bits/stat.h:207

   type stat_uu_unused_array is array (0 .. 2) of aliased long;
   type stat is record
      st_dev : aliased bits_types_h.uu_dev_t;  -- /usr/include/bits/stat.h:45
      st_ino : aliased bits_types_h.uu_ino_t;  -- /usr/include/bits/stat.h:50
      st_nlink : aliased bits_types_h.uu_nlink_t;  -- /usr/include/bits/stat.h:58
      st_mode : aliased bits_types_h.uu_mode_t;  -- /usr/include/bits/stat.h:59
      st_uid : aliased bits_types_h.uu_uid_t;  -- /usr/include/bits/stat.h:61
      st_gid : aliased bits_types_h.uu_gid_t;  -- /usr/include/bits/stat.h:62
      uu_pad0 : aliased int;  -- /usr/include/bits/stat.h:64
      st_rdev : aliased bits_types_h.uu_dev_t;  -- /usr/include/bits/stat.h:66
      st_size : aliased bits_types_h.uu_off_t;  -- /usr/include/bits/stat.h:71
      st_blksize : aliased bits_types_h.uu_blksize_t;  -- /usr/include/bits/stat.h:75
      st_blocks : aliased bits_types_h.uu_blkcnt_t;  -- /usr/include/bits/stat.h:77
      st_atim : aliased time_h.timespec;  -- /usr/include/bits/stat.h:88
      st_mtim : aliased time_h.timespec;  -- /usr/include/bits/stat.h:89
      st_ctim : aliased time_h.timespec;  -- /usr/include/bits/stat.h:90
      uu_unused : aliased stat_uu_unused_array;  -- /usr/include/bits/stat.h:103
   end record;
   pragma Convention (C_Pass_By_Copy, stat);  -- /usr/include/bits/stat.h:43

   type stat64_uu_unused_array is array (0 .. 2) of aliased long;
   type stat64 is record
      st_dev : aliased bits_types_h.uu_dev_t;  -- /usr/include/bits/stat.h:118
      st_ino : aliased bits_types_h.uu_ino64_t;  -- /usr/include/bits/stat.h:120
      st_nlink : aliased bits_types_h.uu_nlink_t;  -- /usr/include/bits/stat.h:121
      st_mode : aliased bits_types_h.uu_mode_t;  -- /usr/include/bits/stat.h:122
      st_uid : aliased bits_types_h.uu_uid_t;  -- /usr/include/bits/stat.h:129
      st_gid : aliased bits_types_h.uu_gid_t;  -- /usr/include/bits/stat.h:130
      uu_pad0 : aliased int;  -- /usr/include/bits/stat.h:132
      st_rdev : aliased bits_types_h.uu_dev_t;  -- /usr/include/bits/stat.h:133
      st_size : aliased bits_types_h.uu_off_t;  -- /usr/include/bits/stat.h:134
      st_blksize : aliased bits_types_h.uu_blksize_t;  -- /usr/include/bits/stat.h:140
      st_blocks : aliased bits_types_h.uu_blkcnt64_t;  -- /usr/include/bits/stat.h:141
      st_atim : aliased time_h.timespec;  -- /usr/include/bits/stat.h:149
      st_mtim : aliased time_h.timespec;  -- /usr/include/bits/stat.h:150
      st_ctim : aliased time_h.timespec;  -- /usr/include/bits/stat.h:151
      uu_unused : aliased stat64_uu_unused_array;  -- /usr/include/bits/stat.h:164
   end record;
   pragma Convention (C_Pass_By_Copy, stat64);  -- /usr/include/bits/stat.h:116

end bits_stat_h;
