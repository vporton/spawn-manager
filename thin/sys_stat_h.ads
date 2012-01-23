with Interfaces.C; use Interfaces.C;
with bits_types_h;
with Interfaces.C.Strings;
limited with bits_stat_h;
limited with time_h;

package sys_stat_h is

   --  unsupported macro: S_IFMT __S_IFMT
   --  unsupported macro: S_IFDIR __S_IFDIR
   --  unsupported macro: S_IFCHR __S_IFCHR
   --  unsupported macro: S_IFBLK __S_IFBLK
   --  unsupported macro: S_IFREG __S_IFREG
   --  unsupported macro: S_IFIFO __S_IFIFO
   --  unsupported macro: S_IFLNK __S_IFLNK
   --  unsupported macro: S_IFSOCK __S_IFSOCK
   --  arg-macro: procedure S_ISDIR (mode)
   --    __S_ISTYPE((mode), __S_IFDIR)
   --  arg-macro: procedure S_ISCHR (mode)
   --    __S_ISTYPE((mode), __S_IFCHR)
   --  arg-macro: procedure S_ISBLK (mode)
   --    __S_ISTYPE((mode), __S_IFBLK)
   --  arg-macro: procedure S_ISREG (mode)
   --    __S_ISTYPE((mode), __S_IFREG)
   --  arg-macro: procedure S_ISFIFO (mode)
   --    __S_ISTYPE((mode), __S_IFIFO)
   --  arg-macro: procedure S_ISLNK (mode)
   --    __S_ISTYPE((mode), __S_IFLNK)
   --  arg-macro: procedure S_ISSOCK (mode)
   --    __S_ISTYPE((mode), __S_IFSOCK)
   --  arg-macro: procedure S_TYPEISMQ (buf)
   --    __S_TYPEISMQ(buf)
   --  arg-macro: procedure S_TYPEISSEM (buf)
   --    __S_TYPEISSEM(buf)
   --  arg-macro: procedure S_TYPEISSHM (buf)
   --    __S_TYPEISSHM(buf)
   --  unsupported macro: S_ISUID __S_ISUID
   --  unsupported macro: S_ISGID __S_ISGID
   --  unsupported macro: S_ISVTX __S_ISVTX
   --  unsupported macro: S_IRUSR __S_IREAD
   --  unsupported macro: S_IWUSR __S_IWRITE
   --  unsupported macro: S_IXUSR __S_IEXEC
   --  unsupported macro: S_IRWXU (__S_IREAD|__S_IWRITE|__S_IEXEC)
   --  unsupported macro: S_IREAD S_IRUSR
   --  unsupported macro: S_IWRITE S_IWUSR
   --  unsupported macro: S_IEXEC S_IXUSR
   --  unsupported macro: S_IRGRP (S_IRUSR >> 3)
   --  unsupported macro: S_IWGRP (S_IWUSR >> 3)
   --  unsupported macro: S_IXGRP (S_IXUSR >> 3)
   --  unsupported macro: S_IRWXG (S_IRWXU >> 3)
   --  unsupported macro: S_IROTH (S_IRGRP >> 3)
   --  unsupported macro: S_IWOTH (S_IWGRP >> 3)
   --  unsupported macro: S_IXOTH (S_IXGRP >> 3)
   --  unsupported macro: S_IRWXO (S_IRWXG >> 3)
   --  unsupported macro: ACCESSPERMS (S_IRWXU|S_IRWXG|S_IRWXO)
   --  unsupported macro: ALLPERMS (S_ISUID|S_ISGID|S_ISVTX|S_IRWXU|S_IRWXG|S_IRWXO)
   --  unsupported macro: DEFFILEMODE (S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH)

   S_BLKSIZE : constant := 512;  --  /usr/include/sys/stat.h:205

   subtype dev_t is bits_types_h.uu_dev_t;  -- /usr/include/sys/stat.h:46

   subtype gid_t is bits_types_h.uu_gid_t;  -- /usr/include/sys/stat.h:51

   subtype ino_t is bits_types_h.uu_ino_t;  -- /usr/include/sys/stat.h:57

   subtype mode_t is bits_types_h.uu_mode_t;  -- /usr/include/sys/stat.h:65

   subtype nlink_t is bits_types_h.uu_nlink_t;  -- /usr/include/sys/stat.h:70

   subtype off_t is bits_types_h.uu_off_t;  -- /usr/include/sys/stat.h:76

   subtype uid_t is bits_types_h.uu_uid_t;  -- /usr/include/sys/stat.h:84

   subtype blkcnt_t is bits_types_h.uu_blkcnt_t;  -- /usr/include/sys/stat.h:92

   subtype blksize_t is bits_types_h.uu_blksize_t;  -- /usr/include/sys/stat.h:100

   function stat (uu_file : Interfaces.C.Strings.chars_ptr; uu_buf : access bits_stat_h.stat) return int;  -- /usr/include/sys/stat.h:211
   pragma Import (C, stat, "stat");

   function fstat (uu_fd : int; uu_buf : access bits_stat_h.stat) return int;  -- /usr/include/sys/stat.h:216
   pragma Import (C, fstat, "fstat");

   function stat64 (uu_file : Interfaces.C.Strings.chars_ptr; uu_buf : access bits_stat_h.stat64) return int;  -- /usr/include/sys/stat.h:230
   pragma Import (C, stat64, "stat64");

   function fstat64 (uu_fd : int; uu_buf : access bits_stat_h.stat64) return int;  -- /usr/include/sys/stat.h:232
   pragma Import (C, fstat64, "fstat64");

   function fstatat
     (uu_fd : int;
      uu_file : Interfaces.C.Strings.chars_ptr;
      uu_buf : access bits_stat_h.stat;
      uu_flag : int) return int;  -- /usr/include/sys/stat.h:240
   pragma Import (C, fstatat, "fstatat");

   function fstatat64
     (uu_fd : int;
      uu_file : Interfaces.C.Strings.chars_ptr;
      uu_buf : access bits_stat_h.stat64;
      uu_flag : int) return int;  -- /usr/include/sys/stat.h:255
   pragma Import (C, fstatat64, "fstatat64");

   function lstat (uu_file : Interfaces.C.Strings.chars_ptr; uu_buf : access bits_stat_h.stat) return int;  -- /usr/include/sys/stat.h:265
   pragma Import (C, lstat, "lstat");

   function lstat64 (uu_file : Interfaces.C.Strings.chars_ptr; uu_buf : access bits_stat_h.stat64) return int;  -- /usr/include/sys/stat.h:278
   pragma Import (C, lstat64, "lstat64");

   function chmod (uu_file : Interfaces.C.Strings.chars_ptr; uu_mode : bits_types_h.uu_mode_t) return int;  -- /usr/include/sys/stat.h:286
   pragma Import (C, chmod, "chmod");

   function lchmod (uu_file : Interfaces.C.Strings.chars_ptr; uu_mode : bits_types_h.uu_mode_t) return int;  -- /usr/include/sys/stat.h:293
   pragma Import (C, lchmod, "lchmod");

   function fchmod (uu_fd : int; uu_mode : bits_types_h.uu_mode_t) return int;  -- /usr/include/sys/stat.h:299
   pragma Import (C, fchmod, "fchmod");

   function fchmodat
     (uu_fd : int;
      uu_file : Interfaces.C.Strings.chars_ptr;
      uu_mode : bits_types_h.uu_mode_t;
      uu_flag : int) return int;  -- /usr/include/sys/stat.h:305
   pragma Import (C, fchmodat, "fchmodat");

   function umask (uu_mask : bits_types_h.uu_mode_t) return bits_types_h.uu_mode_t;  -- /usr/include/sys/stat.h:314
   pragma Import (C, umask, "umask");

   function getumask return bits_types_h.uu_mode_t;  -- /usr/include/sys/stat.h:319
   pragma Import (C, getumask, "getumask");

   function mkdir (uu_path : Interfaces.C.Strings.chars_ptr; uu_mode : bits_types_h.uu_mode_t) return int;  -- /usr/include/sys/stat.h:323
   pragma Import (C, mkdir, "mkdir");

   function mkdirat
     (uu_fd : int;
      uu_path : Interfaces.C.Strings.chars_ptr;
      uu_mode : bits_types_h.uu_mode_t) return int;  -- /usr/include/sys/stat.h:330
   pragma Import (C, mkdirat, "mkdirat");

   function mknod
     (uu_path : Interfaces.C.Strings.chars_ptr;
      uu_mode : bits_types_h.uu_mode_t;
      uu_dev : bits_types_h.uu_dev_t) return int;  -- /usr/include/sys/stat.h:338
   pragma Import (C, mknod, "mknod");

   function mknodat
     (uu_fd : int;
      uu_path : Interfaces.C.Strings.chars_ptr;
      uu_mode : bits_types_h.uu_mode_t;
      uu_dev : bits_types_h.uu_dev_t) return int;  -- /usr/include/sys/stat.h:345
   pragma Import (C, mknodat, "mknodat");

   function mkfifo (uu_path : Interfaces.C.Strings.chars_ptr; uu_mode : bits_types_h.uu_mode_t) return int;  -- /usr/include/sys/stat.h:352
   pragma Import (C, mkfifo, "mkfifo");

   function mkfifoat
     (uu_fd : int;
      uu_path : Interfaces.C.Strings.chars_ptr;
      uu_mode : bits_types_h.uu_mode_t) return int;  -- /usr/include/sys/stat.h:359
   pragma Import (C, mkfifoat, "mkfifoat");

   function utimensat
     (uu_fd : int;
      uu_path : Interfaces.C.Strings.chars_ptr;
      uu_times : access constant time_h.timespec;
      uu_flags : int) return int;  -- /usr/include/sys/stat.h:366
   pragma Import (C, utimensat, "utimensat");

   function futimens (uu_fd : int; uu_times : access constant time_h.timespec) return int;  -- /usr/include/sys/stat.h:374
   pragma Import (C, futimens, "futimens");

end sys_stat_h;
