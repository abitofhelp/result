pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__comprehensive_test_result.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__comprehensive_test_result.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E008 : Short_Integer; pragma Import (Ada, E008, "ada__exceptions_E");
   E013 : Short_Integer; pragma Import (Ada, E013, "system__soft_links_E");
   E024 : Short_Integer; pragma Import (Ada, E024, "system__exception_table_E");
   E031 : Short_Integer; pragma Import (Ada, E031, "ada__numerics_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__exceptions_E");
   E020 : Short_Integer; pragma Import (Ada, E020, "system__soft_links__initialize_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__assertions_E");
   E058 : Short_Integer; pragma Import (Ada, E058, "ada__io_exceptions_E");
   E061 : Short_Integer; pragma Import (Ada, E061, "ada__strings_E");
   E063 : Short_Integer; pragma Import (Ada, E063, "ada__strings__utf_encoding_E");
   E096 : Short_Integer; pragma Import (Ada, E096, "interfaces__c_E");
   E126 : Short_Integer; pragma Import (Ada, E126, "system__os_lib_E");
   E071 : Short_Integer; pragma Import (Ada, E071, "ada__tags_E");
   E060 : Short_Integer; pragma Import (Ada, E060, "ada__strings__text_buffers_E");
   E057 : Short_Integer; pragma Import (Ada, E057, "ada__streams_E");
   E132 : Short_Integer; pragma Import (Ada, E132, "system__file_control_block_E");
   E089 : Short_Integer; pragma Import (Ada, E089, "system__finalization_root_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "ada__finalization_E");
   E124 : Short_Integer; pragma Import (Ada, E124, "system__file_io_E");
   E119 : Short_Integer; pragma Import (Ada, E119, "ada__text_io_E");
   E100 : Short_Integer; pragma Import (Ada, E100, "ada__strings__maps_E");
   E091 : Short_Integer; pragma Import (Ada, E091, "ada__strings__unbounded_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E091 := E091 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "ada__strings__unbounded__finalize_spec");
      begin
         F1;
      end;
      E119 := E119 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "ada__text_io__finalize_spec");
      begin
         F2;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "system__file_io__finalize_body");
      begin
         E124 := E124 - 1;
         F3;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (Ada, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;
   pragma Favor_Top_Level (No_Param_Proc);

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");
      Interrupts_Default_To_System : Integer;
      pragma Import (C, Interrupts_Default_To_System, "__gl_interrupts_default_to_system");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := '8';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      Ada.Exceptions'Elab_Spec;
      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E024 := E024 + 1;
      Ada.Numerics'Elab_Spec;
      E031 := E031 + 1;
      System.Exceptions'Elab_Spec;
      E025 := E025 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E020 := E020 + 1;
      E013 := E013 + 1;
      E008 := E008 + 1;
      Ada.Assertions'Elab_Spec;
      E006 := E006 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E058 := E058 + 1;
      Ada.Strings'Elab_Spec;
      E061 := E061 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      E063 := E063 + 1;
      Interfaces.C'Elab_Spec;
      E096 := E096 + 1;
      System.Os_Lib'Elab_Body;
      E126 := E126 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E071 := E071 + 1;
      Ada.Strings.Text_Buffers'Elab_Spec;
      E060 := E060 + 1;
      Ada.Streams'Elab_Spec;
      E057 := E057 + 1;
      System.File_Control_Block'Elab_Spec;
      E132 := E132 + 1;
      System.Finalization_Root'Elab_Spec;
      E089 := E089 + 1;
      Ada.Finalization'Elab_Spec;
      E055 := E055 + 1;
      System.File_Io'Elab_Body;
      E124 := E124 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E119 := E119 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E100 := E100 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E091 := E091 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_comprehensive_test_result");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      if gnat_argc = 0 then
         gnat_argc := argc;
         gnat_argv := argv;
      end if;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /Users/mike/Ada/github.com/abitofhelp/result/tests/obj/comprehensive_test_result.o
   --   -L/Users/mike/Ada/github.com/abitofhelp/result/tests/obj/
   --   -L/Users/mike/Ada/github.com/abitofhelp/result/tests/obj/
   --   -L/Users/mike/Ada/github.com/abitofhelp/result/lib/
   --   -L/users/mike/.local/share/alire/toolchains/gnat_native_15.1.2_2166c311/lib/gcc/x86_64-apple-darwin22.6.0/15.1.0/adalib/
   --   -static
   --   -lgnat
--  END Object file/option list   

end ada_main;
