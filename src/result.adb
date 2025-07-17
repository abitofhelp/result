-- =============================================================================
-- Ada Result Library
-- Copyright (c) 2025 A Bit of Help, Inc.
-- SPDX-License-Identifier: MIT
--
-- High-performance, memory-safe Result type for Ada providing type-safe error
-- handling without exceptions. Part of the Ada Result ecosystem.
--
-- For full license text, see LICENSE.md
-- For documentation, see README.md
-- =============================================================================

-- result.adb
-- Generic Result type package body for robust error handling
--
-- This implementation provides comprehensive error handling capabilities
-- while maintaining high performance through careful memory management
-- and exception safety guarantees.
--
-- Implementation Notes:
--   * Uses controlled types for automatic resource management
--   * Implements zero-copy operations through OUT parameters
--   * Provides exception-safe cleanup in all error paths
--   * Optimized for both small and large data types

with Ada.Exceptions; use Ada.Exceptions;

package body Result is

   -- Targeted pragma suppressions for performance-critical sections
   -- These suppress specific checks only where performance is critical and safety is ensured
   pragma Suppress (Index_Check, Get_State);
   pragma Suppress (Index_Check, Is_Ok);
   pragma Suppress (Index_Check, Is_Error);
   pragma Suppress (Range_Check, Get_Message_Length);
   pragma Suppress (Range_Check, Has_Message);

   ----------------------------------------------------------------------------
   -- INTERNAL VALIDATION HELPERS
   ----------------------------------------------------------------------------

   procedure Ensure_Valid_State (R : Result_Type) is
   begin
      if not R.Is_Initialized then
         raise Invalid_State_Error with "Result type not properly initialized";
      end if;

      if not Is_State_Consistent (R) then
         raise Invalid_State_Error with "Result type in inconsistent state";
      end if;
   end Ensure_Valid_State;
   pragma Inline (Ensure_Valid_State);

   procedure Ensure_Success_State (R : Result_Type) is
   begin
      Ensure_Valid_State (R);
      if R.State /= Success then
         if Length (R.Message) > 0 then
            raise Result_Error
              with "Expected success state but found error: " & To_String (R.Message);
         else
            raise Result_Error with "Expected success state but found error";
         end if;
      end if;
   end Ensure_Success_State;
   pragma Inline (Ensure_Success_State);

   procedure Ensure_Error_State (R : Result_Type) is
   begin
      Ensure_Valid_State (R);
      if R.State /= Result.Error then
         raise Result_Error with "Expected error state but found success";
      end if;
   end Ensure_Error_State;
   pragma Inline (Ensure_Error_State);

   ----------------------------------------------------------------------------
   -- MEMORY MANAGEMENT (RAII PATTERN)
   ----------------------------------------------------------------------------

   overriding
   procedure Initialize (Object : in out Result_Type) is
   begin
      -- Initialize Result in error state with default values
      -- This ensures safe initial state for all instances
      Object.State := Result.Error;
      Object.Value := Default_Value;
      Object.Error := Default_Error;
      Object.Message := Null_Unbounded_String;
      Object.Is_Initialized := True;
      Object.Needs_Cleanup := False;
   end Initialize;

   overriding
   procedure Adjust (Object : in out Result_Type) is
   begin
      -- Handle deep copying when Result is copied (assignment, parameter passing)
      -- This is called automatically by the runtime for controlled types
      case Object.State is
         when Success =>
            begin
               Object.Value := Copy_Value (Object.Value);
            exception
               when others =>
                  -- If copying fails, convert to error state
                  Object.State := Result.Error;
                  Object.Error := Default_Error;
                  Object.Message := To_Unbounded_String ("Value copy failed during adjust");
            end;
         when Result.Error =>
            begin
               Object.Error := Copy_Error (Object.Error);
            exception
               when others =>
                  -- If error copying fails, use default error
                  Object.Error := Default_Error;
                  Object.Message := To_Unbounded_String ("Error copy failed during adjust");
            end;
      end case;
      Object.Is_Initialized := True;
   end Adjust;

   overriding
   procedure Finalize (Object : in out Result_Type) is
      Was_Initialized : constant Boolean := Object.Is_Initialized;
   begin
      -- Clean up resources when Result goes out of scope
      -- This is called automatically when Result instances are destroyed
      if Was_Initialized then
         Object.Is_Initialized := False;

         if Object.Needs_Cleanup then
            Cleanup_Resources (Object);
         end if;

         Object.Message := Null_Unbounded_String;
         Object.Needs_Cleanup := False;
      end if;
   exception
      when others =>
         -- Finalize must never propagate exceptions
         Object.Is_Initialized := False;
         Object.Needs_Cleanup := False;
   end Finalize;

   ----------------------------------------------------------------------------
   -- CONSTRUCTION INTERFACE IMPLEMENTATION
   ----------------------------------------------------------------------------

   procedure Make_Ok (R : out Result_Type; Value : Value_Type) is
   begin
      -- Create a Result containing a successful value
      R.State := Success;
      R.Value := Copy_Value (Value);
      R.Error := Default_Error;
      R.Message := Null_Unbounded_String;
      R.Is_Initialized := True;
      R.Needs_Cleanup := Requires_Deep_Copy (R);
   end Make_Ok;

   procedure Make_Err (R : out Result_Type; Error : Error_Type) is
   begin
      -- Create a Result containing an error
      R.State := Result.Error;
      R.Value := Default_Value;
      R.Error := Copy_Error (Error);
      R.Message := Null_Unbounded_String;
      R.Is_Initialized := True;
      R.Needs_Cleanup := Requires_Deep_Copy (R);
   end Make_Err;

   procedure Make_Err
     (R : out Result_Type; Error : Error_Type; Message : String) is
   begin
      -- Create a Result containing an error with a descriptive message
      R.State := Result.Error;
      R.Value := Default_Value;
      R.Error := Copy_Error (Error);
      R.Message := To_Unbounded_String (Message);
      R.Is_Initialized := True;
      R.Needs_Cleanup := Requires_Deep_Copy (R);
   end Make_Err;

   ----------------------------------------------------------------------------
   -- STATE INSPECTION INTERFACE IMPLEMENTATION
   ----------------------------------------------------------------------------

   function Is_Ok (R : Result_Type) return Boolean is
   begin
      Ensure_Valid_State (R);
      return R.State = Success;
   end Is_Ok;

   function Is_Error (R : Result_Type) return Boolean is
   begin
      Ensure_Valid_State (R);
      return R.State = Result.Error;
   end Is_Error;

   function Get_State (R : Result_Type) return Result_State is
   begin
      Ensure_Valid_State (R);
      return R.State;
   end Get_State;

   ----------------------------------------------------------------------------
   -- VALUE EXTRACTION INTERFACE IMPLEMENTATION
   ----------------------------------------------------------------------------

   function Unwrap (R : Result_Type) return Value_Type is
   begin
      -- Extract the value from a successful Result (raises exception if error)
      -- This function should only be called when you're certain the Result contains a value
      if not Is_Ok (R) then
         if Length (R.Message) > 0 then
            raise Unwrap_Error
              with "Called unwrap on error result: " & To_String (R.Message);
         else
            raise Unwrap_Error with "Called unwrap on error result";
         end if;
      end if;
      return R.Value;
   end Unwrap;

   procedure Unwrap_Into (R : Result_Type; Value : out Value_Type) is
   begin
      if not Is_Ok (R) then
         if Length (R.Message) > 0 then
            raise Unwrap_Error
              with "Called unwrap on error result: " & To_String (R.Message);
         else
            raise Unwrap_Error with "Called unwrap on error result";
         end if;
      end if;
      Value := R.Value;
   end Unwrap_Into;

   function Unwrap_Or (R : Result_Type; Default : Value_Type) return Value_Type is
   begin
      -- Extract the value, or return a default if the Result contains an error
      -- This is a safe way to get a value without risking exceptions
      Ensure_Valid_State (R);
      if R.State = Success then
         return R.Value;
      else
         return Default;
      end if;
   end Unwrap_Or;

   procedure Unwrap_Or_Into
     (R : Result_Type; Default : Value_Type; Value : out Value_Type) is
   begin
      Ensure_Valid_State (R);
      if R.State = Success then
         Value := R.Value;
      else
         Value := Default;
      end if;
   end Unwrap_Or_Into;

   function Expect (R : Result_Type; Message : String) return Value_Type is
   begin
      if not Is_Ok (R) then
         raise Expect_Error with Message;
      end if;
      return R.Value;
   end Expect;

   procedure Expect_Into
     (R : Result_Type; Message : String; Value : out Value_Type) is
   begin
      if not Is_Ok (R) then
         raise Expect_Error with Message;
      end if;
      Value := R.Value;
   end Expect_Into;

   function Unwrap_Err (R : Result_Type) return Error_Type is
   begin
      if not Is_Error (R) then
         raise Unwrap_Error with "Called unwrap_err on success result";
      end if;
      return R.Error;
   end Unwrap_Err;

   procedure Unwrap_Err_Into (R : Result_Type; Error : out Error_Type) is
   begin
      if not Is_Error (R) then
         raise Unwrap_Error with "Called unwrap_err on success result";
      end if;
      Error := R.Error;
   end Unwrap_Err_Into;

   function Expect_Err (R : Result_Type; Message : String) return Error_Type is
   begin
      if not Is_Error (R) then
         raise Expect_Error with Message;
      end if;
      return R.Error;
   end Expect_Err;

   ----------------------------------------------------------------------------
   -- TRANSFORMATION OPERATIONS IMPLEMENTATION
   ----------------------------------------------------------------------------

   package body Map_Operations is

      overriding
      procedure Initialize (Object : in out New_Result_Type) is
      begin
         Object.State := Result.Error;
         Object.Message := Null_Unbounded_String;
         Object.Is_Initialized := True;
      end Initialize;

      overriding
      procedure Adjust (Object : in out New_Result_Type) is
      begin
         Object.Is_Initialized := True;
      end Adjust;

      overriding
      procedure Finalize (Object : in out New_Result_Type) is
      begin
         Object.Message := Null_Unbounded_String;
         Object.Is_Initialized := False;
      end Finalize;

      procedure Make_Ok (R : out New_Result_Type; Value : New_Value_Type) is
      begin
         R.State := Success;
         R.Value := Value;
         R.Message := Null_Unbounded_String;
         R.Is_Initialized := True;
      end Make_Ok;

      procedure Make_Err (R : out New_Result_Type; Error : Error_Type) is
      begin
         R.State := Result.Error;
         R.Error := Error;
         R.Message := Null_Unbounded_String;
         R.Is_Initialized := True;
      end Make_Err;

      procedure Make_Err (R : out New_Result_Type; Error : Error_Type; Message : String) is
      begin
         R.State := Result.Error;
         R.Error := Error;
         R.Message := To_Unbounded_String (Message);
         R.Is_Initialized := True;
      end Make_Err;

      function Is_Ok (R : New_Result_Type) return Boolean is
      begin
         return R.Is_Initialized and then R.State = Success;
      end Is_Ok;

      function Is_Error (R : New_Result_Type) return Boolean is
      begin
         return R.Is_Initialized and then R.State = Result.Error;
      end Is_Error;

      function Unwrap (R : New_Result_Type) return New_Value_Type is
      begin
         if not Is_Ok (R) then
            raise Unwrap_Error with "Called unwrap on error result";
         end if;
         return R.Value;
      end Unwrap;

      function Unwrap_Err (R : New_Result_Type) return Error_Type is
      begin
         if not Is_Error (R) then
            raise Unwrap_Error with "Called unwrap_err on success result";
         end if;
         return R.Error;
      end Unwrap_Err;

      procedure Map (R : Result_Type; New_R : out New_Result_Type) is
      begin
         -- Transform the success value if present, leave errors unchanged
         -- This allows you to convert one value type to another while preserving errors
         Ensure_Valid_State (R);

         if R.State = Success then
            declare
               Transformed_Value : New_Value_Type;
            begin
               Transform (R.Value, Transformed_Value);
               Make_Ok (New_R, Transformed_Value);
            exception
               when Ex : others =>
                  Make_Err (New_R, Default_Error, "Map operation failed: " & Exception_Information (Ex));
            end;
         else
            Make_Err (New_R, Default_Error, To_String (R.Message));
         end if;
      end Map;
   end Map_Operations;

   package body And_Then_Operations is
      procedure And_Then (R : Result_Type; New_R : out Result_Type) is
      begin
         -- Chain operations that might fail
         -- Use this when your transformation function can itself return a Result
         Ensure_Valid_State (R);

         if R.State = Success then
            begin
               Transform (R.Value, New_R);
            exception
               when Ex : others =>
                  Make_Err (New_R, Default_Error, "And_Then operation failed: " & Exception_Information (Ex));
            end;
         else
            Make_Err (New_R, R.Error, To_String (R.Message));
         end if;
      end And_Then;
   end And_Then_Operations;

   package body Map_Error_Operations is
      procedure Map_Err (R : Result_Type; New_R : out Result_Type) is
      begin
         Ensure_Valid_State (R);

         if R.State = Result.Error then
            declare
               Transformed_Error : Error_Type;
            begin
               Transform (R.Error, Transformed_Error);
               Make_Err (New_R, Transformed_Error, To_String (R.Message));
            exception
               when Ex : others =>
                  Make_Err (New_R, Default_Error, "Map_Err operation failed: " & Exception_Information (Ex));
            end;
         else
            Make_Ok (New_R, R.Value);
         end if;
      end Map_Err;
   end Map_Error_Operations;

   package body Bind_Operations is
      procedure Bind (R : Result_Type; New_R : out Result_Type) is
      begin
         Ensure_Valid_State (R);

         if R.State = Success then
            begin
               Transform (R.Value, New_R);
            exception
               when Ex : others =>
                  Make_Err (New_R, Default_Error, "Bind operation failed: " & Exception_Information (Ex));
            end;
         else
            Make_Err (New_R, R.Error, To_String (R.Message));
         end if;
      end Bind;
   end Bind_Operations;

   package body Fmap_Operations is

      overriding
      procedure Initialize (Object : in out New_Result_Type) is
      begin
         Object.State := Result.Error;
         Object.Message := Null_Unbounded_String;
         Object.Is_Initialized := True;
      end Initialize;

      overriding
      procedure Adjust (Object : in out New_Result_Type) is
      begin
         Object.Is_Initialized := True;
      end Adjust;

      overriding
      procedure Finalize (Object : in out New_Result_Type) is
      begin
         Object.Message := Null_Unbounded_String;
         Object.Is_Initialized := False;
      end Finalize;

      procedure Make_Ok (R : out New_Result_Type; Value : New_Value_Type) is
      begin
         R.State := Success;
         R.Value := Value;
         R.Message := Null_Unbounded_String;
         R.Is_Initialized := True;
      end Make_Ok;

      procedure Make_Err (R : out New_Result_Type; Error : Error_Type) is
      begin
         R.State := Result.Error;
         R.Error := Error;
         R.Message := Null_Unbounded_String;
         R.Is_Initialized := True;
      end Make_Err;

      procedure Make_Err (R : out New_Result_Type; Error : Error_Type; Message : String) is
      begin
         R.State := Result.Error;
         R.Error := Error;
         R.Message := To_Unbounded_String (Message);
         R.Is_Initialized := True;
      end Make_Err;

      function Is_Ok (R : New_Result_Type) return Boolean is
      begin
         return R.Is_Initialized and then R.State = Success;
      end Is_Ok;

      function Is_Error (R : New_Result_Type) return Boolean is
      begin
         return R.Is_Initialized and then R.State = Result.Error;
      end Is_Error;

      function Unwrap (R : New_Result_Type) return New_Value_Type is
      begin
         if not Is_Ok (R) then
            raise Unwrap_Error with "Called unwrap on error result";
         end if;
         return R.Value;
      end Unwrap;

      function Unwrap_Err (R : New_Result_Type) return Error_Type is
      begin
         if not Is_Error (R) then
            raise Unwrap_Error with "Called unwrap_err on success result";
         end if;
         return R.Error;
      end Unwrap_Err;

      procedure Fmap (R : Result_Type; New_R : out New_Result_Type) is
      begin
         Ensure_Valid_State (R);

         if R.State = Success then
            declare
               Transformed_Value : New_Value_Type;
            begin
               Transform (R.Value, Transformed_Value);
               Make_Ok (New_R, Transformed_Value);
            exception
               when Ex : others =>
                  Make_Err (New_R, Default_Error, "Fmap operation failed: " & Exception_Information (Ex));
            end;
         else
            Make_Err (New_R, Default_Error, To_String (R.Message));
         end if;
      end Fmap;
   end Fmap_Operations;

   ----------------------------------------------------------------------------
   -- PATTERN MATCHING INTERFACE IMPLEMENTATION
   ----------------------------------------------------------------------------

   package body Match_Operations is
      procedure Match (R : Result_Type; Output : out Return_Type) is
      begin
         Ensure_Valid_State (R);
         case R.State is
            when Success =>
               On_Success (R.Value, Output);
            when Result.Error =>
               On_Error (R.Error, Output);
         end case;
      end Match;
   end Match_Operations;

   package body Fold_Operations is
      procedure Fold (R : Result_Type; Output : out Return_Type) is
      begin
         Ensure_Valid_State (R);
         case R.State is
            when Success =>
               Success_Transform (R.Value, Output);
            when Result.Error =>
               Error_Transform (R.Error, Output);
         end case;
      end Fold;
   end Fold_Operations;

   package body Swap_Operations is
      procedure Swap (R : Result_Type; Swapped_R : out Result_Type) is
      begin
         Ensure_Valid_State (R);
         case R.State is
            when Success =>
               Make_Err (Swapped_R, Value_To_Error (R.Value), "Swapped from success");
            when Result.Error =>
               Make_Ok (Swapped_R, Error_To_Value (R.Error));
         end case;
      end Swap;
   end Swap_Operations;

   ----------------------------------------------------------------------------
   -- ADVANCED RUST-STYLE OPERATIONS IMPLEMENTATION
   ----------------------------------------------------------------------------

   package body Lazy_Operations is
      function Unwrap_Or_Else (R : Result_Type) return Value_Type is
      begin
         Ensure_Valid_State (R);
         if R.State = Success then
            return R.Value;
         else
            return Default_Fn;
         end if;
      end Unwrap_Or_Else;
   end Lazy_Operations;

   package body Map_Or_Operations is
      function Map_Or (R : Result_Type; Default : Value_Type) return Value_Type is
      begin
         Ensure_Valid_State (R);
         if R.State = Success then
            return Transform_Fn (R.Value);
         else
            return Default;
         end if;
      end Map_Or;
   end Map_Or_Operations;

   package body Value_Predicate_Operations is
      function Is_Ok_And (R : Result_Type) return Boolean is
      begin
         Ensure_Valid_State (R);
         return R.State = Success and then Predicate (R.Value);
      end Is_Ok_And;
   end Value_Predicate_Operations;

   package body Error_Predicate_Operations is
      function Is_Err_And (R : Result_Type) return Boolean is
      begin
         Ensure_Valid_State (R);
         return R.State = Result.Error and then Predicate (R.Error);
      end Is_Err_And;
   end Error_Predicate_Operations;

   ----------------------------------------------------------------------------
   -- SAFE EXTRACTION INTERFACE IMPLEMENTATION
   ----------------------------------------------------------------------------

   function Is_Valid_State (R : Result_Type) return Boolean is
   begin
      return R.Is_Initialized and then Is_State_Consistent (R);
   end Is_Valid_State;

   function Is_State_Consistent (R : Result_Type) return Boolean is
   begin
      if not R.Is_Initialized then
         return False;
      end if;

      case R.State is
         when Success =>
            return True;
         when Result.Error =>
            return True;
      end case;
   end Is_State_Consistent;
   pragma Inline (Is_State_Consistent);

   function Has_Message (R : Result_Type) return Boolean is
   begin
      Ensure_Valid_State (R);
      return Length (R.Message) > 0;
   end Has_Message;

   function Get_Message_Length (R : Result_Type) return Natural is
   begin
      Ensure_Valid_State (R);
      return Length (R.Message);
   end Get_Message_Length;

   function Try_Get_Value
     (R : Result_Type; Value : out Value_Type) return Boolean is
   begin
      if not Is_Valid_State (R) or else R.State /= Success then
         return False;
      end if;
      Value := R.Value;
      return True;
   end Try_Get_Value;

   function Try_Get_Error
     (R : Result_Type; Error : out Error_Type) return Boolean is
   begin
      if not Is_Valid_State (R) or else R.State /= Result.Error then
         return False;
      end if;
      Error := R.Error;
      return True;
   end Try_Get_Error;

   function Try_Get_Message
     (R : Result_Type; Message : out Unbounded_String) return Boolean is
   begin
      if not Is_Valid_State (R)
        or else R.State /= Result.Error
        or else Length (R.Message) = 0
      then
         return False;
      end if;
      Message := R.Message;
      return True;
   end Try_Get_Message;

   function Get_Message (R : Result_Type) return String is
   begin
      Ensure_Error_State (R);
      return To_String (R.Message);
   end Get_Message;

   ----------------------------------------------------------------------------
   -- RESOURCE MANAGEMENT IMPLEMENTATION
   ----------------------------------------------------------------------------

   procedure Cleanup_Resources (R : in out Result_Type) is
   begin
      -- This procedure cleans up any resources associated with the Result
      -- It provides a framework for handling cleanup of Value_Type and Error_Type instances
      
      -- Clear the cleanup flag first to prevent recursive cleanup
      R.Needs_Cleanup := False;
      
      -- Perform cleanup based on the current state
      case R.State is
         when Success =>
            -- Clean up value-related resources
            -- For types that implement Ada.Finalization.Controlled,
            -- finalization will be handled automatically
            
            -- For access types, we would typically null the access value
            -- after deallocating the pointed-to object:
            -- if Value_Type in Access_Type'Class then
            --    Free(R.Value);  -- Using appropriate deallocation
            -- end if;
            
            -- For file handles or other system resources:
            -- if Value_Type has file handles then
            --    Close_File(R.Value);
            -- end if;
            
            null; -- Default case for basic types
            
         when Result.Error =>
            -- Clean up error-related resources
            -- Similar cleanup logic for Error_Type
            
            null; -- Default case for basic types
      end case;
      
      -- Additional cleanup for message string is handled by finalization
      -- R.Message will be automatically cleaned up by Unbounded_String
      
   exception
      when others =>
         -- Ensure cleanup flag is cleared even if cleanup fails
         R.Needs_Cleanup := False;
         -- We don't re-raise the exception to prevent finalization issues
   end Cleanup_Resources;

   function Requires_Deep_Copy (R : Result_Type) return Boolean is
      pragma Unreferenced (R);
   begin
      -- This function determines whether the Value_Type and Error_Type require 
      -- deep copying based on their characteristics
      
      -- Check for characteristics that require deep copying:
      
      -- 1. Access types (pointers) - These always require deep copying
      --    to avoid aliasing and dangling pointer issues
      -- if Value_Type'Has_Access_Values or Error_Type'Has_Access_Values then
      --    return True;
      -- end if;
      
      -- 2. Types with finalization (controlled types) - These may require
      --    deep copying if they manage resources
      -- if Value_Type in Ada.Finalization.Controlled'Class or
      --    Error_Type in Ada.Finalization.Controlled'Class then
      --    return True;
      -- end if;
      
      -- 3. Composite types containing access values or controlled types
      -- if Value_Type'Has_Discriminants or Error_Type'Has_Discriminants then
      --    -- Additional analysis needed for discriminated records
      --    return True;
      -- end if;
      
      -- 4. Types that are large enough to benefit from reference counting
      --    or have expensive copying operations
      -- if Value_Type'Size > 1024 or Error_Type'Size > 1024 then
      --    return True;
      -- end if;
      
      -- 5. String types and unbounded containers typically require deep copying
      -- Note: This is a conservative approach - in practice, we'd want to be
      --       more specific about which types actually need deep copying
      
      -- For most basic types (Integer, Boolean, Character, enums, small records),
      -- deep copying is not necessary and would add unnecessary overhead
      
      -- This implementation defaults to False for performance with basic types
      -- Users can override this by providing their own Cleanup_Resources implementation
      -- or by using types that are known to require deep copying
      
      return False;
      
      -- NOTE: In a production implementation, this function could be enhanced to:
      -- 1. Use Ada 2012 aspects to declare copy requirements
      -- 2. Analyze type characteristics at compile time
      -- 3. Allow user override through generic parameters
      -- 4. Use type-specific heuristics for common patterns
   end Requires_Deep_Copy;

   ----------------------------------------------------------------------------
   -- DEBUGGING AND DIAGNOSTICS IMPLEMENTATION
   ----------------------------------------------------------------------------

   function To_String (R : Result_Type) return String is
   begin
      if not Is_Valid_State (R) then
         return "Invalid(uninitialized)";
      end if;

      case R.State is
         when Success =>
            return "Ok(value)";
         when Result.Error =>
            if Length (R.Message) > 0 then
               return "Err(" & To_String (R.Message) & ")";
            else
               return "Err(error)";
            end if;
      end case;
   end To_String;

   function To_Debug_String (R : Result_Type) return String is
   begin
      if not Is_Valid_State (R) then
         return "Result { State: Invalid, Initialized: False }";
      end if;

      case R.State is
         when Success =>
            return
              "Result { State: Success, Has_Value: True, Initialized: True, "
              & "Needs_Cleanup: " & Boolean'Image (R.Needs_Cleanup) & " }";
         when Result.Error =>
            return
              "Result { State: Error, Has_Error: True, Message_Length: "
              & Natural'Image (Length (R.Message))
              & ", Initialized: True, Needs_Cleanup: " 
              & Boolean'Image (R.Needs_Cleanup) & " }";
      end case;
   end To_Debug_String;

   procedure Validate_State (R : Result_Type) is
   begin
      if not R.Is_Initialized then
         raise Invalid_State_Error with "Result not initialized - object may be corrupted";
      end if;

      if not Is_State_Consistent (R) then
         raise Invalid_State_Error with "Result state is inconsistent - internal invariants violated";
      end if;

      case R.State is
         when Success =>
            -- Success state validation - ensure value is accessible
            -- Additional validation could check Value_Type specific constraints
            null;
         when Result.Error =>
            -- Error state validation - ensure error information is consistent
            -- Additional validation could check Error_Type specific constraints
            null;
      end case;
   end Validate_State;

end Result;
