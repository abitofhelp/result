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

-- result.ads
-- Generic Result type package specification for robust error handling
-- 
-- This package implements a Result<T, E> type that combines the best features
-- of Rust's Result type and functional programming Either types. It provides
-- type-safe error handling without exceptions for normal control flow.
--
-- Key Benefits:
--   * Explicit error handling - errors cannot be ignored
--   * Type safety - compile-time guarantees about error handling
--   * Performance - optimized with OUT parameters to minimize copying
--   * Composability - functional operations for chaining computations
--   * Flexibility - both procedural and functional programming styles supported
--
-- Basic Usage:
--   package My_Result is new Result (Integer, String);
--   R : My_Result.Result_Type;
--   My_Result.Make_Ok (R, 42);
--   if My_Result.Is_Ok (R) then
--      Value := My_Result.Unwrap (R);
--   end if;
--
-- Advanced Usage Examples:
--   * Chain operations with And_Then for error propagation
--   * Transform values with Map operations while preserving errors
--   * Pattern match with Match_Operations for explicit handling
--   * Use safe extraction with Try_Get_* operations
--
-- Thread Safety: 
--   * Individual Result instances are NOT thread-safe
--   * Do not access the same Result instance from multiple threads simultaneously
--   * Use separate Result instances for each thread when possible
--   * For shared access, use Ada protected types or other synchronization mechanisms
--   * This library contains no global state - all operations work on instance data
--
-- Performance: 
--   * Uses OUT parameters to avoid copying large data structures
--   * Minimizes memory allocation by using stack-based storage
--   * Compiler optimizations applied to frequently called functions
--   * Simple internal representation for fast state checking
--
-- Memory Management: 
--   * Automatic cleanup when Result instances go out of scope
--   * Uses Ada's controlled types for predictable resource management
--   * Safe handling of types that contain pointers or file handles
--   * Cleanup operations will not raise exceptions

with Ada.Strings.Unbounded;
with Ada.Finalization;

generic
   type Value_Type is private;
   -- The type stored when the operation succeeds

   type Error_Type is private;
   -- The type stored when the operation fails

   -- Copy functions - provide custom functions if your types need special copying
   -- For simple types like Integer, the default copying is sufficient
   with function Copy_Value (Source : Value_Type) return Value_Type is <>;
   with function Copy_Error (Source : Error_Type) return Error_Type is <>;

   -- Default constructors - these create initial values for your types
   -- Example: for Integer, this might return 0
   with function Default_Value return Value_Type is <>;
   with function Default_Error return Error_Type is <>;
package Result is

   pragma Preelaborate (Result);

   use Ada.Strings.Unbounded;

   ----------------------------------------------------------------------------
   -- CORE TYPES AND CONSTANTS
   ----------------------------------------------------------------------------

   type Result_State is (Success, Error);

   type Result_Type is new Ada.Finalization.Controlled with private;

   ----------------------------------------------------------------------------
   -- CONSTRUCTION INTERFACE
   -- These procedures create Result instances containing either a value or an error
   ----------------------------------------------------------------------------

   -- Create a Result containing a successful value
   procedure Make_Ok (R : out Result_Type; Value : Value_Type)
     with Post => Is_Ok (R) and then Unwrap (R) = Value;
   
   -- Create a Result containing an error
   procedure Make_Err (R : out Result_Type; Error : Error_Type)
     with Post => Is_Error (R);
   
   -- Create a Result containing an error with a descriptive message
   procedure Make_Err (R : out Result_Type; Error : Error_Type; Message : String)
     with Post => Is_Error (R);

   ----------------------------------------------------------------------------
   -- STATE INSPECTION INTERFACE
   -- These functions check whether a Result contains a value or an error
   ----------------------------------------------------------------------------

   -- Returns True if the Result contains a successful value
   function Is_Ok (R : Result_Type) return Boolean;
   pragma Inline (Is_Ok);

   -- Returns True if the Result contains an error
   function Is_Error (R : Result_Type) return Boolean;
   pragma Inline (Is_Error);

   -- Returns the internal state (Success or Error)
   function Get_State (R : Result_Type) return Result_State;
   pragma Inline (Get_State);

   ----------------------------------------------------------------------------
   -- VALUE EXTRACTION INTERFACE
   -- These functions and procedures extract values from Result instances
   -- WARNING: Some of these will raise exceptions if used incorrectly
   ----------------------------------------------------------------------------

   -- Extract the value from a successful Result (raises exception if error)
   function Unwrap (R : Result_Type) return Value_Type
     with Pre => Is_Ok (R);
   
   -- Extract the value into an OUT parameter (raises exception if error)
   procedure Unwrap_Into (R : Result_Type; Value : out Value_Type)
     with Pre => Is_Ok (R);
   
   -- Extract the value, or return a default if the Result contains an error
   function Unwrap_Or (R : Result_Type; Default : Value_Type) return Value_Type
     with Post => (if Is_Ok (R) then Unwrap_Or'Result = Unwrap (R) else Unwrap_Or'Result = Default);
   
   -- Extract the value into an OUT parameter, or use default if error
   procedure Unwrap_Or_Into (R : Result_Type; Default : Value_Type; Value : out Value_Type)
     with Post => (if Is_Ok (R) then Value = Unwrap (R) else Value = Default);
   
   -- Extract the value with a custom error message (raises exception if error)
   function Expect (R : Result_Type; Message : String) return Value_Type
     with Pre => Is_Ok (R);
   
   -- Extract the value into an OUT parameter with custom error message
   procedure Expect_Into (R : Result_Type; Message : String; Value : out Value_Type)
     with Pre => Is_Ok (R);
   
   -- Extract the error from a failed Result (raises exception if success)
   function Unwrap_Err (R : Result_Type) return Error_Type
     with Pre => Is_Error (R);
   
   -- Extract the error into an OUT parameter (raises exception if success)
   procedure Unwrap_Err_Into (R : Result_Type; Error : out Error_Type)
     with Pre => Is_Error (R);
   
   -- Extract the error with a custom message (raises exception if success)
   function Expect_Err (R : Result_Type; Message : String) return Error_Type
     with Pre => Is_Error (R);

   ----------------------------------------------------------------------------
   -- TRANSFORMATION OPERATIONS
   -- These generic packages provide ways to transform Result values
   ----------------------------------------------------------------------------

   -- Map operation - transform the success value if present, leave errors unchanged
   -- Example: transform Result containing Integer to Result containing String
   generic
      type New_Value_Type is private;
      with procedure Transform (Input : Value_Type; Output : out New_Value_Type);
   package Map_Operations is
      type New_Result_Type is new Ada.Finalization.Controlled with private;

      procedure Map (R : Result_Type; New_R : out New_Result_Type);
      procedure Make_Ok (R : out New_Result_Type; Value : New_Value_Type);
      procedure Make_Err (R : out New_Result_Type; Error : Error_Type);
      procedure Make_Err (R : out New_Result_Type; Error : Error_Type; Message : String);
      function Is_Ok (R : New_Result_Type) return Boolean;
      function Is_Error (R : New_Result_Type) return Boolean;
      function Unwrap (R : New_Result_Type) return New_Value_Type;
      function Unwrap_Err (R : New_Result_Type) return Error_Type;

   private
      type New_Result_Type is new Ada.Finalization.Controlled with record
         State : Result_State := Error;
         Value : New_Value_Type;
         Error : Error_Type;
         Message : Unbounded_String := Null_Unbounded_String;
         Is_Initialized : Boolean := False;
      end record;

      overriding procedure Initialize (Object : in out New_Result_Type);
      overriding procedure Adjust (Object : in out New_Result_Type);
      overriding procedure Finalize (Object : in out New_Result_Type);
   end Map_Operations;

   -- And_Then operation - chain operations that might fail
   -- Use this when your transformation function can itself return a Result
   generic
      with procedure Transform (Input : Value_Type; Output : out Result_Type);
   package And_Then_Operations is
      procedure And_Then (R : Result_Type; New_R : out Result_Type);
   end And_Then_Operations;

   -- Map_Err operation - transform error values while leaving success unchanged
   -- Use this to convert one error type to another or add context to errors
   generic
      with procedure Transform (Input : Error_Type; Output : out Error_Type);
   package Map_Error_Operations is
      procedure Map_Err (R : Result_Type; New_R : out Result_Type);
   end Map_Error_Operations;

   ----------------------------------------------------------------------------
   -- PATTERN MATCHING INTERFACE
   -- These operations handle both success and error cases explicitly
   ----------------------------------------------------------------------------

   -- Pattern matching - provide handlers for both success and error cases
   -- The appropriate handler will be called based on the Result's state
   generic
      type Return_Type is private;
      with procedure On_Success (V : Value_Type; Output : out Return_Type);
      with procedure On_Error (E : Error_Type; Output : out Return_Type);
   package Match_Operations is
      procedure Match (R : Result_Type; Output : out Return_Type);
   end Match_Operations;

   -- Fold operation - convert Result to a single value by handling both cases
   -- Similar to Match but emphasizes the reduction to a single output type
   generic
      type Return_Type is private;
      with procedure Success_Transform (V : Value_Type; Output : out Return_Type);
      with procedure Error_Transform (E : Error_Type; Output : out Return_Type);
   package Fold_Operations is
      procedure Fold (R : Result_Type; Output : out Return_Type);
   end Fold_Operations;

   -- Swap operation - convert success to error and error to success
   -- Use this when you want to reverse the meaning of success/failure
   generic
      with function Value_To_Error (V : Value_Type) return Error_Type;
      with function Error_To_Value (E : Error_Type) return Value_Type;
   package Swap_Operations is
      procedure Swap (R : Result_Type; Swapped_R : out Result_Type);
   end Swap_Operations;

   -- Bind operation - same as And_Then, for functional programming style
   -- Use this when chaining operations that can fail
   generic
      with procedure Transform (Input : Value_Type; Output : out Result_Type);
   package Bind_Operations is
      procedure Bind (R : Result_Type; New_R : out Result_Type);
   end Bind_Operations;

   -- Fmap operation - same as Map, for functional programming style
   -- Transform successful values while preserving errors
   generic
      type New_Value_Type is private;
      with procedure Transform (Input : Value_Type; Output : out New_Value_Type);
   package Fmap_Operations is
      type New_Result_Type is new Ada.Finalization.Controlled with private;
      procedure Fmap (R : Result_Type; New_R : out New_Result_Type);

      procedure Make_Ok (R : out New_Result_Type; Value : New_Value_Type);
      procedure Make_Err (R : out New_Result_Type; Error : Error_Type);
      procedure Make_Err (R : out New_Result_Type; Error : Error_Type; Message : String);
      function Is_Ok (R : New_Result_Type) return Boolean;
      function Is_Error (R : New_Result_Type) return Boolean;
      function Unwrap (R : New_Result_Type) return New_Value_Type;
      function Unwrap_Err (R : New_Result_Type) return Error_Type;

   private
      type New_Result_Type is new Ada.Finalization.Controlled with record
         State : Result_State := Error;
         Value : New_Value_Type;
         Error : Error_Type;
         Message : Unbounded_String := Null_Unbounded_String;
         Is_Initialized : Boolean := False;
      end record;

      overriding procedure Initialize (Object : in out New_Result_Type);
      overriding procedure Adjust (Object : in out New_Result_Type);
      overriding procedure Finalize (Object : in out New_Result_Type);
   end Fmap_Operations;

   ----------------------------------------------------------------------------
   -- SAFE EXTRACTION INTERFACE
   -- These functions extract values without raising exceptions
   ----------------------------------------------------------------------------

   -- Check if the Result is in a valid state (properly initialized)
   function Is_Valid_State (R : Result_Type) return Boolean;
   pragma Inline (Is_Valid_State);

   -- Check if the Result has an error message
   function Has_Message (R : Result_Type) return Boolean;
   pragma Inline (Has_Message);

   -- Get the length of the error message
   function Get_Message_Length (R : Result_Type) return Natural;
   pragma Inline (Get_Message_Length);

   -- Try to get the value - returns True if successful, False if error
   function Try_Get_Value (R : Result_Type; Value : out Value_Type) return Boolean
     with Post => (if Try_Get_Value'Result then Is_Ok (R) else Is_Error (R));
   
   -- Try to get the error - returns True if successful, False if contains value
   function Try_Get_Error (R : Result_Type; Error : out Error_Type) return Boolean
     with Post => (if Try_Get_Error'Result then Is_Error (R) else Is_Ok (R));
   
   -- Try to get the error message - returns True if successful
   function Try_Get_Message (R : Result_Type; Message : out Unbounded_String) return Boolean
     with Post => (if Try_Get_Message'Result then Is_Error (R) and Has_Message (R) else True);

   ----------------------------------------------------------------------------
   -- ADVANCED OPERATIONS
   -- These operations provide more specialized functionality
   ----------------------------------------------------------------------------

   -- Lazy default evaluation - only call the default function if needed
   generic
      with function Default_Fn return Value_Type;
   package Lazy_Operations is
      function Unwrap_Or_Else (R : Result_Type) return Value_Type;
   end Lazy_Operations;

   -- Conditional transformation - transform success value or use default
   generic
      with function Transform_Fn (V : Value_Type) return Value_Type;
   package Map_Or_Operations is
      function Map_Or (R : Result_Type; Default : Value_Type) return Value_Type;
   end Map_Or_Operations;

   -- Conditional success checking - check if success AND predicate is true
   generic
      with function Predicate (V : Value_Type) return Boolean;
   package Value_Predicate_Operations is
      function Is_Ok_And (R : Result_Type) return Boolean;
   end Value_Predicate_Operations;

   -- Conditional error checking - check if error AND predicate is true
   generic
      with function Predicate (E : Error_Type) return Boolean;
   package Error_Predicate_Operations is
      function Is_Err_And (R : Result_Type) return Boolean;
   end Error_Predicate_Operations;

   -- Get the error message as a String (raises exception if no message)
   function Get_Message (R : Result_Type) return String;

   ----------------------------------------------------------------------------
   -- DEBUGGING AND DIAGNOSTICS
   -- These functions help with debugging and internal state management
   ----------------------------------------------------------------------------

   -- Get a human-readable representation of the Result
   function To_String (R : Result_Type) return String;
   
   -- Get detailed debug information about the Result
   function To_Debug_String (R : Result_Type) return String;
   
   -- Validate that the Result is in a consistent state (raises exception if not)
   procedure Validate_State (R : Result_Type);
   
   -- Check if the Result's internal state is consistent
   function Is_State_Consistent (R : Result_Type) return Boolean;
   
   -- Clean up any resources associated with this Result
   procedure Cleanup_Resources (R : in out Result_Type);
   
   -- Check if the Result's types require deep copying
   function Requires_Deep_Copy (R : Result_Type) return Boolean;

   ----------------------------------------------------------------------------
   -- EXCEPTION TYPES
   -- These exceptions are raised when operations fail
   ----------------------------------------------------------------------------

   Result_Error : exception;        -- General Result operation error
   Invalid_State_Error : exception; -- Result is in an invalid state
   Unwrap_Error : exception;        -- Unwrap called on error Result
   Expect_Error : exception;        -- Expect called on error Result

private

   type Result_Type is new Ada.Finalization.Controlled with record
      State : Result_State := Error;
      Value : Value_Type := Default_Value;
      Error : Error_Type := Default_Error;
      Message : Unbounded_String := Null_Unbounded_String;
      Is_Initialized : Boolean := False;
      Needs_Cleanup : Boolean := False;
   end record;

   overriding procedure Initialize (Object : in out Result_Type);
   overriding procedure Adjust (Object : in out Result_Type);
   overriding procedure Finalize (Object : in out Result_Type);

   procedure Ensure_Valid_State (R : Result_Type);
   procedure Ensure_Success_State (R : Result_Type);
   procedure Ensure_Error_State (R : Result_Type);

end Result;
