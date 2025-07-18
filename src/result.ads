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
-- @summary
-- This package implements a Result<T, E> type that combines the best features
-- of Rust's Result type and functional programming Either types. It provides
-- type-safe error handling without exceptions for normal control flow.
--
-- @description
-- The Result type encapsulates the outcome of an operation that may succeed 
-- (returning a value) or fail (returning an error). This approach makes error 
-- handling explicit and impossible to ignore, while providing both performance 
-- and safety guarantees.
--
-- @key_benefits
--   * Explicit error handling - errors cannot be ignored by the compiler
--   * Type safety - compile-time guarantees about error handling paths
--   * Performance - optimized with OUT parameters to minimize copying
--   * Composability - functional operations for chaining computations
--   * Flexibility - both procedural and functional programming styles supported
--   * Memory safety - automatic resource management via controlled types
--
-- @basic_usage
--   declare
--      function Default_Int return Integer is (0);
--      function Default_Err return String is ("");
--      
--      package My_Result is new Result 
--        (Value_Type => Integer, 
--         Error_Type => String,
--         Default_Value => Default_Int,
--         Default_Error => Default_Err);
--      use My_Result;
--      
--      R : Result_Type;
--      Value : Integer;
--   begin
--      Make_Ok (R, 42);
--      if Is_Ok (R) then
--         Unwrap_Into (R, Value);
--         -- Value is now 42
--      end if;
--   end;
--
-- @advanced_patterns
--   * Chain operations with And_Then for automatic error propagation
--   * Transform values with Map operations while preserving error states
--   * Pattern match with Match_Operations for comprehensive error handling
--   * Use safe extraction with Try_Get_* operations to avoid exceptions
--   * Implement validation pipelines with multiple And_Then operations
--
-- @thread_safety
--   Individual Result instances are NOT thread-safe. For concurrent access:
--   * Use separate Result instances for each thread when possible
--   * Synchronize access using Ada protected types or other mechanisms
--   * Consider message passing patterns instead of shared state
--   * This library contains no global state - all operations work on instance data
--
-- @performance_characteristics
--   * Uses OUT parameters to avoid copying large data structures
--   * Minimizes memory allocation by using stack-based storage
--   * Compiler optimizations applied to frequently called functions
--   * Simple internal representation for fast state checking
--   * Zero-cost abstractions for most operations
--
-- @memory_management
--   * Automatic cleanup when Result instances go out of scope
--   * Uses Ada's controlled types for predictable resource management
--   * Safe handling of types that contain pointers or file handles
--   * Cleanup operations will not raise exceptions
--   * Custom copy functions supported for complex resource management
--
-- @see_also
--   * README.md - Comprehensive usage examples and patterns
--   * tests/comprehensive_test_result.adb - Complete test coverage examples

with Ada.Strings.Unbounded;
with Ada.Finalization;

generic
   type Value_Type is private;
   -- @param Value_Type The type stored when the operation succeeds
   --
   -- This can be any Ada type (scalar, composite, access, etc.). The Result
   -- type will store instances of this type when operations complete successfully.
   --
   -- @example Integer, String, My_Record_Type, File_Handle

   type Error_Type is private;
   -- @param Error_Type The type stored when the operation fails
   --
   -- This represents the error information returned when operations fail.
   -- Common choices include enumeration types, string types, or custom
   -- error record types.
   --
   -- @example String, Error_Code, Exception_Information

   -- @param Copy_Value Custom copy function for Value_Type (optional)
   --
   -- Provide this function if Value_Type requires special copying behavior
   -- (e.g., deep copying for types with pointers, reference counting, etc.).
   -- For simple types like Integer, the default assignment copying is sufficient.
   --
   -- @example
   --   function Copy_File_Handle (Source : File_Handle) return File_Handle is
   --   begin
   --      return Duplicate_Handle (Source);  -- Custom duplication logic
   --   end Copy_File_Handle;
   with function Copy_Value (Source : Value_Type) return Value_Type is <>;

   -- @param Copy_Error Custom copy function for Error_Type (optional)
   --
   -- Provide this function if Error_Type requires special copying behavior.
   -- Most error types are simple and don't need custom copying.
   --
   -- @example
   --   function Copy_Error_Info (Source : Error_Info) return Error_Info is
   --   begin
   --      return (Code => Source.Code, Message => Clone (Source.Message));
   --   end Copy_Error_Info;
   with function Copy_Error (Source : Error_Type) return Error_Type is <>;

   -- @param Default_Value Default constructor for Value_Type (required)
   --
   -- This function creates a default/initial value for Value_Type.
   -- It's used for internal initialization and must not raise exceptions.
   --
   -- @example
   --   function Default_Int return Integer is (0);
   --   function Default_String return String is ("");
   --   function Default_Record return My_Record is (Field1 => 0, Field2 => False);
   with function Default_Value return Value_Type is <>;

   -- @param Default_Error Default constructor for Error_Type (required)
   --
   -- This function creates a default/initial error value for Error_Type.
   -- It's used for internal initialization and must not raise exceptions.
   --
   -- @example
   --   function Default_Error_Code return Error_Code is (Unknown_Error);
   --   function Default_Error_String return String is ("Unknown error");
   with function Default_Error return Error_Type is <>;
package Result is

   pragma Preelaborate (Result);

   use Ada.Strings.Unbounded;

   ----------------------------------------------------------------------------
   -- CORE TYPES AND CONSTANTS
   ----------------------------------------------------------------------------

   type Result_State is (Success, Error);
   -- @type Result_State
   --
   -- Represents the two possible states of a Result: Success or Error.
   -- This enumeration is used internally and returned by Get_State.
   --
   -- @values
   --   Success - The Result contains a valid value
   --   Error   - The Result contains an error

   type Result_Type is new Ada.Finalization.Controlled with private;
   -- @type Result_Type
   --
   -- The main Result type that encapsulates either a successful value or an error.
   -- This type is automatically managed (initialized, copied, finalized) through
   -- Ada's controlled type mechanism.
   --
   -- @characteristics
   --   * Controlled type - automatic resource management
   --   * Contains either Value_Type or Error_Type, never both
   --   * Thread-safe for individual instance access from single thread
   --   * Supports both functional and procedural programming styles
   --
   -- @size_estimate
   --   Approximately sizeof(Value_Type) + sizeof(Error_Type) + 48 bytes overhead

   ----------------------------------------------------------------------------
   -- CONSTRUCTION INTERFACE
   -- These procedures create Result instances containing either a value or an error
   ----------------------------------------------------------------------------

   -- @procedure Make_Ok
   --
   -- Creates a Result containing a successful value.
   --
   -- @param R The Result instance to initialize (out parameter)
   -- @param Value The successful value to store in the Result
   --
   -- @postcondition Is_Ok (R) = True
   -- @postcondition Unwrap (R) = Value
   --
   -- @example
   --   R : Result_Type;
   --   Make_Ok (R, 42);
   --   -- R now contains the value 42
   --
   -- @complexity O(1) plus cost of copying Value_Type
   -- @thread_safety Safe for different Result instances
   procedure Make_Ok (R : out Result_Type; Value : Value_Type)
     with Post => Is_Ok (R) and then Unwrap (R) = Value;
   
   -- @procedure Make_Err
   --
   -- Creates a Result containing an error without a descriptive message.
   --
   -- @param R The Result instance to initialize (out parameter)
   -- @param Error The error value to store in the Result
   --
   -- @postcondition Is_Error (R) = True
   -- @postcondition Unwrap_Err (R) = Error
   --
   -- @example
   --   R : Result_Type;
   --   Make_Err (R, Division_By_Zero);
   --   -- R now contains the error Division_By_Zero
   --
   -- @complexity O(1) plus cost of copying Error_Type
   -- @thread_safety Safe for different Result instances
   procedure Make_Err (R : out Result_Type; Error : Error_Type)
     with Post => Is_Error (R);
   
   -- @procedure Make_Err (with message)
   --
   -- Creates a Result containing an error with a descriptive message.
   --
   -- @param R The Result instance to initialize (out parameter)
   -- @param Error The error value to store in the Result
   -- @param Message Descriptive error message for debugging/logging
   --
   -- @postcondition Is_Error (R) = True
   -- @postcondition Unwrap_Err (R) = Error
   -- @postcondition Has_Message (R) = True
   --
   -- @example
   --   R : Result_Type;
   --   Make_Err (R, Division_By_Zero, "Cannot divide by zero in calculation");
   --   -- R now contains the error with a descriptive message
   --
   -- @complexity O(1) plus cost of copying Error_Type and Message
   -- @thread_safety Safe for different Result instances
   -- @note The message is stored as an Unbounded_String for flexibility
   procedure Make_Err (R : out Result_Type; Error : Error_Type; Message : String)
     with Post => Is_Error (R);

   ----------------------------------------------------------------------------
   -- STATE INSPECTION INTERFACE
   -- These functions check whether a Result contains a value or an error
   ----------------------------------------------------------------------------

   -- @function Is_Ok
   --
   -- Returns True if the Result contains a successful value.
   --
   -- @param R The Result instance to check
   -- @return True if R contains a value, False if it contains an error
   --
   -- @example
   --   if Is_Ok (My_Result) then
   --      -- Safe to call Unwrap or Unwrap_Into
   --      Value := Unwrap (My_Result);
   --   end if;
   --
   -- @complexity O(1)
   -- @thread_safety Safe for read-only access
   function Is_Ok (R : Result_Type) return Boolean;
   pragma Inline (Is_Ok);

   -- @function Is_Error
   --
   -- Returns True if the Result contains an error.
   --
   -- @param R The Result instance to check
   -- @return True if R contains an error, False if it contains a value
   --
   -- @example
   --   if Is_Error (My_Result) then
   --      -- Safe to call Unwrap_Err or handle error
   --      Error := Unwrap_Err (My_Result);
   --   end if;
   --
   -- @complexity O(1)
   -- @thread_safety Safe for read-only access
   -- @note Equivalent to "not Is_Ok (R)"
   function Is_Error (R : Result_Type) return Boolean;
   pragma Inline (Is_Error);

   -- @function Get_State
   --
   -- Returns the internal state enumeration (Success or Error).
   --
   -- @param R The Result instance to check
   -- @return Success if R contains a value, Error if it contains an error
   --
   -- @example
   --   case Get_State (My_Result) is
   --      when Success => Handle_Success;
   --      when Error   => Handle_Error;
   --   end case;
   --
   -- @complexity O(1)
   -- @thread_safety Safe for read-only access
   function Get_State (R : Result_Type) return Result_State;
   pragma Inline (Get_State);

   ----------------------------------------------------------------------------
   -- VALUE EXTRACTION INTERFACE
   -- These functions and procedures extract values from Result instances
   -- WARNING: Some of these will raise exceptions if used incorrectly
   ----------------------------------------------------------------------------

   -- @function Unwrap
   --
   -- Extracts the value from a successful Result.
   --
   -- @param R The Result instance containing a value
   -- @return The value stored in the Result
   --
   -- @precondition Is_Ok (R) = True
   -- @raises Unwrap_Error if R contains an error
   --
   -- @example
   --   if Is_Ok (My_Result) then
   --      Value := Unwrap (My_Result);  -- Safe
   --   end if;
   --
   -- @complexity O(1) plus cost of copying Value_Type
   -- @thread_safety Safe for read-only access
   -- @note For large types, consider using Unwrap_Into for better performance
   function Unwrap (R : Result_Type) return Value_Type
     with Pre => Is_Ok (R);
   
   -- @procedure Unwrap_Into
   --
   -- Extracts the value from a successful Result into an OUT parameter.
   -- This avoids copying for large types.
   --
   -- @param R The Result instance containing a value
   -- @param Value OUT parameter to receive the extracted value
   --
   -- @precondition Is_Ok (R) = True
   -- @raises Unwrap_Error if R contains an error
   --
   -- @example
   --   if Is_Ok (My_Result) then
   --      Unwrap_Into (My_Result, Value);  -- Zero-copy extraction
   --   end if;
   --
   -- @complexity O(1) - zero-copy operation
   -- @thread_safety Safe for read-only access
   -- @performance Preferred for large Value_Type instances
   procedure Unwrap_Into (R : Result_Type; Value : out Value_Type)
     with Pre => Is_Ok (R);
   
   -- @function Unwrap_Or
   --
   -- Extracts the value from a Result, or returns a default if it contains an error.
   --
   -- @param R The Result instance to extract from
   -- @param Default The value to return if R contains an error
   -- @return The value from R if successful, otherwise Default
   --
   -- @example
   --   Value := Unwrap_Or (My_Result, 0);  -- Returns 0 if error
   --
   -- @complexity O(1) plus cost of copying Value_Type
   -- @thread_safety Safe for read-only access
   -- @note Never raises exceptions - safe for all Result states
   function Unwrap_Or (R : Result_Type; Default : Value_Type) return Value_Type;
   
   -- @procedure Unwrap_Or_Into
   --
   -- Extracts the value from a Result into an OUT parameter, or uses default if error.
   --
   -- @param R The Result instance to extract from
   -- @param Default The value to use if R contains an error
   -- @param Value OUT parameter to receive the extracted value or default
   --
   -- @example
   --   Unwrap_Or_Into (My_Result, 0, Value);  -- Value gets result or 0
   --
   -- @complexity O(1) - zero-copy operation
   -- @thread_safety Safe for read-only access
   -- @performance Preferred for large Value_Type instances
   -- @note Never raises exceptions - safe for all Result states
   procedure Unwrap_Or_Into (R : Result_Type; Default : Value_Type; Value : out Value_Type);
   
   -- @function Expect
   --
   -- Extracts the value from a successful Result with a custom error message.
   --
   -- @param R The Result instance containing a value
   -- @param Message Custom error message to use if R contains an error
   -- @return The value stored in the Result
   --
   -- @precondition Is_Ok (R) = True
   -- @raises Expect_Error with the custom message if R contains an error
   --
   -- @example
   --   Value := Expect (My_Result, "Expected valid calculation result");
   --
   -- @complexity O(1) plus cost of copying Value_Type
   -- @thread_safety Safe for read-only access
   -- @note Useful for providing context-specific error messages
   function Expect (R : Result_Type; Message : String) return Value_Type
     with Pre => Is_Ok (R);
   
   -- @procedure Expect_Into
   --
   -- Extracts the value from a successful Result into an OUT parameter with custom error message.
   --
   -- @param R The Result instance containing a value
   -- @param Message Custom error message to use if R contains an error
   -- @param Value OUT parameter to receive the extracted value
   --
   -- @precondition Is_Ok (R) = True
   -- @raises Expect_Error with the custom message if R contains an error
   --
   -- @example
   --   if Is_Ok (My_Result) then
   --      Expect_Into (My_Result, "Expected valid result", Value);
   --   end if;
   --
   -- @complexity O(1) - zero-copy operation
   -- @thread_safety Safe for read-only access
   -- @performance Preferred for large Value_Type instances
   procedure Expect_Into (R : Result_Type; Message : String; Value : out Value_Type)
     with Pre => Is_Ok (R);
   
   -- @function Unwrap_Err
   --
   -- Extracts the error from a failed Result.
   --
   -- @param R The Result instance containing an error
   -- @return The error stored in the Result
   --
   -- @precondition Is_Error (R) = True
   -- @raises Unwrap_Error if R contains a value
   --
   -- @example
   --   if Is_Error (My_Result) then
   --      Error := Unwrap_Err (My_Result);  -- Safe
   --   end if;
   --
   -- @complexity O(1) plus cost of copying Error_Type
   -- @thread_safety Safe for read-only access
   -- @note Complement to Unwrap - extracts error instead of value
   function Unwrap_Err (R : Result_Type) return Error_Type
     with Pre => Is_Error (R);
   
   -- @procedure Unwrap_Err_Into
   --
   -- Extracts the error from a failed Result into an OUT parameter.
   --
   -- @param R The Result instance containing an error
   -- @param Error OUT parameter to receive the extracted error
   --
   -- @precondition Is_Error (R) = True
   -- @raises Unwrap_Error if R contains a value
   --
   -- @example
   --   if Is_Error (My_Result) then
   --      Unwrap_Err_Into (My_Result, Error);  -- Zero-copy extraction
   --   end if;
   --
   -- @complexity O(1) - zero-copy operation
   -- @thread_safety Safe for read-only access
   -- @performance Preferred for large Error_Type instances
   procedure Unwrap_Err_Into (R : Result_Type; Error : out Error_Type)
     with Pre => Is_Error (R);
   
   -- @function Expect_Err
   --
   -- Extracts the error from a failed Result with a custom message.
   --
   -- @param R The Result instance containing an error
   -- @param Message Custom error message to use if R contains a value
   -- @return The error stored in the Result
   --
   -- @precondition Is_Error (R) = True
   -- @raises Expect_Error with the custom message if R contains a value
   --
   -- @example
   --   Error := Expect_Err (My_Result, "Expected error but got success");
   --
   -- @complexity O(1) plus cost of copying Error_Type
   -- @thread_safety Safe for read-only access
   -- @note Useful for cases where you expect an error but want custom messaging
   function Expect_Err (R : Result_Type; Message : String) return Error_Type
     with Pre => Is_Error (R);

   ----------------------------------------------------------------------------
   -- TRANSFORMATION OPERATIONS
   -- These generic packages provide ways to transform Result values
   ----------------------------------------------------------------------------

   -- @generic_package Map_Operations
   --
   -- Transforms the success value if present, leaves errors unchanged.
   -- This implements the "map" operation from functional programming.
   --
   -- @generic_param New_Value_Type The type of the transformed value
   -- @generic_param Transform Procedure to transform Value_Type to New_Value_Type
   --
   -- @description
   --   If the input Result contains a value, Transform is applied to create a new
   --   Result containing the transformed value. If the input contains an error,
   --   the error is copied unchanged to the output Result.
   --
   -- @example
   --   procedure Double (Input : Integer; Output : out Integer) is
   --   begin
   --      Output := Input * 2;
   --   end Double;
   --   
   --   package Double_Map is new Int_Result.Map_Operations (Integer, Double);
   --   
   --   Double_Map.Map (Input_Result, Output_Result);
   --   -- If Input_Result contains 5, Output_Result will contain 10
   --   -- If Input_Result contains error, Output_Result will contain the same error
   --
   -- @complexity O(1) plus cost of Transform procedure
   -- @thread_safety Safe for different Result instances
   generic
      type New_Value_Type is private;
      -- @param New_Value_Type The type of the transformed value
      
      with procedure Transform (Input : Value_Type; Output : out New_Value_Type);
      -- @param Transform Procedure to convert Value_Type to New_Value_Type
      -- Must not raise exceptions for valid inputs
   package Map_Operations is
      type New_Result_Type is new Ada.Finalization.Controlled with private;
      -- @type New_Result_Type Result type containing New_Value_Type

      -- @procedure Map
      -- Applies the transformation to a successful Result
      -- @param R Input Result to transform
      -- @param New_R Output Result containing transformed value or original error
      procedure Map (R : Result_Type; New_R : out New_Result_Type);
      
      -- Construction operations for New_Result_Type
      procedure Make_Ok (R : out New_Result_Type; Value : New_Value_Type);
      procedure Make_Err (R : out New_Result_Type; Error : Error_Type);
      procedure Make_Err (R : out New_Result_Type; Error : Error_Type; Message : String);
      
      -- Inspection operations for New_Result_Type
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

   -- @generic_package And_Then_Operations
   --
   -- Chains operations that might fail. This implements monadic bind operation.
   --
   -- @generic_param Transform Procedure that takes a value and returns a Result
   --
   -- @description
   --   If the input Result contains a value, Transform is applied to create a new
   --   Result. If the input contains an error, the error is copied unchanged.
   --   This allows chaining operations where each step might fail.
   --
   -- @example
   --   procedure Validate_Positive (Input : Integer; Output : out Int_Result.Result_Type) is
   --   begin
   --      if Input > 0 then
   --         Int_Result.Make_Ok (Output, Input);
   --      else
   --         Int_Result.Make_Err (Output, Invalid_Input, "Must be positive");
   --      end if;
   --   end Validate_Positive;
   --   
   --   package Validate is new Int_Result.And_Then_Operations (Validate_Positive);
   --   
   --   Validate.And_Then (Input_Result, Output_Result);
   --   -- Validation only applied if Input_Result contains a value
   --
   -- @complexity O(1) plus cost of Transform procedure
   -- @thread_safety Safe for different Result instances  
   generic
      with procedure Transform (Input : Value_Type; Output : out Result_Type);
      -- @param Transform Procedure that converts Value_Type to Result_Type
      -- Should handle all valid inputs without raising exceptions
   package And_Then_Operations is
      -- @procedure And_Then
      -- Chains a fallible operation onto a Result
      -- @param R Input Result to chain operation onto
      -- @param New_R Output Result from chained operation or original error
      procedure And_Then (R : Result_Type; New_R : out Result_Type);
   end And_Then_Operations;

   -- @generic_package Map_Error_Operations
   --
   -- Transforms error values while leaving success unchanged.
   -- This is the dual of Map_Operations, operating on errors instead of values.
   --
   -- @generic_param Transform Procedure to transform Error_Type to Error_Type
   --
   -- @description
   --   If the input Result contains an error, Transform is applied to create a new
   --   Result containing the transformed error. If the input contains a value,
   --   the value is copied unchanged to the output Result.
   --
   -- @example
   --   procedure Add_Context (Input : String; Output : out String) is
   --   begin
   --      Output := "Database Error: " & Input;
   --   end Add_Context;
   --   
   --   package Error_Context is new DB_Result.Map_Error_Operations (Add_Context);
   --   
   --   Error_Context.Map_Err (Input_Result, Output_Result);
   --   -- Adds context to error messages while preserving successful values
   --
   -- @complexity O(1) plus cost of Transform procedure
   -- @thread_safety Safe for different Result instances
   generic
      with procedure Transform (Input : Error_Type; Output : out Error_Type);
      -- @param Transform Procedure to convert Error_Type to Error_Type
      -- Must not raise exceptions for valid inputs
   package Map_Error_Operations is
      -- @procedure Map_Err
      -- Applies the transformation to an error Result
      -- @param R Input Result to transform
      -- @param New_R Output Result containing transformed error or original value
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
