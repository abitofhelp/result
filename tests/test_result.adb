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

-- Simple test program to verify the Result library works
with Ada.Text_IO; use Ada.Text_IO;
with Result;

procedure Test_Result is
   
   -- Helper functions for generic instantiation
   function Copy_Integer (Source : Integer) return Integer is (Source);
   function Default_Integer return Integer is (0);
   function Default_Error_Integer return Integer is (-1);
   
   -- Test instantiation
   package Integer_Result is new Result
     (Value_Type => Integer,
      Error_Type => Integer,
      Copy_Value => Copy_Integer,
      Copy_Error => Copy_Integer,
      Default_Value => Default_Integer,
      Default_Error => Default_Error_Integer);

   R : Integer_Result.Result_Type;
   Value : Integer;
   
begin
   Put_Line ("Testing Result Library - Basic Functionality");
   
   -- Test Make_Ok
   Integer_Result.Make_Ok (R, 42);
   if Integer_Result.Is_Ok (R) then
      Put_Line ("PASS: Make_Ok works correctly");
   else
      Put_Line ("FAIL: Make_Ok failed");
   end if;
   
   -- Test Unwrap
   Value := Integer_Result.Unwrap (R);
   if Value = 42 then
      Put_Line ("PASS: Unwrap works correctly");
   else
      Put_Line ("FAIL: Unwrap failed");
   end if;
   
   -- Test Make_Err
   Integer_Result.Make_Err (R, 99);
   if Integer_Result.Is_Error (R) then
      Put_Line ("PASS: Make_Err works correctly");
   else
      Put_Line ("FAIL: Make_Err failed");
   end if;
   
   Put_Line ("Basic tests completed successfully!");
   
end Test_Result;