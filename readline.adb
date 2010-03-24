with Ada.Characters.Handling;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;       use Interfaces.C, Interfaces.C.Strings;

package body Readline is

   pragma Linker_Options ("-lreadline");
   pragma Linker_Options ("-lncurses");

   type rl_compentry_func_t is
      access function (Text : chars_ptr; State : int) return chars_ptr;

   rl_completion_entry_function : rl_compentry_func_t;
   pragma Import (C, rl_completion_entry_function,
      "rl_completion_entry_function");

   type String_Access is access String;
   List_Index     : Integer := 0;
   List_Size      : Integer := 0;
   List           : array (1 .. 1024) of String_Access;
   Case_Sensitive : Boolean := True;

   Too_Many_Words : exception;

   function Completer (Text : chars_ptr; State : int)
         return chars_ptr;

   --------------
   -- Add_Word --
   --------------

   procedure Add_Word (Word : String) is
   begin
      List_Size := List_Size + 1;
      if List_Size > List'Length then
         raise Too_Many_Words;
      end if;
      List (List_Size) := new String'(Word);
   end Add_Word;

   ---------------------
   -- Clear_All_Words --
   ---------------------

   procedure Clear_All_Words is
      procedure Delete is
         new Ada.Unchecked_Deallocation (String, String_Access);
   begin
      for I in 1 .. List_Size loop
         if List (I) /= null then
            Delete (List (I));
         end if;
         List (I) := null;
      end loop;
      List_Size := 0;
   end Clear_All_Words;

   ---------------
   -- Completer --
   ---------------

   function Completer (Text : chars_ptr; State : int)
         return chars_ptr is

      function Starts_With (Text : String; Start : String;
                            Case_Sensitive : Boolean) return Boolean;

      function To_Lower (C : Character) return Character renames
            Ada.Characters.Handling.To_Lower;

      function To_Lower (Str : String) return String;

      T : constant String := Value (Text);

      --  To_Lower - translates all alphabetic, uppercase characters
      --  in Str to lowercase
      function To_Lower (Str : String) return String is
         Result : String (Str'Range);
      begin
         for C in  Str'Range loop
            Result (C) := To_Lower (Str (C));
         end loop;
         return Result;
      end To_Lower;

      --  Starts_With - returns true if Text starts with Start
      function Starts_With (Text : String; Start : String;
                            Case_Sensitive : Boolean)
            return Boolean is
      begin
         if Text'Length < Start'Length then
            return False;
         end if;
         if Case_Sensitive then
            return Text (Text'First .. Text'First + Start'Length - 1) = Start;
         else
            declare
               LText : constant String := To_Lower (Text);
            begin
               return LText (LText'First .. LText'First + Start'Length - 1)
                  = To_Lower (Start);
            end;
         end if;
      end Starts_With;

   begin
      if State = 0 then
         List_Index := 0;
         Case_Sensitive := Variable_Value ("completion-ignore-case") = "off";
      end if;
      while List_Index < List_Size loop
         List_Index := List_Index + 1;
         if Starts_With (List (List_Index).all, T, Case_Sensitive) then
            return New_String (List (List_Index).all);
         end if;
      end loop;
      return Null_Ptr;
   end Completer;

   ---------------
   -- Read_Line --
   ---------------

   function Read_Line (Prompt : String := "") return String is

      function Readline (Prompt : char_array) return chars_ptr;
      pragma Import (C, Readline, "readline");

      procedure Add_History (Line : chars_ptr);
      pragma Import (C, Add_History, "add_history");

      C_Line   : chars_ptr;

   begin
      C_Line := Readline (To_C (Prompt));

      if C_Line = Null_Ptr then
         raise End_Error;
      end if;

      declare
         Result : constant String := Value (C_Line);
      begin
         Add_History (C_Line);
         Free (C_Line);
         return Result;
      end;
   end Read_Line;

   -------------------
   -- Variable_Bind --
   -------------------

   procedure Variable_Bind (Name : String; Value : String) is

      procedure rl_variable_bind (variable : char_array;
                                  value    : char_array);
      pragma Import (C, rl_variable_bind, "rl_variable_bind");

   begin
      rl_variable_bind (To_C (Name), To_C (Value));
   end Variable_Bind;

   --------------------
   -- Variable_Value --
   --------------------

   function Variable_Value (Name : String) return String is

      function rl_variable_value (variable : char_array) return chars_ptr;
      pragma Import (C, rl_variable_value, "rl_variable_value");

   begin
      return Value (rl_variable_value (To_C (Name)));
   end Variable_Value;

begin
   rl_completion_entry_function := Completer'Access;
end Readline;
