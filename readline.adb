with Ada.Characters.Handling;
with Ada.Containers.Vectors;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;       use Interfaces.C, Interfaces.C.Strings;

package body Readline is

   pragma Linker_Options ("-lreadline");
   pragma Linker_Options ("-lncurses");

   type rl_compentry_func_t is
      access function (Text : chars_ptr; State : int) return chars_ptr;
   pragma Convention (C, rl_compentry_func_t);

   rl_completion_entry_function : rl_compentry_func_t;
   pragma Import (C, rl_completion_entry_function,
      "rl_completion_entry_function");

   type String_Access is access String;
   package String_Arrays is
      new Ada.Containers.Vectors (Positive, String_Access);
   use String_Arrays;

   List        : Vector;
   List_Cursor : Cursor;

   Case_Sensitive : Boolean := True;

   function Completer (Text : chars_ptr; State : int)
         return chars_ptr;
   pragma Convention (C, Completer);

   --------------
   -- Add_Word --
   --------------

   procedure Add_Word (Word : String) is
   begin
      Append (List, new String'(Word));
   end Add_Word;

   ---------------------
   -- Clear_All_Words --
   ---------------------

   procedure Clear_All_Words is
      procedure Delete is
         new Ada.Unchecked_Deallocation (String, String_Access);
   begin
      for I in First_Index (List) .. Last_Index (List) loop
         declare
            Current : String_Access := Element (List, I);
         begin
            Delete (Current);
         end;
      end loop;
      Clear (List);
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
         List_Cursor := First (List);
         Case_Sensitive := Variable_Value ("completion-ignore-case") = "off";
      end if;

      while List_Cursor /= No_Element loop
         declare
            Current : String renames Element (List_Cursor).all;
         begin
            Next (List_Cursor);
            if Starts_With (Current, T, Case_Sensitive) then
               return New_String (Current);
            end if;
         end;
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
