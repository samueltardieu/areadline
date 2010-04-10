with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Readline.Variables is

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

end Readline.Variables;
