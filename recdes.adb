-- Jared Dominguez
--
-- This is a recursive descent compiler for the following grammar:
-- <program>     ->  begin <stmt_list> end
-- <stmt>        ->  <id> := <expr> | ε
-- <stmt_list>   ->  <stmt_list> ; <stmt> | <stmt>
-- <expr>        ->  <expr> + <term> | <expr> - <term> |  <term>
-- <term>        ->  <term> * <factor> | <term> div <factor> | <term> mod <factor> |  <factor>
-- <factor>      ->  <primary> ^ <factor> | <primary>
-- <primary>     ->  <id> | <num> | ( <expr> )
--
-- Check out all my error messages, they're pretty descriptive for most syntax errors.
with
   Ada.Text_IO,
   Ada.Command_Line,
   Ada.Strings.Unbounded,
   Ada.Characters.Handling,
   Ada.Characters.Latin_1,
   Ada.Exceptions;
use
   Ada.Text_IO,
   Ada.Command_Line,
   Ada.Characters.Handling,
   Ada.Exceptions;

procedure recdes is
   Malformed_Expression,
   Unexpected_Symbol, Unexpected_Keyword,
   Failed_Match, Failed_Match_Begin, Failed_Match_End, Failed_Match_Semi : exception;

   procedure Getc(c : in out Character); 
   procedure Ungetc;
   procedure error (message : in String); 
   procedure openDataFile; 
   procedure scan; 
   procedure match(token : in String); 

   function strcmp(Str1 : String; Str2 : String) return Boolean;
   function isKeyword(token : in String) return Boolean; 
   function identifier(token : in String) return Boolean; 
   function digit(c : in Character) return Boolean;
   function number(token : in String) return Boolean; 

   procedure primary;
   procedure factor;
   procedure term;
   procedure expr;
   procedure stmt; 
   procedure stmtList;
   procedure program; 
   procedure parse;

   File : File_Type;
   LookAhead : String (1..80) := (others => Character'Val(0));

   procedure Getc(c : in out Character) is
   begin
      Get (File, c);
   end Getc;

   --Using this because Ada.Text_IO doesn't have an unget, and using its
   --Look_Ahead function would be just a little more messy.
   procedure Ungetc is
      prevLine, prevCol : Positive_Count := 1;
   begin
      prevLine := Line(File);
      prevCol := Col(File);
      Reset(File);

      if(prevCol = 1) and (prevLine > 1) 
      then
         Set_Line(File, prevLine - 1);
         Set_Col(Line_Length(File));
      elsif(prevCol > 1)
      then
         Set_Line(File, prevLine);
         Set_Col(File, prevCol - 1);
      end if;
   end Ungetc;

   --Using this since strings in Ada compare false if their sizes 
   --are different, even when terminated with a null character.
   function strcmp(Str1 : String; Str2 : String) return Boolean is
   begin
      for i in Str1'Range loop
         if (Str1(i) = Character'Val(0)) and (Str2(i) = Character'Val(0))
         then
            exit;
         end if;

         if (Str1(i) /= Str2(i))
         then 
            return false;
         end if;
      end loop;

      return true;
   exception
      when CONSTRAINT_ERROR =>
         return strcmp(Str2, Str1);
   end strcmp;

   procedure error (message : in String) is
   begin
      Put_Line(message);
   end error;

   procedure openDataFile is
      package SU renames Ada.Strings.Unbounded;
      use SU;

      Filename       : SU.Unbounded_String := SU.Null_Unbounded_String;
      StdInputItem   : String (1 .. 80);
      Last           : Natural;
   begin
      -- Get filename from either terminal args or standard input
      if (Argument_Count > 0)
      then
         Filename := SU.To_Unbounded_String(Argument(1));
      else
         Put_Line ("Enter an input file:");
         Get_Line(StdInputItem, Last);
         Filename := SU.To_Unbounded_String(StdInputItem);
      end if;

      -- Open the file with the given filename
      Open (File => File,
            Mode => In_File,
            Name => SU.To_String(Filename));
   end openDataFile;

   procedure scan is
      ch : Character := Character'Val(0);
      i : Integer := 1;
      startingLine : Positive_Count;
   begin
      LookAhead := (others => Character'Val(0));

      -- Read and skip whitespace
      loop
         Getc (ch);
         exit when ((ch /= ' ') and (ch /= Ada.Characters.Latin_1.HT));
      end loop;

      case ch is
         when '+' | '-' | '*' | '(' | ')' | '^' | ';' => 
            LookAhead(LookAhead'First) := ch;
            LookAhead(LookAhead'First+1) := Character'Val(0);

         -- Check if we have a := operator. Special case since it's the only one that's neither
         -- a keyword nor a single character.
         when ':' =>
            LookAhead(LookAhead'First) := ch;
            Getc (ch);
            
            if (ch = '=')
            then
               LookAhead(LookAhead'First+1) := ch;
               LookAhead(LookAhead'First+2) := Character'Val(0);
            -- If not, Unget the character that was just scanned and probably come back with an error
            else
               LookAhead(LookAhead'First+1) := Character'Val(0);
               Ungetc;
            end if;
         when others =>
            startingLine := Line(File);
            loop
               LookAhead (i) := ch;
               i := i + 1;
               Getc (ch);

               exit when ((not ((To_Upper(ch) >= 'A' and To_Upper(ch) <= 'Z') or digit(ch))) or (startingLine < Line(File))); 
            end loop;
            LookAhead(i) := Character'Val(0);
            Ungetc;
      end case;
   exception
      when END_ERROR => null;
   end scan;

   procedure match(token : in String) is
   begin
      if (strcmp(token, LookAhead))
      then 
         scan;
      else
         if (strcmp(token, "begin"))
         then
            raise Failed_Match_Begin;
         elsif (strcmp(token, "end"))
         then
            if (identifier(LookAhead))
            then
               raise Failed_Match_Semi;
            elsif(LookAhead(LookAhead'First) = Character'Val(0))
            then
               raise Failed_Match_End;
            elsif(isKeyword(LookAhead))
            then
               raise Unexpected_Keyword;
            else
               raise Unexpected_Symbol;
            end if;
         else
            Raise_Exception( Failed_Match'Identity, token);
         end if;
      end if;
   end match;

   function isKeyword(token : in String) return Boolean is
   begin
      return
         -- In a perfect world, these would be searched through a B tree,
         -- but there are only four of them so it's okay.
         (strcmp("end", token)) or
         (strcmp("begin", token)) or
         (strcmp("mod", token)) or
         (strcmp("div", token));
   exception
      when others =>
         return false;
   end isKeyword;

   function identifier(token : in String) return Boolean is
   begin
      return (
         (((token(token'First) >= 'A') and (token(token'First) <= 'Z')) or 
         ((token(token'First) >= 'a') and (token(token'First) <= 'z'))) and not
         isKeyword(token));
   end identifier;

   function digit(c : in Character) return Boolean is
   begin
      case c is
         when '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
            return true;
         when others =>
            return false;
      end case;
   end digit;

   function number(token : in String) return Boolean is
   begin
      for i in token'Range loop
         if (not digit(token(i))) and (not (token(i) = Character'Val(0)))
         then
            return false;
         end if;
         if token(i) = Character'Val(0)
         then
            exit;
         end if;
      end loop;
      return true;
   end number;

   procedure primary is
   begin
      if (identifier(LookAhead))
      then
         Put_Line ("RVALUE " & LookAhead);
         match(LookAhead);
      elsif (number(LookAhead))
      then
         Put_Line ("PUSH " & LookAhead);
         match(LookAhead);
      else
         if (LookAhead(LookAhead'First) = '(')
         then
            match("(");
            expr;
            match(")");
         else
            raise Malformed_Expression;
         end if;
      end if;
   end primary;

   procedure factor is
   begin
      primary;

      while (LookAhead(LookAhead'First) = '^') loop
         match(LookAhead);
         factor;
         Put_Line("POW");
      end loop;
   end factor;

   procedure term is
      -- LookAhead is checked to see if it contains "*", "div" or "mod" operator,
      -- and the result is stored before scanning again.
      isMpy : Boolean;
      isDiv : Boolean;
   begin
      factor;

      while 
         ((LookAhead(LookAhead'First) = '*') or 
         (LookAhead(LookAhead'First..(LookAhead'First+2)) = "div") or
         (LookAhead(LookAhead'First..(LookAhead'First+2)) = "mod"))
      loop
         isMpy := strcmp("*", LookAhead);
         isDiv := strcmp("div", LookAhead);
         match(LookAhead);
         factor;
         if (isMpy)
         then
            Put_Line("MPY");
         elsif (isDiv)
         then
            Put_Line("DIV");
         else
            Put_Line("MOD");
         end if;
      end loop;
   end term;

   procedure expr is
      -- LookAhead is checked to see if it contains a plus or minus operator,
      -- and the result is stored before scanning again.
      isPlus : Boolean;
   begin
      term;

      while ((LookAhead(LookAhead'First) = '+') or (LookAhead(LookAhead'First) = '-')) loop
         isPlus := strcmp("+", LookAhead);
         match(LookAhead);
         term;
         if (isPlus)
         then
            Put_Line("ADD");
         else
            Put_Line("SUB");
         end if;
      end loop;
   end expr;

   procedure stmt is
      tmp : String(1..80);
   begin
      -- If LookAhead contains an identifier, then an entire statement must be evaluated.
      -- Else, the statement is empty
      if (identifier(LookAhead))
      then
         -- In correct code, the identifier in LookAhead will be followed by [ := <expr> ],
         -- but it's stored in a temporary array beforehand just in case of syntax errors.
         tmp := LookAhead;
         match(LookAhead);
         match(":=");
         Put_Line("LVALUE " & tmp);
         expr;
         Put_Line("STO");
      end if;
   end;

   procedure stmtList is
   begin
      stmt;

      while (LookAhead(LookAhead'First) = ';') loop
         match(LookAhead);
         stmtList;
      end loop;
   end stmtList;

   procedure program is
   begin
      match("begin");
      stmtList;
      match("end");
      Put_Line("HALT");
      Put_Line("Compilation completed");
   exception
      -- Syntax errors raise exceptions that are handled as compilation errors
      when Unexpected_Keyword =>
         error("Syntax Error: Unexpected keyword found on line"& Positive_Count'Image(Line(File)) &": " & "'" & LookAhead & "'");
         error("Compilation failed");
      when Unexpected_Symbol =>
         error("Syntax Error: Unexpected symbol found on line"& Positive_Count'Image(Line(File)) &": " & "'" & LookAhead & "'");
         error("Compilation failed");
      when Malformed_Expression =>
         error("Syntax Error: Malformed expression on line"& Positive_Count'Image(Line(File)) &", expected identifier before '"&LookAhead&"'");
         error("Compilation failed");
      when Failed_Match_Semi =>
         error("Syntax Error: Semicolon expected on line"& Positive_Count'Image(Line(File)));
         error("Compilation failed");
      when Failed_Match_Begin =>
         error("Syntax Error: 'Begin' expected on line"& Positive_Count'Image(Line(File)) &", found: " & "'" & LookAhead & "'");
         error("Compilation failed");
      when Failed_Match_End =>
         error("Syntax Error: 'End' expected on line"& Positive_Count'Image(Line(File)));
         error("Compilation failed");
      when Failure : Failed_Match =>
         error("Syntax Error: "&Exception_Message(Failure)&" expected on line " &Positive_Count'Image(Line(File))&", found: " & "'" & LookAhead & "'");
         error("Compilation failed");
   end program;

   procedure parse is
   begin
      scan;
      program;
   end parse;

begin
   openDataFile;
   parse;

   Close (File);
end recdes;

