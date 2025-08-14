with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;

procedure Day1 is
    use Ada.Text_IO;

    package Integer_IO is
      new Ada.Text_IO.Integer_IO(Integer);
    use Integer_IO;

    F_Type    : File_Type;
    File_Name : Unbounded_String;

    package Integer_Vectors is new Ada.Containers.Vectors
       (Index_Type => Natural, Element_Type => Integer);

    package Integer_Vectors_Sorting is new Integer_Vectors.Generic_Sorting;

    use Integer_Vectors;
    use Integer_Vectors_Sorting;

    Left_Vector  : Vector;
    Right_Vector : Vector;

    Result : Integer := 0;
    Result2 : Integer := 0;
begin
    if Ada.Command_Line.Argument_Count = 0 then
      Put("Enter the name of the file: ");
      File_Name := To_Unbounded_String(Get_Line);
    else
      File_Name := To_Unbounded_String(Ada.Command_Line.Argument(1));
    end if;

    Ada.Text_IO.Open(F_Type, In_File, To_String(File_Name));

    declare
        Number  : Integer := 0;
        Counter : Natural := 0;
    begin
        while not End_Of_File(F_Type) loop
            Integer_IO.Get(F_Type, Number);
            if (Counter mod 2) = 0 then
                Left_Vector.Append(Number);
            else
                Right_Vector.Append(Number);
            end if;

            Counter := Counter + 1;
        end loop;
    end;

    Close(F_Type);

    Sort(Left_Vector);
    Sort(Right_Vector);

    declare
        Difference : Integer;
    begin
        for I in Left_Vector.First_Index .. Left_Vector.Last_Index loop
            Difference := abs(Left_Vector(I) - Right_Vector(I));
            Result     := Result + Difference;
        end loop;
    end;

    Put_Line(Result'Image);

    for Left_Element of Left_Vector loop
      declare
        Occurrance_Count : Integer := 0;
      begin
        for Right_Element of Right_Vector loop
          if Left_Element = Right_Element then
            Occurrance_Count := Occurrance_Count + 1;
          end if;
        end loop;
        Result2 := Result2 + (Left_Element * Occurrance_Count);
      end;
    end loop;

    Put_Line(Result2'Image);

exception
    when Name_Error =>
        Put_Line ("File: '" & To_String(File_Name) & "' does not exist");
    when others     =>
        Put_Line
           ("Encountered unknown error while processing file: " & To_String(File_Name));
end Day1;
