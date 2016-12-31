with Ada.Text_IO;

with SK.Machine;

with Leander.Environments;

with Leander.Kinds.Trees;

with Leander.Types.Trees;

with Leander.Core.Trees;
with Leander.Parser.Modules;
with Leander.Syntax;

with Leander.Repl;

with Leander.Paths;

procedure Leander.Driver is
   Type_Con_0  : constant Leander.Kinds.Trees.Tree_Type :=
                   Leander.Kinds.Trees.Leaf
                     (Leander.Kinds.Primitive);
   Type_Con_1  : constant Leander.Kinds.Trees.Tree_Type :=
                   Leander.Kinds.Trees.Apply
                     (Leander.Kinds.Trees.Apply
                        (Leander.Kinds.Map,
                         Type_Con_0),
                      Type_Con_0);
   Type_Con_2  : constant Leander.Kinds.Trees.Tree_Type :=
                   Leander.Kinds.Trees.Apply
                     (Leander.Kinds.Trees.Apply
                        (Leander.Kinds.Map,
                         Type_Con_0),
                      Type_Con_1);

   List_A      : constant Leander.Types.Trees.Tree_Type :=
                   Leander.Types.Trees.Apply
                     (Leander.Types.Constructor ("[]", Type_Con_1),
                      Leander.Types.Variable ("a", Type_Con_0));
   List_B      : constant Leander.Types.Trees.Tree_Type :=
                   Leander.Types.Trees.Apply
                     (Leander.Types.Constructor ("[]", Type_Con_1),
                      Leander.Types.Variable ("b", Type_Con_0));
   A_To_B      : constant Leander.Types.Trees.Tree_Type :=
                   Leander.Types.Trees.Apply
                     (Leander.Types.Trees.Apply
                        (Leander.Types.Constructor ("->", Type_Con_2),
                         Leander.Types.Variable ("a", Type_Con_0)),
                      Leander.Types.Variable ("b", Type_Con_0));
   List_A_To_B : constant Leander.Types.Trees.Tree_Type :=
                   Leander.Types.Trees.Apply
                     (Leander.Types.Trees.Apply
                        (Leander.Types.Constructor ("->", Type_Con_2),
                         List_A),
                      Leander.Types.Variable ("b", Type_Con_0));
   A_To_List_B : constant Leander.Types.Trees.Tree_Type :=
                   Leander.Types.Trees.Apply
                     (Leander.Types.Trees.Apply
                        (Leander.Types.Constructor ("->", Type_Con_2),
                         Leander.Types.Variable ("a", Type_Con_0)),
                      List_B);

begin
   Ada.Text_IO.Put_Line ("*       " & Leander.Kinds.Trees.Show (Type_Con_0));
   Ada.Text_IO.Put_Line ("*->*    " & Leander.Kinds.Trees.Show (Type_Con_1));
   Ada.Text_IO.Put_Line ("*->*->* " & Leander.Kinds.Trees.Show (Type_Con_2));
   Ada.Text_IO.Put_Line ("[a]     " & Leander.Types.Trees.Show (List_A));
   Ada.Text_IO.Put_Line ("a->b    " & Leander.Types.Trees.Show (A_To_B));
   Ada.Text_IO.Put_Line ("[a]->b  " & Leander.Types.Trees.Show (List_A_To_B));
   Ada.Text_IO.Put_Line ("a->[b]  " & Leander.Types.Trees.Show (A_To_List_B));

   declare
      Env : Leander.Environments.Environment :=
              Leander.Parser.Modules.Load_Module
                ("Prelude",
                 Leander.Paths.Config_File
                   ("libraries/Prelude.hs"));
      Machine : constant SK.Machine.SK_Machine :=
                  SK.Machine.Create_Machine (65536);
   begin
      Env.Compile (Machine);
      Leander.Repl.Start_Repl (Env);
   end;

end Leander.Driver;
