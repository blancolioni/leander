private package Leander.Syntax.Bindings.Guards is

   procedure Lower
     (Name      : Leander.Names.Leander_Name;
      Equations : in out Binding_Record_Lists.List);
   --  Bind the free fallthrough variable that Leander.Parser.Expressions
   --  left in each guarded equation, so that a failed guard continues at
   --  the next equation the way Haskell says it should.
   --
   --  This has to happen here, before Transform.To_Alts, because the alts
   --  compiler cannot express it.  It files one alternative per constructor
   --  slot, first writer wins, which is correct only while guards do not
   --  exist: without them a later equation naming a constructor an earlier
   --  one already named is unreachable.  And the Scott encoding offers no
   --  way to repair it afterwards -- dispatch is "v E1 .. EN", and applying
   --  the scrutinee to the continuations *is* the commit, so a branch that
   --  has been entered cannot hand control back.  The only thing a failed
   --  guard can do is call something that was already in scope before the
   --  dispatch, which is what this pass arranges.

end Leander.Syntax.Bindings.Guards;
