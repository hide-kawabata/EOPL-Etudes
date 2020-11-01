# Grammar
    Grammar X4S = <ProdRules, InitSym>

    ProdRules = {
        L -> C L | eps
        C -> print E ; | def id E ;
        E -> id | num | + | -
    }
    
    InitSym = L
    
    Terminals = { ;, print, def, id, num, +, - }

# Examples of programs in X4S
- 
``
    def x 1; print + x 1;
``
- 
``
    def x 1; def y x 2; print + x y;
``
- 
``def x 1; print y;`` (semantically illegal)

# Usage
    > echo 'def x 1; print + x 1;' | cargo run |indent -i4 -di0
    ...
    #include <stdio.h>
    int main()
    {
        int x =
        (1);
        printf("%d",
               (
                (x)
                ) + (
                     (1)
                     )
            );
        return 0;
    }

# Current status
- Checks syntax (LL(1) recursive descent parsing)
- Checks if references to undefined ids exist
- Outputs a C program
