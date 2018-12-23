(*
type op_t = Nop | Hlt |
          Ba | Bnz | Bz | Bn |
          Sra | Sla |
          Ld | St |
          Add | Sub | Mul | Div |
          Cmp |
          And | Or | Not |
          Call | Ret
 *)
type reg_t = R of int
type num_t = N of int
type lab_t = L of string
type addr_t = A of int

type opr_t = RR of (reg_t * reg_t) |
             RImm of (reg_t * num_t) |
             RImm2 of (reg_t * lab_t) |
             RDir of (reg_t * lab_t) |
             RDis of (reg_t * reg_t * lab_t)

type op_t = Nop | Hlt |
            Out of reg_t | In of reg_t |
            Ba of lab_t | Bnz of lab_t | Bz of lab_t | Bn of lab_t |
            Bgt of lab_t | Bge of lab_t | Blt of lab_t | Ble of lab_t |
            Sra of reg_t | Sla of reg_t |
            Ld of opr_t | St of opr_t |
            Add of opr_t | Sub of opr_t | Mul of opr_t | Div of opr_t |
            Cmp of opr_t |
            And of opr_t | Or of opr_t | Not of reg_t |
            CallR of reg_t | CallI of lab_t | Ret |
            Label of lab_t | End | Comment of string |
            Push of reg_t | Pop of reg_t 
