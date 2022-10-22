
pub struct ConstEval<const V: u8>;

impl<const V: u8> ConstEval<V> {
    pub const VALUE: u8 = V;
}


/* This method of generating opcode handler is similar to tgbr's method 
 * instruction_opcodes will call a processing macro to generate 
 * handlers, the processing macro is run_macro
 *
 * the indexing macro will generate a opcode number to for each
 * table entry
 *
 * the format is one of the following:
 * MNE OP : N 
 * MNE OP1, OP2 : N
 * MNE OP : N
 *
 *
 * where MNE is the mnemonic, 
 *       OP, OP1, OP2 is the operands, 
 *          (nn) means nn is pointer (2 bytes) from instruction
 *          n means n is byte from instrction
 *          (op), means register op is used as pointer
 *          op, op1, op2 is register
 *
 *       N is the length of instruction in bytes
 */
#[rustfmt::skip]
macro_rules! use_z80_opcodes {
    ($run_macro:ident) => { indexing!($run_macro  @start:
         /*       00               01              02              03             04              05              06              07             */ 
         /* 00 */ nop :1;          ld bc, nn :3;   ld (bc), a :1;  inc bc :1;     inc b :1;       dec b :1;       ld b, n :2;    rlca :1;
         /* 01 */ ld (nn), sp :3;  add hl, bc :1;  ld a, (bc) :1;  dec bc :1;     inc c :1;       dec c :1;       ld c, n :2;    rrca :1;
         /* 02 */ stop :2;         ld de, nn :3;   ld (de), a :1;  inc de :1;     inc d :1;       dec d :1;       ld d, n :2;    rla :1; 
         /* 03 */ jr n :2;         add hl, de :1;  ld a, (de) :1;  dec de :1;     inc e :1;       dec e :1;       ld e, n :2;    rra :1;
    );
    }
}

/*
 * process each element of table, creating a list of entries with it's associated index code
 * is this what is called a tt muncher? 
 */
macro_rules! indexing {
    // start index counting at 0
    ($run_macro:ident @start: $($input:tt)*) =>  {
        indexing!($run_macro @indexing: 0 => $($input)* @end);
    };

    // mne : n 
    ($run_macro:ident @indexing: $ix:expr => $mne:ident :$n:expr; $($rest:tt)*) => {
        indexing!($run_macro @indexing: $ix + 1 => $($rest)* $ix => $mne [], $n;);
    };

    // mne op : n
    ($run_macro:ident @indexing: $ix:expr => $mne:ident $opr:tt :$n:expr; $($rest:tt)*) => {
        indexing!($run_macro @indexing: $ix + 1 => $($rest)* $ix => $mne [$opr], $n;);
    };

    // mne op, op : n
    ($run_macro:ident @indexing: $ix:expr => $mne:ident $dst:tt , $src:tt :$n:expr; $($rest:tt)*) => {
        indexing!($run_macro @indexing: $ix + 1 => $($rest)* $ix => $mne [$dst, $src], $n;);
    };

    // end of list
    ($run_macro:ident @indexing: $_:expr => @end $($ix:expr => $mne:ident  $opr:tt, $n:expr; )*) => {
        $run_macro!($($ix => $mne $opr, $n;)*);
    };
}

macro_rules! gen_z80_handlers {
    ($($ix:expr => $mne:tt $opr:tt, $n:expr;)*) => {
        pub fn do_it(opc: u8) {
                use crate::cpu::ConstEval;
            match opc {
                $( ConstEval::<{$ix}>::VALUE => {
                    gen_a_z80_handler!($mne $opr $n)
                }, )*
                _ => { panic!("problem!"); }
            }
        }
    }
}

macro_rules! gen_a_z80_handler {
    ($mne:ident [] $n:expr) => { 
        gen_mne!($mne $n)
    };

    ($mne:ident [$opr:tt] $n:expr) => {
        gen_mne!($mne $opr $n)
    };

    ($mne:ident [$dst:tt, $src:tt] $n:expr) => {
        gen_mne!($mne $dst $src $n)
    };
}

macro_rules! gen_mne {
    (nop $n:expr) => {
        println!("{} bytes: {}", $n, "nop")
    };

    (ld $dst:tt $src:tt $n:expr) => {
        println!("{} bytes: ld {} = {}", $n, stringify!($dst), stringify!($src))
    };

    (inc $opr:tt $n:expr) => {
        println!("{} bytes: inc {}", $n, stringify!($opr))
    };

    (dec $opr:tt $n:expr) => {
        println!("{} bytes: inc {}", $n, stringify!($opr))
    };

    (add $dst:tt $src:tt $n:expr) => {
        println!("{} bytes: add {} = {} + {}", $n, stringify!($dst), stringify!($dst), stringify!(src))
    };


    (jr n $n:expr) => {
        println!("{} bytes: jr n", $n)
    };
    
    (stop $n:expr) => {
        println!("{} bytes: stop", $n)
    };

    (rra $n:expr)  => {
        println!("{} bytes: r__a", $n)
    };
    
    (rla $n:expr)  => {
        println!("{} bytes: r__a", $n)
    };
    
    (rlca $n:expr)  => {
        println!("{} bytes: r__a", $n)
    };
    
    (rrca $n:expr)  => {
        println!("{} bytes: r__a", $n)
    };
}

mod mystuff {
use_z80_opcodes!(gen_z80_handlers);
}
                
#[test]
fn try_opcode() {
    use crate::cpu::mystuff::do_it;
    for i in (0..=10) {
        do_it(i as u8);
    }
}

