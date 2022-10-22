
pub struct ConstEval<const V: u8>;

impl<const V: u8> ConstEval<V> {
    pub const VALUE: u8 = V;
}


/* This method of generating opcode handler is similar to tgbr's method 
 * use_z80_opcodes specifies a table of instructions and will call a 
 * processing macro to generate handlers, the processing macro is run_macro
 *
 * the indexing macro will generate a opcode number to for each
 * table entry it will then call the run_macro that will
 * generate the actual handlers
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
         /* 08 */ ld (nn), sp :3;  add hl, bc :1;  ld a, (bc) :1;  dec bc :1;     inc c :1;       dec c :1;       ld c, n :2;    rrca :1;
         /* 10 */ stop :2;         ld de, nn :3;   ld (de), a :1;  inc de :1;     inc d :1;       dec d :1;       ld d, n :2;    rla :1; 
         /* 18 */ jr n :2;         add hl, de :1;  ld a, (de) :1;  dec de :1;     inc e :1;       dec e :1;       ld e, n :2;    rra :1;
         /* 20 */ jr nz, n :2;     ld hl, nn :3;   ld (hl+), a :1; inc hl :1;     inc h :1;       dec h :1;       ld h, n :2;    daa :1;
         /* 28 */ jr z, n :2;      add hl, hl :1;  ld a, (hl+) :1; dec hl :1;     inc l :1;       dec l :1;       ld l, n :2;    cpl :1;
         /* 30 */ jr nc, n :2;     ld sp, nn :3;   ld (hl-), a :1; inc sp :1;     inc (hl) :1;    dec (hl) :1;    ld (hl), n :2; scf :1;
         /* 38 */ jr c, n :2;      add hl, sp :1;  ld a, (hl-) :1; dec sp :1;     inc a :1;       dec a :1;       ld a, n :2;    ccf :1;
            
    )
    }
}

/*
 * process each element of table, creating a list of entries with it's associated index code
 * is this what is called a tt muncher? 
 */
macro_rules! indexing {
    // start index counting at 0
    ($run_macro:ident @start: $($input:tt)*) =>  {
        indexing!($run_macro @indexing: 0 => $($input)* @end)
    };

    // mne : n 
    ($run_macro:ident @indexing: $ix:expr => $mne:ident :$n:expr; $($rest:tt)*) => {
        indexing!($run_macro @indexing: $ix + 1 => $($rest)* $ix => $mne [], $n;)
    };

    // mne op : n
    ($run_macro:ident @indexing: $ix:expr => $mne:ident $opr:tt :$n:expr; $($rest:tt)*) => {
        indexing!($run_macro @indexing: $ix + 1 => $($rest)* $ix => $mne [$opr], $n;)
    };

    // mne op, op : n
    ($run_macro:ident @indexing: $ix:expr => $mne:ident $dst:tt , $src:tt :$n:expr; $($rest:tt)*) => {
        indexing!($run_macro @indexing: $ix + 1 => $($rest)* $ix => $mne [$dst, $src], $n;)
    };

    // end of list
    ($run_macro:ident @indexing: $_:expr => @end $($ix:expr => $mne:ident  $opr:tt, $n:expr; )*) => {
        $run_macro!($($ix => $mne $opr, $n;)*)
    };
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
    (nop $n:expr) => { {
        println!("{} bytes: {}", $n, "nop");
        ($n as u8)
    }};

    (ld $dst:tt $src:tt $n:expr) => {{
        let _srcval = load!($src);
        println!("{} bytes: ld {} = {}", $n, stringify!($dst), _srcval);
        ($n as u8)
    }};

    (inc $opr:tt $n:expr) => {{
        println!("{} bytes: inc {}", $n, stringify!($opr));
        ($n as u8)
    }};

    (dec $opr:tt $n:expr) => {{
        println!("{} bytes: inc {}", $n, stringify!($opr));
        ($n as u8)
    }};

    (add $dst:tt $src:tt $n:expr) => {{
        println!("{} bytes: add {} = {} + {}", $n, stringify!($dst), stringify!($dst), stringify!(src));
        ($n as u8)
    }};


    (jr n $n:expr) => {{
        println!("{} bytes: jr n", $n);
        ($n as u8)
    }};

    (jr $op1:tt $op2:tt $n:expr) => {{
        println!("{} bytes: jr {}, {}", $n, stringify!($op1), stringify!($op2));
        ($n as u8)
    }};
    
    (stop $n:expr) => {{
        println!("{} bytes: stop", $n);
        ($n as u8)
    }};

    (rra $n:expr)  => {{
        println!("{} bytes: rra", $n);
        ($n as u8)
    }};
    
    (rla $n:expr)  => {{
        println!("{} bytes: rla", $n);
        ($n as u8)
    }};
    
    (rlca $n:expr)  => {{
        println!("{} bytes: rlca", $n);
        ($n as u8)
    }};
    
    (rrca $n:expr)  => {{
        println!("{} bytes: rrca", $n);
        ($n as u8)
    }};
    
    (cpl $n:expr) => {{
        println!("{} bytes: cpl", $n);
        ($n as u8)
    }};

    (daa $n:expr) => {{
        println!("{} bytes: daa", $n);
        ($n as u8)
    }};

    (scf $n:expr) => {{
        println!("{} bytes: scf", $n);
        ($n as u8)
    }};

    (ccf $n:expr) => {{
        println!("{} bytes: ccf", $n);
        ($n as u8)
    }};

}


pub struct LR35902Cpu<'a> {
    mem: &'a [u8],
    regs: Registers,
}

#[derive(Default, Debug)]
pub struct Registers {
    pub a: u8,
    pub f: Flags,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub h: u8,
    pub l: u8,
    pub sp: u16,
    pub pc: u16,
}

#[derive(Default, Debug)]
pub struct Flags {
    pub z: bool,
    pub n: bool,
    pub h: bool,
    pub c: bool,
}

impl<'a> LR35902Cpu<'a> {

    fn new(start_pc: u16, mem: &'a[u8]) -> Self {
        let mut cpu = LR35902Cpu {
            mem,
            regs: Default::default()
        };
        cpu.regs.pc = start_pc;
        cpu
    }
    // TODO: This should be changed to read from the bus object instead of directly from mem
    fn read8(&self, addr: u16) -> u8 {
        self.mem[addr as usize]
    }


    fn pc(&self) -> u16 {
        self.regs.pc
    }

    fn set_pc(&mut self, pc: u16) {
        self.regs.pc = pc;
    }

    /*
     * disassemble opcode, using pc to fetch the rest of 
     * the instruction operands 
     *
     * returns the length of the instruction, this will
     * be used to drive the rest of the disassembly
     */
    fn disasm(&self, opc: u8) -> u8 {
        /* generates a function do_it(opc: u8) that for now just decodes
         * opcode and prints out the disassembled instruction
         */
        macro_rules! gen_z80_handlers {
            ($($ix:expr => $mne:tt $opr:tt, $n:expr;)*) => {
                match opc {
                    $( ConstEval::<{$ix}>::VALUE => {
                        gen_a_z80_handler!($mne $opr $n)
                    }, )*
                    _=> { panic!("problem!"); }
                }
            }
        }
       
        /* note: this macro must be declared inside disasm() because it 
         * references self. If we wanted this macro to be declared at the
         * module level, we would have to pass in self as an identifier
         * to this macro
         */
        macro_rules! load {
            (n) => {{
                self.read8(self.pc().wrapping_add(1))
            }};

            (nn) => {{
                let lo = self.read8(self.pc().wrapping_add(1));
                let hi = self.read8(self.pc().wrapping_add(2));
                lo as u16 | ((hi as u16) << 8)
            }};

            ((b)) => {{
                todo!();
               0 
            }};

            ((bc)) => {{
                todo!();
               0 
            }};

            ((de)) => {{
                todo!();
               0 
            }};

            (sp)  => {{
                todo!();
                0
            }};

            (a) => {{
                todo!();
                0
            }};

            ((hl+)) => {{
                todo!();
                0
            }};

            ((hl-)) => {{
                todo!();
                0
            }};


        }

        use_z80_opcodes!(gen_z80_handlers)

    }

}



                
#[test]
fn test_disasm() {
    use crate::cpu::LR35902Cpu;
    let code_buffer = [0x00, // nop
                       0x06, // ld b, n (0xff) 
                       0xff,
                       0x03]; // inc b
    let mut cpu = LR35902Cpu::new(0, &code_buffer);
    while cpu.pc() < (code_buffer.len() as u16) {
        let opcode = cpu.read8(cpu.pc()); 
        let oplen = cpu.disasm(opcode) as usize;
        cpu.set_pc(cpu.pc() + (oplen as u16));
    }
}

