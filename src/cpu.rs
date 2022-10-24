
use crate::bus::Bus;
use crate::util::Shared;
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
macro_rules! use_z80_table {
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
         /* 40 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* 40 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* 50 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* 40 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* 60 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* 40 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* 70 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* 40 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* 80 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* 40 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* 90 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* 40 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* A0 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* 40 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* B0 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* 40 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* C0 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* C8 */ TODO :1; TODO :1; TODO :1; cb :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* D0 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* 40 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* E0 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* 40 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* F0 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
         /* F8 */ TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1; TODO :1;
            
    )
    }
}

#[rustfmt::skip]
macro_rules! use_z80_cb_table {
    ($run_macro:ident) => { indexing!($run_macro @start:
         /*       00               01              02              03             04              05              06              07             */ 
         /* 00 */ TODO :1; 
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





/* Registers is the backing storage for the cpu registers.
 *
 * An interesting thing about Z80 is that the instruction
 * set may load and store to registers as either 8-bit
 * or as 16-bit.
 *
 * 16-bit registers are comprised of 2-bit registers:
 *
 * AF = A  + flags (bits 7-4) bits 3-0 are zero always??
 * BC = B + C 
 * DE = D + E
 * HL = H + L
 * SP is true 16-bit register
 * PC is true 16-bit register
 */
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

impl Registers {
    pub fn bc(&self) -> u16 {
        self.c as u16 | ((self.b as u16) << 8)
    }

    pub fn set_bc(&mut self, bc: u16) {
        self.b = (bc >> 8) as u8 & 0xFF;
        self.c = (bc & 0xFF) as u8;
    }

    pub fn hl(&self) -> u16 {
        self.l as u16 | ((self.h as u16) << 8)
    }

    pub fn set_hl(&mut self, hl: u16) {
        self.h = (hl >> 8) as u8 & 0xFF;
        self.l = (hl & 0xFF) as u8;
    }

    pub fn de(&self) -> u16 {
        self.e as u16 | ((self.d as u16) << 8)
    }

    pub fn set_de(&mut self, hl: u16) {
        self.d = (hl >> 8) as u8 & 0xFF;
        self.e = (hl & 0xFF) as u8;
    }

    pub fn af(&self) -> u16 {
        let flags = ((self.f.z as u8) << 7) |
                    ((self.f.n as u8) << 6) |
                    ((self.f.h as u8) << 5) |
                    ((self.f.c as u8) << 4);

        flags as u16 | ((self.a as u16) << 8)
    }


    pub fn set_af(&mut self, af: u16)  {
        self.a = (af >> 8) as u8 & 0xFF;
        self.f.z = ((af >> 7) & 0x1) == 1;
        self.f.n = ((af >> 6) & 0x1) == 1;
        self.f.h = ((af >> 5) & 0x1) == 1;
        self.f.c = ((af >> 4) & 0x1) == 1;
    }



}
#[derive(Default, Debug)]
pub struct Flags {
    pub z: bool,
    pub n: bool,
    pub h: bool,
    pub c: bool,
}

/*
 * The LR35902 is a Z80 compatible CPU made by Sharp
 * In the Gameboy Color it runs at 8.2 Mhz
 *
 */
pub struct LR35902Cpu {
    bus: Shared<Bus>,
    regs: Registers,
}

impl LR35902Cpu {

    fn new(start_pc: u16, bus: Shared<Bus>) -> Self {
        let mut cpu = LR35902Cpu {
            bus,
            regs: Default::default()
        };
        cpu.regs.pc = start_pc;
        cpu
    }

    fn load8(&self, addr: u16) -> u8 {
        self.bus.read8(addr)
    }

    fn store8(&mut self, addr: u16, val: u8) {
        self.bus.write8(addr, val);
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

        macro_rules! gen_z80_disasm_handlers {
            ($($ix:expr => $mne:tt $opr:tt, $n:expr;)*) => {
                match opc {
                    $( ConstEval::<{$ix}>::VALUE => {
                        gen_z80_disasm_handlers!(@one $mne $opr $n)
                    }, )*
                    _=> { panic!("problem!"); }
                }
            };

            (@one $mne:ident [] $n:expr) => {{
                let dasm = stringify!($mne);
                println!("{}", dasm);
                ($n as u8)
            }};

            (@one $mne:ident [$opr:tt] $n:expr) => {{
                let dasm = format!("{} {}", stringify!($mne), stringify!($opr));
                println!("{}", dasm);
                ($n as u8)
            }};

            (@one $mne:ident [$dst:tt, $src:tt] $n:expr) => {{
                let dasm = format!("{} {}, {}", stringify!($mne), stringify!($dst), stringify!($src));
                println!("{}", dasm);
                ($n as u8)

            }};

            (@one cb [] $n:expr) => {{
                let dasm = "cb dasm unimplemented!";
                println!("{}", dasm);
                ($n as u8)
            }};
        }

        use_z80_table!(gen_z80_disasm_handlers)

    }


    fn exec_one_instruction(&mut self) -> u8 {
        let opc = self.load8(self.pc());

        macro_rules! gen_a_z80_handler {
            ($mne:ident [] $n:expr) => { 
                gen_exec_mne!($mne $n)
            };

            ($mne:ident [$opr:tt] $n:expr) => {
                gen_exec_mne!($mne $opr $n)
            };

            ($mne:ident [$dst:tt, $src:tt] $n:expr) => {
                gen_exec_mne!($mne $dst $src $n)
            };
        }

        macro_rules! gen_exec_mne {
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
                let val = load!($opr);
                if std::mem::size_of_val(&val) == 1 {
                    println!("incrementing u8 register");
                    let val: u8 = val as u8;
                    store!($opr, val.wrapping_add(1));
                } else {
                    println!("incrementing u16 register");
                    let val: u16 = val as u16;
                    store!($opr, val.wrapping_add(1));
                }
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

            // CB prefix 
            (cb $n:expr) => {{
                use_z80_cb_table!(gen_z80_exec_cb_handlers)
            }};


            (TODO $n:expr) => {{
                todo!("unknown opcode:{:x}", opc);
                ($n as u8)
            }};


        }

        /* generates a match clause to handle opcodes
         * prints out the disassembled instruction
         */
        macro_rules! gen_z80_exec_handlers {
            ($($ix:expr => $mne:tt $opr:tt, $n:expr;)*) => {
                match opc {
                    $( ConstEval::<{$ix}>::VALUE => {
                        gen_a_z80_handler!($mne $opr $n)
                    }, )*
                    _=> { panic!("problem!"); }
                }
            }
        }

        /* generates a sub match clause to handle CB prefix instructions
         *
         */

        macro_rules! gen_z80_exec_cb_handlers {
            ($($ix:expr => $mne:tt $opr:tt, $n:expr;)*) => {{
                let  opc_cb = self.load8(self.pc().wrapping_add(1));
                match opc_cb {
                    $( ConstEval::<{$ix}>::VALUE => {
                        gen_a_z80_handler!($mne $opr $n)
                    }, )*
                    _=> { panic!("problem!"); }
                }
            }}
        }
       
        /* note: this macro must be declared inside disasm() because it 
         * references self. If we wanted this macro to be declared at the
         * module level, we would have to pass in self as an identifier
         * to this macro
         */
        macro_rules! load {
            (n) => {{
                self.load8(self.pc())
            }};

            (nn) => {{
                let lo = self.load8(self.pc().wrapping_add(1));
                let hi = self.load8(self.pc().wrapping_add(2));
                lo as u16 | ((hi as u16) << 8)
            }};

            ((bc)) => {{
                let bc = self.regs.bc();
                let lo = self.load8(bc);
                let hi = self.load8(bc.wrapping_add(1));
                lo as u16 | ((hi as u16) <<8)
            }};

            ((de)) => {{
                let de = self.regs.de();
                let lo = self.load8(de);
                let hi = self.load8(de.wrapping_add(1));
                lo as u16 | ((hi as u16) <<8)
            }};

            (sp)  => {{
                self.regs.sp 
            }};

            (a) => {
                self.regs.a 
            };

            (b) => {
                self.regs.b
            };

            (c) => {
                self.regs.c 
            };

            (d) => {
                self.regs.d
            };

            (e) => {
                self.regs.e
            };

            (h) => {
                self.regs.h
            };

            (l) => {
                self.regs.l
            };


            (de) => {
                self.regs.de() 
            };

            (hl) => {
                self.regs.hl()
            };

            (sp) => {
                self.regs.sp
            };

            ((hl)) => {{
                let hl = self.regs.hl();
                let lo = self.load8(hl);
                lo 
            }};
            
            ((hl+)) => {{
                let hl = self.regs.hl();
                let lo = self.load8(hl);
                self.regs.set_hl(hl.wrapping_add(1));
                lo 
            }};

            ((hl-)) => {{
                let hl = self.regs.hl();
                let lo = self.load8(hl);
                self.regs.set_hl(hl.wrapping_sub(1));
                lo 
            }};

            (bc) => {
                self.regs.bc() as u16
            };
        }


        macro_rules! store {
            (a, $val:expr) => {{
                self.regs.a = $val as u8;
            }};
            (b, $val:expr) => {{
                self.regs.b = $val as u8;
            }};

            (h, $val:expr) => {{
                self.regs.h = $val as u8;
            }};

            (l, $val:expr) => {{
                self.regs.l = $val as u8;
            }};

            (c, $val:expr) => {{
                self.regs.c = $val as u8;
            }};

            (d, $val:expr) => {{
                self.regs.d = $val as u8;
            }};

            (e, $val:expr) => {{
                self.regs.e = $val as u8;
            }};
    
            (bc, $val:expr) => {{
                self.regs.set_bc($val as u16);
            }};

            (de, $val:expr) => {{
                self.regs.set_de($val as u16);
            }};

            (sp, $val:expr) => {{
                self.regs.sp = $val as u16; 
            }};

            ((hl), $val:expr) => {{
                let addr = self.regs.hl();
                let val = self.load8(addr);
                self.store8(addr, val);
            }};

            (hl, $val:expr) => {{
                self.regs.set_hl($val as u16)
            }};
        }

        let pc = self.pc();
        // add one to the pc, so that the instructions 
        // that contains immediate data will load 
        // from the correct bytes
        self.set_pc(pc.wrapping_add(1));
        let oplen = use_z80_table!(gen_z80_exec_handlers);
        // TODO: actually implement updating pc, 
        // This is a horrible kludge, will not work because once we handle jumps and calls
        // the pc will be set by those instructions, we dont want to automatically jump
        // to the next instruction in sequence during execution, we can still use
        // the following for disassembly though
        if oplen > 1 {
            self.set_pc(self.pc() + ((oplen - 1) as u16));
        }
        oplen
    }

}



                
#[test]
fn test_disasm() {
    use crate::cpu::LR35902Cpu;
    let code_buffer = [0x00, // nop
                       0x06, // ld b, n (0xff) 
                       0xff,
                       0x03]; // inc bc
    let bus = Shared::new(Bus::new(&code_buffer)); 
    let mut cpu = LR35902Cpu::new(0, bus.clone());
    /*
    while cpu.pc() < (code_buffer.len() as u16) {
        let opcode = cpu.read8(cpu.pc()); 
        let oplen = cpu.disasm(opcode) as usize;
        cpu.set_pc(cpu.pc() + (oplen as u16));
    }
    */
    while cpu.pc() < (code_buffer.len() as u16) {
        cpu.exec_one_instruction() as usize;
    }
}

