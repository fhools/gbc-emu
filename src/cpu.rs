
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
         /*       00/08            01/09           02/0A           03/0B          04/0C           05/0D           06/0E          07/0F     */ 
         /* 00 */ nop :1;          ld bc, nn :3;   ld (bc), a :1;  inc bc :1;     inc b :1;         dec b :1;       ld b, n :2;     rlca :1;
         /* 08 */ ld (nn), sp :3;  add hl, bc :1;  ld a, (bc) :1;  dec bc :1;     inc c :1;         dec c :1;       ld c, n :2;     rrca :1;
         /* 10 */ stop :2;         ld de, nn :3;   ld (de), a :1;  inc de :1;     inc d :1;         dec d :1;       ld d, n :2;     rla :1; 
         /* 18 */ jr n :2;         add hl, de :1;  ld a, (de) :1;  dec de :1;     inc e :1;         dec e :1;       ld e, n :2;     rra :1;
         /* 20 */ jr nz, n :2;     ld hl, nn :3;   ld (hl+), a :1; inc hl :1;     inc h :1;         dec h :1;       ld h, n :2;     daa :1;
         /* 28 */ jr z, n :2;      add hl, hl :1;  ld a, (hl+) :1; dec hl :1;     inc l :1;         dec l :1;       ld l, n :2;     cpl :1;
         /* 30 */ jr nc, n :2;     ld sp, nn :3;   ld (hl-), a :1; inc sp :1;     inc (hl) :1;      dec (hl) :1;    ld (hl), n :2;  scf :1;
         /* 38 */ jr c, n :2;      add hl, sp :1;  ld a, (hl-) :1; dec sp :1;     inc a :1;         dec a :1;       ld a, n :2;     ccf :1;
         /* 40 */ ld b, b :1;      ld b, c :1;     ld b, d :1;     ld b, e :1;    ld b, h :1;       ld b, l :1;     ld b, (hl) :1;  ld b, a :1;
         /* 48 */ ld c, b :1;      ld c, c :1;     ld c, d :1;     ld c, e :1;    ld c, h :1;       ld c, l :1;     ld c, (hl) :1;  ld c, a :1;
         /* 50 */ ld d, b :1;      ld d, c :1;     ld d, d :1;     ld d, e :1;    ld d, h :1;       ld d, l :1;     ld d, (hl) :1;  ld d, a :1;
         /* 58 */ ld e, b :1;      ld e, c :1;     ld e, d :1;     ld e, e :1;    ld e, h :1;       ld e, l :1;     ld e, (hl) :1;  ld e, a :1;
         /* 60 */ ld h, b :1;      ld h, c :1;     ld h, d :1;     ld h, e :1;    ld h, h :1;       ld h, l :1;     ld h, (hl) :1;  ld h, a :1;
         /* 68 */ ld l, b :1;      ld l, c :1;     ld l, d :1;     ld l, e :1;    ld l, h :1;       ld l, l :1;     ld l, (hl) :1;  ld l, a :1;
         /* 70 */ ld (hl), b :1;   ld (hl), c :1;  ld (hl), d :1;  ld (hl), e :1; ld (hl), h :1;    ld (hl), l :1;  halt :1;        ld (hl), a :1;
         /* 78 */ ld a, b :1;      ld a, c :1;     ld a, d :1;     ld a, e :1;    ld a, h :1;       ld a, l :1;     ld a, (hl) :1;  ld a, a :1;
         /* 80 */ add a, b :1;     add a, c :1;    add a, d :1;    add a ,e :1;   add a, h :1;      add a, l :1;    add a, (hl) :1; add a, a :1;
         /* 88 */ adc a, b :1;     adc a, c :1;    adc a, d :1;    adc a, e :1;   adc a, h :1;      adc a, l :1;    adc a, (hl) :1; adc a, a :1;
         /* 90 */ sub b :1;        sub c :1;       sub d :1;       sub e :1;      sub h :1;         sub l :1;       sub (hl) :1;    sub a :1;
         /* 98 */ sbc a, b :1;     sbc a, c :1;    sbc a, d :1;    sbc a, e :1;   sbc a, h :1;      sbc a, l :1;    sbc a, (hl) :1; sbc a, a :1;
         /* A0 */ and b :1;        and c :1;       and d :1;       and e :1;      and h :1;         and l :1;       and (hl) :1;    and a :1;
         /* A8 */ xor b :1;        xor c :1;       xor d  :1;      xor e :1;      xor h :1;         xor l  :1;      xor (hl) :1;    xor a :1;
         /* B0 */ or b :1;         or c :1;        or d :1;        or e :1;       or h :1;          or l :1;        or (hl) :1;     or a :1;
         /* B8 */ cp b :1;         cp c :1;        cp d :1;        cp e :1;       cp h  :1;         cp l :1;        cp (hl) :1;     cp a :1;
         /* C0 */ ret nz :1;       pop bc :1;      jp nz, nn :3;   jp nn :3;      call nz, nn :3;   push bc :1;     add a, n :2;    rst 0 :1;
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
                    // TODO: update flags for carry, etc.
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
                let val = load!($opr);
                if std::mem::size_of_val(&val) == 1 {
                    println!("decrementing u8 register");
                    let val: u8 = val as u8;
                    store!($opr, val.wrapping_sub(1));
                } else {
                    println!("decrementing u16 register");
                    let val: u16 = val as u16;
                    store!($opr, val.wrapping_sub(1));
                } 
                println!("{} bytes: inc {}", $n, stringify!($opr));
                ($n as u8)
            }};

            (add $dst:tt $src:tt $n:expr) => {{
                let srcval = load!($src);
                let dstval = load!($dst);
                let resultval = dstval + srcval;
                println!("add resultval: 0x{:x}, srcval: 0x{:x}, dstval: 0x{:x}", resultval, srcval, dstval);
                store!($dst, resultval);
                println!("{} bytes: add {} = {} + {}", $n, stringify!($dst), stringify!($dst), stringify!($src));
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

            // rotate right, bit 0 into carry, carry into bit 7
            (rra $n:expr)  => {{
                let mut aval= load!(a) as u8;
                let bit0 = aval & 0x01;
                aval = aval >> 1;
                aval = ((self.regs.f.c as u8) << 7) | aval;
                store!(a, aval);
                self.regs.f.c = bit0 == 0x1;
                println!("{} bytes: rra", $n);
                ($n as u8)
            }};

            // rotate left, into carry, from carry to bit 0 of a
            (rla $n:expr)  => {{
                let mut aval = load!(a) as u8;
                let bit7 = aval >> 7; 
                aval = aval << 1;
                aval = (self.regs.f.c as u8) | aval;
                store!(a, aval);
                self.regs.c = bit7;
                println!("{} bytes: rla", $n);
                ($n as u8)
            }};

            // rotate left register a, bit 7 into carry and into bit 0
            (rlca $n:expr)  => {{
                let mut aval = load!(a) as u8;
                let bit7 = aval >> 7; 
                aval = aval << 1;
                aval = bit7 | aval;
                store!(a, aval);
                self.regs.c = bit7;
                println!("{} bytes: rlca", $n);
                ($n as u8)
            }};
            // rotate right register a, bit 0 into carry and into bit 7
            (rrca $n:expr)  => {{
                let mut aval = load!(a) as u8;
                let bit0 = aval & 0x01; 
                aval = aval >> 1;
                aval = (bit0 << 7) | aval;
                store!(a, aval);
                self.regs.c = bit0;
                println!("{} bytes: rrca", $n);
                ($n as u8)
            }};

            // one's complement of a 
            // sets n and h flags
            (cpl $n:expr) => {{
                let mut aval = load!(a) as u8;
                aval = !aval;
                store!(a, aval);
                self.regs.f.n = true;
                self.regs.f.h = true;
                println!("{} bytes: cpl", $n);
                ($n as u8)
            }};
            // adjust A register after an add or subtract to be valid BCD encoding?
            // see (https://forums.nesdev.org/viewtopic.php?t=15944)
            // used pseudocode from forum post
            (daa $n:expr) => {{
                let mut aval = self.regs.a;
                if !self.regs.f.n {
                    // check if A register is greater than 0x99 
                    // that means upper nibble needs to be adjusted back
                    // to 0-9. and lower nibble will be adjusted 
                    // if value is 0x9B the lower nibble will be adjusted
                    // this will cause upper nibble to wrap to A 
                    // so it will need to be adjusted.
                    if self.regs.f.c || aval > 0x99 {
                        aval += 0x60;
                        self.regs.f.c = true;
                    }
                    // adjust lower nibble, upper nibble will already be adjusted
                    // via the above section
                    if self.regs.f.h || (aval & 0x0f) > 0x9 {
                        aval += 0x6;
                    }
                } else {
                    // if carry flag set upper nibble needs to be adjusted
                    if self.regs.f.c {
                        aval -= 0x60;
                    }
                    // if half-carry was set then lower nibble needs to be adjusted
                    if self.regs.f.h {
                        aval -= 0x6;
                    }
                }
                self.regs.f.z = (aval == 0x00);
                self.regs.f.h = false;
                store!(a, aval);
                ($n as u8)
            }};

            (scf $n:expr) => {{
                self.regs.f.c = true;
                self.regs.f.n = false;
                self.regs.f.h = false;
                println!("{} bytes: scf", $n);
                ($n as u8)
            }};

            (ccf $n:expr) => {{
                // toggle carry flag?
                self.regs.f.c = (self.regs.f.c as u8 ^ 0x1)  == 0x1;
                println!("{} bytes: ccf", $n);
                ($n as u8)
            }};

            // CB prefix 
            (cb $n:expr) => {{
                use_z80_cb_table!(gen_z80_exec_cb_handlers)
            }};

            (halt $n:expr) => {{
                println!("{} bytes: halt", $n);
                ($n as u8)
            }};

            (adc $dst:tt $src:tt $n:expr) => {{
                let dstval = load!($dst);
                let srcval = load!($src);
                let result = dstval + srcval + (self.regs.f.c as u8);
                store!($dst, result);
                println!("{} bytes: adc {}, {}", $n, stringify!($dst), stringify!($src));
                ($n as u8)
            }};

            (sub $opr:tt $n:expr) => {{
                let oprval = load!($opr);
                self.regs.a = self.regs.a - oprval;
                println!("{} bytes: sub {}", $n, stringify!($opr));
                ($n as u8)
            }};

            (sbc $dst:tt $src:tt $n:expr) => {{
                let dstval = load!($dst);
                let srcval = load!($src);
                let result = dstval - srcval - (self.regs.f.c as u8);
                store!($dst, result);
                println!("{} bytes: sbc {}, {}", $n, stringify!($dst), stringify!($src));
                ($n as u8)
            }};

            (and $opr:tt $n:expr) => {{
                let oprval = load!($opr);
                self.regs.a = self.regs.a & oprval;
                println!("{} bytes: and {}", $n, stringify!($opr));
                ($n as u8)
            }};

            (xor $opr:tt $n:expr) => {{
                let oprval = load!($opr);
                self.regs.a = self.regs.a ^ oprval;
                println!("{} bytes: xor {}", $n, stringify!($opr));
                ($n as u8)
            }};

            (or $opr:tt $n:expr) => {{
                let oprval = load!($opr);
                self.regs.a = self.regs.a | oprval;
                println!("{} bytes: or {}", $n, stringify!($opr));
                ($n as u8)
            }};

            (cp $opr:tt $n:expr) => {{
                let oprval = load!($opr);
                self.regs.f.z = (self.regs.a - oprval) == 0x0;
                println!("{} bytes: cp {}", $n, stringify!($opr));
                ($n as u8)
            }};

            (ret nz $n:expr) => {{
                println!("{} bytes: ret nz", $n);
                ($n as u8)
            }};
           
            (pop bc $n:expr) => {{
                let hi = self.load8(self.regs.sp);
                let lo = self.load8(self.regs.sp + 1);
                self.regs.set_bc( ((hi as u16) << 8) | lo as u16);
                self.regs.sp += 2;
                ($n as u8)
            }};

            (push bc $n:expr) => {{
                let hi: u8 = (self.regs.bc() >> 8) as u8;
                let lo: u8 = (self.regs.bc() & 0xFF) as u8;
                self.regs.sp -= 1;
                self.store8(self.regs.sp, hi);
                self.regs.sp -= 1;
                self.store8(self.regs.sp, lo);
                ($n as u8)
            }};

            (rst $opr:literal $n:expr) => {{
                let  rst_vector_num = $opr;
                println!("{} bytes: rst {}", $n, stringify!($opr));
                ($n as u8)
            }};
            
             (call nz nn $n:expr) => {{
                let addr = load!(nn);
                println!("{} bytes: call nz {:x}", $n, addr);
                ($n as u8)
             }};
             
             (jp nn $n:expr) => {{ 
                let addr = load!(nn);
                println!("{} bytes: jp {:x}", $n, addr);
                ($n as u8)
             }};

             (jp nz nn $n:expr) => {{ 
                 let addr = load!(nn);
                println!("{} bytes: jp nz {:x}", $n, addr);
                ($n as u8)
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
                       0x03, // inc bc
                       0x24, // inc h
                       0x24, // inc h
                       0x09]; // add hl, bc
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

