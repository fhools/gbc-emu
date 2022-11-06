
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
         /* C8 */ ret z :1;        ret :1;         jp z, nn :3;    cb :1;         call z, nn :3;    call nn :3;     adc a, n :2;    rst 1 :1;
         /* D0 */ ret nc :1;       pop de :1;      jp nc, nn :3;   TODO :1;       call nc, nn :3;   push de :1;     sub :1;         rst 2 :1;
         /* D8 */ ret c :1;        reti :1;        jp c, nn :3;    TODO :1;       call c, nn :3;    TODO :1;        sbc a, n :1;    rst 3 :1;
         /* E0 */ ld (n), a :2;    pop hl :1;      ld (c), a :1;   TODO :1;       TODO :1;          push hl :1;     and n :2;       rst 4 :1;
         /* E8 */ add sp, n :2;    jp hl :1;       ld (nn), a :3;  TODO :1;       TODO :1;          TODO :1;        xor n :2;       rst 5 :1;
         /* F0 */ ld a, (n) :2;    pop af :1;      ld a, (c) :1;   di :1;         TODO :1;          push af :1;     or n :1;        rst 6 :1;
         /* F8 */ ld hl, spn :2;   ld sp, hl :1;   ld a, (nn) :3;  ei :1;         TODO :1;          TODO :1;        cp n :2;        rst 7 :1;
            
    )
    }
}

// NOTES: the instruction length in this case does not include the cb prefix. 
#[rustfmt::skip]
macro_rules! use_z80_cb_table {
    ($run_macro:ident) => { indexing!($run_macro @start:
         /*       00/08            01/09           02/0A           03/0B          04/0C           05/0D           06/0E           07/0F          */ 
         /* 00 */ rlc b :1;        rlc c :1;       rlc d :1;       rlc e :1;      rlc h :1;       rlc l :1;       rlc (hl) :1;    rlc a :1;
         /* 08 */ rrc b :1;        rrc c :1;       rrc d :1;       rrc e :1;      rrc h :1;       rrc l :1;       rrc (hl) :1;    rrc a :1;  
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

#[derive(Default, Debug)]
pub struct CpuStatistics {
    pub num_instrs_executed: i32,
}

/*
 * The LR35902 is a Z80 compatible CPU made by Sharp
 * In the Gameboy Color it runs at 8.2 Mhz
 *
 */
pub struct LR35902Cpu {
    bus: Shared<Bus>,
    regs: Registers,
    prev_ime: bool, // since EI instruction effect is delayed, we use prev_ime to actually test 
                    // if interrupt processing should occur
    ime: bool,     // interrupt enable flag
    stats: CpuStatistics,
}

impl LR35902Cpu {

    fn new(start_pc: u16, bus: Shared<Bus>) -> Self {
        let mut cpu = LR35902Cpu {
            bus,
            regs: Default::default(),
            prev_ime: true,
            ime: true,
            stats: Default::default()
        };
        cpu.regs.pc = start_pc;
        cpu
    }

    // load byte at pc and update pc, used for instructions that fetch data 
    // has immediate data
    fn fetch8(&mut self) -> u8 {
        let byte = self.bus.read8(self.regs.pc);
        self.set_pc(self.regs.pc + 1);
        byte
    }

    // same as fetch8 but update pc twice and grab u16
    fn fetch16(&mut self) -> u16 {
        let lo = self.bus.read8(self.regs.pc);
        let hi = self.bus.read8(self.regs.pc+1);
        self.set_pc(self.regs.pc + 2);
        (lo as u16) | ((hi as u16)<< 8)
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
        let opc = self.fetch8();

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
            (nop $n:expr) => {{
                println!("{} bytes: {}", $n, "nop");
                ($n as u8)
            }};

            (ld (n)  a $n:expr) => {{
                let lo = load!(n) as u16;
                let srcval = load!(a);
                let addr = 0xFF00 + lo;
                self.store8(addr, srcval);
                ($n as u8)
            }};

            (ld a  (n) $n:expr) => {{
                let lo = load!(n) as u16;
                let addr = 0xFF00 + lo;
                let val = self.load8(addr);
                store!(a, val);
                ($n as u8)
            }};

            (ld (c)  a $n:expr) => {{
                let lo = load!(c) as u16;
                let srcval = load!(a);
                let addr = 0xFF00 + lo;
                self.store8(addr, srcval);
                ($n as u8)
            }};

            (ld a (c) $n:expr) => {{
                let lo = load!(c) as u16;
                let addr = 0xFF00 + lo;
                let val = self.load8(addr);
                store!(a, val);
                ($n as u8)
            }};

            (ld hl spn $n:expr) => {{
                let lo = load!(n) as u16;
                let spval = load!(sp) as u16;
                let val = spval.wrapping_add(lo); 
                store!(hl, val);
                ($n as u8)
            }};

            (ld $dst:tt $src:tt $n:expr) => {{
                let srcval = load!($src);
                store!($dst, srcval);
                println!("{} bytes: ld {} = {:#X}", $n, stringify!($dst), srcval);
                ($n as u8)
            }};

            (inc $opr:tt $n:expr) => {{
                let prev_val = load!($opr);
                if std::mem::size_of_val(&prev_val) == 1 {
                    println!("incrementing u8 register");
                    let val: u8 = prev_val.wrapping_add(1) as u8;
                    self.regs.f.z = (val == 0);
                    self.regs.f.n = false;
                    self.regs.f.h = ((prev_val as u8) ^ val) & 0x10 != 0;
                    store!($opr, val);
                } else {
                    println!("incrementing u16 register");
                    let val: u16 = prev_val as u16;
                    store!($opr, val.wrapping_add(1));
                }
                println!("{} bytes: inc {}", $n, stringify!($opr));
                ($n as u8)
            }};

            (dec $opr:tt $n:expr) => {{
                let prev_val = load!($opr);
                if std::mem::size_of_val(&prev_val) == 1 {
                    println!("decrementing {} (u8) register", stringify!($opr));
                    let val: u8 = prev_val.wrapping_sub(1) as u8;
                    self.regs.f.z = (val == 0);
                    self.regs.f.n = true;
                    self.regs.f.h = ((prev_val as u8) ^ val) & 0x10 != 0;
                    store!($opr, val);
                } else {
                    println!("decrementing {} u16 register", stringify!($opr));
                    let val: u16 = prev_val as u16;
                    store!($opr, val.wrapping_sub(1));
                } 
                println!("{} bytes: dec {}", $n, stringify!($opr));
                ($n as u8)
            }};

            (add $dst:tt $src:tt $n:expr) => {{
                // TODO: add more code to detect different $dst, because this
                // will affect setting of flags
                let srcval = load!($src);
                let dstval = load!($dst) as u16;
                let (resultval, overflowed) = dstval.overflowing_add(srcval as u16);

                self.regs.f.n = false;

                // from stackoverflow:
                // overflow is when you add 2 numbers with the same sign but get a different sign
                if stringify!($dst) == "a" {
                    //bit 4
                    self.regs.f.h = ((dstval as u8) ^ (srcval as u8) ^ (resultval as u8)) & 0x10 != 0;
                    self.regs.f.c = overflowed;
                    self.regs.f.z = resultval == 0;
                }  else if stringify!($dst) == "hl" {
                    // bit 12
                    self.regs.f.h = ((dstval as u16) ^ (srcval as u16) ^ (resultval as u16)) & 0x1000 != 0;
                    self.regs.f.c = overflowed;
                    self.regs.f.z = resultval == 0;
                } else if stringify!($dst) == "sp" {
                    // bit 4
                    self.regs.f.h = ((dstval as u16) ^ (srcval as u16) ^ (resultval as u16)) & 0x10 != 0;
                    // bit 8
                    self.regs.f.c = ((dstval as u16) ^ (srcval as u16) ^ (resultval as u16)) & 0x100 != 0;
                    self.regs.f.z = false;
                }
                println!("add resultval: {:#X}, srcval: {:#X}, dstval: {:#X}", resultval, srcval, dstval);
                store!($dst, resultval);
                println!("{} bytes: add {} = {} + {}", $n, stringify!($dst), stringify!($dst), stringify!($src));
                ($n as u8)
            }};


            (jr n $n:expr) => {{
                // n is signed 8 byte offset
                let addr_offset = load!(n) as i8 as i16 as u16;
                let addr = self.regs.pc.wrapping_add(addr_offset);
                self.set_pc(addr);
                ($n as u8)
            }};

            (jr $op1:tt $op2:tt $n:expr) => {{
                let addr_offset = load!($op2) as i8 as i16 as u16;
                let condflags = stringify!($op1); 
                let condmet = match condflags {
                    "nz" => { !self.regs.f.z },
                    "z" => { self.regs.f.z },
                    "nc" => { !self.regs.f.c },
                    "c" =>  { self.regs.f.c },
                    _ => { panic!("jr condflags unknown {}", condflags);}
                };
                if condmet {
                    self.set_pc(self.regs.pc.wrapping_add(addr_offset));
                }
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
                // TODO: set carry/h flags
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

            (sub $n:expr) => {{
                let oprval = load!(n);
                self.regs.a = self.regs.a - oprval;
                println!("{} bytes: sub {:#04X}", $n, oprval);
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

            (reti $n:expr) => {{
                let lo = self.load8(self.regs.sp) as u16;
                self.regs.sp += 1;
                let  hi = self.load8(self.regs.sp) as u16;
                self.regs.sp += 1;
                let ret_addr = (hi << 8) | lo;
                self.set_pc(ret_addr);
                // enable interrupt when returning form ISR via RETI
                self.ime = true;
                println!("{} bytes: ret", $n);
                ($n as u8)
            }};

            (ret $n:expr) => {{
                // grab low then high
                let lo = self.load8(self.regs.sp) as u16;
                self.regs.sp += 1;
                let  hi = self.load8(self.regs.sp) as u16;
                self.regs.sp += 1;
                let ret_addr = (hi << 8) | lo;
                self.set_pc(ret_addr);
                println!("{} bytes: ret", $n);
                ($n as u8)
            }};
            
            (ret $conds:ident $n:expr) => {{
                let condflags = stringify!($conds);
                let cond_met = match condflags {
                    "nz" => { !self.regs.f.z },
                    "z" => { self.regs.f.z },
                    "nc" => { !self.regs.f.c },
                    "c" => { self.regs.f.c },
                    _ => { panic!("ret cond: {} unknown", condflags); }
                };
                if cond_met {
                    let lo = self.load8(self.regs.sp) as u16;
                    self.regs.sp += 1;
                    let  hi = self.load8(self.regs.sp) as u16;
                    self.regs.sp += 1;
                    let ret_addr = (hi << 8) | lo;
                    self.set_pc(ret_addr);
                }
                println!("{} bytes: ret {}", $n, condflags);
                ($n as u8)
            }};
           
            (pop bc $n:expr) => {{
                let hi = self.load8(self.regs.sp);
                let lo = self.load8(self.regs.sp + 1);
                self.regs.set_bc( ((hi as u16) << 8) | lo as u16);
                self.regs.sp += 2;
                ($n as u8)
            }};

            (pop de $n:expr) => {{
                let hi = self.load8(self.regs.sp);
                let lo = self.load8(self.regs.sp + 1);
                self.regs.set_de( ((hi as u16) << 8) | lo as u16);
                self.regs.sp += 2;
                ($n as u8)
            }};

            (pop hl $n:expr) => {{
                let hi = self.load8(self.regs.sp);
                let lo = self.load8(self.regs.sp + 1);
                self.regs.set_hl( ((hi as u16) << 8) | lo as u16);
                self.regs.sp += 2;
                ($n as u8)
            }};

            (pop af $n:expr) => {{
                let hi = self.load8(self.regs.sp);
                let lo = self.load8(self.regs.sp + 1);
                self.regs.set_af( ((hi as u16) << 8) | lo as u16);
                self.regs.sp += 2;
                ($n as u8)
            }};

            (push $reg:tt $n:expr) => {{
                let regval = load!($reg);
                self.regs.sp -= 1;
                self.store8(self.regs.sp, (regval >> 8) as u8);
                self.regs.sp -= 1;
                self.store8(self.regs.sp, (regval & 0xFF) as u8);
                ($n as u8)

            }};

            (rst $opr:literal $n:expr) => {{
                let  _rst_vector_num = $opr;
                println!("{} bytes: rst {}", $n, stringify!($opr));
                ($n as u8)
            }};
            
            (call $conds:ident nn $n:expr) => {{
                let addr = load!(nn);
                let condflags = stringify!($conds);
                let cond_met = match condflags {
                    "nz" => { !self.regs.f.z },
                    "z" => { self.regs.f.z },
                    _ => { panic!("call cond flag: {} is unknown!", condflags); }
                };
                if cond_met {
                    // push high then low bytes
                    self.regs.sp -= 1;
                    self.store8(self.regs.sp, (self.regs.pc >> 8) as u8);
                    self.regs.sp -= 1;
                    self.store8(self.regs.sp, (self.regs.pc &0xFF) as u8);
                    self.set_pc(addr);
                }
                println!("{} bytes: call nz {:x}", $n, addr);
                ($n as u8)
            }};
            
            (call nn $n:expr) => {{
                let addr = load!(nn);
                // push high then low bytes
                self.regs.sp -= 1;
                self.store8(self.regs.sp, (self.regs.pc >> 8) as u8);
                self.regs.sp -= 1;
                self.store8(self.regs.sp, (self.regs.pc &0xFF) as u8);
                self.set_pc(addr);
                println!("{} bytes: call nz {:x}", $n, addr);
                ($n as u8)
            }};
             
             (jp nn $n:expr) => {{ 
                let addr = load!(nn);
                self.set_pc(addr);
                println!("{} bytes: jp {:x}", $n, addr);
                ($n as u8)
             }};

             (jp $cond:ident nn $n:expr) => {{ 
                 let addr = load!(nn);
                 let condflags = stringify!($cond);
                 let cond_met = match condflags {
                     "nz" => { !self.regs.f.z },
                     "z" => { self.regs.f.z },
                     "nc" => { !self.regs.f.c },
                     "c" => { self.regs.f.c},
                     _ => { panic!("cp cond: {} unknown", condflags); }
                 };
                 if cond_met {
                     self.set_pc(addr);
                 }
                 println!("{} bytes: jp {} {:x}", $n, condflags, addr);
                ($n as u8)
             }};

             (jp hl $n:expr) => {{
                 let addr = self.regs.hl();
                 self.set_pc(addr);
                 println!("{} bytes: jp hl {:x}", $n, addr);
                 ($n as u8)
             }};

             (di $n:expr) => {{
                 // NOTE: DI takes effect right away
                 self.ime = false;
                 self.prev_ime = false;
                 println!("{} bytes: di", $n);
                 ($n as u8)
             }}; 
             
             (ei $n:expr) => {{
                 // NOTE: EI is delayed 1 instruction. we set ime but use prev_ime to test
                 // interrupt enable
                 self.ime = true;
                 println!("{} bytes: ei", $n);
                 ($n as u8)
             }}; 

            (TODO $n:expr) => {{
                todo!("unknown opcode:{:x}", opc);
                ($n as u8)
            }};


        }

        macro_rules! gen_a_z80_cb_handler {
            ($mne:ident [] $n:expr) => { 
                gen_exec_cb_mne!($mne $n)
            };

            ($mne:ident [$opr:tt] $n:expr) => {
                gen_exec_cb_mne!($mne $opr $n)
            };

            ($mne:ident [$dst:tt, $src:tt] $n:expr) => {
                gen_exec_cb_mne!($mne $dst $src $n)
            };
        }

        macro_rules! gen_exec_cb_mne {

            (rlc $oper:tt $n:expr) => {{
                let mut val = load!($oper) as u8;
                let bit7 = val >> 7; 
                val = val << 1;
                val = bit7 | val;
                store!($oper, val);
                self.regs.c = bit7;
                println!("{} bytes: rlc {}", stringify!($n), stringify!($oper));
                ($n as u8)
            }};

            (rrc $oper:tt $n:expr) => {{
                let mut aval = load!($oper) as u8;
                let bit0 = aval & 0x01; 
                aval = aval >> 1;
                aval = (bit0 << 7) | aval;
                store!($oper, aval);
                self.regs.c = bit0;
                println!("{} bytes: rrc {}", stringify!($n), stringify!($oper));
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
                let  opc_cb = self.fetch8();
                match opc_cb {
                    $( ConstEval::<{$ix}>::VALUE => {
                        gen_a_z80_cb_handler!($mne $opr $n)
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
                // assumes pc is a the next byte after opcode
                self.fetch8()
            }};

            (nn) => {{
                // assumes pc is at the next byte after opcode
                self.fetch16()
            }};

            ((nn)) => {{
                let addr = self.fetch16();
                self.load8(addr)
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

            (af) => {
                self.regs.af()
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

            ((bc), $val:expr) => {{
                let addr = self.regs.bc();
                self.store8(addr, $val);
            }};

            ((nn), $val:expr) => {{
                let addr = self.fetch16();
                self.store8(addr, $val as u8);
            }};

            ((de), $val:expr) => {{
                let addr = self.regs.de();
                self.store8(addr, $val as u8);
            }};

            ((hl), $val:expr) => {{
                let addr = self.regs.hl();
                self.store8(addr, $val as u8);
            }};

            ((hl-), $val:expr) => {{
                let addr = self.regs.hl();
                self.regs.set_hl(self.regs.hl().wrapping_sub(1));
                self.store8(addr, $val as u8);
            }};

            ((hl+), $val:expr) => {{
                let addr = self.regs.hl();
                self.regs.set_hl(self.regs.hl().wrapping_add(1));
                self.store8(addr, $val as u8);
            }};

            (hl, $val:expr) => {{
                self.regs.set_hl($val as u16)
            }};
        }

        let oplen = use_z80_table!(gen_z80_exec_handlers);
        self.stats.num_instrs_executed += 1;
        oplen
    }

    pub fn instructions_executed(&self) -> i32 {
        self.stats.num_instrs_executed
    }

}

impl std::fmt::Debug for LR35902Cpu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "REGS: PC: {:#06X} A: {:#06X} BC: {:#06X} DE: {:#06X} HL: {:#06X} SP: {:#06X} Flags: z:{} n:{} h:{} c:{}",
               self.regs.pc,
               self.regs.a,
               self.regs.bc(),
               self.regs.de(),
               self.regs.hl(),
               self.regs.sp,
               if self.regs.f.z  { "1" } else { "0"},
               if self.regs.f.n  { "1" } else { "0"},
               if self.regs.f.h  { "1" } else { "0"},
               if self.regs.f.c  { "1" } else { "0"},
               )
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
                       0x09, // add hl, bc
                       0x01, // ld bc, nn (where nn = 0xbabe)
                       0xbe, // z80 is little endien so 0xbabe is 0xbe 0xba
                       0xba,
                       0x25,
                       0xCB, // rlc b 
                       0x00,
                       0xC3, // jp 0x0000 ; go back to beginning
                       0x00,
                       0x00,
    ]; 
    let bus = Shared::new(Bus::new(&code_buffer)); 
    let mut cpu = LR35902Cpu::new(0, bus.clone());
   
    // Disassemble.  simple view, does not show operand values
    while cpu.pc() < (code_buffer.len() as u16) {
        let opcode = cpu.load8(cpu.pc()); 
        let oplen = cpu.disasm(opcode) as usize;
        cpu.set_pc(cpu.pc() + (oplen as u16));
    }
  
    // Execute instructions (may hop around due to jumps/calls)
    const MAX_INSTRUCTIONS_TO_RUN : i32 = 20;
    cpu.set_pc(0); 
    while cpu.pc() < (code_buffer.len() as u16) && cpu.instructions_executed() < MAX_INSTRUCTIONS_TO_RUN {
        cpu.exec_one_instruction() as usize;
        println!("CPU:");
        println!("{:?}", cpu);
    }

}

