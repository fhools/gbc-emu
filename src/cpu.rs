
use crate::bus::Bus;
use crate::util::Shared;
pub struct ConstEval<const V: u8>;

impl<const V: u8> ConstEval<V> {
    pub const VALUE: u8 = V;
}

/*
 * Blargg's cpu test
 *
 * [OK] 01-special
 * [  ] 02-interrupts
 * [OK] 03-op, sp,hl 
 *      NOTE: fixed ld (nn), sp! it loads the 16 bit value of sp onto address specified by nn. i
 *      was incorrectly loading 8 bit value of the top of stack value! 
 * [OK] 04-op r, imm
 * [OK] 05 op, rp 
 * [OK] 06-op ld r,r
 * [OK] 07-jr-jp-call-ret-rst
 *      NOTE: Was not passing due to bad ld (nn), sp implementation 
 * [OK] 08-op misc
 *      NOTE: Was not passing due to bad ld (nn), sp implementation
 * [OK] 09-op r,r
 * [OK] 10-bit ops
 * [OK] 11-op a,(hl)
 */

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
         /* 18 */ jr s8 :2;         add hl, de :1;  ld a, (de) :1;  dec de :1;     inc e :1;         dec e :1;       ld e, n :2;     rra :1;
         /* 20 */ jr nz, s8 :2;     ld hl, nn :3;   ld (hl+), a :1; inc hl :1;     inc h :1;         dec h :1;       ld h, n :2;     daa :1;
         /* 28 */ jr z, s8 :2;      add hl, hl :1;  ld a, (hl+) :1; dec hl :1;     inc l :1;         dec l :1;       ld l, n :2;     cpl :1;
         /* 30 */ jr nc, s8 :2;     ld sp, nn :3;   ld (hl-), a :1; inc sp :1;     inc (hl) :1;      dec (hl) :1;    ld (hl), n :2;  scf :1;
         /* 38 */ jr c, s8 :2;      add hl, sp :1;  ld a, (hl-) :1; dec sp :1;     inc a :1;         dec a :1;       ld a, n :2;     ccf :1;
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
         /* D0 */ ret nc :1;       pop de :1;      jp nc, nn :3;   TODO :1;       call nc, nn :3;   push de :1;     sub :2;         rst 2 :1;
         /* D8 */ ret c :1;        reti :1;        jp c, nn :3;    TODO :1;       call c, nn :3;    TODO :1;        sbc a, n :1;    rst 3 :1;
         /* E0 */ ld (n), a :2;    pop hl :1;      ld (c), a :1;   TODO :1;       TODO :1;          push hl :1;     and n :2;       rst 4 :1;
         /* E8 */ add sp, s8 :2;   jp hl :1;       ld (nn), a :3;  TODO :1;       TODO :1;          TODO :1;        xor n :2;       rst 5 :1;
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
         /* 10 */ rl b :1;         rl c :1;        rl d :1;        rl e :1;       rl h :1;        rl l :1;        rl (hl) :1;     rl a :1;
         /* 18 */ rr b :1;         rr c :1;        rr d :1;        rr e :1;       rr h :1;        rr l :1;        rr (hl) :1;     rr a :1;  
         /* 20 */ sla b :1;        sla c :1;       sla d :1;       sla e :1;      sla h :1;       sla l :1;       sla (hl) :1;    sla a :1;
         /* 28 */ sra b :1;        sra c :1;       sra d :1;       sra e :1;      sra h :1;       sra l :1;       sra (hl) :1;    sra a :1;
         /* 30 */ swap b :1;       swap c :1;      swap d :1;      swap e :1;     swap h :1;      swap l :1;      swap (hl) :1;   swap a :1;
         /* 38 */ srl b :1;        srl c :1;       srl d :1;       srl e :1;      srl h :1;       srl l :1;       srl (hl) :1;    srl a :1;
         /* 40 */ bit 0, b :1;     bit 0, c :1;    bit 0, d :1;    bit 0, e :1;   bit 0, h :1;    bit 0, l :1;    bit 0, (hl) :1; bit 0, a :1;
         /* 48 */ bit 1, b :1;     bit 1, c :1;    bit 1, d :1;    bit 1, e :1;   bit 1, h :1;    bit 1, l :1;    bit 1, (hl) :1; bit 1, a :1;
         /* 50 */ bit 2, b :1;     bit 2, c :1;    bit 2, d :1;    bit 2, e :1;   bit 2, h :1;    bit 2, l :1;    bit 2, (hl) :1; bit 2, a :1;
         /* 58 */ bit 3, b :1;     bit 3, c :1;    bit 3, d :1;    bit 3, e :1;   bit 3, h :1;    bit 3, l :1;    bit 3, (hl) :1; bit 3, a :1;
         /* 60 */ bit 4, b :1;     bit 4, c :1;    bit 4, d :1;    bit 4, e :1;   bit 4, h :1;    bit 4, l :1;    bit 4, (hl) :1; bit 4, a :1;
         /* 68 */ bit 5, b :1;     bit 5, c :1;    bit 5, d :1;    bit 5, e :1;   bit 5, h :1;    bit 5, l :1;    bit 5, (hl) :1; bit 5, a :1;
         /* 70 */ bit 6, b :1;     bit 6, c :1;    bit 6, d :1;    bit 6, e :1;   bit 6, h :1;    bit 6, l :1;    bit 6, (hl) :1; bit 6, a :1;
         /* 78 */ bit 7, b :1;     bit 7, c :1;    bit 7, d :1;    bit 7, e :1;   bit 7, h :1;    bit 7, l :1;    bit 7, (hl) :1; bit 7, a :1;
         /* 80 */ res 0, b :1;     res 0, c :1;    res 0, d :1;    res 0, e :1;   res 0, h :1;    res 0, l :1;    res 0, (hl) :1; res 0, a :1;
         /* 88 */ res 1, b :1;     res 1, c :1;    res 1, d :1;    res 1, e :1;   res 1, h :1;    res 1, l :1;    res 1, (hl) :1; res 1, a :1;
         /* 90 */ res 2, b :1;     res 2, c :1;    res 2, d :1;    res 2, e :1;   res 2, h :1;    res 2, l :1;    res 2, (hl) :1; res 2, a :1;
         /* 98 */ res 3, b :1;     res 3, c :1;    res 3, d :1;    res 3, e :1;   res 3, h :1;    res 3, l :1;    res 3, (hl) :1; res 3, a :1;
         /* A0 */ res 4, b :1;     res 4, c :1;    res 4, d :1;    res 4, e :1;   res 4, h :1;    res 4, l :1;    res 4, (hl) :1; res 4, a :1;
         /* A8 */ res 5, b :1;     res 5, c :1;    res 5, d :1;    res 5, e :1;   res 5, h :1;    res 5, l :1;    res 5, (hl) :1; res 5, a :1;
         /* B0 */ res 6, b :1;     res 6, c :1;    res 6, d :1;    res 6, e :1;   res 6, h :1;    res 6, l :1;    res 6, (hl) :1; res 6, a :1;
         /* B8 */ res 7, b :1;     res 7, c :1;    res 7, d :1;    res 7, e :1;   res 7, h :1;    res 7, l :1;    res 7, (hl) :1; res 7, a :1;
         /* C0 */ set 0, b :1;     set 0, c :1;    set 0, d :1;    set 0, e :1;   set 0, h :1;    set 0, l :1;    set 0, (hl) :1; set 0, a :1;
         /* C8 */ set 1, b :1;     set 1, c :1;    set 1, d :1;    set 1, e :1;   set 1, h :1;    set 1, l :1;    set 1, (hl) :1; set 1, a :1;
         /* D0 */ set 2, b :1;     set 2, c :1;    set 2, d :1;    set 2, e :1;   set 2, h :1;    set 2, l :1;    set 2, (hl) :1; set 2, a :1;
         /* D8 */ set 3, b :1;     set 3, c :1;    set 3, d :1;    set 3, e :1;   set 3, h :1;    set 3, l :1;    set 3, (hl) :1; set 3, a :1;
         /* E0 */ set 4, b :1;     set 4, c :1;    set 4, d :1;    set 4, e :1;   set 4, h :1;    set 4, l :1;    set 4, (hl) :1; set 4, a :1;
         /* E8 */ set 5, b :1;     set 5, c :1;    set 5, d :1;    set 5, e :1;   set 5, h :1;    set 5, l :1;    set 5, (hl) :1; set 5, a :1;
         /* F0 */ set 6, b :1;     set 6, c :1;    set 6, d :1;    set 6, e :1;   set 6, h :1;    set 6, l :1;    set 6, (hl) :1; set 6, a :1;
         /* F8 */ set 7, b :1;     set 7, c :1;    set 7, d :1;    set 7, e :1;   set 7, h :1;    set 7, l :1;    set 7, (hl) :1; set 7, a :1;
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
    pub bus: Shared<Bus>,
    pub regs: Registers,
    pub halted: bool,
    stats: CpuStatistics,
}

impl LR35902Cpu {

    pub fn new(start_pc: u16, bus: Shared<Bus>) -> Self {
        let mut cpu = LR35902Cpu {
            bus,
            regs: Default::default(),
            halted: false,
            stats: Default::default()
        };
        cpu.regs.pc = start_pc;
        cpu
    }

    // load byte at pc and update pc, used for instructions that fetch data 
    // has immediate data
    pub fn fetch8(&mut self) -> u8 {
        let byte = self.bus.read8(self.regs.pc);
        self.set_pc(self.regs.pc.wrapping_add(1));
        byte
    }

    // same as fetch8 but update pc twice and grab u16
    pub fn fetch16(&mut self) -> u16 {
        let lo = self.bus.read8(self.regs.pc);
        let hi = self.bus.read8(self.regs.pc.wrapping_add(1));
        self.set_pc(self.regs.pc.wrapping_add(2));
        (lo as u16) | ((hi as u16)<< 8)
    }

    pub fn load8(&self, addr: u16) -> u8 {
        self.bus.read8(addr)
    }

    pub fn store8(&mut self, addr: u16, val: u8) {
        self.bus.write8(addr, val);
    }

    pub fn pc(&self) -> u16 {
        self.regs.pc
    }

    pub fn set_pc(&mut self, pc: u16) {
        self.regs.pc = pc;
    }

    pub fn get_ld_cycles(&self, opr: &str) -> u8 {
        match opr {
            "a"|"f"|"b"|"c"|"h"|"l"|"d"|"e" => 0,
            "n"|"(de)"|"(bc)"|"(hl +)"|"(hl -)"|"(hl)"|"hl"|"bc"|"de"|"af" => 1,
            "nn"|"(n)"|"(nn)" => 2,
            // TODO: LD SP,HL is 2 cycles
            "sp" => 1,
            _ => panic!("unknown ld opr: {}", opr)
        }
    }

    pub fn get_ld_cycles2(&self, dest: &str, src: &str) -> u8 {
        match (dest, src) {
            ("bc", "nn") | ("de", "nn") | ("hl", "nn") | ("sp", "nn") => 3,
            ("(bc)", "a") | ("(de)", "a") | ("(hl+)", "a") | ("(hl-)", "a") => 2,
            ("b", "n") | ("d", "n") | ("h" , "n") | ("(hl)", "n") => 2, 
            ("(hl)", "n") => 3,
            ("a", "(bc)") | ("a" , "(de)") | ("a", "(hl+)") | ("a", "(hl-)") => 2,
            ("(n)", "a") | ("a", "(n)") => 3,
            ("(c)", "a") | ("a", "(c)") => 2,
            ("sp", "hl") => 2,
            ("(nn)", "a") | ("a", "(nn)") => 4,
            _ => 1
        }
    }
    /*
     * disassemble opcode, using pc to fetch the rest of 
     * the instruction operands 
     *
     * returns the length of the instruction, this will
     * be used to drive the rest of the disassembly
     */
    pub fn disasm(&self, opc: u8) -> (u8, String) {

        macro_rules! gen_z80_disasm_handlers {
            ($($ix:expr => $mne:tt $opr:tt, $n:expr;)*) => {
                match opc {
                    $( ConstEval::<{$ix}>::VALUE => {
                        gen_z80_disasm_handlers!(@one $mne $opr $n)
                    }, )*
                    _=> { panic!("problem!"); }
                }
            };

            (@one cb [] $n:expr) => {{
                let dasm = "cb dasm unimplemented!";
                (($n as u8), dasm.into())
            }};
            (@one $mne:ident [] $n:expr) => {{
                let dasm = stringify!($mne);
                (($n as u8), dasm.into())
            }};

            (@one $mne:ident [$opr:tt] $n:expr) => {{
                let dasm = format!("{} {}", stringify!($mne), stringify!($opr));
                (($n as u8), dasm.into())
            }};

            (@one $mne:ident [$dst:tt, $src:tt] $n:expr) => {{
                let dasm = format!("{} {}, {}", stringify!($mne), stringify!($dst), stringify!($src));
                (($n as u8), dasm.into())

            }};

        }

        use_z80_table!(gen_z80_disasm_handlers)

    }


    pub fn step(&mut self) -> u8 {
        // check pending interrupts 
        let prev_ime = self.bus.interrupts.prev_ime;

        // roll the prior ime for the 1 cycle delay of EI instruction
        self.bus.interrupts.prev_ime = self.bus.interrupts.ime;

        // if interrupt enabled and we got a interrupt pending
        if self.halted  {
            if (self.bus.interrupts.interrupt_enable_reg  & self.bus.interrupts.interrupt_flag) != 0 {
                if prev_ime {
                    self.halted = false;
                } else {
                    self.halted = false;
                }
            } 
            // TODO: I think there is a bug, RGBDS website says that if IME is not set then the
            // interrupt handler is not called when CPU resumes from interrupt pending
        }
        let pc = self.pc();
        let opcode = self.load8(pc);
        //println!("pc: {:04X} opcode: {:02X}", pc, opcode);

        if prev_ime && (self.bus.interrupts.interrupt_enable_reg  & self.bus.interrupts.interrupt_flag) != 0 {
            // get ISR address
            let which_int = self.bus.interrupts.interrupt_flag.trailing_zeros();
            //println!("ISR # raised: {}", which_int);
            let isr_addr = (0x0040 as u16).wrapping_add(8*which_int as u16);
            let pc = self.pc();

            // push return address onto stack
            self.regs.sp = self.regs.sp.wrapping_sub(1);
            self.store8(self.regs.sp, (pc >> 8) as u8);
            self.regs.sp = self.regs.sp.wrapping_sub(1);
            self.store8(self.regs.sp, (pc & 0xFF) as u8);

            // disable interrupt 
            self.bus.interrupts.ime = false;
            self.bus.interrupts.prev_ime = false;
            // clear interrupt flag for the interrupt
            self.bus.interrupts.interrupt_flag = self.bus.interrupts.interrupt_flag & !((1 << which_int) as u8);
            //println!("interrupt flag is now: {:02X}", self.bus.interrupts.interrupt_flag);
            //println!("running isr: addr: {:04X}", isr_addr);
            self.set_pc(isr_addr);
            self.bus.tick(); 
            self.bus.tick(); 
            self.bus.tick(); 
            // return so that we don't actually execute during this call to step().
            // the ISR will execute on the next call to step()
            return 4;
        }

        if !self.halted {
            let (_, cycles) = self.exec_one_instruction();
            for _ in 0..cycles {
                self.bus.tick(); 
            }
            cycles
        } else {
            self.bus.tick();
            1
        }
    }

    // Returns (instruction length in bytes, cycles)
    pub fn exec_one_instruction(&mut self) -> (u8, u8) {
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
                //println!("{} bytes: {}", $n, "nop");
                ($n as u8, 1)
            }};

            (ld (n) a $n:expr) => {{
                let lo = load!(n) as u16;
                let srcval = load!(a);
                let addr = 0xFF00 | lo;
                self.store8(addr, srcval);
                if addr >= 0xFF80 {
                    //println!("{} bytes: ld (n) addr:{:#X} a lo:{:#x} val: {:#X} ", $n, addr, lo, srcval);
                }
                ($n as u8, 3)
            }};

            (ld a (n) $n:expr) => {{
                let lo = load!(n) as u16;
                let addr = 0xFF00 | lo;
                let val = self.load8(addr);
                store!(a, val);
                if addr >= 0xFF80 {
                    //println!("{} bytes: ld a (n) addr:{:#X}  val: {:#X}, lo: {:#X}", $n, addr, val, lo);
                }
                ($n as u8, 3)
            }};

            (ld (c) a $n:expr) => {{
                let lo = load!(c) as u16;
                let srcval = load!(a);
                let addr = 0xFF00 | lo;
                self.store8(addr, srcval);
                if addr >= 0xFF80 {
                    //println!("{} bytes: ld (c) addr:{:#X} a  lo: {:#X} val:{:#X}", $n, addr, lo, srcval);
                }
                ($n as u8, 2)
            }};

            (ld a (c) $n:expr) => {{
                let lo = load!(c) as u16;
                let addr = 0xFF00 | lo;
                let val = self.load8(addr);
                store!(a, val);
                if addr >= 0xFF80 {
                    //println!("{} bytes: ld a (c) addr:{:#X} lo: {:#X} val: {:#x}", $n, addr, lo, val);
                }
                ($n as u8, 2)
            }};

            (ld hl spn $n:expr) => {{
                // TODO: check to see if we can go from i8 to u16, sign extending i8 to 16 bit
                let nval = load!(n) as i8 as i16 as u16;
                let spval = self.regs.sp;
                let result = spval.wrapping_add(nval); 
                self.regs.f.z = false;
                self.regs.f.n = false;
                self.regs.f.h = (nval ^ spval ^ result) & 0x10 != 0; 
                self.regs.f.c = (nval ^ spval ^ result) & 0x100 != 0;
                store!(hl, result);
                //println!("{} bytes: ld hl spn ", $n);
                ($n as u8, 3)
            }};

            (ld (nn) sp $n:expr) => {{
               store!((nn), sp);
               ($n as u8, 5)
            }};

            (ld $dst:tt $src:tt $n:expr) => {{
                let srcval = load!($src);
                store!($dst, srcval);
                let mut ldcycles = 1;
                //ldcycles += self.get_ld_cycles(stringify!($dst));
                //ldcycles += self.get_ld_cycles(stringify!($src));
                ldcycles = self.get_ld_cycles2(stringify!($dst), stringify!($src));
                //println!("{} bytes: ld {} = {:#X}", $n, stringify!($dst), srcval);
                ($n as u8, ldcycles)
            }};

            (inc $opr:tt $n:expr) => {{
                let prev_val = load!($opr);
                if std::mem::size_of_val(&prev_val) == 1 {
                    let val: u8 = prev_val.wrapping_add(1) as u8;
                    self.regs.f.z = (val == 0);
                    self.regs.f.n = false;
                    self.regs.f.h = ((prev_val as u8) ^ val) & 0x10 != 0;
                    store!($opr, val);
                } else {
                    let val: u16 = prev_val as u16;
                    store!($opr, val.wrapping_add(1));
                }

                let inc_cycles = match stringify!($opr) {
                    "bc"|"de"|"hl"|"sp" => 2,
                    "b"|"d"|"h"|"c"|"e"|"l"|"a" => 1,
                    "(hl)" => 3,
                    _ => panic!("inc bad opr: {}", stringify!($opr)),
                };
                //println!("{} bytes: inc {}", $n, stringify!($opr));
                ($n as u8, inc_cycles)
            }};

            (dec $opr:tt $n:expr) => {{
                let prev_val = load!($opr);
                if std::mem::size_of_val(&prev_val) == 1 {
                    //println!("decrementing {} (u8) register", stringify!($opr));
                    let val: u8 = prev_val.wrapping_sub(1) as u8;
                    self.regs.f.z = (val == 0);
                    self.regs.f.n = true;
                    self.regs.f.h = ((prev_val as u8) ^ val) & 0x10 != 0;
                    store!($opr, val);
                } else {
                    let val: u16 = prev_val as u16;
                    store!($opr, val.wrapping_sub(1));
                } 

                let dec_cycles = match stringify!($opr) {
                    "bc"|"de"|"hl"|"sp" => 2,
                    "b"|"d"|"h"|"c"|"e"|"l"|"a" => 1,
                    "(hl)" => 3,
                    _ => panic!("dec bad opr: {}", stringify!($opr)) 
                };
                //println!("{} bytes: dec {}", $n, stringify!($opr));
                ($n as u8, dec_cycles)
            }};

            (add $dst:tt $src:tt $n:expr) => {{

                self.regs.f.n = false;

                let mut add_cycles = 1;
                // from stackoverflow:
                // overflow is when you add 2 numbers with the same sign but get a different sign
                if stringify!($dst) == "a" {
                    let srcval = load!($src) as u8;
                    let dstval = load!($dst) as u8;
                    let (resultval, overflowed) = dstval.overflowing_add(srcval);
                    //bit 4
                    self.regs.f.h = ((dstval as u8) ^ (srcval as u8) ^ (resultval as u8)) & 0x10 != 0;
                    self.regs.f.c = overflowed;
                    self.regs.f.z = resultval == 0;
                    if stringify!($src) == "(hl)" || stringify!($src) == "n" {
                        add_cycles = 2;
                    } else {
                        add_cycles = 1;
                    }
                    store!($dst, resultval);
                }  else if stringify!($dst) == "hl" {
                    let srcval = load!($src) as u16;
                    let dstval = load!($dst) as u16;
                    let (resultval, overflowed) = dstval.overflowing_add(srcval);
                    // bit 12
                    self.regs.f.h = ((dstval as u16) ^ (srcval as u16) ^ (resultval as u16)) & 0x1000 != 0;
                    self.regs.f.c = overflowed;
                    add_cycles = 2;
                    store!($dst, resultval);
                } else if stringify!($dst) == "sp" {
                    let srcval = load!($src) as i8 as i16 as u16;
                    let dstval = load!($dst) as u16;
                    let (resultval, _) = dstval.overflowing_add(srcval);
                    // bit 4
                    self.regs.f.h = ((dstval as u16) ^ (srcval as u16) ^ (resultval as u16)) & 0x10 != 0;
                    //self.regs.f.h = ((dstval as u16) ^ (srcval as u16) ^ (resultval as u16)) & 0x100 != 0;
                    // bit 8
                    self.regs.f.c = ((dstval as u16) ^ (srcval as u16) ^ (resultval as u16)) & 0x100 != 0;
                    //self.regs.f.c = overflowed;
                    self.regs.f.z = false;
                    add_cycles = 4;
                    store!($dst, resultval);
                }
                //println!("add resultval: {:#X}, srcval: {:#X}, dstval: {:#X}", resultval, srcval, dstval);
                //println!("{} bytes: add {} = {} + {}", $n, stringify!($dst), stringify!($dst), stringify!($src));
                ($n as u8, add_cycles)
            }};


            (jr s8 $n:expr) => {{
                // n is signed 8 byte offset
                let addr_offset = load!(s8) as i16 as u16;
                let addr = self.regs.pc.wrapping_add(addr_offset);
                self.set_pc(addr);
                //println!("{} bytes: jr {:#X}", $n, addr);

                // TODO: Cycles is actually 3/2
                ($n as u8, 2)
            }};

            (jr $op1:tt $op2:tt $n:expr) => {{
                // TODO: check to see if we can go straight from i8 to u16, will it sign extend it?
                let addr_offset = (load!($op2) as i8 as i16 as u16);
                let addr = self.regs.pc.wrapping_add(addr_offset as u16);
                let condflags = stringify!($op1); 
                let condmet = match condflags {
                    "nz" => { !self.regs.f.z },
                    "z" => { self.regs.f.z },
                    "nc" => { !self.regs.f.c },
                    "c" =>  { self.regs.f.c },
                    _ => { panic!("jr condflags unknown {}", condflags);}
                };
                //println!("{} bytes: jr {}, {:#06X} (final: {:#06X})", $n, stringify!($op1), addr_offset, addr);
                if condmet {
                    self.set_pc(addr);
                }
                //println!("{} bytes: jr {}, {:#06X} (final: {:#06X})", $n, stringify!($op1), addr_offset, addr);
                // TODO: Cycles is actually 3/2
                ($n as u8, 2)
            }};

            (stop $n:expr) => {{
                // TODO: dont know what this does yet
                //println!("{} bytes: stop", $n);
                ($n as u8, 1)
            }};

            // rotate right,  carry into bit 7, carry = A0
            (rra $n:expr)  => {{
                let mut aval= load!(a) as u8;
                let bit0 = aval & 0x01;
                aval = aval >> 1;
                aval = ((self.regs.f.c as u8) << 7) | aval;
                store!(a, aval);
                self.regs.f.z = false;
                self.regs.f.n = false;
                self.regs.f.h = false;
                self.regs.f.c = bit0 == 1;
                //println!("{} bytes: rra", $n);
                ($n as u8, 1)
            }};

            // rotate left, carry into bit0, A7 into carry
            (rla $n:expr)  => {{
                let mut aval = load!(a) as u8;
                let bit7 = aval >> 7;
                aval = aval << 1;
                aval = (self.regs.f.c as u8) | aval;
                store!(a, aval);
                self.regs.f.z = false;
                self.regs.f.n = false;
                self.regs.f.h = false;
                self.regs.f.c = bit7 == 1;
                //println!("{} bytes: rla", $n);
                ($n as u8, 1)
            }};

            // rotate left, bit7 into carry and bit0
            (rlca $n:expr)  => {{
                let mut aval = load!(a) as u8;
                let bit7 = aval >> 7; 
                aval = aval << 1;
                aval = bit7 | aval;
                store!(a, aval);
                self.regs.f.c = bit7 == 1;
                self.regs.f.z = false;
                self.regs.f.n = false;
                self.regs.f.h = false;
                //println!("{} bytes: rlca", $n);
                ($n as u8, 1)
            }};
            // rotate right , bit 0 into carry and into bit7, A0 into carry
            (rrca $n:expr)  => {{
                let mut aval = load!(a) as u8;
                let bit0 = aval & 0x01; 
                aval = aval >> 1;
                aval = (bit0 << 7) | aval;
                store!(a, aval);
                self.regs.f.c = bit0 == 1;
                self.regs.f.z = false;
                self.regs.f.n = false;
                self.regs.f.h = false;
                //println!("{} bytes: rrca", $n);
                ($n as u8, 1)
            }};

            // one's complement of a 
            // sets n and h flags
            (cpl $n:expr) => {{
                let mut aval = load!(a) as u8;
                aval = !aval;
                store!(a, aval);
                self.regs.f.n = true;
                self.regs.f.h = true;
                //println!("{} bytes: cpl", $n);
                ($n as u8, 1)
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
                        aval = aval.wrapping_add(0x60);
                        self.regs.f.c = true;
                    }
                    // adjust lower nibble, upper nibble will already be adjusted
                    // via the above section
                    if self.regs.f.h || (aval & 0x0f) > 0x9 {
                        aval = aval.wrapping_add(0x6);
                    }
                } else {
                    // if carry flag set upper nibble needs to be adjusted
                    if self.regs.f.c {
                        aval  = aval.wrapping_sub(0x60);
                    }
                    // if half-carry was set then lower nibble needs to be adjusted
                    if self.regs.f.h {
                        aval = aval.wrapping_sub(0x6);
                    }
                }
                self.regs.f.z = (aval == 0x00);
                self.regs.f.h = false;
                store!(a, aval);
                //println!("{} bytes: daa", $n);
                ($n as u8, 1)
            }};

            (scf $n:expr) => {{
                self.regs.f.c = true;
                self.regs.f.n = false;
                self.regs.f.h = false;
                //println!("{} bytes: scf", $n);
                ($n as u8, 1)
            }};

            (ccf $n:expr) => {{
                // toggle carry flag?
                self.regs.f.c = !self.regs.f.c;
                self.regs.f.n = false;
                self.regs.f.h = false;
                //println!("{} bytes: ccf", $n);
                ($n as u8, 1)
            }};

            // CB prefix 
            (cb $n:expr) => {{
                use_z80_cb_table!(gen_z80_exec_cb_handlers)
            }};

            (halt $n:expr) => {{
                //println!("{} bytes: halt", $n);
                self.halted = true;
                ($n as u8, 1)
            }};

            (adc $dst:tt $src:tt $n:expr) => {{
                let dstval = load!($dst);
                let srcval = load!($src);
                let (result, overflowed) = dstval.overflowing_add(srcval);
                let (result2, overflowed2) = result.overflowing_add((self.regs.f.c as u8));
                self.regs.f.z = result2 == 0;  
                self.regs.f.n = false;
                self.regs.f.h = (result2 ^ dstval ^ srcval) & 0x10 != 0; 
                self.regs.f.c = overflowed || overflowed2;
                store!($dst, result2);

                let adc_cycles = if stringify!($src) == "(hl)" || stringify!($src) == "n" { 2 } else { 1 };
                
                //println!("{} bytes: adc {}, {}", $n, stringify!($dst), stringify!($src));
                ($n as u8, adc_cycles)
            }};

            (sub $opr:tt $n:expr) => {{
                let oprval = load!($opr);
                let (result, overflowed) = self.regs.a.overflowing_sub(oprval);
                self.regs.f.z = result == 0;
                self.regs.f.n = true;
                self.regs.f.h = (result ^ oprval ^ self.regs.a) & 0x10 != 0;
                self.regs.f.c = overflowed;
                store!(a, result);
                let sub_cycles = if stringify!($opr) == "(hl)" { 2 } else { 1 };
                //println!("{} bytes: sub {}", $n, stringify!($opr));
                ($n as u8, sub_cycles)
            }};

            (sub $n:expr) => {{
                let oprval = load!(n);
                let (result, overflowed) = self.regs.a.overflowing_sub(oprval);
                self.regs.f.z = result == 0;
                self.regs.f.n = true;
                self.regs.f.h = (result ^ oprval ^ self.regs.a) & 0x10 != 0;
                self.regs.f.c = overflowed;
                store!(a, result);
                //println!("{} bytes: sub {:#04X}", $n, oprval);
                ($n as u8, 2)
            }};

            (sbc $dst:tt $src:tt $n:expr) => {{
                let dstval = load!($dst);
                let srcval = load!($src);
                let (result1, overflowed1) = dstval.overflowing_sub(srcval);
                let (result2, overflowed2) = result1.overflowing_sub((self.regs.f.c as u8));
                store!($dst, result2);
                self.regs.f.z = result2 == 0;
                self.regs.f.n = true;
                self.regs.f.h = (dstval ^ srcval ^ result2) & 0x10 != 0;
                self.regs.f.c = overflowed1 || overflowed2;
                //println!("{} bytes: sbc {}, {}", $n, stringify!($dst), stringify!($src));
                let sub_cycles = if stringify!($src) == "(hl)" || stringify!($src) == "n" { 2 } else { 1 };
                ($n as u8, sub_cycles)
            }};

            (and $opr:tt $n:expr) => {{
                let oprval = load!($opr);
                self.regs.a = self.regs.a & oprval;
                self.regs.f.z = self.regs.a == 0;
                self.regs.f.n = false;
                self.regs.f.h = true;
                self.regs.f.c = false;
                //println!("{} bytes: and {}", $n, stringify!($opr));
                let and_cycles = if stringify!($opr) == "(hl)" || stringify!($opr) == "n" { 2 } else { 1 };
                ($n as u8, and_cycles)
            }};

            (xor $opr:tt $n:expr) => {{
                let oprval = load!($opr);
                self.regs.a = self.regs.a ^ oprval;
                self.regs.f.z = self.regs.a == 0;
                self.regs.f.n = false;
                self.regs.f.h = false;
                self.regs.f.c = false;
                //println!("{} bytes: xor {}", $n, stringify!($opr));
                let xor_cycles = if stringify!($opr) == "(hl)" || stringify!($opr) == "n" { 2 } else { 1 };
                ($n as u8, xor_cycles)
            }};

            (or $opr:tt $n:expr) => {{
                let oprval = load!($opr);
                self.regs.a = self.regs.a | oprval;
                self.regs.f.z = self.regs.a == 0;
                self.regs.f.n = false;
                self.regs.f.h = false;
                self.regs.f.c = false;
                //println!("{} bytes: or {}", $n, stringify!($opr));
                let or_cycles = if stringify!($opr) == "(hl)" || stringify!($opr) == "n" { 2 } else { 1 };
                ($n as u8, or_cycles)
            }};

            (cp $opr:tt $n:expr) => {{
                let oprval = load!($opr);
                let (result, overflowed) = self.regs.a.overflowing_sub(oprval);
                self.regs.f.z = result == 0;
                self.regs.f.n = true;
                self.regs.f.h = (result ^ oprval ^ self.regs.a) & 0x10 != 0;
                self.regs.f.c = overflowed;
                //println!("{} bytes: cp {}", $n, stringify!($opr));
                let cp_cycle = if stringify!($opr) == "n" || stringify!($opr) == "(hl)" { 2 } else { 1 };
                ($n as u8, cp_cycle)
            }};

            (reti $n:expr) => {{
                let lo = self.load8(self.regs.sp) as u16;
                self.regs.sp = self.regs.sp.wrapping_add(1);
                let  hi = self.load8(self.regs.sp) as u16;
                self.regs.sp = self.regs.sp.wrapping_add(1);
                let ret_addr = (hi << 8) | lo;
                println!("reti: pc: {:04X}, returning to: {:04X}", self.pc() - 1, ret_addr);
                self.set_pc(ret_addr);
                // enable interrupt when returning form ISR via RETI
                // also has 1 cycle delay
                self.bus.interrupts.ime = true;
                ($n as u8, 4)
            }};

            (ret $n:expr) => {{
                // grab low then high
                let lo = self.load8(self.regs.sp) as u16;
                self.regs.sp = self.regs.sp.wrapping_add(1);
                let  hi = self.load8(self.regs.sp) as u16;
                self.regs.sp = self.regs.sp.wrapping_add(1);
                let ret_addr = (hi << 8) | lo;
                self.set_pc(ret_addr);
                //println!("{} bytes: ret", $n);

                ($n as u8, 4)
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
                let mut ret_cycles = 2;
                if cond_met {
                    let lo = self.load8(self.regs.sp) as u16;
                    self.regs.sp = self.regs.sp.wrapping_add(1);
                    let  hi = self.load8(self.regs.sp) as u16;
                    self.regs.sp = self.regs.sp.wrapping_add(1);
                    let ret_addr = (hi << 8) | lo;
                    self.set_pc(ret_addr);
                    ret_cycles = 5;
                }
                //println!("{} bytes: ret {}", $n, condflags);
                ($n as u8, ret_cycles)
            }};
           
            (pop bc $n:expr) => {{
                let lo = self.load8(self.regs.sp);
                let hi = self.load8(self.regs.sp.wrapping_add(1));
                self.regs.set_bc( ((hi as u16) << 8) | lo as u16);
                self.regs.sp = self.regs.sp.wrapping_add(2);
                //println!("{} bytes: pop bc ", $n);
                ($n as u8, 3)
            }};

            (pop de $n:expr) => {{
                let lo = self.load8(self.regs.sp);
                let hi = self.load8(self.regs.sp.wrapping_add(1));
                self.regs.set_de( ((hi as u16) << 8) | lo as u16);
                self.regs.sp = self.regs.sp.wrapping_add(2);
                //println!("{} bytes: pop de ", $n);
                ($n as u8, 3)
            }};

            (pop hl $n:expr) => {{
                let lo = self.load8(self.regs.sp);
                let hi = self.load8(self.regs.sp.wrapping_add(1));
                self.regs.set_hl( ((hi as u16) << 8) | lo as u16);
                self.regs.sp = self.regs.sp.wrapping_add(2);
                //println!("{} bytes: pop hl ", $n);
                ($n as u8, 3)
            }};

            (pop af $n:expr) => {{
                let lo = self.load8(self.regs.sp) & 0xF0;
                let hi = self.load8(self.regs.sp.wrapping_add(1));
                self.regs.set_af( ((hi as u16) << 8) | lo as u16);
                self.regs.sp  = self.regs.sp.wrapping_add(2);
                //println!("{} bytes: pop af ", $n);
                ($n as u8, 3)
            }};

            (push $reg:tt $n:expr) => {{
                let regval = load!($reg);
                self.regs.sp = self.regs.sp.wrapping_sub(1);
                self.store8(self.regs.sp, (regval >> 8) as u8);
                self.regs.sp = self.regs.sp.wrapping_sub(1);
                self.store8(self.regs.sp, (regval & 0xFF) as u8);
                //println!("{} bytes: push {} ", $n, stringify!($reg));
                ($n as u8, 4)
            }};

            (rst $opr:literal $n:expr) => {{
                let rst_num = $opr;
                let rst_addr = rst_num*0x8 as u16;
                self.regs.sp = self.regs.sp.wrapping_sub(1);
                self.store8(self.regs.sp, (self.regs.pc >> 8) as u8);
                self.regs.sp = self.regs.sp.wrapping_sub(1);
                self.store8(self.regs.sp, (self.regs.pc & 0xFF) as u8);
                self.set_pc(rst_addr);
                //println!("{} bytes: rst {}", $n, stringify!($opr));
                ($n as u8, 4)
            }};
            
            (call $conds:ident nn $n:expr) => {{
                let addr = load!(nn);
                let condflags = stringify!($conds);
                let cond_met = match condflags {
                    "nz" => { !self.regs.f.z },
                    "z" => { self.regs.f.z },
                    "nc" => { !self.regs.f.c },
                    "c" => {  self.regs.f.c },
                    _ => { panic!("call cond flag: {} is unknown!", condflags); }
                };
                let mut call_cycles = 3;
                if cond_met {
                    self.regs.sp = self.regs.sp.wrapping_sub(1);
                    self.store8(self.regs.sp, (self.regs.pc >> 8) as u8);
                    self.regs.sp = self.regs.sp.wrapping_sub(1);
                    self.store8(self.regs.sp, (self.regs.pc & 0xFF) as u8);
                    self.set_pc(addr);
                    call_cycles = 6;
                }
                //println!("{} bytes: call {} {:x}", $n, condflags, addr);
                ($n as u8, call_cycles)
            }};
            
            (call nn $n:expr) => {{
                let addr = load!(nn);
                self.regs.sp = self.regs.sp.wrapping_sub(1);
                self.store8(self.regs.sp, (self.regs.pc >> 8) as u8);
                self.regs.sp = self.regs.sp.wrapping_sub(1);
                self.store8(self.regs.sp, (self.regs.pc & 0xFF) as u8);
                self.set_pc(addr);
                //println!("{} bytes: call nn (addr:{:#X})", $n, addr);
                ($n as u8, 6)
            }};
             
             (jp nn $n:expr) => {{ 
                let addr = load!(nn);
                self.set_pc(addr);
                //println!("{} bytes: jp nn:{:#06X}", $n, addr);
                ($n as u8, 4)
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

                 let mut jp_cycles = 3;
                 if cond_met {
                     self.set_pc(addr);
                     jp_cycles = 4;
                 }
                 //println!("{} bytes: jp {} {:#06X}", $n, condflags, addr);
                ($n as u8, jp_cycles)
             }};

             (jp hl $n:expr) => {{
                 let addr = self.regs.hl();
                 self.set_pc(addr);
                 //println!("{} bytes: jp hl {:x}", $n, addr);
                 ($n as u8, 1)
             }};

             (di $n:expr) => {{
                 // NOTE: DI takes effect right away
                 self.bus.interrupts.ime = false;
                 self.bus.interrupts.prev_ime = false;
                 //println!("{} bytes: di", $n);
                 ($n as u8, 1)
             }}; 
             
             (ei $n:expr) => {{
                 // NOTE: EI is delayed 1 instruction. we set ime but use prev_ime to test
                 // interrupt enable
                 self.bus.interrupts.ime = true;
                 //println!("{} bytes: ei", $n);
                 ($n as u8, 1)
             }}; 

            (TODO $n:expr) => {{
                todo!("unknown opcode:{:x}", opc);
                ($n as u8, 1)
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

            // rotate left, bit7 into bit0 and carry
            (rlc $oper:tt $n:expr) => {{
                let mut val = load!($oper) as u8;
                let bit7 = val >> 7; 
                val = val << 1;
                val = bit7 | val;
                store!($oper, val);
                self.regs.f.z = val == 0;
                self.regs.f.n = false;
                self.regs.f.h = false;
                self.regs.f.c = bit7 == 1;
                //println!("{} bytes: rlc {}", stringify!($n), stringify!($oper));
                ($n as u8)
            }};

            // rotate right, bit0 into bit7 and carry
            (rrc $oper:tt $n:expr) => {{
                let mut val = load!($oper) as u8;
                let bit0 = val & 0x01; 
                val = val >> 1;
                val = (bit0 << 7) | val;
                store!($oper, val);
                self.regs.f.z = val == 0;
                self.regs.f.n = false;
                self.regs.f.h = false;
                self.regs.f.c = bit0 == 1;
                //println!("{} bytes: rrc {}", stringify!($n), stringify!($oper));
                ($n as u8)
            }};

            // rotate left, carry into bit0, bit7 into carry
            (rl $oper:tt $n:expr) => {{
                let mut val = load!($oper) as u8;
                let bit7 = val >> 7;
                val = val << 1;
                val = (self.regs.f.c as u8) | val;
                store!($oper, val);
                self.regs.f.z = val == 0;
                self.regs.f.n = false;
                self.regs.f.h = false;
                self.regs.f.c = bit7 == 1;
                //println!("{} bytes: rl", $n);
                ($n as u8)
            }};

            // rotate right, carry into bit7, bit0 into carry
            (rr $oper:tt $n:expr) => {{
                let mut val= load!($oper) as u8;
                let bit0 = val & 0x1;
                val = val >> 1;
                val = ((self.regs.f.c as u8) << 7) | val;
                store!($oper, val);
                self.regs.f.z = val == 0;
                self.regs.f.n = false;
                self.regs.f.h = false;
                self.regs.f.c = bit0 == 1;
                //println!("{} bytes: rra", $n);
                ($n as u8)
            }};

            // shift left, bit7 into carry
            (sla $oper:tt $n:expr) => {{
                let mut val = load!($oper) as u8;
                let bit7 = val >> 7;
                val = val << 1;
                store!($oper, val);
                self.regs.f.z = val == 0;
                self.regs.f.n = false;
                self.regs.f.h = false;
                self.regs.f.c = bit7 == 0x1;
                //println!("{} bytes: sla {}", $n, stringify!($oper));
                ($n as u8)
            }};

            // arithmetic shift right, bit0 into carry, bit 7 unchanged
            (sra $oper:tt $n:expr) => {{
                let mut val = load!($oper) as i8;
                self.regs.f.c = (val & 0x1) == 1;
                val = val >> 1;
                store!($oper, val as u8);
                self.regs.f.z = val == 0;
                self.regs.f.n = false;
                self.regs.f.h = false;
                //println!("{} bytes: sra {}", $n, stringify!($oper));
                ($n as u8)
            }};

            // swap the nibbles of register
            (swap $oper:tt $n:expr) => {{
                let mut val = load!($oper);
                let lo = val & 0xF;
                let hi = val >> 4; 
                val = lo << 4 | hi;
                store!($oper, val);
                self.regs.f.z = val == 0;
                self.regs.f.n = false;
                self.regs.f.h = false;
                self.regs.f.c = false;
                //println!("{} bytes: swap {}", $n, stringify!($oper));
                ($n as u8)
            }};

            // logical shift right, bit0 into carry
            (srl $oper:tt $n:expr) => {{
                let mut val = load!($oper);
                self.regs.f.c = (val & 0x1) == 1;
                val = val >> 1;
                store!($oper, val);
                self.regs.f.z = val == 0;
                self.regs.f.n = false;
                self.regs.f.h = false;
                //println!("{} bytes: srl {}", $n, stringify!($oper));
                ($n as u8)
            }};

            // bit n, reg. copies complement bit n of reg into z flag
            (bit $bitpos:literal $reg:tt $n:expr) => {{
                let val = load!($reg);
                let bit = (val >> $bitpos) & 0x1;
                self.regs.f.z = bit == 0;
                self.regs.f.n = false;
                self.regs.f.h = true;
                //println!("{} bytes: bit {} {}", $n, $bitpos, stringify!($reg));
                ($n as u8)
            }};

            // clear bit n of register  reg
            (res $bitpos:literal $reg:tt $n:expr) => {{
                let mut val = load!($reg);
                val = val & (!(1 << $bitpos));
                store!($reg, val);
                //println!("{} bytes: res {} {}", $n, $bitpos, stringify!($reg));
                ($n as u8)
            }};

            // set bit n of register reg
            (set $bitpos:literal $reg:tt $n:expr) => {{
                let mut val = load!($reg);
                val = val | (1 << $bitpos);
                store!($reg, val);
                //println!("{} bytes: set {} {}", $n, $bitpos, stringify!($reg));
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
                        gen_a_z80_cb_handler!($mne $opr $n);
                        // CB area always 2 byte length and 2 clock cycles
                        (2, 2)
                    }, )*
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

            (s8) => {{
                // assumes pc is a the next byte after opcode
                self.fetch8() as i8
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
                self.load8(bc)
            }};

            ((de)) => {{
                let de = self.regs.de();
                self.load8(de)
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


            (de) => {{
                self.regs.de() 
            }};

            (hl) => {{
                self.regs.hl()
            }};

            (af) => {
                self.regs.af()
            };

            (sp) => {{
                self.regs.sp
            }};

            ((hl)) => {{
                let hl = self.regs.hl();
                self.load8(hl)
            }};
            
            ((hl+)) => {{
                let hl = self.regs.hl();
                let val = self.load8(hl);
                self.regs.set_hl(hl.wrapping_add(1));
                val 
            }};

            ((hl-)) => {{
                let hl = self.regs.hl();
                let val = self.load8(hl);
                self.regs.set_hl(hl.wrapping_sub(1));
                val 
            }};

            (bc) => {{
                self.regs.bc()
            }};
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

            ((nn), sp) => {{
                let addr = self.fetch16();
                self.store8(addr, (self.regs.sp & 0xFF) as u8);
                self.store8(addr.wrapping_add(1), (self.regs.sp >> 8) as u8);
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

        let (oplen, cycles) = use_z80_table!(gen_z80_exec_handlers);
        self.stats.num_instrs_executed = self.stats.num_instrs_executed.wrapping_add(1);
        (oplen, cycles)
    }

    pub fn instructions_executed(&self) -> i32 {
        self.stats.num_instrs_executed
    }

}

impl std::fmt::Debug for LR35902Cpu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "REGS: PC: {:#06X} AF: {:#06X} BC: {:#06X} DE: {:#06X} HL: {:#06X} SP: {:#06X} SPVAL: 0x{:02X}{:02X} Flags: z:{} n:{} h:{} c:{}",
               self.regs.pc,
               self.regs.af(),
               self.regs.bc(),
               self.regs.de(),
               self.regs.hl(),
               self.regs.sp,
               self.load8(self.regs.sp.wrapping_add(1)),
               self.load8(self.regs.sp),
               if self.regs.f.z  { "1" } else { "0"},
               if self.regs.f.n  { "1" } else { "0"},
               if self.regs.f.h  { "1" } else { "0"},
               if self.regs.f.c  { "1" } else { "0"},
               )
    }
}

#[ignore]
#[test]
fn test_disasm() {
    use crate::cpu::LR35902Cpu;
    use crate::bus::Interrupts;
    use crate::ppu::Ppu;
    use crate::bootrom::BOOT_ROM;
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
    let interrupts = Shared::new(Interrupts::default());
    let mem = Shared::new(vec![0u8; 0x10000]);
    let ppu = Shared::new(Ppu::new(interrupts.clone()));
    let bus = Shared::new(Bus::new(&code_buffer, &BOOT_ROM, mem.clone(), interrupts.clone(), ppu.clone())); 
    let mut cpu = LR35902Cpu::new(0, bus.clone());
   
    // Disassemble.  simple view, does not show operand values
    while cpu.pc() < (code_buffer.len() as u16) {
        let opcode = cpu.load8(cpu.pc()); 
        let (oplen, _) = cpu.disasm(opcode);
        cpu.set_pc(cpu.pc() + (oplen as u16));
    }
  
    // Execute instructions (may hop around due to jumps/calls)
    const MAX_INSTRUCTIONS_TO_RUN : i32 = 20;
    cpu.set_pc(0); 
    while cpu.pc() < (code_buffer.len() as u16) && cpu.instructions_executed() < MAX_INSTRUCTIONS_TO_RUN {
        cpu.exec_one_instruction();
        println!("CPU:");
        println!("{:?}", cpu);
    }

}

#[ignore]
#[test]
fn test_run_gb() {
    use crate::cpu::LR35902Cpu;
    use crate::rom::read_rom;
    use crate::bus::Interrupts;
    use crate::ppu::Ppu;
    use crate::bootrom::BOOT_ROM;

    let rom = read_rom("rom/cpu_instrs.gb").unwrap();
    println!("rom size: {}", rom.len());
    println!("rom: {:?}", rom);
    let interrupts = Shared::new(Interrupts::default());
    let mem = Shared::new(vec![0u8; 0x10000]);
    let ppu = Shared::new(Ppu::new(interrupts.clone()));
    let bus = Shared::new(Bus::new(&rom, &BOOT_ROM, mem.clone(), interrupts.clone(), ppu.clone())); 
    let mut cpu = LR35902Cpu::new(0x100, bus.clone());
    println!("instructions executed: {}", cpu.instructions_executed());
    // Execute instructions (may hop around due to jumps/calls)
    const MAX_INSTRUCTIONS_TO_RUN : i32 = 99999999;
    while (cpu.pc() as i32) < (rom.len() as i32) && cpu.instructions_executed() < MAX_INSTRUCTIONS_TO_RUN {
        cpu.exec_one_instruction();
        println!("CPU:");
        println!("{:?}", cpu);
    }
}

#[ignore]
#[test]
fn test_signext() {
    // Just testing that sign extension can work for i8 to u16
    let i: i8 = -1;
    let u: u16 = i as u16;
    println!("u: {:#06X}", u);
}

#[ignore]
#[test]
fn test_interrupts() {
    use crate::util::Shared;
    use crate::bus::Interrupts;
    use crate::ppu::Ppu;
    use crate::cpu::LR35902Cpu;
    
    const MAX_INSTRUCTIONS_TO_RUN : i32 = 15;

    let main_program = [0x00, // nop
                       0x3E,    // ld a, n -- n = 0x0F
                       0x0F,    //
                       0xEA,    // ld (nn), a -- nn = 0xFFFF Interrupt Enable  
                       0xFF,
                       0xFF,
                       0x3E,    // ld a, n -- n = 0xFF set 0xFF06 = 0xFE
                       0xFE,    //
                       0xEA,    // ld (nn), a -- nn = 0xFF06  TMA
                       0x06,
                       0xFF,
                       0xFB,    // ei
                       0x03,    // inc bc
                       0x24,    // inc h
                       0x24,    // inc h
                       0x09,    // add hl, bc
                       0x01,    // ld bc, nn (where nn = 0xbabe)
                       0xbe,    // z80 is little endien so 0xbabe is 0xbe 0xba
                       0xba,
                       0x25,
                       0xCB,    // rlc b 
                       0x00,
                       0xC3,    // jp 0x0000 ; go back to beginning
                       0x00,
                       0x00,
    ]; 


    // FIXME: ISR is not at 0xFF40! starts at 0x0040
    let isr_FF40 = [
                    0x03, // inc h
                    0xD9, // RETI
    ];

    let mut code_buffer = vec![0u8; 0xFFFF];
    code_buffer[0..main_program.len()].copy_from_slice(&main_program[..]);
    code_buffer[0xFF40..0xFF40 + 2].copy_from_slice(&isr_FF40[..]);

    let interrupts = Shared::new(Interrupts::default());
    let mem = Shared::new(vec![0u8; 0x10000]);
    let ppu = Shared::new(Ppu::new(interrupts.clone()));
    let bus = Shared::new(Bus::new_without_bootrom(&code_buffer, mem.clone(), interrupts.clone(), ppu.clone())); 
    let mut cpu = LR35902Cpu::new(0x0, bus.clone());
    
    while (((cpu.pc() as i32) < (main_program.len() as i32)) ||
           // FIXME: Wrong ISR
           ((cpu.pc() as u16) == 0xFF40) || 
           ((cpu.pc() as u16) == 0xFF41)) 
        && cpu.instructions_executed() < MAX_INSTRUCTIONS_TO_RUN {
        println!("CPU BEFORE:");
        println!("{:?}", cpu);
        cpu.step();
        println!("CPU AFTER:");
        println!("{:?}", cpu);
    }


}

#[test]
fn test_op_ld_highram() {
    use crate::util::Shared;
    use crate::bus::Interrupts;
    use crate::ppu::Ppu;
    use crate::cpu::LR35902Cpu;
    const MAX_INSTRUCTIONS_TO_RUN : i32 = 15;

    let main_program = [0x00, // nop
                        0x3E, // ld a, $FE
                        0xFE,
                        0xEA, // ld ($FF90), a
                        0x90,
                        0xFF,
                        0x3E, 
                        0xDC, // ld a, $DC
                        0xEA, // ld $(FF91), a
                        0x91,
                        0xFF,
                        0x3E, // ld a, $BA
                        0xBA, 
                        0xEA, // ld $(FF92), a
                        0x92,
                        0xFF,
                        0xCE, // jp $0000
                        0x00,
                        0x00,
                       0x00,
    ]; 

    let mut code_buffer = vec![0u8; 0xFFFF];
    code_buffer[0..main_program.len()].copy_from_slice(&main_program[..]);

    let interrupts = Shared::new(Interrupts::default());
    let mem = Shared::new(vec![0u8; 0x10000]);
    let ppu = Shared::new(Ppu::new(interrupts.clone()));
    
    let bus = Shared::new(Bus::new_without_bootrom(&code_buffer, mem.clone(), interrupts.clone(), ppu.clone())); 
    let mut cpu = LR35902Cpu::new(0x0, bus.clone());
    
    while ((cpu.pc() as i32) < (main_program.len() as i32)) 
        && cpu.instructions_executed() < MAX_INSTRUCTIONS_TO_RUN {
        let opcode = cpu.load8(cpu.pc());
        let (_, disasm) = cpu.disasm(opcode);
        println!("disasm: {}", disasm);
        cpu.step();
        println!("{:?}", cpu);
    }


}
