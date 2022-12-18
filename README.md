# Gameboy Color Emulator
This is a gameboy emulator written in Rust just for fun
It uses the excellent [egui](https://github.com/emilk/egui/) framework for UI.

# How to run it.
The emulator runs from the command-line. 

Rom is selected via --rom argument

--rom <ROM FILE>

```code
cargo run --release --bin gbc-emu-egui -- --rom ROM.gb
```

## Input
Up/Down/Left/Right = Arrow keys

A button = A Key

B button = B Key

Start = Enter Key 

Select = Spacebar

# Resources
Here are the resources that have been used while developing the emulator.

* [Pan's Docs](https://gbdev.io/pandocs/) - The one and only, describes the hardware and behavior in detial
* [Gameboy Instructions Table](https://meganesu.github.io/generate-gb-opcodes/) - A periodic table like diagram of Gameboy Z80 instructions, easy to navigate and has good description of instruction behavior
* [RGBDS](https://rgbds.gbdev.io/docs/v0.4.2/gbz80.7/#LEGEND) - RGBDS Instruction Description are also very helpful to understand what cpu flags are set.


# Credit
cpu_instr.gb test rom is from [retrio's gb-test-rom](https://github.com/retrio/gb-test-roms)
mts test roms are from [MoonEye Test Roms](https://github.com/Gekkio/mooneye-test-suite)

