# Gameboy Color Emulator
This is a gameboy emulator written in Rust just for fun
It uses the excellent [egui](https://github.com/emilk/egui/) framework for UI.

# How to run it.
The emulator runs from the command-line. 

Rom is selected via --rom argument

--rom <ROM FILE>

## Input
Up/Down/Left/Right = Arrow keys
A button = A Key
B button = B Key
Start = Enter Key 
Select = Spacebar

```code
cargo run --release --bin gbc-emu-egui -- --rom ROM.gb
```

# Credit
cpu_instr.gb test rom is from [retrio's gb-test-rom](https://github.com/retrio/gb-test-roms)
mts test roms are from [MoonEye Test Roms](https://github.com/Gekkio/mooneye-test-suite)
