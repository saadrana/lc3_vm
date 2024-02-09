const vm = @import("lc3.zig");
const termios = @import("termios.zig");

pub fn main() !void {
    var term = try termios.Term.init();
    var lc3 = vm.LC3.init(&term);
    try lc3.load_rom("src/roms/2048.obj");
    try lc3.start();
}
