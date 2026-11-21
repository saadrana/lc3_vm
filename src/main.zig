const std = @import("std");
const vm = @import("lc3.zig");
const termios = @import("termios.zig");

pub fn main(init: std.process.Init) !void {
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len != 2) {
        std.debug.print("Usage: lc3 <rom.obj>\n", .{});
        std.process.exit(2);
    }

    var term = try termios.Term.init(init.io);
    defer term.deinit();

    var lc3 = vm.LC3.init(&term);
    try lc3.load_rom(init.io, args[1]);
    try term.enableImmediateInput();
    try lc3.start();
}
