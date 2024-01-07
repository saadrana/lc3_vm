const std = @import("std");

pub const Term = struct {
    buffer: [1]u8 = undefined,

    pub fn init() Term {
        return .{};
    }

    pub fn get_char(self: *Term) !u8 {
        const reader = std.io.getStdIn().reader();

        _ = try reader.read(self.buffer[0..]);

        return self.buffer[0];
    }

    pub fn put_char(self: *Term, char: u8) !void {
        _ = self;
        const writer = std.io.getStdOut().writer();
        try writer.writeByte(char);
        try writer.flush();
    }

    pub fn put_string(self: *Term, str: []const u8) !void {
        _ = self;
        const writer = std.io.getStdOut().writer();
        try writer.writeAll(str);
        try writer.flush();
    }
};
