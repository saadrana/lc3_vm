const std = @import("std");
const os = std.os;

pub const Term = struct {
    buffer: [1]u8,
    ttyfile: std.fs.File,
    termios: os.termios,

    pub fn init() !Term {
        var ttyfile = try std.fs.openFileAbsolute("/dev/tty", .{ .mode = .read_write });
        errdefer ttyfile.close();
        const termios = try os.tcgetattr(ttyfile.handle);

        return .{ .buffer = undefined, .ttyfile = ttyfile, .termios = termios };
    }

    pub fn get_char(self: *Term) !u16 {
        _ = try self.ttyfile.read(self.buffer[0..]);

        return @as(u16, self.buffer[0]);
    }

    pub fn put_char(self: *Term, char: u8) !void {
        try self.ttyfile.writeAll(&[1]u8{char});
    }

    pub fn put_string(self: *Term, str: []const u8) !void {
        try self.ttyfile.writeAll(str);
    }

    pub fn check_key(self: *Term) bool {
        var pollfds = [_]std.os.pollfd{
            .{ .fd = self.ttyfile.handle, .events = std.os.POLL.IN, .revents = 0 },
        };

        const timeout = -1;

        const poll = std.os.poll(&pollfds, timeout) catch {
            return false;
        };

        return poll > 0;
    }

    pub fn restore_input_buffering(self: *Term) !void {
        try os.tcsetattr(self.ttyfile.handle, os.TCSA.NOW, self.termios);
    }

    pub fn disable_input_buffering(self: *Term) !void {
        self.termios = try os.tcgetattr(self.ttyfile.handle);
        var new_termios = self.termios;
        new_termios.lflag &= ~(os.linux.ECHO | os.linux.ICANON);
        try os.tcsetattr(self.ttyfile.handle, os.TCSA.NOW, new_termios);
    }
};
