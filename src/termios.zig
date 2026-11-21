const std = @import("std");
const Io = std.Io;
const posix = std.posix;

// The signal callback has no context pointer, so it looks up the active instance here.
var active_input: ?*const ImmediateInput = null;

const ImmediateInput = struct {
    const exit_signals = [_]posix.SIG{ .INT, .TERM };

    fd: posix.fd_t,
    settings: posix.termios,
    old_actions: [exit_signals.len]posix.Sigaction = undefined,

    fn init(fd: posix.fd_t) !ImmediateInput {
        return .{ .fd = fd, .settings = try posix.tcgetattr(fd) };
    }

    fn enable(self: *ImmediateInput) !void {
        if (active_input != null) return error.ImmediateInputAlreadyActive;
        active_input = self;

        var action: posix.Sigaction = .{
            .handler = .{ .handler = restoreOnSignal },
            .mask = posix.sigemptyset(),
            .flags = posix.SA.RESETHAND,
        };
        for (exit_signals) |signal| posix.sigaddset(&action.mask, signal);
        for (exit_signals, &self.old_actions) |signal, *old| posix.sigaction(signal, &action, old);
        errdefer self.restoreHandlers();

        var immediate = self.settings;
        immediate.lflag.ECHO = false;
        immediate.lflag.ICANON = false;
        immediate.cc[@backingInt(posix.V.MIN)] = 1;
        immediate.cc[@backingInt(posix.V.TIME)] = 0;
        try posix.tcsetattr(self.fd, .NOW, immediate);
    }

    fn deinit(self: *const ImmediateInput) void {
        posix.tcsetattr(self.fd, .NOW, self.settings) catch |err| {
            std.log.warn("could not restore terminal settings: {s}", .{@errorName(err)});
        };
        self.restoreHandlers();
    }

    fn restoreHandlers(self: *const ImmediateInput) void {
        for (exit_signals, &self.old_actions) |signal, *old| posix.sigaction(signal, old, null);
        active_input = null;
    }

    fn restoreOnSignal(signal: posix.SIG) callconv(.c) void {
        if (active_input) |state| {
            // tcsetattr is async-signal-safe. Re-raise with the default disposition.
            _ = posix.system.tcsetattr(state.fd, .NOW, &state.settings);
        }
        posix.raise(signal) catch {};
    }
};

pub const Term = struct {
    io: Io,
    ttyfile: Io.File,
    immediate_input: ?ImmediateInput = null,

    pub fn init(io: Io) !Term {
        const ttyfile = try Io.Dir.openFileAbsolute(io, "/dev/tty", .{ .mode = .read_write });
        return .{ .io = io, .ttyfile = ttyfile };
    }

    pub fn deinit(self: *Term) void {
        if (self.immediate_input) |*input| input.deinit();
        self.immediate_input = null;
        self.ttyfile.close(self.io);
    }

    /// Keep this Term at the same address until deinit.
    pub fn enableImmediateInput(self: *Term) !void {
        if (self.immediate_input != null) return;
        self.immediate_input = try ImmediateInput.init(self.ttyfile.handle);
        errdefer self.immediate_input = null;
        try self.immediate_input.?.enable();
    }

    pub fn get_char(self: *Term) !u16 {
        var buffer: [1]u8 = undefined;
        while (try self.ttyfile.readStreaming(self.io, &.{&buffer}) == 0) {}
        return buffer[0];
    }

    pub fn put_char(self: *Term, char: u8) !void {
        try self.put_string(&.{char});
    }

    pub fn put_string(self: *Term, str: []const u8) !void {
        try self.ttyfile.writeStreamingAll(self.io, str);
    }

    pub fn check_key(self: *Term) bool {
        var pollfds = [_]posix.pollfd{
            .{ .fd = self.ttyfile.handle, .events = posix.POLL.IN, .revents = 0 },
        };

        const timeout = -1;

        const poll = posix.poll(&pollfds, timeout) catch {
            return false;
        };

        return poll > 0;
    }
};
