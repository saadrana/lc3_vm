const std = @import("std");
const Io = std.Io;

const bit_ops = @import("bit_ops.zig");
const termios = @import("termios.zig");

const MAX_MEMORY = 1 << 16;
const PC_START = 0x3000;

const R = enum(u16) {
    R0 = 0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    PC, // program counter
    COND,
    COUNT,
};

const FL = enum(u3) {
    POS = 1 << 0,
    ZRO = 1 << 1,
    NEG = 1 << 2,
};

const OP = enum(u4) {
    BR = 0, // branch
    ADD, // add
    LD, // load
    ST, // store
    JSR, // jump register
    AND, // bitwise and
    LDR, // load register
    STR, // store register
    RTI, // unused
    NOT, // bitwise not
    LDI, // load indirect
    STI, // store indirect
    JMP, // jump
    RES, // reserved (unused)
    LEA, // load effective address
    TRAP, // execute trap
};

const TRAP = enum(u8) {
    GETC = 0x20, // get character from keyboard, not echoed onto the terminal
    OUT = 0x21, // output a character
    PUTS = 0x22, // output a word string
    IN = 0x23, // get character from keyboard, echoed onto the terminal
    PUTSP = 0x24, // output a byte string
    HALT = 0x25, // halt the program
};

const MR = enum(u16) {
    KBSR = 0xFE00, // keyboard status
    KBDR = 0xFE02, // keyboard data
};

pub const LC3 = struct {
    const Self = @This();
    memory: [MAX_MEMORY]u16 = undefined,
    reg: [@backingInt(R.COUNT)]u16 = undefined,
    running: bool = false,
    term: *termios.Term,

    pub fn init(term: *termios.Term) Self {
        return .{
            .term = term,
        };
    }

    pub fn reset(self: *Self) void {
        self.running = false;

        self.memory = @splat(0);
        self.reg = @splat(0);

        self.reg[@backingInt(R.PC)] = PC_START;
        self.reg[@backingInt(R.COND)] = @backingInt(FL.ZRO);
    }

    pub fn load_rom(self: *Self, io: Io, rom_path: []const u8) !void {
        const file = try Io.Dir.cwd().openFile(io, rom_path, .{});
        defer file.close(io);

        var buffer: [4096]u8 = undefined;
        var reader = file.reader(io, &buffer);
        try self.load_image(&reader.interface);
    }

    fn load_image(self: *Self, reader: *Io.Reader) !void {
        self.reset();

        const origin = try reader.takeInt(u16, .big);
        self.reg[@backingInt(R.PC)] = origin;
        var offset: usize = origin;
        var word: [2]u8 = undefined;

        while (true) : (offset += 1) {
            switch (try reader.readSliceShort(&word)) {
                0 => return,
                1 => return error.TruncatedRom,
                else => {},
            }
            if (offset >= self.memory.len) return error.RomTooLarge;
            self.memory[offset] = std.mem.readInt(u16, &word, .big);
        }
    }

    pub fn start(self: *Self) !void {
        self.running = true;
        defer self.running = false;

        while (self.running) {
            const instr = self.memory[self.reg[@backingInt(R.PC)]];
            self.reg[@backingInt(R.PC)] +%= 1;

            const opcode: OP = @fromBackingInt(@intCast(instr >> 12)); // only the 4 leftmost bits are the opcode

            try switch (opcode) {
                .ADD => self.op_add(instr),
                .AND => self.op_and(instr),
                .BR => self.op_br(instr),
                .JMP => self.op_jmp(instr),
                .JSR => self.op_jsr(instr),
                .LD => self.op_ld(instr),
                .LDI => self.op_ldi(instr),
                .LDR => self.op_ldr(instr),
                .LEA => self.op_lea(instr),
                .NOT => self.op_not(instr),
                .RTI => error.UnsupportedRTI,
                .ST => self.op_st(instr),
                .STI => self.op_sti(instr),
                .STR => self.op_str(instr),
                .RES => error.IllegalOpcode,
                .TRAP => self.op_trap(instr),
            };
        }
    }

    pub fn pause(self: *Self) void {
        self.running = false;
    }

    pub fn play(self: *Self) void {
        self.running = true;
    }

    fn mem_write(self: *Self, addr: u16, val: u16) void {
        self.memory[addr] = val;
    }

    fn mem_read(self: *Self, addr: u16) !u16 {
        if (addr == @backingInt(MR.KBSR)) {
            if (self.term.check_key()) {
                self.memory[@backingInt(MR.KBSR)] = (1 << 15);
                self.memory[@backingInt(MR.KBDR)] = try self.term.get_char();
            } else {
                self.memory[@backingInt(MR.KBSR)] = 0;
            }
        }
        return self.memory[addr];
    }

    fn update_flag(self: *Self, r: u16) void {
        if (self.reg[r] == 0) {
            self.reg[@backingInt(R.COND)] = @backingInt(FL.ZRO);
        } else if (self.reg[r] >> 15 == 1) {
            self.reg[@backingInt(R.COND)] = @backingInt(FL.NEG);
        } else {
            self.reg[@backingInt(R.COND)] = @backingInt(FL.POS);
        }
    }

    fn op_add(self: *Self, instr: u16) void {
        // destination register DR
        const dr = (instr >> 9) & 0x7;
        // first operand SR1
        const sr1 = (instr >> 6) & 0x7;
        // check if immediate mode
        const imm_flag = (instr >> 5) & 0x1;

        if (imm_flag == 0) {
            const sr2 = instr & 0x7;
            self.reg[dr] = self.reg[sr1] +% self.reg[sr2];
        } else {
            const imm5 = bit_ops.sign_extend(instr & 0x1F, 5);
            self.reg[dr] = self.reg[sr1] +% imm5;
        }

        self.update_flag(dr);
    }

    fn op_and(self: *Self, instr: u16) void {
        // destination register DR
        const dr = (instr >> 9) & 0x7;
        // first operand SR1
        const sr1 = (instr >> 6) & 0x7;
        // check if immediate mode
        const imm_flag = (instr >> 5) & 0x1;

        if (imm_flag == 0) {
            const sr2 = instr & 0x7;
            self.reg[dr] = self.reg[sr1] & self.reg[sr2];
        } else {
            const imm5 = bit_ops.sign_extend(instr & 0x1F, 5);
            self.reg[dr] = self.reg[sr1] & imm5;
        }

        self.update_flag(dr);
    }

    fn op_br(self: *Self, instr: u16) void {
        // we will only brnach if the nzp condition flag matches the condition flag in the register
        const pc_offset = bit_ops.sign_extend(instr & 0x1FF, 9);
        const cond_code = (instr >> 9) & 0x7;
        if ((cond_code & self.reg[@backingInt(R.COND)]) != 0) {
            self.reg[@backingInt(R.PC)] +%= pc_offset;
        }
    }

    fn op_jmp(self: *Self, instr: u16) void {
        const br = (instr >> 6) & 0x7;
        self.reg[@backingInt(R.PC)] = self.reg[br];
    }

    fn op_jsr(self: *Self, instr: u16) void {
        // this bit will determine if the pc will be set or offset
        const mode_flag = (instr >> 11) & 0x1;
        // save checkpoint of current index of pc
        self.reg[@backingInt(R.R7)] = self.reg[@backingInt(R.PC)];
        if (mode_flag == 0) {
            const br = (instr >> 6) & 0x7;
            self.reg[@backingInt(R.PC)] = self.reg[br]; // JSSR
        } else {
            const pc_offset = bit_ops.sign_extend(instr & 0x7FF, 11);
            self.reg[@backingInt(R.PC)] +%= pc_offset; // JSR
        }
    }

    fn op_ld(self: *Self, instr: u16) !void {
        const dr = (instr >> 9) & 0x7;
        const pc_offset = bit_ops.sign_extend(instr & 0x1FF, 9);
        // use the offset address, to move data from memory to DR
        self.reg[dr] = try self.mem_read(self.reg[@backingInt(R.PC)] +% pc_offset);

        self.update_flag(dr);
    }

    fn op_ldi(self: *Self, instr: u16) !void {
        const dr = (instr >> 9) & 0x7;
        const pc_offset = bit_ops.sign_extend(instr & 0x1FF, 9);
        // take the address at one location in memory, to access another location, and store data in DR
        self.reg[dr] = try self.mem_read(try self.mem_read(self.reg[@backingInt(R.PC)] +% pc_offset));

        self.update_flag(dr);
    }

    fn op_ldr(self: *Self, instr: u16) !void {
        const dr = (instr >> 9) & 0x7;
        const br = (instr >> 6) & 0x7;
        const pc_offset = bit_ops.sign_extend(instr & 0x3F, 6);

        self.reg[dr] = try self.mem_read(self.reg[br] +% pc_offset);

        self.update_flag(dr);
    }

    fn op_lea(self: *Self, instr: u16) void {
        const dr = (instr >> 9) & 0x7;
        const pc_offset = bit_ops.sign_extend(instr & 0x1FF, 9);

        self.reg[dr] = self.reg[@backingInt(R.PC)] +% pc_offset;

        self.update_flag(dr);
    }

    fn op_not(self: *Self, instr: u16) void {
        const dr = (instr >> 9) & 0x7;
        const sr = (instr >> 6) & 0x7;

        self.reg[dr] = ~self.reg[sr];

        self.update_flag(dr);
    }

    fn op_st(self: *Self, instr: u16) void {
        const sr = (instr >> 9) & 0x7;
        const pc_offset = bit_ops.sign_extend(instr & 0x1FF, 9);

        const addr = self.reg[@backingInt(R.PC)] +% pc_offset;
        const val = self.reg[sr];

        self.mem_write(addr, val);
    }

    fn op_sti(self: *Self, instr: u16) !void {
        const sr = (instr >> 9) & 0x7;
        const pc_offset = bit_ops.sign_extend(instr & 0x1FF, 9);

        const addr = try self.mem_read(self.reg[@backingInt(R.PC)] +% pc_offset);
        const val = self.reg[sr];

        self.mem_write(addr, val);
    }

    fn op_str(self: *Self, instr: u16) void {
        const sr = (instr >> 9) & 0x7;
        const br = (instr >> 6) & 0x7;
        const pc_offset = bit_ops.sign_extend(instr & 0x3F, 6);

        const addr = self.reg[br] +% pc_offset;
        const val = self.reg[sr];

        self.mem_write(addr, val);
    }

    fn op_trap(self: *Self, instr: u16) !void {
        self.reg[@backingInt(R.R7)] = self.reg[@backingInt(R.PC)];
        const trap_code: TRAP = @fromBackingInt(@intCast(instr & 0xFF));
        switch (trap_code) {
            .GETC => try self.trap_getc(),
            .OUT => try self.trap_out(),
            .PUTS => try self.trap_puts(),
            .IN => try self.trap_in(),
            .PUTSP => try self.trap_putsp(),
            .HALT => try self.trap_halt(),
        }
    }

    fn trap_getc(self: *Self) !void {
        const char = try self.term.get_char();

        self.reg[@backingInt(R.R0)] = @as(u16, char);

        self.update_flag(@backingInt(R.R0));
    }

    fn trap_out(self: *Self) !void {
        const char = self.reg[@backingInt(R.R0)];
        try self.term.put_char(@intCast(char));
        // try self.term.flush();
    }

    fn trap_puts(self: *Self) !void {
        var index = self.reg[@backingInt(R.R0)];
        var char = try self.mem_read(index);

        while (char != 0x0000) : ({
            index +%= 1;
            char = try self.mem_read(index);
        }) {
            try self.term.put_char(@intCast(char));
        }

        // try self.term.flush();
    }

    fn trap_in(self: *Self) !void {
        try self.term.put_string("Enter a character: ");

        const char = try self.term.get_char();

        // try self.term.flush();

        self.reg[@backingInt(R.R0)] = @as(u16, char);
        self.update_flag(@backingInt(R.R0));
    }

    fn trap_putsp(self: *Self) !void {
        var index = self.reg[@backingInt(R.R0)];
        var char = try self.mem_read(index);

        while (char != 0x0000) : ({
            index +%= 1;
            char = try self.mem_read(index);
        }) {
            const char1: u8 = @intCast(char & 0xFF);
            try self.term.put_char(char1);

            const char2: u8 = @intCast(char >> 8);
            if (char2 != 0) {
                try self.term.put_char(char2);
            }
        }

        // try self.term.flush();
    }

    fn trap_halt(self: *Self) !void {
        try self.term.put_string("HALT");
        // try self.term.flush();

        self.running = false;
    }
};

test "load an LC-3 image at the origin in its header" {
    var term: termios.Term = undefined;
    var vm = LC3.init(&term);
    var reader = Io.Reader.fixed(&.{ 0x40, 0x00, 0x12, 0x34, 0xF0, 0x25 });

    try vm.load_image(&reader);

    try std.testing.expectEqual(@as(u16, 0x4000), vm.reg[@backingInt(R.PC)]);
    try std.testing.expectEqualSlices(u16, &.{ 0x1234, 0xF025 }, vm.memory[0x4000..0x4002]);
}

test "reject truncated and oversized images" {
    var term: termios.Term = undefined;
    var vm = LC3.init(&term);
    var truncated = Io.Reader.fixed(&.{ 0x30, 0x00, 0xF0 });
    try std.testing.expectError(error.TruncatedRom, vm.load_image(&truncated));

    var oversized = Io.Reader.fixed(&.{ 0xFF, 0xFF, 0xF0, 0x25, 0x00, 0x00 });
    try std.testing.expectError(error.RomTooLarge, vm.load_image(&oversized));
}

test "negative PC-relative offsets use 16-bit arithmetic" {
    var term: termios.Term = undefined;
    var vm = LC3.init(&term);
    vm.reset();

    vm.op_br(0x05FF); // BRz #-1

    try std.testing.expectEqual(@as(u16, PC_START - 1), vm.reg[@backingInt(R.PC)]);
}

test "RTI and reserved opcodes stop execution with distinct errors" {
    var term: termios.Term = undefined;
    var vm = LC3.init(&term);

    vm.reset();
    vm.memory[PC_START] = 0x8000; // RTI
    try std.testing.expectError(error.UnsupportedRTI, vm.start());
    try std.testing.expect(!vm.running);

    vm.reset();
    vm.memory[PC_START] = 0xD000; // Reserved
    try std.testing.expectError(error.IllegalOpcode, vm.start());
    try std.testing.expect(!vm.running);
}
