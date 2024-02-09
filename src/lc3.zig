const std = @import("std");
const fs = std.fs;
const mem = std.mem;
const assert = std.debug.assert;
const expect = std.testing.expect;

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
    memory: [MAX_MEMORY]u16 = undefined,
    reg: [@intFromEnum(R.COUNT)]u16 = undefined,
    running: bool = false,
    term: *termios.Term,

    pub fn init(term: *termios.Term) LC3 {
        return .{
            .term = term,
        };
    }

    pub fn reset(self: *LC3) void {
        self.running = false;

        self.memory = [_]u16{0} ** MAX_MEMORY;
        self.reg = [_]u16{0} ** @intFromEnum(R.COUNT);

        self.reg[@intFromEnum(R.PC)] = PC_START;
        self.reg[@intFromEnum(R.COND)] = @intFromEnum(FL.ZRO);
    }

    pub fn load_rom(self: *LC3, rom_path: []const u8) !void {
        self.reset();

        const file = try std.fs.cwd().openFile(rom_path, .{});
        defer file.close();

        var reader = file.reader();

        var buffer: [2]u8 = undefined;
        var offset: usize = PC_START;

        while (try reader.read(&buffer) != 0) : (offset += 1) {
            self.memory[offset] = std.mem.readVarInt(u16, buffer[0..], std.builtin.Endian.big);
        }
    }

    pub fn start(self: *LC3) !void {
        self.running = true;

        while (self.running) {
            const instr = self.memory[self.reg[@intFromEnum(R.PC)]];
            self.reg[@intFromEnum(R.PC)] += 1;

            const opcode: OP = @enumFromInt(instr >> 12); // only the 4 leftmost bits are the opcode

            std.debug.print("opcode: {}\n", .{opcode});
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
                .RTI => op_rti(),
                .ST => self.op_st(instr),
                .STI => self.op_sti(instr),
                .STR => self.op_str(instr),
                .RES => op_res(),
                .TRAP => self.op_trap(instr),
            };
        }
    }

    pub fn pause(self: *LC3) void {
        self.running = false;
    }

    pub fn play(self: *LC3) void {
        self.running = true;
    }

    fn mem_write(self: *LC3, addr: u16, val: u16) void {
        self.memory[addr] = val;
    }

    fn mem_read(self: *LC3, addr: u16) !u16 {
        if (addr == @intFromEnum(MR.KBSR)) {
            if (self.term.check_key()) {
                self.memory[@intFromEnum(MR.KBSR)] = (1 << 15);
                self.memory[@intFromEnum(MR.KBDR)] = try self.term.get_char();
            } else {
                self.memory[@intFromEnum(MR.KBSR)] = 0;
            }
        }
        return self.memory[addr];
    }

    fn update_flag(self: *LC3, r: u16) void {
        if (self.reg[r] == 0) {
            self.reg[@intFromEnum(R.COND)] = @intFromEnum(FL.ZRO);
        } else if (self.reg[r] >> 15 == 1) {
            self.reg[@intFromEnum(R.COND)] = @intFromEnum(FL.NEG);
        } else {
            self.reg[@intFromEnum(R.COND)] = @intFromEnum(FL.POS);
        }
    }

    fn op_add(self: *LC3, instr: u16) void {
        // destination register DR
        const dr = (instr >> 9) & 0x7;
        // first operand SR1
        const sr1 = (instr >> 6) & 0x7;
        // check if immediate mode
        const imm_flag = (instr >> 5) & 0x1;

        if (imm_flag == 0) {
            const sr2 = instr & 0x7;
            self.reg[dr] = self.reg[sr1] + self.reg[sr2];
        } else {
            const imm5 = bit_ops.sign_extend(instr & 0x1F, 5);
            self.reg[dr] = self.reg[sr1] + imm5;
        }

        self.update_flag(dr);
    }

    fn op_and(self: *LC3, instr: u16) void {
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

    fn op_br(self: *LC3, instr: u16) void {
        // we will only brnach if the nzp condition flag matches the condition flag in the register
        const pc_offset = bit_ops.sign_extend(instr & 0x1FF, 9);
        const cond_code = (instr >> 9) & 0x7;
        if ((cond_code & self.reg[@intFromEnum(R.COND)]) != 0) {
            self.reg[@intFromEnum(R.PC)] += pc_offset;
        }
    }

    fn op_jmp(self: *LC3, instr: u16) void {
        const br = (instr >> 6) & 0x7;
        self.reg[@intFromEnum(R.PC)] = self.reg[br];
    }

    fn op_jsr(self: *LC3, instr: u16) void {
        // this bit will determine if the pc will be set or offset
        const mode_flag = (instr >> 11) & 0x1;
        // save checkpoint of current index of pc
        self.reg[@intFromEnum(R.R7)] = self.reg[@intFromEnum(R.PC)];
        if (mode_flag == 0) {
            const br = (instr >> 6) & 0x7;
            self.reg[@intFromEnum(R.PC)] = self.reg[br]; // JSSR
        } else {
            const pc_offset = bit_ops.sign_extend(instr & 0x7FF, 11);
            self.reg[@intFromEnum(R.PC)] += pc_offset; // JSR
        }
    }

    fn op_ld(self: *LC3, instr: u16) !void {
        const dr = (instr >> 9) & 0x7;
        const pc_offset = bit_ops.sign_extend(instr & 0x1FF, 9);
        // use the offset address, to move data from memory to DR
        self.reg[dr] = try self.mem_read(self.reg[@intFromEnum(R.PC)] + pc_offset);

        self.update_flag(dr);
    }

    fn op_ldi(self: *LC3, instr: u16) !void {
        const dr = (instr >> 9) & 0x7;
        const pc_offset = bit_ops.sign_extend(instr & 0x1FF, 9);
        // take the address at one location in memory, to access another location, and store data in DR
        self.reg[dr] = try self.mem_read(try self.mem_read(self.reg[@intFromEnum(R.PC)] + pc_offset));

        self.update_flag(dr);
    }

    fn op_ldr(self: *LC3, instr: u16) !void {
        const dr = (instr >> 9) & 0x7;
        const br = (instr >> 6) & 0x7;
        const pc_offset = bit_ops.sign_extend(instr & 0x3F, 6);

        self.reg[dr] = try self.mem_read(self.reg[br] + pc_offset);

        self.update_flag(dr);
    }

    fn op_lea(self: *LC3, instr: u16) void {
        const dr = (instr >> 9) & 0x7;
        const pc_offset = bit_ops.sign_extend(instr & 0x1FF, 9);

        self.reg[dr] = self.reg[@intFromEnum(R.PC)] + pc_offset;

        self.update_flag(dr);
    }

    fn op_not(self: *LC3, instr: u16) void {
        const dr = (instr >> 9) & 0x7;
        const sr = (instr >> 6) & 0x7;

        self.reg[dr] = ~self.reg[sr];

        self.update_flag(dr);
    }

    fn op_rti() void {
        std.debug.panic("unused op code", .{});
    }

    fn op_st(self: *LC3, instr: u16) void {
        const sr = (instr >> 9) & 0x7;
        const pc_offset = bit_ops.sign_extend(instr & 0x1FF, 9);

        const addr = self.reg[@intFromEnum(R.PC)] + pc_offset;
        const val = self.reg[sr];

        self.mem_write(addr, val);
    }

    fn op_sti(self: *LC3, instr: u16) !void {
        const sr = (instr >> 9) & 0x7;
        const pc_offset = bit_ops.sign_extend(instr & 0x1FF, 9);

        const addr = try self.mem_read(self.reg[@intFromEnum(R.PC)] + pc_offset);
        const val = self.reg[sr];

        self.mem_write(addr, val);
    }

    fn op_str(self: *LC3, instr: u16) void {
        const sr = (instr >> 9) & 0x7;
        const br = (instr >> 6) & 0x7;
        const pc_offset = bit_ops.sign_extend(instr & 0x3F, 6);

        const addr = self.reg[br] + pc_offset;
        const val = self.reg[sr];

        self.mem_write(addr, val);
    }

    fn op_res() void {
        std.debug.panic("unused op code", .{});
    }

    fn op_trap(self: *LC3, instr: u16) !void {
        self.reg[@intFromEnum(R.R7)] = self.reg[@intFromEnum(R.PC)];
        const trap_code: TRAP = @enumFromInt(instr & 0xFF);
        std.debug.print("trap: {}\n", .{trap_code});
        switch (trap_code) {
            .GETC => try self.trap_getc(),
            .OUT => try self.trap_out(),
            .PUTS => try self.trap_puts(),
            .IN => try self.trap_in(),
            .PUTSP => try self.trap_putsp(),
            .HALT => try self.trap_halt(),
        }
    }

    fn trap_getc(self: *LC3) !void {
        const char = try self.term.get_char();

        self.reg[@intFromEnum(R.R0)] = @as(u16, char);

        self.update_flag(@intFromEnum(R.R0));
    }

    fn trap_out(self: *LC3) !void {
        const char = self.reg[@intFromEnum(R.R0)];
        try self.term.put_char(@intCast(char));
        // try self.term.flush();
    }

    fn trap_puts(self: *LC3) !void {
        var index = self.reg[@intFromEnum(R.R0)];
        var char = try self.mem_read(index);

        while (char != 0x0000) : ({
            index += 1;
            char = try self.mem_read(index);
        }) {
            try self.term.put_char(@intCast(char));
        }

        // try self.term.flush();
    }

    fn trap_in(self: *LC3) !void {
        try self.term.put_string("Enter a character: ");

        const char = try self.term.get_char();

        // try self.term.flush();

        self.reg[@intFromEnum(R.R0)] = @as(u16, char);
        self.update_flag(@intFromEnum(R.R0));
    }

    fn trap_putsp(self: *LC3) !void {
        var index = self.reg[@intFromEnum(R.R0)];
        var char = try self.mem_read(index);

        while (char != 0x0000) : ({
            index += 1;
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

    fn trap_halt(self: *LC3) !void {
        try self.term.put_string("HALT");
        // try self.term.flush();

        self.running = false;
    }
};
