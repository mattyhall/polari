const std = @import("std");

/// Local is a local variable. It has a name.
pub const Local = struct {
    name: []const u8,

    fn print(self: Local, w: anytype) !void {
        try w.print("{s}", .{self.name});
    }
};

/// Locals store the local variables in the chunk.
pub const Locals = std.ArrayListUnmanaged(Local);

/// Object represents boxed values.
///
/// NOTE: Defined as a struct with one field because when we implement a GC then we will need to add further fields.
pub const Object = struct {
    v: union(enum) {
        function: struct { chunk: Chunk, arity: u32, name: []const u8 },
    },

    fn print(self: *Object, w: anytype) !void {
        switch (self.v) {
            .function => |f| try w.print("<func {s}>", .{f.name}),
        }
    }

    fn deinit(self: *Object) void {
        switch (self.v) {
            .function => |*f| {
                var gpa = f.chunk.gpa;
                f.chunk.deinit();
                gpa.destroy(f);
            },
        }
    }
};

pub const Value = union(enum) {
    integer: i64,
    boolean: bool,
    object: *Object,

    pub fn print(self: Value, w: anytype) !void {
        switch (self) {
            .integer => |i| try w.print("{}", .{i}),
            .boolean => |b| try w.print("{}", .{b}),
            .object => |o| try o.print(w),
        }
    }

    pub fn deinit(self: *Value) void {
        switch (self.*) {
            .object => |o| o.deinit(),
            else => {},
        }
    }
};

comptime {
    // Need 8 bytes for a pointer/i64, u2 for the tag and it will then be aligned to 8byte boundaries
    std.debug.assert(@sizeOf(Value) == 16);
}

/// Op lists the most common bytecode operations.
pub const Op = enum(u8) {
    /// CONST a: a gives the index into the constants array to push onto the stack.
    const8,

    /// GET a: a gives the index of a value in this chunk's stack to push onto the top.
    get8,
    // GETA a: a gives the index into the entire stack to push onto the top.
    geta8,
    /// SET a: sets the local with index a to the value on the top of the stack.
    set8,
    /// POP: pops a single stack value.
    pop,
    /// POPN n: pops n values off the stack.
    pop_n,

    /// ONE: pushes 1 onto the stack.
    one,
    /// NONE: pushes -1 onto the stack.
    neg_one,

    /// TRUE: adds true to the stack.
    true,
    /// FALSE: adds false to the stack.
    false,

    /// ADD: adds the last two values on the stack and pushes the result onto the stack.
    add,
    /// SUB: substracts the last value on the stack to the penultimate value on the stack.
    subtract,
    /// MULT: multiplies the last two values on the stack and pushes the result onto the stack.
    multiply,
    /// DIV: divides the penultimate value on the stack by the last value on the stack and pushes the result onto the
    /// stack.
    divide,

    /// NEG: negates the last value on the stack and pushes the result onto the stack.
    negate,

    /// EQ: checks whether the last two values on the stack are equal and pushes the result onto the stack.
    eq,
    /// NEQ: checks whether the last two values on the stack are equal and pushes the result onto the stack.
    neq,
    /// LT: checks whether the penultimate value on the stack is less than the last value on the stack pushes the
    /// result onto the stack.
    lt,
    /// LTE: checks whether the penultimate value on the stack is less than or equal to the last value on the stack
    /// pushes the result onto the stack.
    lte,
    /// LT: checks whether the penultimate value on the stack is greater than the last value on the stack pushes the
    /// result onto the stack.
    gt,
    /// GTE: checks whether the penultimate value on the stack is greater than or equal to the last value on the stack
    /// pushes the result onto the stack.
    gte,

    /// CALL n: calls the function n values down on the stack.
    ///
    /// NOTE: n is the number of arguments. The arguments come in left to right order on the stack, after the function
    /// to call.
    call,
    /// RET: return from function, removing the function off the stack.
    ret,

    /// JMP a: jump to bytecode position a.
    jmp8,
    /// JMPF a: if value on the top of the stack is false then jumps to bytecode position a.
    jmpf8,

    /// RARE a: a is a RareOp.
    rare,

    pub fn print(self: Op, writer: anytype) !void {
        var s: struct { op: []const u8, rest: []const u8 } = switch (self) {
            .const8 => .{ .op = "CONST", .rest = " c" },
            .get8 => .{ .op = "GET", .rest = " l" },
            .geta8 => .{ .op = "GETA", .rest = " l" },
            .set8 => .{ .op = "SET", .rest = " l" },
            .pop => .{ .op = "POP", .rest = "" },
            .pop_n => .{ .op = "POPN", .rest = "  " },
            .one => .{ .op = "ONE", .rest = "" },
            .neg_one => .{ .op = "NONE", .rest = "" },
            .true => .{ .op = "TRUE", .rest = "" },
            .false => .{ .op = "FALSE", .rest = "" },
            .add => .{ .op = "ADD", .rest = "" },
            .subtract => .{ .op = "SUB", .rest = "" },
            .multiply => .{ .op = "MULT", .rest = "" },
            .divide => .{ .op = "DIV", .rest = "" },
            .negate => .{ .op = "NEG", .rest = "" },
            .eq => .{ .op = "EQ", .rest = "" },
            .neq => .{ .op = "NEQ", .rest = "" },
            .lt => .{ .op = "LT", .rest = "" },
            .lte => .{ .op = "LTE", .rest = "" },
            .gt => .{ .op = "GT", .rest = "" },
            .gte => .{ .op = "GTE", .rest = "" },
            .call => .{ .op = "CALL", .rest = "  " },
            .ret => .{ .op = "RET", .rest = "" },
            .jmp8 => .{ .op = "JMP", .rest = " p" },
            .jmpf8 => .{ .op = "JMPF", .rest = " p" },
            .rare => return,
        };

        try writer.print("{s:<6}{s}", .{ s.op, s.rest });
    }
};

/// RareOp is for operations that should not be commonly used, e.g. getting locals/globals with large indices.
pub const RareOp = enum(u8) {
    /// CONST a: a gives the index into the constants array to push onto the stack.
    const16,
    const32,

    /// GET a: a gives the index into the stack to push onto the top.
    get16,
    get32,

    // GETA a: a gives the index into the entire stack to push onto the stop.
    geta16,
    geta32,

    /// SET a: sets the local with index a to the value on the top of the stack.
    set16,
    set32,

    /// JMP a: jump to bytecode position a.
    jmp16,
    jmp32,

    /// JMPF a: if value on the top of the stack is false then jumps to bytecode position a.
    jmpf16,
    jmpf32,

    pub fn print(self: RareOp, writer: anytype) !void {
        var s: struct { op: []const u8, rest: []const u8 } = switch (self) {
            .const16, .const32 => .{ .op = "CONST", .rest = " c" },
            .get16, .get32 => .{ .op = "GET", .rest = " l" },
            .geta16, .geta32 => .{ .op = "GETA", .rest = " l" },
            .set16, .set32 => .{ .op = "SET", .rest = " l" },
            .jmp16, .jmp32 => .{ .op = "JMP", .rest = " p" },
            .jmpf16, .jmpf32 => .{ .op = "JMPF", .rest = " p" },
        };

        try writer.print("{s:<6}{s}", .{ s.op, s.rest });
    }
};

/// Jmp represents a jmp instruction
pub const Jmp = struct {
    chunk: *Chunk,

    /// index is the index of the jump opcode.
    ///
    /// NOTE: we assume that the opcode is a single byte.
    index: usize,

    pub fn set(self: *Jmp) !void {
        const pos = self.chunk.code.items.len;
        if (pos > std.math.maxInt(u8)) {
            // TODO: implement using a wider jmp by shuffling code rightwards to make space
            @panic("not implemented");
        }

        self.chunk.code.items[self.index + 1] = @intCast(u8, pos);
    }
};

/// Chunk is a section of bytecode.
pub const Chunk = struct {
    gpa: std.mem.Allocator,
    code: std.ArrayListUnmanaged(u8),
    locals: Locals,
    constants: std.ArrayListUnmanaged(Value),

    pub fn init(gpa: std.mem.Allocator) Chunk {
        return .{
            .gpa = gpa,
            .code = std.ArrayListUnmanaged(u8){},
            .locals = std.ArrayListUnmanaged(Local){},
            .constants = std.ArrayListUnmanaged(Value){},
        };
    }

    /// addConstant adds v as a constant and returns its index.
    pub fn addConstant(self: *Chunk, v: Value) !u32 {
        try self.constants.append(self.gpa, v);
        return @intCast(u32, self.constants.items.len - 1);
    }

    /// addLocal adds a local with name and returns its index.
    pub fn addLocal(self: *Chunk, name: []const u8) !u32 {
        try self.locals.append(self.gpa, .{ .name = name });
        return @intCast(u32, self.locals.items.len - 1);
    }

    pub fn popLocals(self: *Chunk, n: u32) void {
        self.locals.shrinkRetainingCapacity(self.locals.items.len - n);
    }

    /// getLocal returns the local with name, if it exists.
    pub fn getLocal(self: *Chunk, name: []const u8) ?u32 {
        if (self.locals.items.len == 0) return null;

        var i = self.locals.items.len - 1;
        while (true) : (i -= 1) {
            const l = self.locals.items[i];
            if (std.mem.eql(u8, l.name, name)) return @intCast(u32, i);

            if (i == 0) break;
        }

        return null;
    }

    /// writeOp writes op into the chunk.
    pub fn writeOp(self: *Chunk, op: Op) !void {
        try self.writeU8(@enumToInt(op));
    }

    pub fn writeRateOp(self: *Chunk, op: RareOp) !void {
        try self.writeU8(@enumToInt(op));
    }

    /// writeU8 writes a byte into chunk.
    pub fn writeU8(self: *Chunk, v: u8) !void {
        try self.code.append(self.gpa, v);
    }

    /// writeU16 writes a u16 into chunk.
    pub fn writeU16(self: *Chunk, v: u16) !void {
        try self.code.writer(self.gpa).writeIntLittle(u16, v);
    }

    /// writeU32 writes a u32 into chunk.
    pub fn writeU32(self: *Chunk, v: u32) !void {
        try self.code.writer(self.gpa).writeIntLittle(u32, v);
    }

    /// writeJmp writes the jmp opcode Op and returns a Jmp so the location can be set later.
    pub fn writeJmp(self: *Chunk, comptime op: Op) !Jmp {
        switch (op) {
            .jmpf8, .jmp8 => {},
            else => @compileError("can only writeJmp with jmp opcodes"),
        }

        const index = self.code.items.len;

        try self.writeOp(op);
        try self.writeU8(undefined);

        return Jmp{ .chunk = self, .index = index };
    }

    /// next increase i by one if there is still code left.
    fn next(self: *const Chunk, i: *u32) error{EndOfFile}!u8 {
        if (i.* + 1 > self.code.items.len - 1) return error.EndOfFile;

        i.* = i.* + 1;
        return self.code.items[i.*];
    }

    /// disassembleRate writes the dissassembled output of a rare op to writer.
    fn disassembleRare(self: *const Chunk, i: *u32, writer: anytype) !void {
        const op = try std.meta.intToEnum(RareOp, self.code.items[i.*]);
        try op.print(writer);
        i.* += 1;

        var arg = switch (op) {
            .const16, .get16, .geta16, .set16, .jmp16, .jmpf16 => b: {
                var fbs = std.io.fixedBufferStream(self.code.items[i.*..]);
                const arg = try fbs.reader().readIntLittle(u16);
                i.* = i.* + 1;
                break :b @intCast(usize, arg);
            },
            .const32, .get32, .geta32, .set32, .jmp32, .jmpf32 => b: {
                var fbs = std.io.fixedBufferStream(self.code.items[i.*..]);
                const arg = try fbs.reader().readIntLittle(u32);
                i.* = i.* + 3;
                break :b @intCast(usize, arg);
            },
        };
        try writer.print("{x:<4} ", .{arg});

        // TODO: check that there are at least arg items.
        switch (op) {
            .const16, .const32 => {
                try writer.writeAll("; ");
                try self.constants.items[arg].print(writer);
            },
            else => {},
        }
    }

    /// disassemble writes the disassembled output of the chunk to writer.
    pub fn disassemble(self: *const Chunk, writer: anytype) !void {
        for (self.constants.items) |v| switch (v) {
            .object => |o| switch (o.v) {
                .function => |f| {
                    try writer.print("============ {s} ============\n", .{f.name});
                    try f.chunk.disassemble(writer);
                    try writer.writeAll("============================\n");
                },
            },
            else => {},
        };

        var i: u32 = 0;
        while (i < self.code.items.len) : (i += 1) {
            const op = try std.meta.intToEnum(Op, self.code.items[i]);
            switch (op) {
                .const8 => {
                    const arg = try self.next(&i);
                    try op.print(writer);
                    try writer.print("{x:<4} ; ", .{arg});

                    try self.constants.items[arg].print(writer);
                },
                .pop_n, .get8, .geta8, .set8, .jmp8, .jmpf8, .call => {
                    const arg = try self.next(&i);
                    try op.print(writer);
                    try writer.print("{x:<4}", .{arg});
                },
                .one, .neg_one, .true, .false, .add, .subtract, .multiply, .divide, .negate => try op.print(writer),
                .eq, .neq, .lt, .lte, .gt, .gte => try op.print(writer),
                .pop, .ret => try op.print(writer),
                .rare => {
                    i += 1;
                    try self.disassembleRare(&i, writer);
                },
            }

            try writer.writeAll("\n");
        }
    }

    pub fn deinit(self: *Chunk) void {
        self.locals.deinit(self.gpa);

        for (self.constants.items) |*c| c.deinit();
        self.constants.deinit(self.gpa);

        self.code.deinit(self.gpa);
    }
};

const testing = std.testing;

fn testDissassemble(chunk: *const Chunk, expected: []const u8) !void {
    var al = std.ArrayList(u8).init(testing.allocator);
    defer al.deinit();

    try chunk.disassemble(al.writer());

    try testing.expectEqualStrings(expected, al.items);
}

test "disassemble" {
    var chunk = Chunk.init(testing.allocator);
    defer chunk.deinit();

    _ = try chunk.addConstant(.{ .integer = 147 });
    _ = try chunk.addLocal("foo");
    _ = try chunk.addLocal("bar");

    try chunk.writeOp(.const8);
    try chunk.writeU8(0);
    try chunk.writeOp(.get8);
    try chunk.writeU8(0);
    try chunk.writeOp(.rare);
    try chunk.writeRateOp(.get32);
    try chunk.writeU32(0);
    try chunk.writeOp(.one);
    try chunk.writeOp(.neg_one);
    try chunk.writeOp(.set8);
    try chunk.writeU8(1);
    try chunk.writeOp(.pop);
    try chunk.writeOp(.pop_n);
    try chunk.writeU8(5);
    try chunk.writeOp(.jmpf8);
    try chunk.writeU8(0);
    try chunk.writeOp(.jmp8);
    try chunk.writeU8(0);

    try testDissassemble(&chunk,
        \\CONST  c0    ; 147
        \\GET    l0   
        \\GET    l0    
        \\ONE   
        \\NONE  
        \\SET    l1   
        \\POP   
        \\POPN    5   
        \\JMPF   p0   
        \\JMP    p0   
        \\
    );
}

test "jmps" {
    var chunk = Chunk.init(testing.allocator);
    defer chunk.deinit();

    _ = try chunk.addConstant(.{ .integer = 147 });
    _ = try chunk.addLocal("foo");

    try chunk.writeOp(.const8);
    try chunk.writeU8(0);
    try chunk.writeOp(.get8);
    try chunk.writeU8(0);

    var jmp = try chunk.writeJmp(.jmp8);

    try chunk.writeOp(.one);

    try jmp.set();

    try chunk.writeOp(.neg_one);

    try testDissassemble(&chunk,
        \\CONST  c0    ; 147
        \\GET    l0   
        \\JMP    p7   
        \\ONE   
        \\NONE  
        \\
    );
}
