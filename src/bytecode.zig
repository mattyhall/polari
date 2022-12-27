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

pub const Value = union(enum) {
    int: i64,
    boolean: bool,

    pub fn print(self: Value, w: anytype) !void {
        switch (self) {
            .int => |i| try w.print("{}", .{i}),
            .boolean => |b| try w.print("{}", .{b}),
        }
    }
};

/// Op lists the most common bytecode operations.
pub const Op = enum(u8) {
    /// CONST a: a gives the index into the constants array to push onto the stack.
    const8,

    /// GET a: a gives the index into the locals array to push onto the stack.
    get8,

    /// SET a: sets the local with index a to the value on the top of the stack.
    set8,

    /// ONE: pushes 1 onto the stack
    one,

    /// NONE: pushes -1 onto the stack
    neg_one,

    /// RARE a: a is a RareOp.
    rare,

    pub fn print(self: Op, writer: anytype) !void {
        var s: struct { op: []const u8, rest: []const u8 } = switch (self) {
            .const8 => .{ .op = "CONST", .rest = " c" },
            .get8 => .{ .op = "GET", .rest = " l" },
            .set8 => .{ .op = "SET", .rest = " l" },
            .one => .{ .op = "ONE", .rest = "" },
            .neg_one => .{ .op = "NONE", .rest = "" },
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

    /// GET a: a gives the index into the locals array to push onto the stack.
    get16,
    get32,

    /// SET a: sets the local with index a to the value on the top of the stack.
    set16,
    set32,

    pub fn print(self: RareOp, writer: anytype) !void {
        var s: struct { op: []const u8, rest: []const u8 } = switch (self) {
            .const16, .const32 => .{ .op = "CONST", .rest = " c" },
            .get16, .get32 => .{ .op = "GET", .rest = " l" },
            .set16, .set32 => .{ .op = "SET", .rest = " l" },
        };

        try writer.print("{s:<6}{s}", .{ s.op, s.rest });
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

    /// getLocal returns the local with name, if it exists.
    pub fn getLocal(self: *Chunk, name: []const u8) ?u32 {
        for (self.locals.items) |l, i| {
            if (std.mem.eql(u8, l.name, name)) return @intCast(u32, i);
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

    /// next increase i by one if there is still code left.
    fn next(self: *const Chunk, i: *u32) error{EndOfFile}!u8 {
        if (i.* > self.code.items.len - 1) return error.EndOfFile;

        i.* = i.* + 1;
        return self.code.items[i.*];
    }

    /// disassembleRate writes the dissassembled output of a rare op to writer.
    fn disassembleRare(self: *const Chunk, i: *u32, writer: anytype) !void {
        const op = try std.meta.intToEnum(RareOp, self.code.items[i.*]);
        try op.print(writer);
        i.* += 1;

        var arg = switch (op) {
            .const16, .get16, .set16 => b: {
                var fbs = std.io.fixedBufferStream(self.code.items[i.*..]);
                const arg = try fbs.reader().readIntLittle(u16);
                i.* = i.* + 1;
                break :b @intCast(usize, arg);
            },
            .const32, .get32, .set32 => b: {
                var fbs = std.io.fixedBufferStream(self.code.items[i.*..]);
                const arg = try fbs.reader().readIntLittle(u32);
                i.* = i.* + 3;
                break :b @intCast(usize, arg);
            },
        };
        try writer.print("{x:<4} ; ", .{arg});

        // TODO: check that there are at least arg items.
        switch (op) {
            .const16, .const32 => try self.constants.items[arg].print(writer),
            .get16, .set16, .get32, .set32 => try self.locals.items[arg].print(writer),
        }
    }

    /// disassemble writes the disassembled output of the chunk to writer.
    pub fn diassemble(self: *const Chunk, writer: anytype) !void {
        var i: u32 = 0;
        while (i < self.code.items.len) : (i += 1) {
            const op = try std.meta.intToEnum(Op, self.code.items[i]);
            switch (op) {
                .const8, .get8, .set8 => {
                    const arg = try self.next(&i);
                    try op.print(writer);
                    try writer.print("{x:<4} ; ", .{arg});

                    // TODO: check that there are at least arg items.
                    switch (op) {
                        .const8 => try self.constants.items[arg].print(writer),
                        .get8, .set8 => try self.locals.items[arg].print(writer),
                        else => unreachable,
                    }
                },
                .one, .neg_one => try op.print(writer),
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
        self.constants.deinit(self.gpa);
        self.code.deinit(self.gpa);
    }
};

const testing = std.testing;

fn testDissassemble(chunk: *const Chunk, expected: []const u8) !void {
    var al = std.ArrayList(u8).init(testing.allocator);
    defer al.deinit();

    try chunk.diassemble(al.writer());

    try testing.expectEqualStrings(expected, al.items);
}

test "disassemble" {
    var chunk = Chunk.init(testing.allocator);
    defer chunk.deinit();

    _ = try chunk.addConstant(.{ .int = 147 });
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

    try testDissassemble(&chunk,
        \\CONST  c0    ; 147
        \\GET    l0    ; foo
        \\GET    l0    ; foo
        \\ONE   
        \\NONE  
        \\SET    l1    ; bar
        \\
    );
}
