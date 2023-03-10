const std = @import("std");
const bc = @import("bytecode.zig");

/// Frame represents a call frame. It records where the VM should go when the frame returns as well as pointers into
/// the stack for where this call frame's values start (and the same for the locals).
pub const Frame = struct {
    return_address: usize,
    stack_pointer: usize,
    chunk: *const bc.Chunk,
};

pub const Vm = struct {
    gpa: std.mem.Allocator,
    chunk: *const bc.Chunk,

    debug: bool = false,
    pc: usize = 0,
    frames: std.ArrayListUnmanaged(Frame) = .{},
    stack: std.ArrayListUnmanaged(bc.Value) = .{},

    pub fn init(gpa: std.mem.Allocator, chunk: *const bc.Chunk) Vm {
        return Vm{ .gpa = gpa, .chunk = chunk };
    }

    fn stackOffset(self: *const Vm) usize {
        return self.frames.items[self.frames.items.len - 1].stack_pointer;
    }

    /// pop pops a value off the stack, returning an error if there isn't one.
    fn pop(self: *Vm) !bc.Value {
        if (self.stack.items.len == 0) return error.CouldNotParse;

        return self.stack.pop();
    }

    /// pushFrame adds a call frame with the return address being the current location.
    fn pushFrame(self: *Vm, args: usize) !void {
        try self.frames.append(self.gpa, Frame{
            .chunk = self.chunk,
            .return_address = self.pc,
            .stack_pointer = self.stack.items.len - args,
        });
    }

    /// popFrame removes the top frame and goes back to its return address.
    fn popFrame(self: *Vm) void {
        var f = self.frames.pop();
        self.chunk = f.chunk;
        self.pc = f.return_address;
        self.stack.shrinkRetainingCapacity(f.stack_pointer);
    }

    /// runRare assumes the next op is a rare one and runs it.
    fn runRare(self: *Vm) !void {
        _ = self;
        @panic("unimplemented");
    }

    /// runBinop assumes op in a binary operation and runs it.
    fn runBinop(self: *Vm, op: bc.Op) !void {
        // The stack is FIFO so the right operand is at the very end.
        const b = try self.pop();
        const a = try self.pop();
        if (a != .integer or b != .integer) return error.WrongType;

        self.stack.appendAssumeCapacity(switch (op) {
            .add => .{ .integer = a.integer + b.integer },
            .subtract => .{ .integer = a.integer - b.integer },
            .multiply => .{ .integer = a.integer * b.integer },
            .divide => .{ .integer = @divFloor(a.integer, b.integer) },
            .eq => .{ .boolean = a.integer == b.integer },
            .neq => .{ .boolean = a.integer != b.integer },
            .lt => .{ .boolean = a.integer < b.integer },
            .lte => .{ .boolean = a.integer <= b.integer },
            .gt => .{ .boolean = a.integer > b.integer },
            .gte => .{ .boolean = a.integer >= b.integer },
            else => unreachable,
        });
    }

    /// run interprets the bytecode in chunk.
    pub fn run(self: *Vm) !void {
        try self.frames.append(self.gpa, .{
            .chunk = self.chunk,
            .return_address = 0,
            .stack_pointer = 0,
        });

        var w = std.io.getStdErr().writer();

        while (self.pc < self.chunk.code.items.len) {
            const op = try std.meta.intToEnum(bc.Op, self.chunk.code.items[self.pc]);
            self.pc += 1;

            if (self.debug) {
                try w.print("{} {p} stack=[", .{ self.pc, &self.chunk });
                for (self.stack.items) |v, i| {
                    try v.print(w);
                    if (i != self.stack.items.len - 1) try w.writeAll(", ");
                }
                try w.writeAll("]\n");

                try op.print(w);
                try w.writeAll("\n");
            }

            switch (op) {
                .const8 => {
                    if (self.pc >= self.chunk.code.items.len) return error.CouldNotParse;
                    const i = self.chunk.code.items[self.pc];
                    self.pc += 1;
                    if (i >= self.chunk.constants.items.len)
                        return error.UnknownConstant;

                    try self.stack.append(self.gpa, self.chunk.constants.items[i]);
                },
                .get8 => {
                    if (self.pc >= self.chunk.code.items.len) return error.CouldNotParse;
                    const i = self.chunk.code.items[self.pc] + self.stackOffset();
                    self.pc += 1;
                    if (i >= self.stack.items.len) {
                        return error.UnknownLocal;
                    }

                    try self.stack.append(self.gpa, self.stack.items[i]);
                },
                .geta8 => {
                    if (self.pc >= self.chunk.code.items.len) return error.CouldNotParse;
                    const i = self.chunk.code.items[self.pc];
                    self.pc += 1;
                    if (i >= self.stack.items.len) {
                        return error.UnknownLocal;
                    }

                    try self.stack.append(self.gpa, self.stack.items[i]);
                },
                .set8 => {
                    if (self.pc >= self.chunk.code.items.len) return error.CouldNotParse;
                    const i = self.chunk.code.items[self.pc] + self.stackOffset();
                    self.pc += 1;
                    if (i >= self.stack.items.len) return error.CouldNotParse;
                    if (i == self.stack.items.len - 1) continue;
                    self.stack.items[i] = self.stack.pop();
                },
                .pop => _ = try self.pop(),
                .pop_n => {
                    if (self.pc >= self.chunk.code.items.len) return error.CouldNotParse;
                    const i = self.chunk.code.items[self.pc];
                    self.pc += 1;
                    self.stack.shrinkRetainingCapacity(self.stack.items.len - i);
                },
                .one => try self.stack.append(self.gpa, .{ .integer = 1 }),
                .neg_one => try self.stack.append(self.gpa, .{ .integer = -1 }),
                .true => try self.stack.append(self.gpa, .{ .boolean = true }),
                .false => try self.stack.append(self.gpa, .{ .boolean = false }),
                .add, .subtract, .multiply, .divide, .eq, .neq, .lt, .lte, .gt, .gte => try self.runBinop(op),
                .negate => {
                    const arg = try self.pop();
                    if (arg != .integer) return error.WrongType;
                    self.stack.appendAssumeCapacity(.{ .integer = arg.integer * -1 });
                },
                .call => {
                    if (self.pc >= self.chunk.code.items.len) return error.CouldNotParse;
                    // TODO: check the right number of args have been given
                    const args = self.chunk.code.items[self.pc];
                    self.pc += 1;

                    try self.pushFrame(args);

                    var v = switch (self.stack.items[self.stack.items.len - 1 - args]) {
                        .object => |o| switch (o.v) {
                            .function => |f| f,
                        },
                        else => return error.CouldNotParse,
                    };

                    self.chunk = &v.chunk;
                    self.pc = 0;
                },
                .ret => {
                    const return_value = try self.pop();
                    self.popFrame();
                    _ = try self.pop(); // Remove the function we just called from the stack.
                    self.stack.appendAssumeCapacity(return_value);
                },
                .jmp8 => {
                    if (self.pc >= self.chunk.code.items.len) return error.CouldNotParse;
                    const i = self.chunk.code.items[self.pc];
                    self.pc = i;
                },
                .jmpf8 => {
                    if (self.pc >= self.chunk.code.items.len) return error.CouldNotParse;
                    const i = self.chunk.code.items[self.pc];

                    switch (try self.pop()) {
                        .boolean => |b| {
                            if (b) self.pc += 1 else self.pc = i;
                        },
                        else => return error.CouldNotParse,
                    }
                },
                .rare => try self.runRare(),
            }
        }
    }

    pub fn deinit(self: *Vm) void {
        self.stack.deinit(self.gpa);
        self.frames.deinit(self.gpa);
    }
};

const testing = std.testing;

fn testExpectStack(chunk: bc.Chunk, expected: []const bc.Value) !void {
    var vm = Vm.init(testing.allocator, &chunk);
    defer vm.deinit();

    try vm.run();

    try testing.expectEqualSlices(bc.Value, expected, vm.stack.items);
}

test "constants" {
    var chunk = bc.Chunk.init(testing.allocator);
    defer chunk.deinit();

    _ = try chunk.addConstant(.{ .integer = 147 });
    _ = try chunk.addConstant(.{ .boolean = true });
    try chunk.writeOp(.const8);
    try chunk.writeU8(0);
    try chunk.writeOp(.const8);
    try chunk.writeU8(1);

    try testExpectStack(chunk, &.{ .{ .integer = 147 }, .{ .boolean = true } });
}

test "locals" {
    var chunk = bc.Chunk.init(testing.allocator);
    defer chunk.deinit();

    _ = try chunk.addConstant(.{ .integer = 147 });
    try chunk.writeOp(.const8);
    try chunk.writeU8(0);
    try chunk.writeOp(.set8);
    try chunk.writeU8(0);
    try chunk.writeOp(.true);
    try chunk.writeOp(.set8);
    try chunk.writeU8(1);
    try chunk.writeOp(.get8);
    try chunk.writeU8(0);
    try chunk.writeOp(.get8);
    try chunk.writeU8(1);

    try testExpectStack(chunk, &.{
        .{ .integer = 147 },
        .{ .boolean = true },
        .{ .integer = 147 },
        .{ .boolean = true },
    });
}

test "maths" {
    var chunk = bc.Chunk.init(testing.allocator);
    defer chunk.deinit();

    _ = try chunk.addConstant(.{ .integer = 2 });
    _ = try chunk.addConstant(.{ .integer = 3 });
    try chunk.writeOp(.const8);
    try chunk.writeU8(0); // 2
    try chunk.writeOp(.one); // 1
    try chunk.writeOp(.divide); // 2/1 => 2
    try chunk.writeOp(.const8);
    try chunk.writeU8(1); // 3
    try chunk.writeOp(.subtract); // 2 - 3
    try chunk.writeOp(.negate); // *-1;

    try testExpectStack(chunk, &.{.{ .integer = 1 }});
}

test "scopes" {
    var chunk = bc.Chunk.init(testing.allocator);
    defer chunk.deinit();

    {
        var i: usize = 0;
        while (i < 5) : (i += 1) {
            const c = try chunk.addConstant(.{ .integer = @intCast(i64, i) });
            try chunk.writeOp(.const8);
            try chunk.writeU8(@intCast(u8, c));
            try chunk.writeOp(.set8);
            try chunk.writeU8(@intCast(u8, i));
        }
    }

    try chunk.writeOp(.pop);
    try chunk.writeOp(.pop_n);
    try chunk.writeU8(2);

    try testExpectStack(chunk, &.{ .{ .integer = 0 }, .{ .integer = 1 } });
}

test "functions" {
    var f_chunk = bc.Chunk.init(testing.allocator);

    try f_chunk.writeOp(.get8);
    try f_chunk.writeU8(0);
    try f_chunk.writeOp(.get8);
    try f_chunk.writeU8(1);
    try f_chunk.writeOp(.add);
    try f_chunk.writeOp(.ret);

    var chunk = bc.Chunk.init(testing.allocator);
    defer chunk.deinit();

    var o = try testing.allocator.create(bc.Object);
    o.* = .{ .v = .{ .function = .{ .chunk = f_chunk, .name = "", .arity = 2 } } };

    _ = try chunk.addConstant(.{ .object = o });
    try chunk.writeOp(.const8);
    try chunk.writeU8(0);
    try chunk.writeOp(.one);
    try chunk.writeOp(.one);
    try chunk.writeOp(.call);
    try chunk.writeU8(2);

    try testExpectStack(chunk, &.{.{ .integer = 2 }});
}

test "jumps" {
    var chunk = bc.Chunk.init(testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(.false);
    var to_else_jmp = try chunk.writeJmp(.jmpf8);

    try chunk.writeOp(.one);
    var to_after_else_jmp = try chunk.writeJmp(.jmp8);

    try to_else_jmp.set();
    try chunk.writeOp(.neg_one);
    try to_after_else_jmp.set();

    try testExpectStack(chunk, &.{.{ .integer = -1 }});
}

test "boolean binops" {
    var chunk = bc.Chunk.init(testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(.one);
    try chunk.writeOp(.neg_one);
    try chunk.writeOp(.eq);
    try testExpectStack(chunk, &.{.{ .boolean = false }});
}
