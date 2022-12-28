const std = @import("std");
const bc = @import("bytecode.zig");

pub const Vm = struct {
    gpa: std.mem.Allocator,
    chunk: bc.Chunk,

    pc: usize = 0,
    locals: std.ArrayListUnmanaged(bc.Value) = .{},
    stack: std.ArrayListUnmanaged(bc.Value) = .{},

    pub fn init(gpa: std.mem.Allocator, chunk: bc.Chunk) Vm {
        return Vm{ .gpa = gpa, .chunk = chunk };
    }

    /// pop pops a value off the stack, returning an error if there isn't one.
    fn pop(self: *Vm) !bc.Value {
        if (self.stack.items.len == 0) return error.CouldNotParse;

        return self.stack.pop();
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

        self.stack.appendAssumeCapacity(.{ .integer = switch (op) {
            .add => a.integer + b.integer,
            .subtract => a.integer - b.integer,
            .multiply => a.integer * b.integer,
            .divide => @divFloor(a.integer, b.integer),
            else => unreachable,
        } });
    }

    /// run interprets the bytecode in chunk.
    pub fn run(self: *Vm) !void {
        while (self.pc < self.chunk.code.items.len) {
            const op = try std.meta.intToEnum(bc.Op, self.chunk.code.items[self.pc]);
            self.pc += 1;

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
                    const i = self.chunk.code.items[self.pc];
                    self.pc += 1;
                    if (i >= self.locals.items.len)
                        return error.UnknownLocal;

                    try self.stack.append(self.gpa, self.locals.items[i]);
                },
                .set8 => {
                    if (self.pc >= self.chunk.code.items.len) return error.CouldNotParse;
                    const i = self.chunk.code.items[self.pc];
                    self.pc += 1;
                    // TODO: could cause OOM errors on bad input. Can we assume i is the current size and just use
                    // append here? Or do we need to bound it to something sensible?
                    try self.locals.resize(self.gpa, i + 1);
                    self.locals.items[i] = self.stack.pop();
                },
                .one => try self.stack.append(self.gpa, .{ .integer = 1 }),
                .neg_one => try self.stack.append(self.gpa, .{ .integer = -1 }),
                .true => try self.stack.append(self.gpa, .{ .boolean = true }),
                .false => try self.stack.append(self.gpa, .{ .boolean = false }),
                .add, .subtract, .multiply, .divide => try self.runBinop(op),
                .negate => {
                    const arg = try self.pop();
                    if (arg != .integer) return error.WrongType;
                    self.stack.appendAssumeCapacity(.{ .integer = arg.integer * -1 });
                },
                .rare => try self.runRare(),
            }
        }
    }

    pub fn deinit(self: *Vm) void {
        self.locals.deinit(self.gpa);
        self.stack.deinit(self.gpa);
    }
};

const testing = std.testing;

fn testExpectStack(chunk: bc.Chunk, expected: []const bc.Value) !void {
    var vm = Vm.init(testing.allocator, chunk);
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

    try testExpectStack(chunk, &.{ .{ .integer = 147 }, .{ .boolean = true } });
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

    try testExpectStack(chunk, &.{
        .{ .integer = 1 },
    });
}
