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

    fn pop(self: *Vm) !bc.Value {
        if (self.stack.items.len == 0) return error.CouldNotParse;

        return self.stack.items.pop();
    }

    fn runRare(self: *Vm) !void {
        _ = self;
        @panic("unimplemented");
    }

    pub fn run(self: *Vm) !void {
        while (self.pc < self.chunk.code.items.len) {
            const op = try std.meta.intToEnum(bc.Op, self.chunk.code.items[self.pc]);
            self.pc += 1;
            if (self.pc >= self.chunk.code.items.len) return error.CouldNotParse;

            switch (op) {
                .const8 => {
                    const i = self.chunk.code.items[self.pc];
                    self.pc += 1;
                    if (i >= self.chunk.constants.items.len)
                        return error.UnknownConstant;

                    try self.stack.append(self.gpa, self.chunk.constants.items[i]);
                },
                .get8 => {
                    const i = self.chunk.code.items[self.pc];
                    self.pc += 1;
                    if (i >= self.locals.items.len)
                        return error.UnknownLocal;

                    try self.stack.append(self.gpa, self.locals.items[i]);
                },
                .set8 => {
                    const i = self.chunk.code.items[self.pc];
                    self.pc += 1;
                    try self.locals.resize(self.gpa, i + 1);
                    self.locals.items[i] = self.stack.pop();
                },
                .one => try self.stack.append(self.gpa, .{ .integer = 1 }),
                .neg_one => try self.stack.append(self.gpa, .{ .integer = -1 }),
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
    _ = try chunk.addConstant(.{ .boolean = true });
    try chunk.writeOp(.const8);
    try chunk.writeU8(0);
    try chunk.writeOp(.set8);
    try chunk.writeU8(0);
    try chunk.writeOp(.const8);
    try chunk.writeU8(1);
    try chunk.writeOp(.set8);
    try chunk.writeU8(1);
    try chunk.writeOp(.get8);
    try chunk.writeU8(0);
    try chunk.writeOp(.get8);
    try chunk.writeU8(1);

    try testExpectStack(chunk, &.{ .{ .integer = 147 }, .{ .boolean = true } });
}
