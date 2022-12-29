const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const utils = @import("utils.zig");
const sema = @import("sema.zig");
const simargs = @import("simargs");
const compiler = @import("compiler.zig");
const vm = @import("vm.zig");

pub const log_level: std.log.Level = .info;

pub const Opts = struct {
    @"dump-ast": bool = false,
    @"dump-rules": bool = false,
    @"dump-type-checking": bool = false,
    @"dump-bytecode": bool = false,
};

fn readStdin(gpa: std.mem.Allocator) ![]const u8 {
    var stdin = std.io.getStdIn();
    return try stdin.reader().readAllAlloc(gpa, 10 * 1024 * 1024);
}

fn readFile(gpa: std.mem.Allocator, path: []const u8) ![]const u8 {
    var abs_path = if (std.fs.path.isAbsolute(path))
        try gpa.dupe(u8, path)
    else
        try std.fs.cwd().realpathAlloc(gpa, path);

    defer gpa.free(abs_path);

    var f = try std.fs.openFileAbsolute(abs_path, .{});
    defer f.close();

    return try f.reader().readAllAlloc(gpa, 10 * 1024 * 1024);
}

fn DotWriter(comptime W: anytype) type {
    return struct {
        next_id: u64 = 0,
        w: W,

        const Self = @This();

        fn id(self: *Self) u64 {
            self.next_id += 1;
            return self.next_id - 1;
        }

        fn writeExpression(self: *Self, expression: *const parser.Expression) !u64 {
            const e_id = self.id();

            switch (expression.*) {
                .integer => |i| {
                    try self.w.print(
                        \\  e_{} [label="{}",color="white",fontcolor="white"]
                        \\
                    , .{ e_id, i });
                },
                .boolean => |b| {
                    try self.w.print(
                        \\  e_{} [label="{}",color="white",fontcolor="white"]
                        \\
                    , .{ e_id, b });
                },
                .identifier => |i| {
                    try self.w.print(
                        \\  e_{} [label="{s}",color="white",fontcolor="white"]
                        \\
                    , .{ e_id, i });
                },
                .binop => |binop| {
                    const lhs_id = try self.writeExpression(binop.lhs.inner);
                    const rhs_id = try self.writeExpression(binop.rhs.inner);
                    const op = switch (binop.op) {
                        .plus => "+",
                        .minus => "-",
                        .multiply => "*",
                        .divide => "/",
                    };

                    try self.w.print(
                        \\  e_{[e_id]} [label="{[op]s}",color="white",fontcolor="white"]
                        \\  e_{[e_id]} -- e_{[lhs_id]} [color="white"]
                        \\  e_{[e_id]} -- e_{[rhs_id]} [color="white"]
                        \\
                    , .{ .e_id = e_id, .lhs_id = lhs_id, .rhs_id = rhs_id, .op = op });
                },
                .unaryop => |unaryop| {
                    const child_id = try self.writeExpression(unaryop.e.inner);
                    const op = switch (unaryop.op) {
                        .negate => "-",
                        .grouping => "()",
                    };

                    try self.w.print(
                        \\  e_{[e_id]} [label="{[op]s}",color="white",fontcolor="white"]
                        \\  e_{[e_id]} -- e_{[child_id]} [color="white"]
                        \\
                    , .{ .e_id = e_id, .child_id = child_id, .op = op });
                },
                .let => |let| {
                    const equals_id = self.id();
                    const in_id = self.id();
                    try self.w.print(
                        \\  e_{[e_id]} [label="let",color="white",fontcolor="white"]
                        \\  eq_{[equals_id]} [label="[]",color="white",fontcolor="white"]
                        \\  in_{[in_id]} [label="in",color="white",fontcolor="white"]
                        \\  e_{[e_id]} -- eq_{[equals_id]} [color="white"]
                        \\  e_{[e_id]} -- in_{[in_id]} [color="white"]
                    , .{ .e_id = e_id, .equals_id = equals_id, .in_id = in_id });

                    for (let.assignments) |a| {
                        const s_id = try self.writeAssignment(&a.inner);
                        try self.w.print(
                            \\  eq_{[equals_id]} -- s_{[s_id]} [color="white"]
                            \\
                        , .{ .equals_id = equals_id, .s_id = s_id });
                    }

                    const in_e_id = try self.writeExpression(let.in.inner);
                    try self.w.print(
                        \\  in_{[in_id]} -- e_{[in_e_id]} [color="white"]
                        \\
                    , .{ .in_id = in_id, .in_e_id = in_e_id });
                },
            }

            return e_id;
        }

        fn writeAssignment(self: *Self, assignment: *const parser.Assignment) anyerror!u64 {
            const s_id = self.id();
            const i_id = self.id();

            try self.w.print(
                \\  i_{[i_id]} [label="{[name]s}",color="white",fontcolor="white"]
                \\
            , .{ .i_id = i_id, .name = assignment.identifier });

            const e_id = try self.writeExpression(assignment.expression.inner);
            try self.w.print(
                \\  s_{[s_id]} [label="=",color="white",fontcolor="white"]
                \\  s_{[s_id]} -- i_{[i_id]} [color="white"]
                \\  s_{[s_id]} -- e_{[e_id]} [color="white"]
            , .{ .i_id = i_id, .s_id = s_id, .e_id = e_id });

            return s_id;
        }

        fn write(self: *Self, program: *const parser.Program) !void {
            try self.w.writeAll(
                \\strict graph {
                \\  bgcolor="transparent"
                \\  root [color="white",fontcolor="white"]
                \\
            );

            for (program.stmts.items) |stmt| {
                switch (stmt.inner) {
                    .assignment => |a| {
                        const s_id = try self.writeAssignment(&a);
                        try self.w.print(
                            \\  root -- s_{} [color="white"]
                            \\
                        , .{ .s_id = s_id });
                    },
                    .expression => |e| {
                        const e_id = try self.writeExpression(e);
                        try self.w.print(
                            \\  root -- e_{} [color="white"]
                            \\
                        , .{e_id});
                    },
                }
            }

            try self.w.writeAll("}");
        }
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){ .backing_allocator = std.heap.page_allocator };
    defer _ = gpa.deinit();

    var opts = try simargs.parse(gpa.allocator(), Opts);
    defer opts.deinit();

    var source = if (opts.positional_args.items.len == 0)
        try readStdin(gpa.allocator())
    else
        try readFile(gpa.allocator(), opts.positional_args.items[0]);

    defer gpa.allocator().free(source);

    var l = lexer.Lexer{ .source = source };
    var p = parser.Parser.init(gpa.allocator(), .{ .real = l });
    defer p.deinit();

    var program = p.parse() catch |e| {
        switch (e) {
            error.EndOfFile => {
                try utils.printErrors(source, p.getDiags());
                std.debug.print("error: end of file\n", .{});
            },
            else => try utils.printErrors(source, p.getDiags()),
        }

        std.os.exit(1);
    };
    defer program.deinit();

    if (opts.args.@"dump-ast") {
        var w = std.io.getStdOut().writer();
        try (DotWriter(@TypeOf(w)){ .w = w }).write(&program);
        return;
    }

    var s = sema.Sema.init(gpa.allocator(), &program);
    defer s.deinit();

    s.generateRules() catch |e| {
        if (s.diags.items.len == 0) return e;

        try utils.printErrors(source, s.diags.items);
        std.os.exit(1);
    };

    if (opts.args.@"dump-rules") {
        var w = std.io.getStdOut().writer();
        try s.printRules(w);
    }

    s.debug = opts.args.@"dump-type-checking";
    s.solve() catch |e| {
        if (s.diags.items.len == 0) return e;

        try utils.printErrors(source, s.diags.items);
        std.os.exit(1);
    };

    var c = compiler.Compiler.init(gpa.allocator(), &program);
    defer c.deinit();

    try c.compile();

    if (opts.args.@"dump-bytecode") {
        var w = std.io.getStdOut().writer();
        try c.chunk.diassemble(w);
    }

    var machine = vm.Vm.init(gpa.allocator(), c.chunk);
    defer machine.deinit();

    try machine.run();

    var w = std.io.getStdOut().writer();
    try w.writeAll("stack\n");
    for (machine.stack.items) |v| {
        try v.print(w);
        try w.writeAll("\n");
    }
}

const This = @This();

test "all" {
    const bytecode = @import("bytecode.zig");

    std.testing.refAllDecls(This);
    std.testing.refAllDecls(bytecode);
}
