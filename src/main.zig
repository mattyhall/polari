const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const utils = @import("utils.zig");
const simargs = @import("simargs");

pub const log_level: std.log.Level = .info;

pub const Opts = struct {
    @"dump-ast": bool = false,
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
                .binop => |binop| {
                    const lhs_id = try self.writeExpression(binop.lhs);
                    const rhs_id = try self.writeExpression(binop.rhs);
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
                    const child_id = try self.writeExpression(unaryop.e);
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
            }

            return e_id;
        }

        fn write(self: *Self, program: *const parser.Program) !void {
            try self.w.writeAll(
                \\strict graph {
                \\  bgcolor="transparent"
                \\  root [color="white",fontcolor="white"]
                \\
            );

            for (program.stmts.items) |stmt| {
                switch (stmt) {
                    .assignment => |a| {
                        const e_id = try self.writeExpression(a.expression);
                        const s_id = self.id();
                        const i_id = self.id();
                        try self.w.print(
                            \\  i_{[i_id]} [label="{[name]s}",color="white",fontcolor="white"]
                            \\  s_{[s_id]} [label="=",color="white",fontcolor="white"]
                            \\  s_{[s_id]} -- i_{[i_id]} [color="white"]
                            \\  s_{[s_id]} -- {[e_id]} [color="white"]
                            \\  root -- s_{[s_id]} [color="white"]
                        , .{ .i_id = i_id, .s_id = s_id, .e_id = e_id, .name = a.identifier });
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
            error.EndOfFile => std.debug.print("end of file", .{}),
            else => {
                var diag = p.getDiag() orelse std.os.exit(1);
                try utils.printError(source, diag);
            },
        }

        std.os.exit(1);
    };
    defer program.deinit();

    if (opts.args.@"dump-ast") {
        var w = std.io.getStdOut().writer();
        try (DotWriter(@TypeOf(w)){ .w = w }).write(&program);
    }
}

const This = @This();

test "all" {
    std.testing.refAllDeclsRecursive(This);
}
