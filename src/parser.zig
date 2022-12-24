const std = @import("std");
const lexer = @import("lexer.zig");

const TokLoc = lexer.TokLoc;

pub const Expression = union(enum) {
    integer: i64,
    binop: struct { op: Operation, lhs: *Expression, rhs: *Expression },

    fn eql(lhs: *const Expression, rhs: *const Expression) bool {
        if (std.meta.activeTag(lhs.*) != std.meta.activeTag(rhs.*)) return false;

        switch (lhs.*) {
            .integer => |l| return l == rhs.integer,

            .binop => |binop| {
                if (binop.op != rhs.binop.op) return false;

                return binop.lhs.eql(rhs.binop.lhs) and binop.rhs.eql(rhs.binop.rhs);
            },
        }
    }
};

/// Operation represents operators like '+', '-' etc.
pub const Operation = enum { plus, minus, multiply, divide };

pub const Statement = union(enum) {
    assignment: struct { identifier: []const u8, expression: *Expression },
    expression: *Expression,
};

/// Program represents the root of a polari AST. All nodes are arena allocated and freed in one go.
pub const Program = struct {
    arena: std.heap.ArenaAllocator,
    stmts: std.ArrayListUnmanaged(Statement),

    fn init(gpa: std.mem.Allocator) Program {
        return .{ .arena = std.heap.ArenaAllocator.init(gpa), .stmts = std.ArrayListUnmanaged(Statement){} };
    }

    /// create arena allocates an expression.
    fn create(self: *Program, expr: Expression) error{OutOfMemory}!*Expression {
        var e = try self.arena.allocator().create(Expression);
        e.* = expr;
        return e;
    }

    pub fn deinit(self: *Program) void {
        self.arena.deinit();
    }
};

const Lexer = union(enum) {
    real: lexer.Lexer,
    fake: lexer.Fake,

    fn next(self: *Lexer) !?lexer.TokLoc {
        return switch (self.*) {
            .real => |*r| r.next(),
            .fake => |*f| f.next(),
        };
    }

    fn source(self: *Lexer) []const u8 {
        return switch (self) {
            .real => |*r| r.source,
            .fake => "<no source code>",
        };
    }
};

/// bindingPower returns the power of the lhs and rhs of op. This is used in our Pratt parser for expressions. Higher
/// binding power means greater precedence.
///
/// NOTE: rhs must not be the same as lhs. rhs > lhs gives left associativity, lhs < rhs gives right associativity.
fn bindingPower(op: Operation) struct { lhs: u8, rhs: u8 } {
    return switch (op) {
        .minus => .{ .lhs = 10, .rhs = 11 },
        .plus => .{ .lhs = 30, .rhs = 31 },
        .multiply => .{ .lhs = 50, .rhs = 51 },
        .divide => .{ .lhs = 70, .rhs = 71 },
    };
}

pub const Parser = struct {
    lexer: Lexer,
    program: Program,

    peeked: ?TokLoc = null,

    const Error = error{ EndOfFile, UnexpectedChar, UnexpectedToken, OutOfMemory };

    fn init(gpa: std.mem.Allocator, l: Lexer) Parser {
        return .{ .lexer = l, .program = Program.init(gpa) };
    }

    fn peek(self: *Parser) Error!TokLoc {
        if (self.peeked) |tokloc| return tokloc;

        self.peeked = (try self.lexer.next()) orelse return error.EndOfFile;
        return self.peeked.?;
    }

    fn pop(self: *Parser) Error!TokLoc {
        if (self.peeked) |tokloc| {
            self.peeked = null;
            return tokloc;
        }

        const tokloc = (try self.lexer.next()) orelse return error.EndOfFile;
        return tokloc;
    }

    fn parseExpression(self: *Parser, min_bp: u8) Error!*Expression {
        var lhs = switch ((try self.pop()).tok) {
            .integer => |n| try self.program.create(Expression{ .integer = n }),
            else => return error.UnexpectedToken,
        };

        while (true) {
            const tokloc = self.peek() catch |e| switch (e) {
                error.EndOfFile => return lhs,
                else => return e,
            };

            const op = switch (tokloc.tok) {
                .plus => Operation.plus,
                .minus => Operation.minus,
                .asterisk => Operation.multiply,
                .forward_slash => Operation.divide,

                else => return error.UnexpectedToken,
            };

            const power = bindingPower(op);
            if (power.lhs < min_bp) return lhs;

            _ = self.pop() catch unreachable;

            const rhs = try self.parseExpression(power.rhs);

            lhs = try self.program.create(Expression{ .binop = .{ .op = op, .lhs = lhs, .rhs = rhs } });
        }
    }

    pub fn parse(self: *Parser) Error!Program {
        const tokloc = try self.peek();
        const expr = switch (tokloc.tok) {
            .integer => try self.parseExpression(0),
            else => return error.UnexpectedToken,
        };

        try self.program.stmts.append(self.program.arena.allocator(), .{ .expression = expr });

        return self.program;
    }
};

const testing = std.testing;

fn expectEqualParse(toks: []const lexer.Tok, expected: Expression) !void {
    var l = Lexer{ .fake = lexer.Fake{ .toks = toks } };
    var parser = Parser.init(testing.allocator, l);

    var actual = try parser.parse();
    defer actual.deinit();

    try testing.expectEqual(@intCast(usize, 1), actual.stmts.items.len);

    if (!actual.stmts.items[0].expression.eql(&expected)) return error.TestExpectedEquals;
}

const Tests = struct {
    fn e(allocator: std.mem.Allocator, expression: Expression) !*Expression {
        var expr = try allocator.create(Expression);
        expr.* = expression;
        return expr;
    }

    test "maths" {
        try expectEqualParse(&.{.{ .integer = 1 }}, .{ .integer = 1 });

        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var a = arena.allocator();

        try expectEqualParse(
            &.{ .{ .integer = 1 }, .plus, .{ .integer = 2 }, .asterisk, .{ .integer = 3 } },
            .{ .binop = .{
                .op = .plus,
                .lhs = try e(a, .{ .integer = 1 }),
                .rhs = try e(a, .{
                    .binop = .{
                        .op = .multiply,
                        .lhs = try e(a, .{ .integer = 2 }),
                        .rhs = try e(a, .{ .integer = 3 }),
                    },
                }),
            } },
        );

        try expectEqualParse(
            &.{ .{ .integer = 1 }, .asterisk, .{ .integer = 2 }, .minus, .{ .integer = 3 } },
            .{ .binop = .{
                .op = .minus,
                .lhs = try e(a, .{
                    .binop = .{
                        .op = .multiply,
                        .lhs = try e(a, .{ .integer = 1 }),
                        .rhs = try e(a, .{ .integer = 2 }),
                    },
                }),
                .rhs = try e(a, .{ .integer = 3 }),
            } },
        );
    }
};

test "foo" {
    testing.refAllDecls(Tests);
}
