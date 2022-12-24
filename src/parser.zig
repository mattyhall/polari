const std = @import("std");
const lexer = @import("lexer.zig");

const TokLoc = lexer.TokLoc;
const Diag = lexer.Diag;

pub const Expression = union(enum) {
    integer: i64,
    binop: struct { op: BinaryOperation, lhs: *Expression, rhs: *Expression },
    unaryop: struct { op: UnaryOperation, e: *Expression },

    fn eql(lhs: *const Expression, rhs: *const Expression) bool {
        if (std.meta.activeTag(lhs.*) != std.meta.activeTag(rhs.*)) return false;

        switch (lhs.*) {
            .integer => |l| return l == rhs.integer,
            .binop => |binop| {
                if (binop.op != rhs.binop.op) return false;

                return binop.lhs.eql(rhs.binop.lhs) and binop.rhs.eql(rhs.binop.rhs);
            },
            .unaryop => |unaryop| {
                if (unaryop.op != rhs.unaryop.op) return false;

                return unaryop.e.eql(rhs.unaryop.e);
            },
        }
    }
};

/// BinaryOperation represents infix operators like '+', '-' etc.
pub const BinaryOperation = enum { plus, minus, multiply, divide };

/// UnaryOperation represents operators taking one argument, e.g. negation.
pub const UnaryOperation = enum { negate, grouping };

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

pub const Lexer = union(enum) {
    real: lexer.Lexer,
    fake: lexer.Fake,

    fn next(self: *Lexer) !?lexer.TokLoc {
        return switch (self.*) {
            .real => |*r| r.next(),
            .fake => |*f| f.next(),
        };
    }

    pub fn getDiag(self: *const Lexer) ?Diag {
        return switch (self.*) {
            .real => |*r| r.diag,
            .fake => null,
        };
    }

    fn source(self: *Lexer) []const u8 {
        return switch (self) {
            .real => |*r| r.source,
            .fake => "<no source code>",
        };
    }
};

/// infixBindingPower returns the power of the lhs and rhs of op. This is used in our Pratt parser for expressions.
/// Higher binding power means greater precedence.
///
/// NOTE: rhs must not be the same as lhs. rhs > lhs gives left associativity, lhs < rhs gives right associativity.
fn infixBindingPower(op: BinaryOperation) struct { lhs: u8, rhs: u8 } {
    return switch (op) {
        .minus => .{ .lhs = 30, .rhs = 31 },
        .plus => .{ .lhs = 70, .rhs = 71 },
        .multiply => .{ .lhs = 110, .rhs = 111 },
        .divide => .{ .lhs = 150, .rhs = 151 },
    };
}

/// prefixBindingPower returns the power of the prefix operator op.
fn prefixBindingPower(op: UnaryOperation) u8 {
    return switch (op) {
        .negate => 191,
        .grouping => 1,
    };
}

pub const Parser = struct {
    lexer: Lexer,
    program: Program,
    gpa: std.mem.Allocator,

    peeked: ?TokLoc = null,
    diag: ?Diag = null,

    const Error = error{ EndOfFile, UnexpectedChar, UnexpectedToken, OutOfMemory };

    pub fn init(gpa: std.mem.Allocator, l: Lexer) Parser {
        return .{ .gpa = gpa, .lexer = l, .program = Program.init(gpa) };
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

    fn allocDiag(self: *Parser, loc: lexer.Loc, comptime fmt: []const u8, args: anytype) error{OutOfMemory}!void {
        const msg = try std.fmt.allocPrint(self.gpa, fmt, args);
        self.diag = Diag{ .msg = msg, .allocator = self.gpa, .loc = loc };
    }

    /// expect ensures the next token is tok.
    fn expect(self: *Parser, comptime tok: lexer.Tok) Error!void {
        switch (tok) {
            .identifier, .integer => @compileError("expect must be used with a token without a payload"),
            .equals, .plus, .minus, .forward_slash, .asterisk, .semicolon, .left_paren, .right_paren => {},
        }

        const tokloc = try self.pop();
        if (std.meta.activeTag(tokloc.tok) == std.meta.activeTag(tok)) return;

        try self.allocDiag(tokloc.loc, "unexpected token: expected {}, got {}", .{ tok, tokloc.tok });
        return error.UnexpectedToken;
    }

    /// parseStatement parses a single statement.
    fn parseStatement(self: *Parser) Error!Statement {
        const lhs_tokloc = try self.pop();

        if (lhs_tokloc.tok != .identifier) {
            var expr = try self.parseExpressionLhs(0, lhs_tokloc);
            try self.expect(.semicolon);
            return Statement{ .expression = expr };
        }

        const op_tokloc = try self.peek();
        return switch (op_tokloc.tok) {
            .equals => b: {
                var ass = try self.parseAssignment(lhs_tokloc);
                try self.expect(.semicolon);
                break :b ass;
            },
            else => b: {
                var expr = try self.parseExpressionLhs(0, lhs_tokloc);
                try self.expect(.semicolon);
                break :b Statement{ .expression = expr };
            },
        };
    }

    /// parseAssignments parses an assignment.
    ///
    /// NOTE: does not parse the trailing semicolon.
    fn parseAssignment(self: *Parser, lhs: TokLoc) Error!Statement {
        try self.expect(.equals);

        if (lhs.tok != .identifier) {
            try self.allocDiag(lhs.loc, "unexpected token: cannot assign to a {}", .{lhs.tok});
            return error.UnexpectedToken;
        }

        var rhs = try self.parseExpression(0);

        return Statement{ .assignment = .{ .identifier = lhs.tok.identifier, .expression = rhs } };
    }

    /// parseExpression parses a single expression.
    ///
    /// NOTE: does not parse the trailing semicolon.
    fn parseExpression(self: *Parser, min_bp: u8) Error!*Expression {
        const lhs_tokloc = try self.pop();
        return try self.parseExpressionLhs(min_bp, lhs_tokloc);
    }

    /// parseExpressionLhs parses an expression but requires the caller to pass the first token (the lhs) of the
    /// expression. It uses Pratt parsing to handle precedence.
    fn parseExpressionLhs(self: *Parser, min_bp: u8, lhs_tokloc: TokLoc) Error!*Expression {
        var lhs = switch (lhs_tokloc.tok) {
            .integer => |n| try self.program.create(Expression{ .integer = n }),
            .minus, .left_paren => b: {
                const op: UnaryOperation = switch (lhs_tokloc.tok) {
                    .minus => .negate,
                    .left_paren => .grouping,
                    else => unreachable,
                };
                const bp = prefixBindingPower(op);
                const e = try self.parseExpression(bp);
                if (op == .grouping) try self.expect(.right_paren);
                break :b try self.program.create(Expression{ .unaryop = .{ .op = op, .e = e } });
            },
            else => |tok| {
                try self.allocDiag(lhs_tokloc.loc, "unexpected token: expected integer, got {}", .{tok});
                return error.UnexpectedToken;
            },
        };

        while (true) {
            const tokloc = self.peek() catch |e| switch (e) {
                error.EndOfFile => return lhs,
                else => return e,
            };

            const op = switch (tokloc.tok) {
                .plus => BinaryOperation.plus,
                .minus => BinaryOperation.minus,
                .asterisk => BinaryOperation.multiply,
                .forward_slash => BinaryOperation.divide,

                .semicolon => return lhs,

                .right_paren => return lhs,

                else => {
                    try self.allocDiag(
                        tokloc.loc,
                        "unexpected token: expected operator or semicolon, got {}",
                        .{tokloc.tok},
                    );
                    return error.UnexpectedToken;
                },
            };

            const power = infixBindingPower(op);
            if (power.lhs < min_bp) return lhs;

            _ = self.pop() catch unreachable;

            const rhs = try self.parseExpression(power.rhs);

            lhs = try self.program.create(Expression{ .binop = .{ .op = op, .lhs = lhs, .rhs = rhs } });
        }
    }

    /// parse takes tokens from the lexer and parses them into a program.
    pub fn parse(self: *Parser) Error!Program {
        errdefer self.program.deinit();

        while (true) {
            _ = self.peek() catch |e| switch (e) {
                error.EndOfFile => return self.program,
                else => {
                    return e;
                },
            };

            const stmt = try self.parseStatement();
            try self.program.stmts.append(self.program.arena.allocator(), stmt);
        }

        return self.program;
    }

    /// getDiag returns the Diag of the parser or, if there isn't one, that of the lexer.
    pub fn getDiag(self: *const Parser) ?Diag {
        if (self.diag) |d| return d;
        return self.lexer.getDiag();
    }

    /// deinit deallocates parser.
    pub fn deinit(self: *Parser) void {
        if (self.diag) |*d| d.deinit();
    }
};

const testing = std.testing;

fn expectEqualParses(toks: []const lexer.Tok, expecteds: []const Statement) !void {
    var l = Lexer{ .fake = lexer.Fake{ .toks = toks } };
    var parser = Parser.init(testing.allocator, l);
    defer parser.deinit();

    var actuals = try parser.parse();
    defer actuals.deinit();

    try testing.expectEqual(expecteds.len, actuals.stmts.items.len);

    for (actuals.stmts.items) |actual, i| {
        var expected = expecteds[i];
        try testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));

        switch (actual) {
            .assignment => |a| if (!std.mem.eql(u8, a.identifier, expected.assignment.identifier) or
                !a.expression.eql(expected.assignment.expression)) return error.TestExpectedEqual,
            .expression => |e| if (!e.eql(expected.expression)) return,
        }
    }
}

fn expectEqualParse(toks: []const lexer.Tok, expected: Statement) !void {
    return expectEqualParses(toks, &.{expected});
}

fn expectEqualParseExpr(toks: []const lexer.Tok, expected: Expression) !void {
    var l = Lexer{ .fake = lexer.Fake{ .toks = toks } };
    var parser = Parser.init(testing.allocator, l);
    defer parser.deinit();

    var actual = try parser.parse();
    defer actual.deinit();

    try testing.expectEqual(@intCast(usize, 1), actual.stmts.items.len);

    if (!actual.stmts.items[0].expression.eql(&expected)) return error.TestExpectedEquals;
}

fn expectFailParse(expected: anyerror, toks: []const lexer.Tok) !void {
    var l = Lexer{ .fake = lexer.Fake{ .toks = toks } };
    var parser = Parser.init(testing.allocator, l);
    defer parser.deinit();

    try testing.expectError(expected, parser.parse());
}

const Tests = struct {
    fn e(allocator: std.mem.Allocator, expression: Expression) !*Expression {
        var expr = try allocator.create(Expression);
        expr.* = expression;
        return expr;
    }

    test "maths" {
        try expectEqualParseExpr(&.{ .{ .integer = 1 }, .semicolon }, .{ .integer = 1 });

        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var a = arena.allocator();

        try expectEqualParseExpr(
            &.{ .{ .integer = 1 }, .plus, .{ .integer = 2 }, .asterisk, .{ .integer = 3 }, .semicolon },
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

        try expectEqualParseExpr(
            &.{ .{ .integer = 1 }, .asterisk, .{ .integer = 2 }, .minus, .{ .integer = 3 }, .semicolon },
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

        try expectEqualParseExpr(
            &.{ .{ .integer = 1 }, .asterisk, .minus, .{ .integer = 2 }, .forward_slash, .{ .integer = 3 }, .semicolon },
            .{ .binop = .{
                .op = .multiply,
                .lhs = try e(a, .{ .integer = 1 }),
                .rhs = try e(a, .{
                    .binop = .{
                        .op = .divide,
                        .lhs = try e(a, .{ .unaryop = .{ .op = .negate, .e = try e(a, .{ .integer = 2 }) } }),
                        .rhs = try e(a, .{ .integer = 3 }),
                    },
                }),
            } },
        );

        try expectEqualParseExpr(
            &.{ .minus, .minus, .{ .integer = 1 }, .asterisk, .{ .integer = 2 }, .semicolon },
            .{ .binop = .{
                .op = .multiply,
                .lhs = try e(a, .{
                    .unaryop = .{
                        .op = .negate,
                        .e = try e(a, .{ .unaryop = .{ .op = .negate, .e = try e(a, .{ .integer = 1 }) } }),
                    },
                }),
                .rhs = try e(a, .{ .integer = 2 }),
            } },
        );

        try expectEqualParseExpr(
            &.{ .left_paren, .{ .integer = 1 }, .asterisk, .{ .integer = 2 }, .right_paren, .forward_slash, .{ .integer = 3 }, .semicolon },
            .{ .binop = .{
                .op = .divide,
                .lhs = try e(a, .{
                    .unaryop = .{
                        .op = .grouping,
                        .e = try e(a, .{
                            .binop = .{
                                .op = .multiply,
                                .lhs = try e(a, .{ .integer = 1 }),
                                .rhs = try e(a, .{ .integer = 2 }),
                            },
                        }),
                    },
                }),
                .rhs = try e(a, .{ .integer = 3 }),
            } },
        );
    }

    test "fail: maths" {
        try expectFailParse(error.EndOfFile, &.{.{ .integer = 1 }});
        try expectFailParse(error.UnexpectedToken, &.{ .{ .integer = 1 }, .plus, .plus });
        try expectFailParse(error.UnexpectedToken, &.{ .{ .integer = 1 }, .plus, .{ .integer = 3 }, .equals });
    }

    test "assignments" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var a = arena.allocator();

        try expectEqualParse(
            &.{ .{ .identifier = "a" }, .equals, .{ .integer = 1 }, .semicolon },
            Statement{ .assignment = .{ .identifier = "a", .expression = try e(a, .{ .integer = 1 }) } },
        );

        try expectEqualParse(&.{
            .{ .identifier = "a" },
            .equals,
            .{ .integer = 1 },
            .plus,
            .{ .integer = 2 },
            .asterisk,
            .{ .integer = 3 },
            .semicolon,
        }, Statement{ .assignment = .{ .identifier = "a", .expression = try e(a, .{ .binop = .{
            .op = .plus,
            .lhs = try e(a, .{ .integer = 1 }),
            .rhs = try e(a, .{
                .binop = .{
                    .op = .multiply,
                    .lhs = try e(a, .{ .integer = 2 }),
                    .rhs = try e(a, .{ .integer = 3 }),
                },
            }),
        } }) } });
    }

    test "fail: assignments" {
        try expectFailParse(error.UnexpectedToken, &.{.equals});
        try expectFailParse(error.EndOfFile, &.{ .{ .identifier = "a" }, .equals, .{ .integer = 1 } });
        try expectFailParse(error.UnexpectedToken, &.{ .{ .integer = 1 }, .equals, .{ .integer = 3 }, .semicolon });
        try expectFailParse(error.UnexpectedToken, &.{
            .{ .identifier = "a" },
            .equals,
            .{ .integer = 3 },
            .equals,
            .{ .integer = 4 },
            .semicolon,
        });
    }

    test "statements" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var a = arena.allocator();

        try expectEqualParses(
            &.{ .{ .integer = 1 }, .semicolon, .{ .integer = 2 }, .semicolon },
            &.{ .{ .expression = try e(a, .{ .integer = 1 }) }, .{ .expression = try e(a, .{ .integer = 2 }) } },
        );
    }
};

test "all" {
    testing.refAllDecls(Tests);
}