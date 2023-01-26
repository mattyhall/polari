const std = @import("std");
const lexer = @import("lexer.zig");

const TokLoc = lexer.TokLoc;
const Loc = lexer.Loc;
const Diag = lexer.Diag;

pub const Parameter = union(enum) { identifier: []const u8 };

pub const Function = struct { params: []Located(Parameter), body: Located(*Expression) };

pub const Expression = union(enum) {
    integer: i64,
    boolean: bool,
    identifier: []const u8,
    binop: struct { op: BinaryOperation, lhs: Located(*Expression), rhs: Located(*Expression) },
    unaryop: struct { op: UnaryOperation, e: Located(*Expression) },
    let: struct { assignments: []Located(Assignment), in: Located(*Expression) },
    function: Function,
    apply: struct { f: Located(*Expression), args: []Located(*Expression) },
    @"if": struct { condition: Located(*Expression), then: Located(*Expression), @"else": Located(*Expression) },

    pub fn eql(lhs: *const Expression, rhs: *const Expression) bool {
        if (std.meta.activeTag(lhs.*) != std.meta.activeTag(rhs.*)) return false;

        switch (lhs.*) {
            .integer => |l| return l == rhs.integer,
            .boolean => |b| return b == rhs.boolean,
            .identifier => |i| return std.mem.eql(u8, i, rhs.identifier),
            .binop => |binop| {
                if (binop.op != rhs.binop.op) return false;

                return binop.lhs.inner.eql(rhs.binop.lhs.inner) and binop.rhs.inner.eql(rhs.binop.rhs.inner);
            },
            .unaryop => |unaryop| {
                if (unaryop.op != rhs.unaryop.op) return false;

                return unaryop.e.inner.eql(rhs.unaryop.e.inner);
            },
            .let => |let| {
                if (let.assignments.len != rhs.let.assignments.len) return false;

                for (let.assignments) |expr_lhs, i| {
                    const expr_rhs = rhs.let.assignments[i];
                    if (!expr_lhs.inner.eql(&expr_rhs.inner)) return false;
                }

                return let.in.inner.eql(rhs.let.in.inner);
            },
            .function => |f| {
                const rhs_f = rhs.function;
                if (f.params.len != rhs.function.params.len) return false;

                for (f.params) |param, i| {
                    if (std.meta.activeTag(param.inner) != std.meta.activeTag(rhs_f.params[i].inner)) return false;

                    switch (param.inner) {
                        .identifier => |ident| if (!std.mem.eql(u8, rhs_f.params[i].inner.identifier, ident))
                            return false,
                    }
                }

                return f.body.inner.eql(rhs_f.body.inner);
            },
            .apply => |f| {
                if (!f.f.inner.eql(rhs.apply.f.inner)) return false;

                for (f.args) |arg, i| {
                    if (!arg.inner.eql(rhs.apply.args[i].inner)) return false;
                }

                return true;
            },
            .@"if" => |f| {
                const rhs_if = rhs.@"if";
                return f.condition.inner.eql(rhs_if.condition.inner) and
                    f.then.inner.eql(rhs_if.then.inner) and
                    f.@"else".inner.eql(rhs_if.@"else".inner);
            },
        }
    }

    pub fn write(self: *const Expression, writer: anytype) !void {
        switch (self.*) {
            .integer => |i| try writer.print("{}", .{i}),
            .boolean => |b| try writer.print("{}", .{b}),
            .identifier => |n| try writer.print("{s}", .{n}),
            .binop => |binop| {
                const op = switch (binop.op) {
                    .plus => "+",
                    .minus => "-",
                    .multiply => "*",
                    .divide => "/",
                    .eq => "==",
                    .neq => "!=",
                    .lt => "<",
                    .lte => "<=",
                    .gt => ">",
                    .gte => ">=",
                    .apply => {
                        try writer.writeAll("(");
                        try binop.lhs.inner.write(writer);
                        try writer.writeAll(" ");
                        try binop.rhs.inner.write(writer);
                        try writer.writeAll(")");
                        return;
                    },
                };
                try writer.print("({s} ", .{op});
                try binop.lhs.inner.write(writer);
                try writer.print(" ", .{});
                try binop.rhs.inner.write(writer);
                try writer.print(")", .{});
            },
            .unaryop => |unaryop| {
                switch (unaryop.op) {
                    .negate => try writer.print("(- ", .{}),
                    .grouping => try writer.print("(", .{}),
                }

                try unaryop.e.inner.write(writer);
                try writer.print(")", .{});
            },
            .let => |let| {
                try writer.writeAll("(let [");
                for (let.assignments) |assignment| {
                    try writer.print("{s}=", .{assignment.inner.identifier});
                    try assignment.inner.expression.inner.write(writer);
                    try writer.writeAll(";");
                }
                try writer.writeAll("] ");

                try let.in.inner.write(writer);
                try writer.writeAll(")");
            },
            .function => |f| {
                try writer.writeAll("(fn [");
                for (f.params) |param, i| {
                    switch (param.inner) {
                        .identifier => |ident| try writer.writeAll(ident),
                    }

                    if (i != f.params.len - 1) try writer.writeAll(" ");
                }
                try writer.writeAll("] ");

                try f.body.inner.write(writer);
                try writer.writeAll(")");
            },
            .apply => |f| {
                try writer.writeAll("(");
                try f.f.inner.write(writer);
                for (f.args) |arg| {
                    try writer.writeAll(" ");
                    try arg.inner.write(writer);
                }
                try writer.writeAll(")");
            },
            .@"if" => |f| {
                try writer.writeAll("(if ");
                try f.condition.inner.write(writer);
                try writer.writeAll(" ");
                try f.then.inner.write(writer);
                try writer.writeAll(" ");
                try f.@"else".inner.write(writer);
                try writer.writeAll(")");
            },
        }
    }
};

/// BinaryOperation represents infix operators like '+', '-' etc.
pub const BinaryOperation = enum { plus, minus, multiply, divide, apply, eq, neq, lt, lte, gt, gte };

/// UnaryOperation represents operators taking one argument, e.g. negation.
pub const UnaryOperation = enum { negate, grouping };

/// Assignment is an assignment of a variable (identifier) to an expression. Can appear at the top level or in a let..in
/// binding.
pub const Assignment = struct {
    identifier: []const u8,
    expression: Located(*Expression),

    pub fn eql(lhs: *const Assignment, rhs: *const Assignment) bool {
        if (!std.mem.eql(u8, lhs.identifier, rhs.identifier)) return false;
        return lhs.expression.inner.eql(rhs.expression.inner);
    }
};

pub const SignatureType = union(enum) {
    mono: []const u8,
    construct: struct { constructor: []const u8, args: []const SignatureType },

    fn eql(lhs: SignatureType, rhs: SignatureType) bool {
        if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;

        switch (lhs) {
            .mono => |m| return std.mem.eql(u8, m, rhs.mono),
            .construct => |c_lhs| {
                const c_rhs = rhs.construct;
                if (!std.mem.eql(u8, c_lhs.constructor, c_rhs.constructor)) return false;
                if (c_lhs.args.len != c_rhs.args.len) return false;

                for (c_lhs.args) |lhs_arg, i| {
                    if (!lhs_arg.eql(c_rhs.args[i])) return false;
                }

                return true;
            },
        }
    }

    fn write(self: *const SignatureType, writer: anytype) !void {
        switch (self.*) {
            .mono => |m| try writer.writeAll(m),
            .construct => |c| {
                try writer.print("({s} ", .{c.constructor});

                for (c.args) |arg, i| {
                    try arg.write(writer);
                    if (i < c.args.len - 1) try writer.writeAll(" ");
                }

                try writer.writeAll(")");
            },
        }
    }
};

/// Signature represents a type signature. This makes the type of identifier to be type. The type can either be a
/// monotype (e.g. Int, Bool etc) or a construct (e.g. [Int], Option a etc).
pub const Signature = struct {
    identifier: []const u8,
    type: SignatureType,

    fn eql(lhs: Signature, rhs: Signature) bool {
        if (!std.mem.eql(u8, lhs.identifier, rhs.identifier)) return false;

        return lhs.type.eql(rhs.type);
    }
};

pub const Statement = union(enum) {
    assignment: Assignment,
    expression: *Expression,
    signature: Signature,

    pub fn write(self: Statement, w: anytype) !void {
        switch (self) {
            .expression => |e| try e.write(w),
            .assignment => |ass| {
                try w.print("{s} = ", .{ass.identifier});
                try ass.expression.inner.write(w);
            },
            .signature => |s| {
                try w.print("{s} : ", .{s.identifier});
                try s.type.write(w);
            },
        }
        try w.writeAll(";\n");
    }
};

/// Located wraps T in a struct with a Loc.
pub fn Located(comptime T: anytype) type {
    return struct { loc: Loc, inner: T };
}

/// locate puts v in its Located struct, with loc as the location.
pub fn locate(loc: Loc, v: anytype) Located(@TypeOf(v)) {
    return .{ .loc = loc, .inner = v };
}

/// Program represents the root of a polari AST. All nodes are arena allocated and freed in one go.
pub const Program = struct {
    arena: std.heap.ArenaAllocator,
    stmts: std.ArrayListUnmanaged(Located(Statement)),

    fn init(gpa: std.mem.Allocator) Program {
        return .{ .arena = std.heap.ArenaAllocator.init(gpa), .stmts = std.ArrayListUnmanaged(Located(Statement)){} };
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

    pub fn getDiag(self: *const Lexer) ?*const Diag {
        return switch (self.*) {
            .real => |*r| if (r.diag) |*d| d else null,
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
        .apply => .{ .lhs = 240, .rhs = 241 },
        .eq => .{ .lhs = 10, .rhs = 11 },
        .neq => .{ .lhs = 10, .rhs = 11 },
        .lt => .{ .lhs = 10, .rhs = 11 },
        .lte => .{ .lhs = 10, .rhs = 11 },
        .gt => .{ .lhs = 10, .rhs = 11 },
        .gte => .{ .lhs = 10, .rhs = 11 },
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
    diags: std.ArrayListUnmanaged(Diag) = .{},
    err: ?Error = null,

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
        try self.diags.append(self.gpa, Diag{ .msg = msg, .allocator = self.gpa, .loc = loc });
    }

    /// expect ensures the next token is tok.
    fn expect(self: *Parser, comptime tok: lexer.Tok) Error!void {
        switch (tok) {
            .identifier, .integer => @compileError("expect must be used with a token without a payload"),
            .equals, .plus, .minus, .forward_slash, .asterisk, .semicolon, .left_paren, .right_paren => {},
            .true, .false => {},
            .let, .in, .@"fn", .fat_arrow, .@"if", .@"else", .elif, .then, .colon => {},
            .eq, .neq, .gt, .gte, .lt, .lte => {},
        }

        const tokloc = try self.pop();
        if (std.meta.activeTag(tokloc.tok) == std.meta.activeTag(tok)) return;

        try self.allocDiag(tokloc.loc, "unexpected token: expected {}, got {}", .{ tok, tokloc.tok });
        return error.UnexpectedToken;
    }

    /// parseType parses a type in a signature.
    fn parseType(self: *Parser, allow_constructors: bool) Error!SignatureType {
        const tokloc = try self.pop();
        switch (tokloc.tok) {
            .identifier => |i| {
                if (!std.ascii.isUpper(i[0]) or !allow_constructors) {
                    return SignatureType{ .mono = i };
                }

                var args = std.ArrayListUnmanaged(SignatureType){};

                while (true) {
                    const new_tokloc = try self.peek();
                    const ty = switch (new_tokloc.tok) {
                        .left_paren => b: {
                            _ = self.pop() catch unreachable;

                            const ty = try self.parseType(true);
                            try self.expect(.right_paren);
                            break :b ty;
                        },
                        .identifier => |inner_i| b: {
                            _ = self.pop() catch unreachable;
                            break :b SignatureType{ .mono = inner_i };
                        },

                        else => break,
                    };

                    try args.append(self.program.arena.allocator(), ty);
                }

                if (args.items.len == 0) return SignatureType{ .mono = i };

                return SignatureType{ .construct = .{
                    .constructor = i,
                    .args = try args.toOwnedSlice(self.program.arena.allocator()),
                } };
            },
            .left_paren => {
                const ty = try self.parseType(true);
                try self.expect(.right_paren);
                return ty;
            },
            .right_paren => {
                try self.allocDiag(tokloc.loc, "unexpected token ')', expected type", .{});
                return error.UnexpectedToken;
            },
            .@"fn" => {
                var args = std.ArrayListUnmanaged(SignatureType){};

                while (true) {
                    const new_tokloc = try self.peek();
                    const ty = switch (new_tokloc.tok) {
                        .fat_arrow => break,
                        else => try self.parseType(false),
                    };

                    try args.append(self.program.arena.allocator(), ty);
                }

                _ = self.pop() catch unreachable;

                const ret = try self.parseType(false);
                try args.append(self.gpa, ret);

                return SignatureType{ .construct = .{
                    .constructor = "->",
                    .args = try args.toOwnedSlice(self.program.arena.allocator()),
                } };
            },
            else => {
                try self.allocDiag(tokloc.loc, "unexpected token {}, expected type", .{tokloc.tok});
                return error.UnexpectedToken;
            },
        }
    }

    /// parseSignature parses a type signature.
    fn parseSignature(self: *Parser, tokloc: TokLoc) Error!Signature {
        try self.expect(.colon);
        const ty = try self.parseType(true);
        return Signature{ .identifier = tokloc.tok.identifier, .type = ty };
    }

    /// parseStatement parses a single statement.
    fn parseStatement(self: *Parser) Error!Located(Statement) {
        const lhs_tokloc = try self.pop();

        if (lhs_tokloc.tok != .identifier) {
            var expr = try self.parseExpressionLhs(0, lhs_tokloc);
            try self.expect(.semicolon);
            return locate(lhs_tokloc.loc, Statement{ .expression = expr.inner });
        }

        const op_tokloc = try self.peek();
        return switch (op_tokloc.tok) {
            .equals => b: {
                var ass = try self.parseAssignment(lhs_tokloc);
                try self.expect(.semicolon);
                break :b locate(ass.loc, Statement{ .assignment = ass.inner });
            },
            .colon => b: {
                var sig = try self.parseSignature(lhs_tokloc);
                try self.expect(.semicolon);
                break :b locate(lhs_tokloc.loc, Statement{ .signature = sig });
            },
            else => b: {
                var expr = try self.parseExpressionLhs(0, lhs_tokloc);
                try self.expect(.semicolon);
                break :b locate(lhs_tokloc.loc, Statement{ .expression = expr.inner });
            },
        };
    }

    /// parseAssignments parses an assignment.
    ///
    /// NOTE: does not parse the trailing semicolon.
    fn parseAssignment(self: *Parser, lhs: TokLoc) Error!Located(Assignment) {
        try self.expect(.equals);

        if (lhs.tok != .identifier) {
            try self.allocDiag(lhs.loc, "unexpected token: cannot assign to a {}", .{lhs.tok});
            return error.UnexpectedToken;
        }

        var rhs = try self.parseExpression(0);

        return locate(lhs.loc, Assignment{ .identifier = lhs.tok.identifier, .expression = rhs });
    }

    /// parseExpression parses a single expression.
    ///
    /// NOTE: does not parse the trailing semicolon.
    fn parseExpression(self: *Parser, min_bp: u8) Error!Located(*Expression) {
        const lhs_tokloc = try self.pop();
        return try self.parseExpressionLhs(min_bp, lhs_tokloc);
    }

    /// parseLet parses a let ... in expression.
    fn parseLet(self: *Parser, loc: Loc) Error!Located(*Expression) {
        var al = std.ArrayListUnmanaged(Located(Assignment)){};
        errdefer al.deinit(self.program.arena.allocator());

        var i: usize = 0;
        while (true) : (i += 1) {
            const first = try self.peek();
            if (first.tok == .in) {
                _ = try self.pop();
                break;
            }

            var ass = try self.parseAssignment(self.pop() catch unreachable);
            try al.append(self.program.arena.allocator(), ass);

            const tokloc = try self.pop();
            switch (tokloc.tok) {
                .semicolon => continue,
                else => {},
            }

            try self.allocDiag(tokloc.loc, "unexpected token: expected semicolon, got: {}", .{tokloc.tok});
            return error.UnexpectedToken;
        }

        if (i == 0) {
            try self.allocDiag(loc, "let must have at least one binding", .{});
            return error.UnexpectedToken;
        }

        var e = try self.parseExpression(0);
        return locate(loc, try self.program.create(Expression{ .let = .{ .assignments = al.items, .in = e } }));
    }

    /// parseFn parses a function.
    fn parseFn(self: *Parser, loc: Loc) Error!Located(*Expression) {
        var al = std.ArrayListUnmanaged(Located(Parameter)){};
        errdefer al.deinit(self.program.arena.allocator());

        var i: usize = 0;
        while (true) : (i += 1) {
            const tokloc = try self.pop();
            switch (tokloc.tok) {
                .identifier => {},
                .fat_arrow => break,
                else => {
                    try self.allocDiag(
                        tokloc.loc,
                        "unexpected token: expected an identifier, got: {}",
                        .{tokloc.tok},
                    );
                    return error.UnexpectedToken;
                },
            }

            try al.append(self.program.arena.allocator(), .{
                .loc = tokloc.loc,
                .inner = .{ .identifier = tokloc.tok.identifier },
            });
        }

        if (i == 0) {
            try self.allocDiag(loc, "fn must have at least one parameter", .{});
            return error.UnexpectedToken;
        }

        var e = try self.parseExpression(0);
        return locate(loc, try self.program.create(Expression{ .function = .{ .params = al.items, .body = e } }));
    }

    fn parseIf(self: *Parser, loc: Loc) Error!Located(*Expression) {
        var condition = try self.parseExpression(0);

        const hopefully_then = try self.pop();
        switch (hopefully_then.tok) {
            .then => {},
            else => |t| {
                try self.allocDiag(hopefully_then.loc, "unexpected token {}, expected then", .{t});
                return error.UnexpectedToken;
            },
        }

        var then = try self.parseExpression(0);

        const tokloc = try self.pop();
        var else_e = switch (tokloc.tok) {
            .@"else" => try self.parseExpression(0),
            .elif => try self.parseIf(tokloc.loc),
            else => |t| {
                try self.allocDiag(hopefully_then.loc, "unexpected token {}, expected else", .{t});
                return error.UnexpectedToken;
            },
        };

        return .{ .loc = loc, .inner = try self.program.create(Expression{ .@"if" = .{
            .condition = condition,
            .then = then,
            .@"else" = else_e,
        } }) };
    }

    /// parseExpressionLhs parses an expression but requires the caller to pass the first token (the lhs) of the
    /// expression. It uses Pratt parsing to handle precedence.
    fn parseExpressionLhs(self: *Parser, min_bp: u8, lhs_tokloc: TokLoc) Error!Located(*Expression) {
        const l = lhs_tokloc.loc;
        var lhs = switch (lhs_tokloc.tok) {
            .integer => |n| locate(l, try self.program.create(Expression{ .integer = n })),
            .true => locate(l, try self.program.create(Expression{ .boolean = true })),
            .false => locate(l, try self.program.create(Expression{ .boolean = false })),
            .identifier => |n| locate(l, try self.program.create(Expression{ .identifier = n })),
            .minus, .left_paren => b: {
                const op: UnaryOperation = switch (lhs_tokloc.tok) {
                    .minus => .negate,
                    .left_paren => .grouping,
                    else => unreachable,
                };
                const bp = prefixBindingPower(op);
                const e = try self.parseExpression(bp);
                if (op == .grouping) try self.expect(.right_paren);
                break :b locate(l, try self.program.create(Expression{ .unaryop = .{ .op = op, .e = e } }));
            },
            .let => return try self.parseLet(l),
            .@"fn" => return try self.parseFn(l),
            .@"if" => return try self.parseIf(l),
            else => |tok| {
                try self.allocDiag(lhs_tokloc.loc, "unexpected token: expected expression, got {}", .{tok});
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
                .eq => BinaryOperation.eq,
                .neq => BinaryOperation.neq,
                .lt => BinaryOperation.lt,
                .lte => BinaryOperation.lte,
                .gt => BinaryOperation.gt,
                .gte => BinaryOperation.gte,

                .semicolon, .right_paren, .in, .then, .elif, .@"else" => return lhs,

                .identifier, .left_paren, .integer, .true, .false => .apply,

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

            if (op != .apply) _ = self.pop() catch unreachable;

            const rhs = try self.parseExpression(power.rhs);

            lhs = locate(l, try self.program.create(Expression{ .binop = .{ .op = op, .lhs = lhs, .rhs = rhs } }));
        }
    }

    /// skipUntilAfterSemicolon keeps popping tokens until it reaches a semicolon. It then moves to the next token.
    /// This is because we want to capture as many errors as possible.
    fn skipUntilAfterSemicolon(self: *Parser) Error!void {
        while (true) {
            const tokloc = self.pop() catch |e| switch (e) {
                error.EndOfFile => return,
                else => return e,
            };

            if (tokloc.tok == .semicolon) {
                return;
            }
        }
    }

    /// parse takes tokens from the lexer and parses them into a program.
    pub fn parse(self: *Parser) Error!Program {
        errdefer self.program.deinit();

        while (true) {
            _ = self.peek() catch |e| switch (e) {
                error.EndOfFile => break,
                else => return e,
            };

            const stmt = self.parseStatement() catch |e| switch (e) {
                error.EndOfFile, error.OutOfMemory => return e,
                else => {
                    self.err = e;
                    try self.skipUntilAfterSemicolon();
                    continue;
                },
            };
            try self.program.stmts.append(self.program.arena.allocator(), stmt);
        }

        if (self.err) |e| return e;

        return self.program;
    }

    /// getDiags returns the Diags of the parser or, if there isn't one, those of the lexer.
    pub fn getDiags(self: *const Parser) []const Diag {
        if (self.diags.items.len > 0) return self.diags.items;

        if (self.lexer.getDiag()) |d| {
            var a: []const Diag = undefined;
            a.ptr = @ptrCast([*]const Diag, d);
            a.len = 1;
            return a;
        }

        return &[0]Diag{};
    }

    /// deinit deallocates parser.
    pub fn deinit(self: *Parser) void {
        for (self.diags.items) |*diag| {
            diag.deinit();
        }

        self.diags.deinit(self.gpa);
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
        try testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual.inner));

        switch (actual.inner) {
            .assignment => |a| if (!std.mem.eql(u8, a.identifier, expected.assignment.identifier) or
                !a.expression.inner.eql(expected.assignment.expression.inner)) return error.TestExpectedEqual,
            .expression => |e| if (!e.eql(expected.expression)) return,
            .signature => |s| if (!s.eql(expected.signature)) return,
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

    if (!actual.stmts.items[0].inner.expression.eql(&expected)) return error.TestExpectedEquals;
}

fn expectFailParse(expected: anyerror, toks: []const lexer.Tok) !void {
    var l = Lexer{ .fake = lexer.Fake{ .toks = toks } };
    var parser = Parser.init(testing.allocator, l);
    defer parser.deinit();

    try testing.expectError(expected, parser.parse());
}

test "Located" {
    const T = Located(struct { foo: []const u8 });
    var t = T{ .loc = .{}, .inner = .{ .foo = "foo" } };
    try testing.expectEqualStrings("foo", t.inner.foo);
}

const Tests = struct {
    fn e(allocator: std.mem.Allocator, expression: Expression) !*Expression {
        var expr = try allocator.create(Expression);
        expr.* = expression;
        return expr;
    }

    const loc = Loc{};

    test "boolean" {
        try expectEqualParseExpr(&.{ .true, .semicolon }, .{ .boolean = true });
        try expectEqualParseExpr(&.{ .false, .semicolon }, .{ .boolean = false });
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
                .lhs = locate(loc, try e(a, .{ .integer = 1 })),
                .rhs = locate(loc, try e(a, .{
                    .binop = .{
                        .op = .multiply,
                        .lhs = locate(loc, try e(a, .{ .integer = 2 })),
                        .rhs = locate(loc, try e(a, .{ .integer = 3 })),
                    },
                })),
            } },
        );

        try expectEqualParseExpr(
            &.{ .{ .integer = 1 }, .asterisk, .{ .integer = 2 }, .minus, .{ .integer = 3 }, .semicolon },
            .{ .binop = .{
                .op = .minus,
                .lhs = locate(loc, try e(a, .{
                    .binop = .{
                        .op = .multiply,
                        .lhs = locate(loc, try e(a, .{ .integer = 1 })),
                        .rhs = locate(loc, try e(a, .{ .integer = 2 })),
                    },
                })),
                .rhs = locate(loc, try e(a, .{ .integer = 3 })),
            } },
        );

        try expectEqualParseExpr(
            &.{ .{ .integer = 1 }, .asterisk, .minus, .{ .integer = 2 }, .forward_slash, .{ .integer = 3 }, .semicolon },
            .{ .binop = .{
                .op = .multiply,
                .lhs = locate(loc, try e(a, .{ .integer = 1 })),
                .rhs = locate(loc, try e(a, .{
                    .binop = .{
                        .op = .divide,
                        .lhs = locate(
                            loc,
                            try e(a, .{ .unaryop = .{ .op = .negate, .e = locate(loc, try e(a, .{ .integer = 2 })) } }),
                        ),
                        .rhs = locate(loc, try e(a, .{ .integer = 3 })),
                    },
                })),
            } },
        );

        try expectEqualParseExpr(
            &.{ .minus, .minus, .{ .integer = 1 }, .asterisk, .{ .integer = 2 }, .semicolon },
            .{ .binop = .{
                .op = .multiply,
                .lhs = locate(loc, try e(a, .{
                    .unaryop = .{
                        .op = .negate,
                        .e = locate(loc, try e(a, .{
                            .unaryop = .{ .op = .negate, .e = locate(loc, try e(a, .{ .integer = 1 })) },
                        })),
                    },
                })),
                .rhs = locate(loc, try e(a, .{ .integer = 2 })),
            } },
        );

        try expectEqualParseExpr(
            &.{ .left_paren, .{ .integer = 1 }, .asterisk, .{ .integer = 2 }, .right_paren, .forward_slash, .{ .integer = 3 }, .semicolon },
            .{ .binop = .{
                .op = .divide,
                .lhs = locate(loc, try e(a, .{
                    .unaryop = .{
                        .op = .grouping,
                        .e = locate(loc, try e(a, .{
                            .binop = .{
                                .op = .multiply,
                                .lhs = locate(loc, try e(a, .{ .integer = 1 })),
                                .rhs = locate(loc, try e(a, .{ .integer = 2 })),
                            },
                        })),
                    },
                })),
                .rhs = locate(loc, try e(a, .{ .integer = 3 })),
            } },
        );
    }

    test "fail: maths" {
        try expectFailParse(error.EndOfFile, &.{.{ .integer = 1 }});
        try expectFailParse(error.UnexpectedToken, &.{ .{ .integer = 1 }, .plus, .plus });
        try expectFailParse(error.UnexpectedToken, &.{ .{ .integer = 1 }, .plus, .{ .integer = 3 }, .equals });
    }

    test "boolean binops" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var a = arena.allocator();

        try expectEqualParseExpr(
            &.{ .{ .integer = 1 }, .eq, .{ .integer = 2 }, .semicolon },
            .{ .binop = .{
                .op = .eq,
                .lhs = locate(loc, try e(a, .{ .integer = 1 })),
                .rhs = locate(loc, try e(a, .{ .integer = 2 })),
            } },
        );

        try expectEqualParseExpr(
            &.{ .{ .integer = 1 }, .gte, .{ .integer = 2 }, .plus, .{ .identifier = "a" }, .semicolon },
            .{ .binop = .{
                .op = .gte,
                .lhs = locate(loc, try e(a, .{ .integer = 1 })),
                .rhs = locate(loc, try e(a, .{ .binop = .{
                    .op = .plus,
                    .lhs = locate(loc, try e(a, .{ .integer = 2 })),
                    .rhs = locate(loc, try e(a, .{ .identifier = "a" })),
                } })),
            } },
        );
    }

    test "assignments" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var a = arena.allocator();

        try expectEqualParse(
            &.{ .{ .identifier = "a" }, .equals, .{ .integer = 1 }, .semicolon },
            Statement{ .assignment = .{ .identifier = "a", .expression = locate(loc, try e(a, .{ .integer = 1 })) } },
        );

        try expectEqualParse(
            &.{ .{ .identifier = "a" }, .equals, .{ .identifier = "b" }, .semicolon },
            Statement{ .assignment = .{ .identifier = "a", .expression = locate(loc, try e(a, .{ .identifier = "b" })) } },
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
        }, Statement{ .assignment = .{ .identifier = "a", .expression = locate(loc, try e(a, .{ .binop = .{
            .op = .plus,
            .lhs = locate(loc, try e(a, .{ .integer = 1 })),
            .rhs = locate(loc, try e(a, .{
                .binop = .{
                    .op = .multiply,
                    .lhs = locate(loc, try e(a, .{ .integer = 2 })),
                    .rhs = locate(loc, try e(a, .{ .integer = 3 })),
                },
            })),
        } })) } });
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

    test "let..in" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var a = arena.allocator();

        try expectEqualParseExpr(
            &.{
                .let,
                .{ .identifier = "a" },
                .equals,
                .{ .integer = 1 },
                .semicolon,
                .in,
                .{ .identifier = "a" },
                .semicolon,
            },
            .{ .let = .{
                .assignments = &[_]Located(Assignment){
                    locate(loc, Assignment{
                        .identifier = "a",
                        .expression = locate(loc, try e(a, .{ .integer = 1 })),
                    }),
                },
                .in = locate(loc, try e(a, .{ .identifier = "a" })),
            } },
        );

        try expectEqualParseExpr(
            &.{
                .let,
                .{ .identifier = "a" },
                .equals,
                .{ .integer = 1 },
                .semicolon,
                .in,
                .{ .identifier = "a" },
                .plus,
                .let,
                .{ .identifier = "b" },
                .equals,
                .{ .integer = 2 },
                .semicolon,
                .in,
                .{ .identifier = "b" },
                .semicolon,
            },
            .{ .let = .{
                .assignments = &[_]Located(Assignment){
                    locate(loc, Assignment{
                        .identifier = "a",
                        .expression = locate(loc, try e(a, .{ .integer = 1 })),
                    }),
                },
                .in = locate(loc, try e(a, .{ .binop = .{
                    .op = .plus,
                    .lhs = locate(loc, try e(a, .{ .identifier = "a" })),
                    .rhs = locate(loc, try e(a, .{ .let = .{
                        .assignments = &[_]Located(Assignment){
                            locate(loc, Assignment{
                                .identifier = "b",
                                .expression = locate(loc, try e(a, .{ .integer = 2 })),
                            }),
                        },
                        .in = locate(loc, try e(a, .{ .identifier = "b" })),
                    } })),
                } })),
            } },
        );
    }

    test "fail: let..in" {
        try expectFailParse(
            error.UnexpectedToken,
            &.{ .let, .{ .identifier = "a" }, .equals, .{ .integer = 1 }, .in, .{ .identifier = "a" }, .semicolon },
        );

        try expectFailParse(
            error.EndOfFile,
            &.{ .let, .{ .identifier = "a" }, .equals, .{ .integer = 1 }, .{ .identifier = "a" }, .semicolon },
        );
    }

    test "functions" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var a = arena.allocator();

        try expectEqualParseExpr(
            &.{
                .@"fn",
                .{ .identifier = "a" },
                .fat_arrow,
                .{ .identifier = "a" },
                .plus,
                .{ .integer = 1 },
                .semicolon,
            },
            .{ .function = .{
                .params = &[_]Located(Parameter){locate(loc, Parameter{ .identifier = "a" })},
                .body = locate(loc, try e(a, .{ .binop = .{
                    .op = .plus,
                    .lhs = locate(loc, try e(a, .{ .identifier = "a" })),
                    .rhs = locate(loc, try e(a, .{ .integer = 1 })),
                } })),
            } },
        );
    }

    test "fail: functions" {
        try expectFailParse(
            error.UnexpectedToken,
            &.{ .@"fn", .{ .identifier = "a" }, .fat_arrow, .semicolon },
        );
    }

    test "application" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var a = arena.allocator();

        try expectEqualParseExpr(
            &.{ .{ .identifier = "f" }, .true, .semicolon },
            .{ .binop = .{
                .op = .apply,
                .lhs = locate(loc, try e(a, .{ .identifier = "f" })),
                .rhs = locate(loc, try e(a, .{ .boolean = true })),
            } },
        );

        try expectEqualParseExpr(
            &.{
                .{ .identifier = "f" },
                .{ .identifier = "a" },
                .{ .integer = 1 },
                .plus,
                .{ .integer = 2 },
                .semicolon,
            },
            .{
                .binop = .{
                    .op = .plus,
                    .lhs = locate(loc, try e(a, .{ .binop = .{
                        .op = .apply,
                        .lhs = locate(loc, try e(a, .{ .binop = .{
                            .op = .apply,
                            .lhs = locate(loc, try e(a, .{ .identifier = "f" })),
                            .rhs = locate(loc, try e(a, .{ .identifier = "a" })),
                        } })),
                        .rhs = locate(loc, try e(a, .{ .integer = 1 })),
                    } })),
                    .rhs = locate(loc, try e(a, .{ .integer = 2 })),
                },
            },
        );
    }

    test "if..elif..else" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var a = arena.allocator();

        try expectEqualParseExpr(
            &.{ .@"if", .true, .then, .{ .identifier = "a" }, .@"else", .{ .identifier = "b" }, .semicolon },
            .{ .@"if" = .{
                .condition = locate(loc, try e(a, .{ .boolean = true })),
                .then = locate(loc, try e(a, .{ .identifier = "a" })),
                .@"else" = locate(loc, try e(a, .{ .identifier = "b" })),
            } },
        );

        try expectEqualParseExpr(
            &.{
                .@"if",
                .true,
                .then,
                .{ .identifier = "a" },
                .elif,
                .false,
                .then,
                .{ .identifier = "b" },
                .@"else",
                .{ .identifier = "c" },
                .semicolon,
            },
            .{ .@"if" = .{
                .condition = locate(loc, try e(a, .{ .boolean = true })),
                .then = locate(loc, try e(a, .{ .identifier = "a" })),
                .@"else" = locate(loc, try e(a, Expression{ .@"if" = .{
                    .condition = locate(loc, try e(a, .{ .boolean = false })),
                    .then = locate(loc, try e(a, .{ .identifier = "b" })),
                    .@"else" = locate(loc, try e(a, .{ .identifier = "c" })),
                } })),
            } },
        );
    }

    test "signatures" {
        try expectEqualParse(
            &.{ .{ .identifier = "a" }, .colon, .{ .identifier = "Int" }, .semicolon },
            .{ .signature = .{
                .identifier = "a",
                .type = .{ .mono = "Int" },
            } },
        );

        try expectEqualParse(
            &.{ .{ .identifier = "a" }, .colon, .{ .identifier = "Madeup" }, .semicolon },
            .{ .signature = .{
                .identifier = "a",
                .type = .{ .mono = "Madeup" },
            } },
        );

        try expectEqualParse(
            &.{ .{ .identifier = "a" }, .colon, .{ .identifier = "Option" }, .{ .identifier = "a" }, .semicolon },
            .{ .signature = .{
                .identifier = "a",
                .type = .{ .construct = .{ .constructor = "Option", .args = &.{.{ .mono = "a" }} } },
            } },
        );

        try expectEqualParse(
            &.{
                .{ .identifier = "a" },
                .colon,
                .left_paren,
                .{ .identifier = "Option" },
                .{ .identifier = "a" },
                .right_paren,
                .semicolon,
            },
            .{ .signature = .{
                .identifier = "a",
                .type = .{ .construct = .{ .constructor = "Option", .args = &.{.{ .mono = "a" }} } },
            } },
        );

        try expectEqualParse(
            &.{
                .{ .identifier = "a" },
                .colon,
                .@"fn",
                .{ .identifier = "Int" },
                .fat_arrow,
                .{ .identifier = "Int" },
                .semicolon,
            },
            .{ .signature = .{
                .identifier = "a",
                .type = .{ .construct = .{
                    .constructor = "->",
                    .args = &.{ .{ .mono = "Int" }, .{ .mono = "Int" } },
                } },
            } },
        );

        try expectEqualParse(
            &.{
                .{ .identifier = "a" },
                .colon,
                .{ .identifier = "Option" },
                .left_paren,
                .@"fn",
                .{ .identifier = "Int" },
                .fat_arrow,
                .{ .identifier = "Int" },
                .right_paren,
                .semicolon,
            },
            .{ .signature = .{
                .identifier = "a",
                .type = .{ .construct = .{
                    .constructor = "Option",
                    .args = &.{.{ .construct = .{
                        .constructor = "->",
                        .args = &.{ .{ .mono = "Int" }, .{ .mono = "Int" } },
                    } }},
                } },
            } },
        );
    }

    test "fail: signatures" {
        try expectFailParse(
            error.UnexpectedToken,
            &.{ .{ .identifier = "a" }, .colon, .semicolon },
        );

        try expectFailParse(
            error.UnexpectedToken,
            &.{ .{ .identifier = "a" }, .colon, .{ .identifier = "Int" }, .right_paren, .semicolon },
        );

        try expectFailParse(
            error.UnexpectedToken,
            &.{ .{ .identifier = "a" }, .colon, .left_paren, .right_paren, .semicolon },
        );

        try expectFailParse(
            error.UnexpectedToken,
            &.{ .{ .identifier = "a" }, .colon, .@"fn", .{ .identifier = "Int" }, .semicolon },
        );

        try expectFailParse(
            error.UnexpectedToken,
            &.{
                .{ .identifier = "a" },
                .colon,
                .@"fn",
                .{ .identifier = "Int" },
                .fat_arrow,
                .{ .identifier = "Int" },
                .{ .identifier = "Int" },
                .semicolon,
            },
        );
    }
};

test "all" {
    testing.refAllDecls(Tests);
}
