//! sema.zig implements semantic analysis of the program using rewriting.
//!
//! In order to do this we walk the program and generate `Rule`s for expressions. These rules are:
//! 1. Type T: the given expression has a known type T
//! 2. Unify V: the given expression has the same type as V
//! 3. Apply fn [args]: the given expression has the type of applying fn to the given args
//!
//! Once these rules are generated they are rewritten by a well-defined process:
//! 1. Unify V => Type T when V is known to have type T
//! 2. Apply fn [args] => Type fn(args...) when the type of everything in args is known.
//!
//! These are applied until no further progress is made. Type checking is successful if all the rules are now Types. If
//! not then there is an ambiguity. During rewriting using substitution two there may also be an error if the given
//! function can not be applied to the given arguments.
//!
//! Not yet implemented is signatures (although the data structures do contain them). Variables can be annotated with a
//! signature to say what type they are. These modify the basic algorithm in that if no progress is made we then use the
//! signatures to do substiutions where possible. Type checking succeeds if all the rules are Types and an inferred
//! type is compatible with the signature if there is one.

const std = @import("std");
const parser = @import("parser.zig");

const Type = enum { bool, int };

/// Something that can be given a type.
const Variable = union(enum) {
    expr: *const parser.Expression,
    identifier: []const u8,
};

const Function = union(enum) {
    variable: Variable,
    builtin: []const u8,
};

const Apply = struct { f: Function, arguments: []const Variable };

/// Rules are associated with Variables.
const Rule = union(enum) {
    /// type means the type of the variable is known to be Type.
    type: Type,

    /// unify means the type of the variable is the same as Variable.
    unify: Variable,

    /// apply means the type of the variable is the result of applying f to arguments.
    apply: Apply,

    fn deinit(self: Rule, gpa: std.mem.Allocator) void {
        switch (self) {
            .apply => |a| gpa.free(a.arguments),
            .unify, .type => {},
        }
    }
};

fn hashFn(comptime Context: type) (fn (Context, Variable) u64) {
    return struct {
        fn hash(_: Context, v: Variable) u64 {
            return switch (v) {
                .expr => |e| @intCast(u64, @ptrToInt(e)),
                .identifier => |i| std.hash_map.hashString(i),
            };
        }
    }.hash;
}

fn eqlFn(comptime Context: type) (fn (Context, Variable, Variable) bool) {
    return struct {
        fn eql(_: Context, lhs: Variable, rhs: Variable) bool {
            if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;

            return switch (lhs) {
                .identifier => |i| std.mem.eql(u8, i, rhs.identifier),
                .expr => |e| e == rhs.expr,
            };
        }
    }.eql;
}

const MapContext = struct {
    pub const hash = hashFn(@This());
    pub const eql = eqlFn(@This());
};

/// Map stores a mapping of Variables to Rules.
///
/// NOTE: We have to use a custom context as the autoHash cannot handle unions.
const Map = std.HashMapUnmanaged(
    Variable,
    struct { signature: ?Type, inferred: Rule },
    MapContext,
    std.hash_map.default_max_load_percentage,
);

/// Sema does semantic analysis on the given program.
pub const Sema = struct {
    gpa: std.mem.Allocator,
    map: Map,
    program: *const parser.Program,

    debug: bool = false,

    const Error = error{ OutOfMemory, VariableNotFound, CouldNotInferType, TypeMismatch, WrongNumberOfArgs };

    pub fn init(gpa: std.mem.Allocator, program: *const parser.Program) Sema {
        return .{ .gpa = gpa, .map = Map{}, .program = program };
    }

    fn generateRulesForExpression(self: *Sema, expr: *const parser.Expression) Error!void {
        var rule: Rule = switch (expr.*) {
            .integer => Rule{ .type = .int },
            .boolean => Rule{ .type = .bool },
            .binop => |binop| b: {
                try self.generateRulesForExpression(binop.lhs.inner);
                try self.generateRulesForExpression(binop.rhs.inner);

                var args = try self.gpa.alloc(Variable, 2);
                errdefer self.gpa.free(args);

                args[0] = .{ .expr = binop.lhs.inner };
                args[1] = .{ .expr = binop.rhs.inner };

                const op = switch (binop.op) {
                    .plus => "+",
                    .minus => "-",
                    .multiply => "*",
                    .divide => "/",
                };

                break :b Rule{ .apply = .{ .f = .{ .builtin = op }, .arguments = args } };
            },
            .unaryop => |unaryop| b: {
                if (unaryop.op == .grouping) {
                    try self.generateRulesForExpression(unaryop.e.inner);
                    break :b Rule{ .unify = .{ .expr = unaryop.e.inner } };
                }

                try self.generateRulesForExpression(unaryop.e.inner);

                var args = try self.gpa.alloc(Variable, 1);
                errdefer self.gpa.free(args);

                args[0] = .{ .expr = unaryop.e.inner };

                const op = switch (unaryop.op) {
                    .negate => "-",
                    .grouping => unreachable,
                };

                break :b Rule{ .apply = .{ .f = .{ .builtin = op }, .arguments = args } };
            },
            .identifier => |i| Rule{ .unify = .{ .identifier = i } },
        };

        var gop = try self.map.getOrPutValue(
            self.gpa,
            .{ .expr = expr },
            .{ .signature = null, .inferred = undefined },
        );
        gop.value_ptr.inferred = rule;
    }

    pub fn generateRules(self: *Sema) Error!void {
        for (self.program.stmts.items) |stmt| {
            switch (stmt.inner) {
                .assignment => |a| {
                    try self.generateRulesForExpression(a.expression.inner);

                    const gop = try self.map.getOrPutValue(
                        self.gpa,
                        .{ .identifier = a.identifier },
                        .{ .signature = null, .inferred = undefined },
                    );
                    gop.value_ptr.inferred = .{ .unify = .{ .expr = a.expression.inner } };
                },
                .expression => |e| try self.generateRulesForExpression(e),
            }
        }
    }

    fn printVariable(v: Variable, writer: anytype) !void {
        switch (v) {
            .expr => |e| try e.write(writer),
            .identifier => |i| try writer.print("<{s}>", .{i}),
        }
    }

    pub fn printRules(self: *const Sema, writer: anytype) !void {
        try writer.writeAll("================================\n");
        var it = self.map.iterator();
        while (it.next()) |entry| {
            try printVariable(entry.key_ptr.*, writer);
            try writer.print(" : ", .{});

            switch (entry.value_ptr.inferred) {
                .type => |t| switch (t) {
                    .int => try writer.print("Int", .{}),
                    .bool => try writer.print("Bool", .{}),
                },
                .unify => |v| {
                    try writer.print("Unify ", .{});
                    try printVariable(v, writer);
                },
                .apply => |a| {
                    try writer.print("Apply ", .{});

                    switch (a.f) {
                        .variable => |v| try printVariable(v, writer),
                        .builtin => |s| try writer.print("{s}", .{s}),
                    }
                    try writer.print(" ", .{});

                    for (a.arguments) |arg| {
                        try printVariable(arg, writer);
                        try writer.print(" ", .{});
                    }
                },
            }

            try writer.print("\n", .{});
        }
    }

    fn allResolved(self: *const Sema, variables: []const Variable) Error!bool {
        for (variables) |variable| {
            const r = self.map.get(variable) orelse return error.VariableNotFound;
            switch (r.inferred) {
                .apply, .unify => return false,
                .type => {},
            }
        }

        return true;
    }

    fn solveApply(self: *const Sema, apply: Apply) Error!?Type {
        if (!try self.allResolved(apply.arguments)) return null;

        switch (apply.f) {
            .builtin => |b| {
                if (std.mem.indexOf(u8, "+/*-", b) != null) {
                    const is_minus = std.mem.eql(u8, "-", b);
                    if (is_minus and apply.arguments.len != 1 and apply.arguments.len != 2)
                        return error.WrongNumberOfArgs;

                    if (!is_minus and apply.arguments.len != 2) return error.WrongNumberOfArgs;

                    for (apply.arguments) |arg| {
                        const r = self.map.get(arg) orelse return error.VariableNotFound;
                        if (r.inferred.type != .int) return error.TypeMismatch;
                    }
                    return Type.int;
                } else return error.VariableNotFound;
            },
            .variable => unreachable,
        }
    }

    pub fn solve(self: *Sema) !void {
        var made_progress = true;
        var updates = std.HashMapUnmanaged(Variable, Type, MapContext, std.hash_map.default_max_load_percentage){};
        defer updates.deinit(self.gpa);

        var w = std.io.getStdErr().writer();
        if (self.debug) try self.printRules(w);

        while (made_progress) {
            updates.clearRetainingCapacity();

            var it = self.map.iterator();
            while (it.next()) |entry| {
                switch (entry.value_ptr.*.inferred) {
                    .type => continue,
                    .unify => |v| {
                        const e = self.map.get(v) orelse return error.VariableNotFound;
                        switch (e.inferred) {
                            .type => |t| try updates.put(self.gpa, entry.key_ptr.*, t),
                            else => {},
                        }
                    },
                    .apply => |a| {
                        var t = try self.solveApply(a) orelse continue;
                        try updates.put(self.gpa, entry.key_ptr.*, t);
                    },
                }
            }

            if (updates.size == 0) {
                made_progress = false;
                continue;
            }

            var uit = updates.iterator();
            while (uit.next()) |entry| {
                var e = self.map.getEntry(entry.key_ptr.*) orelse return error.VariableNotFound;
                e.value_ptr.inferred.deinit(self.gpa);
                e.value_ptr.inferred = .{ .type = entry.value_ptr.* };
            }

            if (self.debug) try self.printRules(w);
        }

        if (self.debug) try self.printRules(w);

        var it = self.map.iterator();
        while (it.next()) |entry| {
            switch (entry.value_ptr.*.inferred) {
                .type => {},
                else => return error.CouldNotInferType,
            }
        }
    }

    pub fn deinit(self: *Sema) void {
        var it = self.map.iterator();
        while (it.next()) |e| {
            e.value_ptr.inferred.deinit(self.gpa);
        }

        self.map.deinit(self.gpa);
    }
};

const testing = std.testing;
const lexer = @import("lexer.zig");

const TestTypeCheckResult = struct { ident: []const u8, type: Type };

fn expectTypeCheck(toks: []const lexer.Tok, identifiers: []const TestTypeCheckResult) !void {
    var l = parser.Lexer{ .fake = lexer.Fake{ .toks = toks } };
    var p = parser.Parser.init(testing.allocator, l);
    defer p.deinit();

    var program = try p.parse();
    defer program.deinit();

    var sema = Sema.init(testing.allocator, &program);
    defer sema.deinit();

    try sema.generateRules();
    try sema.solve();

    var it = sema.map.iterator();
    while (it.next()) |entry| {
        const name = switch (entry.key_ptr.*) {
            .identifier => |n| n,
            else => continue,
        };

        for (identifiers) |ident| {
            if (std.mem.eql(u8, name, ident.ident)) {
                if (ident.type != entry.value_ptr.inferred.type) {
                    std.debug.print(
                        "Expected \"{s}\" to be of type {}, but it is actually {}\n",
                        .{ name, ident.type, entry.value_ptr.inferred },
                    );
                    return error.MismatchTypes;
                }
            }
        }
    }
}

fn expectTypeCheckError(toks: []const lexer.Tok, expected: anyerror) !void {
    var l = parser.Lexer{ .fake = lexer.Fake{ .toks = toks } };
    var p = parser.Parser.init(testing.allocator, l);
    defer p.deinit();

    var program = try p.parse();
    defer program.deinit();

    var sema = Sema.init(testing.allocator, &program);
    defer sema.deinit();

    try sema.generateRules();
    try testing.expectError(expected, sema.solve());
}

test "identifiers" {
    try expectTypeCheck(
        &.{ .{ .identifier = "a" }, .equals, .{ .integer = 1 }, .semicolon },
        &.{.{ .ident = "a", .type = .int }},
    );

    try expectTypeCheck(
        &.{ .{ .identifier = "a" }, .equals, .true, .semicolon },
        &.{.{ .ident = "a", .type = .bool }},
    );
}

test "maths" {
    try expectTypeCheck(
        &.{ .{ .identifier = "a" }, .equals, .{ .integer = 1 }, .plus, .{ .integer = 2 }, .semicolon },
        &.{.{ .ident = "a", .type = .int }},
    );

    try expectTypeCheck(
        &.{ .{ .identifier = "a" }, .equals, .minus, .{ .integer = 1 }, .semicolon },
        &.{.{ .ident = "a", .type = .int }},
    );

    try expectTypeCheck(
        &.{
            .{ .identifier = "a" },
            .equals,
            .{ .integer = 1 },
            .plus,
            .{ .integer = 2 },
            .semicolon,
            .{ .identifier = "b" },
            .equals,
            .{ .identifier = "a" },
            .plus,
            .{ .integer = 3 },
            .semicolon,
        },
        &.{ .{ .ident = "a", .type = .int }, .{ .ident = "b", .type = .int } },
    );
}

test "fail: maths" {
    try expectTypeCheckError(&.{ .{ .integer = 1 }, .plus, .true, .semicolon }, error.TypeMismatch);

    try expectTypeCheckError(&.{
        .{ .identifier = "a" },
        .equals,
        .{ .integer = 1 },
        .plus,
        .{ .integer = 2 },
        .semicolon,
        .{ .identifier = "b" },
        .equals,
        .{ .identifier = "a" },
        .plus,
        .true,
        .semicolon,
    }, error.TypeMismatch);
}
