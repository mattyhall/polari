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

const Located = parser.Located;

const Type = enum { bool, int };

const Id = u32;

/// Something that can be given a type.
const Variable = union(enum) {
    expr: Located(*const parser.Expression),
    identifier: Id,
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

    /// uninitialised is for globals which have been referenced but not yet declared. E.g. b in the first statement of
    /// `a = b + 10; b = 5`.
    ///
    /// NOTE: Once all rules have been generated a variable being uninitialised should be an error.
    uninitialised,

    fn deinit(self: Rule, gpa: std.mem.Allocator) void {
        switch (self) {
            .apply => |a| gpa.free(a.arguments),
            .unify, .type, .uninitialised => {},
        }
    }
};

fn hashFn(comptime Context: type) (fn (Context, Variable) u64) {
    return struct {
        fn hash(_: Context, v: Variable) u64 {
            return switch (v) {
                .expr => |e| @intCast(u64, @ptrToInt(e.inner)),
                .identifier => |i| @intCast(u64, i),
            };
        }
    }.hash;
}

fn eqlFn(comptime Context: type) (fn (Context, Variable, Variable) bool) {
    return struct {
        fn eql(_: Context, lhs: Variable, rhs: Variable) bool {
            if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;

            return switch (lhs) {
                .identifier => |i| i == rhs.identifier,
                .expr => |e| e.inner == rhs.expr.inner,
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

/// VarMap maps identifier names to their identifier id.
const VarMap = std.StringHashMapUnmanaged(Id);

/// Sema does semantic analysis on the given program.
pub const Sema = struct {
    gpa: std.mem.Allocator,
    map: Map,

    /// var_maps is a list of VarMaps. Each scope has its own VarMap - a mapping of identifier name to id.
    var_maps: std.ArrayListUnmanaged(VarMap),

    program: *const parser.Program,
    id: Id = 0,

    diags: std.ArrayListUnmanaged(lexer.Diag) = std.ArrayListUnmanaged(lexer.Diag){},
    debug: bool = false,

    const Error = error{
        OutOfMemory,
        VariableNotFound,
        CouldNotInferType,
        TypeMismatch,
        WrongNumberOfArgs,
        Recursion,
        Redefinition,
    };

    const FindResult = struct { id: Id, depth: usize };

    pub fn init(gpa: std.mem.Allocator, program: *const parser.Program) Sema {
        return .{ .gpa = gpa, .map = Map{}, .var_maps = .{}, .program = program };
    }

    /// getId returns the next identifier id.
    fn getId(self: *Sema) Id {
        self.id += 1;
        return self.id - 1;
    }

    /// currentVarMap returns the VarMap for the current scope.
    fn currentVarMap(self: *Sema) *VarMap {
        return &self.var_maps.items[self.var_maps.items.len - 1];
    }

    fn generateRulesForExpression(self: *Sema, expr: Located(*parser.Expression)) Error!void {
        var rule: Rule = switch (expr.inner.*) {
            .integer => Rule{ .type = .int },
            .boolean => Rule{ .type = .bool },
            .binop => |binop| b: {
                try self.generateRulesForExpression(binop.lhs);
                try self.generateRulesForExpression(binop.rhs);

                var args = try self.gpa.alloc(Variable, 2);
                errdefer self.gpa.free(args);

                args[0] = .{ .expr = .{ .loc = binop.lhs.loc, .inner = binop.lhs.inner } };
                args[1] = .{ .expr = .{ .loc = binop.rhs.loc, .inner = binop.rhs.inner } };

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
                    try self.generateRulesForExpression(unaryop.e);
                    break :b Rule{ .unify = .{ .expr = .{ .loc = unaryop.e.loc, .inner = unaryop.e.inner } } };
                }

                try self.generateRulesForExpression(unaryop.e);

                var args = try self.gpa.alloc(Variable, 1);
                errdefer self.gpa.free(args);

                args[0] = .{ .expr = .{ .loc = unaryop.e.loc, .inner = unaryop.e.inner } };

                const op = switch (unaryop.op) {
                    .negate => "-",
                    .grouping => unreachable,
                };

                break :b Rule{ .apply = .{ .f = .{ .builtin = op }, .arguments = args } };
            },
            .identifier => |i| Rule{ .unify = .{ .identifier = (try self.findId(expr.loc, i)).id } },
            .let => |let| b: {
                // let defines a new scope so create a new VarMap and put everything in it.
                try self.var_maps.append(self.gpa, .{});

                for (let.assignments) |*a| try self.generateRulesForAssignment(a.loc, &a.inner);
                try self.generateRulesForExpression(let.in);

                var map = self.var_maps.pop();
                map.deinit(self.gpa);

                // The type of a let is the type of it's in clause, so unify.
                break :b Rule{ .unify = .{ .expr = .{ .loc = let.in.loc, .inner = let.in.inner } } };
            },
        };

        var gop = try self.map.getOrPutValue(
            self.gpa,
            .{ .expr = .{ .loc = expr.loc, .inner = expr.inner } },
            .{ .signature = null, .inferred = undefined },
        );
        gop.value_ptr.inferred = rule;
    }

    /// findId finds the deepest identifier id with the name identifier.
    fn findId(self: *Sema, loc: lexer.Loc, identifier: []const u8) Error!FindResult {
        var i: usize = self.var_maps.items.len - 1;
        while (true) : (i -= 1) {
            const id = self.var_maps.items[i].get(identifier) orelse {
                if (i == 0) {
                    try self.allocDiag(loc, "undeclared variable {s}", .{identifier});
                    return error.VariableNotFound;
                }

                continue;
            };

            return .{ .id = id, .depth = self.var_maps.items.len - 1 - i };
        }
    }

    fn generateRulesForAssignment(self: *Sema, loc: lexer.Loc, assignment: *const parser.Assignment) Error!void {
        try self.generateRulesForExpression(assignment.expression);

        if (self.currentVarMap().contains(assignment.identifier)) {
            try self.allocDiag(loc, "redefinition of variable {s}", .{assignment.identifier});
            return error.Redefinition;
        }

        const id = self.getId();
        try self.currentVarMap().put(self.gpa, assignment.identifier, id);
        try self.map.put(self.gpa, .{ .identifier = id }, .{ .signature = null, .inferred = .uninitialised });

        const gop = try self.map.getOrPutValue(
            self.gpa,
            .{ .identifier = id },
            .{ .signature = null, .inferred = undefined },
        );
        gop.value_ptr.inferred = .{ .unify = .{
            .expr = .{ .loc = assignment.expression.loc, .inner = assignment.expression.inner },
        } };
    }

    pub fn generateRules(self: *Sema) Error!void {
        try self.var_maps.append(self.gpa, .{});
        // Prepopulate the globals with uninitialised so we do not get a variable not found error when a statement
        // refers to an identifier defined after itself.
        for (self.program.stmts.items) |stmt| {
            switch (stmt.inner) {
                .assignment => |a| {
                    if (self.var_maps.items[0].contains(a.identifier)) {
                        try self.allocDiag(stmt.loc, "redefinition of variable {s}", .{a.identifier});
                        return error.Redefinition;
                    }

                    var id = self.getId();
                    try self.currentVarMap().put(self.gpa, a.identifier, id);
                    try self.map.put(self.gpa, .{ .identifier = id }, .{ .signature = null, .inferred = .uninitialised });
                },
                else => {},
            }
        }

        for (self.program.stmts.items) |stmt| {
            switch (stmt.inner) {
                .assignment => |assignment| {
                    try self.generateRulesForExpression(assignment.expression);

                    const id = try self.findId(stmt.loc, assignment.identifier);

                    const gop = try self.map.getOrPutValue(
                        self.gpa,
                        .{ .identifier = id.id },
                        .{ .signature = null, .inferred = undefined },
                    );
                    gop.value_ptr.inferred = .{ .unify = .{
                        .expr = .{ .loc = assignment.expression.loc, .inner = assignment.expression.inner },
                    } };
                },
                .expression => |e| try self.generateRulesForExpression(.{ .loc = stmt.loc, .inner = e }),
            }
        }
    }

    fn printVariable(v: Variable, writer: anytype) !void {
        switch (v) {
            .expr => |e| try e.inner.write(writer),
            .identifier => |i| try writer.print("<{}>", .{i}),
        }
    }

    fn printRule(r: Rule, writer: anytype) !void {
        switch (r) {
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
            .uninitialised => try writer.writeAll("???"),
        }
    }

    pub fn printRules(self: *const Sema, writer: anytype) !void {
        try writer.writeAll("================================\n");
        var it = self.map.iterator();
        while (it.next()) |entry| {
            try printVariable(entry.key_ptr.*, writer);
            try writer.print(" : ", .{});
            try printRule(entry.value_ptr.inferred, writer);
            try writer.print("\n", .{});
        }
    }

    fn allResolved(self: *const Sema, variables: []const Variable) Error!bool {
        for (variables) |variable| {
            const r = self.map.get(variable) orelse return error.VariableNotFound;
            switch (r.inferred) {
                .apply, .unify, .uninitialised => return false,
                .type => {},
            }
        }

        return true;
    }

    fn allocDiag(self: *Sema, loc: lexer.Loc, comptime fmt: []const u8, args: anytype) !void {
        var msg = try std.fmt.allocPrint(self.gpa, fmt, args);
        errdefer self.gpa.free(msg);

        try self.diags.append(self.gpa, .{ .loc = loc, .msg = msg, .allocator = self.gpa });
    }

    /// checkForRecusion checks whether needle is referenced in haystack or any of its children.
    fn checkForRecursion(self: *Sema, loc: lexer.Loc, needle: Variable, haystack: Rule) !void {
        var e = switch (needle) {
            .expr => |e| e,
            .identifier => return,
        };

        switch (haystack) {
            .unify => |v| switch (v) {
                .expr => |e2| b: {
                    if (e.inner == e2.inner) break :b;

                    var r = self.map.get(v) orelse unreachable;
                    return try self.checkForRecursion(loc, needle, r.inferred);
                },
                .identifier => |i| {
                    var r = self.map.get(.{ .identifier = i }) orelse unreachable;
                    return try self.checkForRecursion(loc, needle, r.inferred);
                },
            },
            .apply => |a| b: {
                for (a.arguments) |arg| {
                    switch (arg) {
                        .expr => |e2| if (e.inner == e2.inner) break :b,
                        else => {},
                    }

                    var r = self.map.get(arg) orelse unreachable;
                    return try self.checkForRecursion(loc, needle, r.inferred);
                }
            },
            .type => return,
            .uninitialised => return,
        }

        try self.allocDiag(loc, "variable cannot refer to itself", .{});
        return error.Recursion;
    }

    /// solveApply solves an Apply rule - i.e. a function call.
    fn solveApply(self: *const Sema, apply: Apply) Error!?Type {
        if (!try self.allResolved(apply.arguments)) return null;

        switch (apply.f) {
            .builtin => |b| {
                if (std.mem.indexOf(u8, "+/*-", b) != null) {
                    const is_minus = std.mem.eql(u8, "-", b);
                    if (is_minus and apply.arguments.len != 1 and apply.arguments.len != 2) {
                        return error.WrongNumberOfArgs;
                    }

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

    /// solve solves the rules in the map.
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
                        const e = self.map.get(v) orelse {
                            return error.VariableNotFound;
                        };
                        switch (e.inferred) {
                            .type => |t| try updates.put(self.gpa, entry.key_ptr.*, t),
                            else => {},
                        }
                    },
                    .apply => |a| {
                        var t = try self.solveApply(a) orelse continue;
                        try updates.put(self.gpa, entry.key_ptr.*, t);
                    },
                    .uninitialised => return error.Uninitialised,
                }
            }

            if (updates.size == 0) {
                made_progress = false;
                continue;
            }

            var uit = updates.iterator();
            while (uit.next()) |entry| {
                var e = self.map.getEntry(entry.key_ptr.*) orelse unreachable;
                e.value_ptr.inferred.deinit(self.gpa);
                e.value_ptr.inferred = .{ .type = entry.value_ptr.* };
            }

            if (self.debug) try self.printRules(w);
        }

        if (self.debug) try self.printRules(w);

        // Check for variables referring to themselves.
        var it = self.map.iterator();
        while (it.next()) |entry| {
            // We start the checking from variables that are expressions, so skip identifiers.
            if (entry.key_ptr.* != .expr) continue;

            switch (entry.value_ptr.inferred) {
                .type => {},
                else => {
                    try self.checkForRecursion(entry.key_ptr.expr.loc, entry.key_ptr.*, entry.value_ptr.inferred);

                    try self.allocDiag(entry.key_ptr.expr.loc, "could not infer type", .{});
                    return error.CouldNotInferType;
                },
            }
        }
    }

    pub fn deinit(self: *Sema) void {
        var it = self.map.iterator();
        while (it.next()) |e| e.value_ptr.inferred.deinit(self.gpa);
        self.map.deinit(self.gpa);

        for (self.var_maps.items) |*var_map| var_map.deinit(self.gpa);
        self.var_maps.deinit(self.gpa);

        for (self.diags.items) |*diag| diag.deinit();
        self.diags.deinit(self.gpa);
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

    for (identifiers) |ident| {
        const id = sema.var_maps.items[0].get(ident.ident) orelse unreachable;
        const v = sema.map.get(.{ .identifier = id }) orelse unreachable;
        if (ident.type != v.inferred.type) {
            std.debug.print(
                "Expected \"{s}\" to be of type {}, but it is actually {}\n",
                .{ ident.ident, ident.type, v.inferred },
            );
            return error.MismatchTypes;
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

    const err = sema.generateRules();
    err catch {
        try testing.expectError(expected, err);
        return;
    };

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

test "fail: assignment" {
    try expectTypeCheckError(
        &.{ .{ .identifier = "a" }, .equals, .true, .semicolon, .{ .identifier = "a" }, .equals, .true, .semicolon },
        error.Redefinition,
    );
}

test "scopes" {
    try expectTypeCheck(
        &.{
            .{ .identifier = "a" },
            .equals,
            .true,
            .semicolon,
            .{ .identifier = "b" },
            .equals,
            .let,
            .{ .identifier = "a" },
            .equals,
            .{ .integer = 1 },
            .semicolon,
            .in,
            .{ .identifier = "a" },
            .semicolon,
        },
        &.{
            .{ .ident = "a", .type = .bool },
            .{ .ident = "b", .type = .int },
        },
    );
}

test "fail: scopes" {
    try expectTypeCheckError(
        &.{
            .{ .identifier = "b" },
            .equals,
            .let,
            .{ .identifier = "a" },
            .equals,
            .{ .integer = 1 },
            .semicolon,
            .{ .identifier = "a" },
            .equals,
            .{ .integer = 1 },
            .semicolon,
            .in,
            .{ .identifier = "a" },
            .semicolon,
        },
        error.Redefinition,
    );
}
