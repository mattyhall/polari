const std = @import("std");
const parser = @import("parser.zig");
const lexer = @import("lexer.zig");

const Located = parser.Located;
const Expression = parser.Expression;
const Loc = lexer.Loc;
const Diag = lexer.Diag;

/// normaliseApply takes the lhs and rhs of an apply binop and recurses down its lhs to find the function being called.
/// It adds any arguments of that function to args, and returns the function.
///
/// Example (in pseudocode):
///   f = normaliseApply(Apply (Apply f 10) 20, 30, args)
///     f = normaliseApply(Apply f 10, 20, args)
///       args.push(10)
///       return f
///     args.push(20)
///     return f
///   args.push(30)
///   return f
/// => args = [10, 20, 30], ret value = f
fn normaliseApply(
    arena: std.mem.Allocator,
    lhs: Located(*Expression),
    rhs: Located(*Expression),
    args: *std.ArrayList(Located(*Expression)),
) !Located(*Expression) {
    var f = b: {
        switch (lhs.inner.*) {
            .binop => |binop| {
                switch (binop.op) {
                    .apply => break :b try normaliseApply(arena, binop.lhs, binop.rhs, args),
                    else => {},
                }
            },
            else => {},
        }

        try normaliseExpression(arena, lhs.inner);
        break :b lhs;
    };

    try normaliseExpression(arena, rhs.inner);
    try args.append(rhs);

    return f;
}

/// normaliseExpression collapses apply binop chains into a single apply expression.
fn normaliseExpression(arena: std.mem.Allocator, expr: *Expression) error{OutOfMemory}!void {
    switch (expr.*) {
        .binop => |binop| {
            switch (binop.op) {
                .apply => {
                    var al = std.ArrayList(Located(*Expression)).init(arena);
                    errdefer al.deinit();

                    var f = try normaliseApply(arena, binop.lhs, binop.rhs, &al);
                    expr.* = .{ .apply = .{ .f = f, .args = try al.toOwnedSlice() } };
                },
                else => {
                    try normaliseExpression(arena, binop.lhs.inner);
                    try normaliseExpression(arena, binop.rhs.inner);
                },
            }
        },
        .unaryop => |unaryop| try normaliseExpression(arena, unaryop.e.inner),
        .let => |let| {
            for (let.assignments) |ass| {
                try normaliseExpression(arena, ass.inner.expression.inner);
            }

            try normaliseExpression(arena, let.in.inner);
        },
        .function => |f| try normaliseExpression(arena, f.body.inner),
        .@"if" => |f| {
            try normaliseExpression(arena, f.condition.inner);
            try normaliseExpression(arena, f.then.inner);
            try normaliseExpression(arena, f.@"else".inner);
        },
        .integer, .boolean, .identifier => return,
        .apply => unreachable,
    }
}

/// getIdentifiers finds all the indentifiers in expr and adds them to deps. This may include duplicates.
/// It will not add any identifiers in ignore to deps.
fn getIdentifiers(
    a: std.mem.Allocator,
    expr: *Expression,
    deps: *std.ArrayListUnmanaged([]const u8),
    ignore: *std.ArrayListUnmanaged([]const u8),
) !void {
    switch (expr.*) {
        .integer, .boolean => {},
        .identifier => |i| {
            var found = false;
            for (ignore.items) |item| {
                if (std.mem.eql(u8, i, item)) {
                    found = true;
                    break;
                }
            }

            if (!found) try deps.append(a, i);
        },
        .unaryop => |unaryop| try getIdentifiers(a, unaryop.e.inner, deps, ignore),
        .binop => |binop| {
            try getIdentifiers(a, binop.lhs.inner, deps, ignore);
            try getIdentifiers(a, binop.rhs.inner, deps, ignore);
        },
        .apply => |apply| {
            try getIdentifiers(a, apply.f.inner, deps, ignore);
            for (apply.args) |arg| try getIdentifiers(a, arg.inner, deps, ignore);
        },
        .let => |let| {
            for (let.assignments) |ass| {
                try ignore.append(a, ass.inner.identifier);
                try getIdentifiers(a, ass.inner.expression.inner, deps, ignore);
            }

            try getIdentifiers(a, let.in.inner, deps, ignore);
            ignore.shrinkRetainingCapacity(let.assignments.len);
        },
        .function => |f| {
            for (f.params) |param| try ignore.append(a, param.inner.identifier);

            try getIdentifiers(a, f.body.inner, deps, ignore);
            ignore.shrinkRetainingCapacity(f.params.len);
        },
        .@"if" => |f| {
            try getIdentifiers(a, f.condition.inner, deps, ignore);
            try getIdentifiers(a, f.then.inner, deps, ignore);
            try getIdentifiers(a, f.@"else".inner, deps, ignore);
        },
    }
}

/// reorder puts the statements in program into a standard order: assignments first, expressions after. The assignments
/// are also reordered within themselves so that variables are assigned before they are used. If this cannot be done
/// (i.e. there are cyclical dependencies) then we return error.Cycle.
fn reorder(program: *parser.Program) !void {
    // Start by sorting so that assignments are first, expressions are after.
    std.sort.sort(Located(parser.Statement), program.stmts.items, {}, struct {
        fn lt(_: void, lhs: Located(parser.Statement), rhs: Located(parser.Statement)) bool {
            switch (lhs.inner) {
                .expression => switch (rhs.inner) {
                    .expression => return false,
                    else => return false,
                },
                .assignment => |a| switch (rhs.inner) {
                    .expression => return true,
                    .signature => |s| {
                        if (std.mem.eql(u8, a.identifier, s.identifier)) return false;

                        return std.ascii.lessThanIgnoreCase(a.identifier, s.identifier);
                    },
                    .assignment => |a2| return std.ascii.lessThanIgnoreCase(a.identifier, a2.identifier),
                    .typedef => return false,
                },
                .signature => |s| switch (rhs.inner) {
                    .expression => return true,
                    .signature => |s2| return std.ascii.lessThanIgnoreCase(s.identifier, s2.identifier),
                    .assignment => |a| {
                        if (std.mem.eql(u8, a.identifier, s.identifier)) return true;

                        return std.ascii.lessThanIgnoreCase(s.identifier, a.identifier);
                    },
                    .typedef => return false,
                },
                .typedef => |t| switch (rhs.inner) {
                    .expression, .signature, .assignment => return true,
                    .typedef => |t2| return std.ascii.lessThanIgnoreCase(t.identifier, t2.identifier),
                },
            }
        }
    }.lt);

    var arena = std.heap.ArenaAllocator.init(program.arena.child_allocator);
    defer arena.deinit();
    var a = arena.allocator();

    // To reorder the assignments we build a graph. A node is an identifier and there is a directed edge between a and
    // b iff a depends on b.

    // We have an additional datastructure: a map from identifier to an index in the nodes array. This is done so
    // we can build the dependencies with just the identifier name, but then look up the assignment it corresponds to
    // later.
    var map = std.StringHashMapUnmanaged(usize){};
    defer map.deinit(a);

    const Node = struct {
        ident: []const u8,
        expr: *Expression,
        deps: [][]const u8,
        processed: bool,
        index: usize,
    };
    var nodes = std.ArrayListUnmanaged(Node){};
    defer nodes.deinit(a);

    for (program.stmts.items) |stmt, i| {
        if (stmt.inner == .expression or stmt.inner == .signature or stmt.inner == .typedef) break;

        if (map.contains(stmt.inner.assignment.identifier)) return error.Redefinition;

        try map.put(a, stmt.inner.assignment.identifier, i);
    }

    for (program.stmts.items) |stmt, i| {
        if (stmt.inner == .expression or stmt.inner == .signature or stmt.inner == .typedef) break;

        var deps = std.ArrayListUnmanaged([]const u8){};
        errdefer deps.deinit(a);
        var ignore = std.ArrayListUnmanaged([]const u8){};
        errdefer ignore.deinit(a);

        if (stmt.inner.assignment.expression.inner.* == .function)
            try ignore.append(a, stmt.inner.assignment.identifier);

        try getIdentifiers(a, stmt.inner.assignment.expression.inner, &deps, &ignore);
        try nodes.append(a, .{
            .ident = stmt.inner.assignment.identifier,
            .expr = stmt.inner.assignment.expression.inner,
            .deps = try deps.toOwnedSlice(a),
            .processed = false,
            .index = i,
        });
    }

    // We then reorder. We keep looping through the nodes, skipping ones that have been processed. If a node has no
    // dependencies or all its dependencies have been processed then we can "emit" the assignment (i.e. put it at the
    // front of statements). Once we stop making any progress we need to check that we actually processed every node.
    var made_progress = true;
    var reorder_index: usize = 0;
    while (made_progress) {
        made_progress = false;
        for (nodes.items) |*node| {
            if (node.processed) continue;

            var all_deps_processed = true;
            for (node.deps) |dep| {
                const index = map.get(dep) orelse return;
                if (!nodes.items[index].processed) {
                    all_deps_processed = false;
                    break;
                }
            }

            if (!all_deps_processed) continue;

            made_progress = true;

            const tmp = program.stmts.items[reorder_index];
            const tmp_node_index = map.get(tmp.inner.assignment.identifier) orelse return;
            const tmp_node = &nodes.items[tmp_node_index];
            std.debug.assert(tmp_node.index == reorder_index);

            program.stmts.items[reorder_index] = program.stmts.items[node.index];
            program.stmts.items[node.index] = tmp;

            tmp_node.index = node.index;
            node.index = reorder_index;

            node.processed = true;
            reorder_index += 1;
        }
    }

    for (nodes.items) |node| {
        if (!node.processed) return error.Cycle;
    }
}

/// normaliseProgram normalises an AST into a standard, simpler form than that which comes out of the parser. Currently
/// it just chains of apply binops into a single Apply expression and reorders assignments so that variables are
/// declared before they are used.
pub fn normaliseProgram(program: *parser.Program) !void {
    for (program.stmts.items) |stmt| {
        switch (stmt.inner) {
            .assignment => |a| try normaliseExpression(program.arena.allocator(), a.expression.inner),
            .expression => |e| try normaliseExpression(program.arena.allocator(), e),
            .signature => {},
            .typedef => {},
        }
    }

    try reorder(program);
}

var PlusExpr = Expression{ .identifier = "+" };
var MinusExpr = Expression{ .identifier = "-" };
var MultiplyExpr = Expression{ .identifier = "*" };
var DivideExpr = Expression{ .identifier = "/" };
var EqExpr = Expression{ .identifier = "==" };
var NeqExpr = Expression{ .identifier = "!=" };
var LtExpr = Expression{ .identifier = "<" };
var LteExpr = Expression{ .identifier = "<=" };
var GtExpr = Expression{ .identifier = ">" };
var GteExpr = Expression{ .identifier = ">=" };
var NegateExpr = Expression{ .identifier = "negate" };

fn unaryopToExpr(op: parser.UnaryOperation) Located(*Expression) {
    return Located(*Expression){ .loc = .{}, .inner = switch (op) {
        .negate => &NegateExpr,
        .grouping => unreachable,
    } };
}

fn binopToExpr(op: parser.BinaryOperation) Located(*Expression) {
    return Located(*Expression){ .loc = .{}, .inner = switch (op) {
        .plus => &PlusExpr,
        .minus => &MinusExpr,
        .multiply => &MultiplyExpr,
        .divide => &DivideExpr,
        .eq => &EqExpr,
        .neq => &NeqExpr,
        .lt => &LtExpr,
        .lte => &LteExpr,
        .gt => &GtExpr,
        .gte => &GteExpr,
        .apply => unreachable,
    } };
}

/// BuiltinType represents a common, builtin type of a language - e.g. numbers, booleans etc.
const BuiltinType = enum {
    int,
    boolean,

    pub fn format(self: *const BuiltinType, comptime fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) !void {
        _ = opts;
        _ = fmt;

        return switch (self.*) {
            .int => writer.writeAll("Int"),
            .boolean => writer.writeAll("Bool"),
        };
    }
};

const Variable = usize;

const Constructor = []const u8;

/// Type represents a fully known type. It is the result of the inference done in `Sema`.
const Type = union(enum) {
    /// builtin is for a know, built into the language type - e.g. Int.
    builtin: BuiltinType,

    /// construct is for a type which is made from another type - e.g. `Maybe a` (a maybe that stores any type)
    /// `(->) Int Bool` (a function from int to boolean).
    construct: struct { constructor: Constructor, args: []Type },

    /// generic is for when what this type has been assigned to can take any type - e.g. [a] could be a list of ints,
    /// bools, `Maybe Int`s etc.
    generic: Variable,

    pub fn format(self: *const Type, comptime fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) !void {
        _ = opts;
        _ = fmt;
        switch (self.*) {
            .builtin => |b| try writer.print("{}", .{b}),
            .generic => |v| try writer.print("g{}", .{v}),
            .construct => |c| {
                try writer.print("({s} ", .{c.constructor});

                for (c.args) |arg, i| {
                    try writer.print("{}", .{arg});
                    if (i < c.args.len - 1) try writer.writeAll(" ");
                }

                try writer.writeAll(")");
            },
        }
    }

    fn clone(self: *const Type, gpa: std.mem.Allocator) !Type {
        switch (self.*) {
            .construct => |c| {
                var args = try gpa.alloc(Type, c.args.len);
                errdefer gpa.free(args);

                for (c.args) |arg, i| args[i] = try arg.clone(gpa);

                return Type{ .construct = .{ .constructor = c.constructor, .args = args } };
            },
            else => return self.*,
        }
    }

    fn deinit(self: *Type, gpa: std.mem.Allocator) void {
        switch (self.*) {
            .construct => |c| {
                for (c.args) |*arg| arg.deinit(gpa);
                gpa.free(c.args);
            },
            .builtin, .generic => {},
        }
    }
};

/// TypeVar is something which is given to everything (i.e. expressions and variables) which can have a type. That type
/// may be known (e.g. Int, Maybe Bool), or not.
const TypeVar = union(enum) {
    /// builtin is assigned when we know the type of what is given this typevar and it is a builtin type.
    builtin: BuiltinType,

    /// construct is assigned when we know at least the "shape" of what was given this typevar. E.g. a Maybe Int or a
    /// function (which has constructor "->") of two unknown types.
    construct: struct { constructor: Constructor, args: []TypeVar },

    /// variable is for an unknown type.
    variable: Variable,

    /// unifiableWith returns true if self can be unified with other.
    fn unifiableWith(self: TypeVar, other: TypeVar) bool {
        if (self == .variable or other == .variable) return true;
        if (self == .builtin and other == .builtin and self.builtin == other.builtin) return true;

        if (self != .construct or other != .construct) return false;

        var lhs = self.construct;
        var rhs = other.construct;

        if (!std.mem.eql(u8, lhs.constructor, rhs.constructor)) return false;
        if (lhs.args.len != rhs.args.len) return false;

        for (lhs.args) |lhs_arg, i| {
            if (!lhs_arg.unifiableWith(rhs.args[i])) return false;
        }

        return true;
    }

    fn clone(self: TypeVar, gpa: std.mem.Allocator) !TypeVar {
        switch (self) {
            .builtin, .variable => return self,
            else => {},
        }

        var c = self.construct;

        var args = try gpa.alloc(TypeVar, c.args.len);
        errdefer gpa.free(args);

        for (c.args) |a, i| {
            args[i] = try a.clone(gpa);
        }

        return .{ .construct = .{ .constructor = c.constructor, .args = args } };
    }

    fn eql(lhs: TypeVar, rhs: TypeVar) bool {
        if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;

        return switch (lhs) {
            .builtin => |b| b == rhs.builtin,
            .construct => |c| {
                if (!std.mem.eql(u8, c.constructor, rhs.construct.constructor)) return false;

                if (c.args.len != rhs.construct.args.len) return false;
                for (c.args) |lhs_arg, i| {
                    const rhs_arg = rhs.construct.args[i];
                    if (!lhs_arg.eql(rhs_arg)) return false;
                }

                return true;
            },
            .variable => |v| v == rhs.variable,
        };
    }

    pub fn format(self: *const TypeVar, comptime fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) !void {
        _ = opts;
        _ = fmt;
        switch (self.*) {
            .builtin => |b| try writer.print("{}", .{b}),
            .variable => |v| try writer.print("t{}", .{v}),
            .construct => |c| {
                try writer.print("({s} ", .{c.constructor});

                for (c.args) |arg, i| {
                    try writer.print("{}", .{arg});
                    if (i < c.args.len - 1) try writer.writeAll(" ");
                }

                try writer.writeAll(")");
            },
        }
    }

    fn deinit(self: *TypeVar, gpa: std.mem.Allocator) void {
        switch (self.*) {
            .construct => |c| {
                for (c.args) |*arg| arg.deinit(gpa);
                gpa.free(c.args);
            },
            .builtin, .variable => {},
        }
    }
};

/// A Constraint of `lhs`, `rhs` means that both `lhs` and `rhs` must have the same type. This can be a trivial
/// constraint (e.g `Int ~ Int`), one where one side is unknown (e.g `t0 ~ Int` - meaning t0 must have the type Int) or
/// one where both sides are unknown (`t0 ~ t1`).
const Constraint = struct {
    lhs: TypeVar,
    rhs: TypeVar,

    /// provenance corresponds to the expression that this constraint came from. It is stored to enable error
    /// reporting.
    provenance: Located(*Expression),

    pub fn format(self: *const Constraint, comptime fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) !void {
        _ = opts;
        _ = fmt;

        try writer.print("{} ~ {}", .{ self.lhs, self.rhs });
    }

    fn deinit(self: *Constraint, gpa: std.mem.Allocator) void {
        self.lhs.deinit(gpa);
        self.rhs.deinit(gpa);
    }
};

/// Substitution of two variables (`lhs` and `rhs`) means wherever we see `lhs` we can instead put `rhs`, and vice
/// versa.
const Substitution = struct {
    lhs: TypeVar,
    rhs: TypeVar,

    pub fn format(self: *const Substitution, comptime fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) !void {
        _ = opts;
        _ = fmt;

        return writer.print("{} := {}", .{ self.lhs, self.rhs });
    }
};

/// VarTypeVars holds a mapping of identifiers to typevars and the scopes that introduced them.
const VarTypeVars = struct {
    removed: std.ArrayListUnmanaged(struct { depth: usize, map: std.StringHashMapUnmanaged(Variable) }),
    maps: std.ArrayListUnmanaged(std.StringHashMapUnmanaged(Variable)),

    const GetOrPutResult = std.StringHashMapUnmanaged(Variable).GetOrPutResult;

    fn init(gpa: std.mem.Allocator) !VarTypeVars {
        var maps = std.ArrayListUnmanaged(std.StringHashMapUnmanaged(Variable)){};
        try maps.append(gpa, .{});
        return VarTypeVars{ .removed = .{}, .maps = maps };
    }

    /// beginScope beings a new scope, creating a fresh map for variables declared in the scope.
    fn beginScope(self: *VarTypeVars, gpa: std.mem.Allocator) !void {
        try self.maps.append(gpa, .{});
    }

    /// get finds k in any scope, searching deepest to shallowest.
    fn get(self: *const VarTypeVars, k: []const u8) ?Variable {
        var i: isize = @intCast(isize, self.maps.items.len - 1);
        while (i >= 0) : (i -= 1) {
            const j = @intCast(usize, i);
            if (self.maps.items[j].get(k)) |v| return v;
        }

        return null;
    }

    /// getOrPut either inserts k or returns a struct containing pointers to its entry.
    ///
    /// NOTE: If k is not found in any scope then it is put in the deepest (i.e. most recent one). If k is found then
    /// it could be in any scope.
    fn getOrPut(self: *VarTypeVars, gpa: std.mem.Allocator, k: []const u8) !GetOrPutResult {
        var i: isize = @intCast(isize, self.maps.items.len - 1);
        while (i >= 0) : (i -= 1) {
            const j = @intCast(usize, i);
            if (self.maps.items[j].contains(k)) return self.maps.items[j].getOrPut(gpa, k);
        }

        return self.maps.items[self.maps.items.len - 1].getOrPut(gpa, k);
    }

    /// put associated k with v in the deepest scope.
    fn put(self: *VarTypeVars, gpa: std.mem.Allocator, k: []const u8, v: Variable) !void {
        try self.maps.items[self.maps.items.len - 1].put(gpa, k, v);
    }

    /// putAssumeCapacity associates k with v in the deepest scope without allocation.
    fn putAssumeCapacity(self: *VarTypeVars, k: []const u8, v: Variable) void {
        self.maps.items[self.maps.items.len - 1].putAssumeCapacity(k, v);
    }

    /// ensureUnusedCapacity ensures the deepest scope has at least n unused capacity.
    fn ensureUnusedCapacity(self: *VarTypeVars, gpa: std.mem.Allocator, n: u32) !void {
        return self.maps.items[self.maps.items.len - 1].ensureUnusedCapacity(gpa, n);
    }

    /// endScope removes the last scope
    fn endScope(self: *VarTypeVars, gpa: std.mem.Allocator) !void {
        var scope = self.maps.pop();
        try self.removed.append(gpa, .{ .depth = self.maps.items.len + 1, .map = scope });
    }

    fn printMap(map: std.StringHashMapUnmanaged(Variable)) void {
        var it = map.iterator();
        while (it.next()) |entry|
            std.debug.print("{s} : t{}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
    }

    fn print(self: *const VarTypeVars) void {
        std.debug.assert(self.maps.items.len == 1);
        printMap(self.maps.items[0]);

        for (self.removed.items) |map| {
            std.debug.print("--- depth {}\n", .{map.depth});
            printMap(map.map);
        }
    }
};

/// Sema implements type checking of Polari code. It is done per-decl (i.e. the type of a decl depends only on its
/// definition) and consists of two phases:
///   1. Give the body and its expressions type variables and _generate constraints_
///   2. _Solve_ the constraints using unification.
///
/// Currently we just check the types and infer any unknown ones. We do not annotate the AST with the types (also known
/// as _elaborating_ the program). This may be added in the future.
pub const Sema = struct {
    /// gpa is a general purpose allocator and is used to allocate any value that is to be returned.
    gpa: std.mem.Allocator,

    /// arena is used for short lived allocations which span only the length of time of a decl.
    arena: std.heap.ArenaAllocator,

    /// decl_types stores the types of all decls in the program. It is allocated using `gpa`.
    decl_types: std.StringHashMapUnmanaged(Type),

    /// var_type_vars stores the typevar for every named thing (i.e. a variable) in the current decl.
    var_type_vars: VarTypeVars,

    /// expr_type_vars stores the variable for every expression in the current decl.
    expr_type_vars: std.AutoHashMapUnmanaged(*const Expression, Variable),

    /// constraints stores all the constraints for the current decl as well as any unsolved constraints for previous
    /// ones. It does the latter to enable type errors to be reported once as much of the program has been type checked
    /// as possible.
    constraints: std.ArrayListUnmanaged(Constraint),

    /// substitutions is a list of all substitutions for the current decl.
    substitutions: std.ArrayListUnmanaged(Substitution),

    /// types is a hashmap of types defined in the current compilation unit.
    types: std.StringHashMapUnmanaged(Type),

    /// next_var holds the next typevar to give out.
    next_var: usize,

    /// debug determines whether we dump debug infor
    debug: bool,

    pub fn init(gpa: std.mem.Allocator, debug: bool) Sema {
        return .{
            .gpa = gpa,
            .arena = std.heap.ArenaAllocator.init(gpa),
            .decl_types = .{},
            .expr_type_vars = .{},
            .var_type_vars = undefined,
            .constraints = .{},
            .substitutions = .{},
            .types = .{},
            .next_var = 0,
            .debug = debug,
        };
    }

    /// printVars will output the typevars in a human readable format to stderr.
    fn printVars(self: *const Sema) !void {
        std.debug.print("####################################\n", .{});
        std.debug.print("=============== VARS ===============\n", .{});

        var writer = std.io.getStdErr().writer();

        self.var_type_vars.print();

        {
            var it = self.expr_type_vars.iterator();
            while (it.next()) |entry| {
                try Expression.write(entry.key_ptr.*, writer);
                // try entry.key_ptr.write(writer);
                std.debug.print(" : t{}\n", .{entry.value_ptr.*});
            }
        }
    }

    /// printConstraints will output the constraints in a human readable format to stderr.
    fn printConstraints(self: *const Sema) !void {
        std.debug.print("============ CONSTRAINT ============\n", .{});
        for (self.constraints.items) |constraint| {
            std.debug.print("{}\n", .{constraint});
        }
    }

    /// prepopulate inserts builtin decls (e.g. the maths operations) into decl_types.
    fn prepopulate(self: *Sema) !void {
        try self.decl_types.ensureUnusedCapacity(self.gpa, 4 + 6 + 1);

        for (&[_][]const u8{ "+", "-", "*", "/" }) |op| {
            var args = try self.gpa.alloc(Type, 3);
            args[0] = .{ .builtin = .int };
            args[1] = .{ .builtin = .int };
            args[2] = .{ .builtin = .int };

            const math_binop_ty = Type{ .construct = .{ .constructor = "->", .args = args } };
            self.decl_types.putAssumeCapacity(op, math_binop_ty);
        }

        for (&[_][]const u8{ "==", "!=", ">", ">=", "<", "<=" }) |op| {
            var args = try self.gpa.alloc(Type, 3);
            args[0] = .{ .builtin = .int };
            args[1] = .{ .builtin = .int };
            args[2] = .{ .builtin = .boolean };

            const bool_binop_ty = Type{ .construct = .{ .constructor = "->", .args = args } };
            self.decl_types.putAssumeCapacity(op, bool_binop_ty);
        }

        {
            var args = try self.gpa.alloc(Type, 2);
            args[0] = .{ .builtin = .int };
            args[1] = .{ .builtin = .int };

            const negate_unaryop_ty = Type{ .construct = .{ .constructor = "->", .args = args } };
            self.decl_types.putAssumeCapacity("negate", negate_unaryop_ty);
        }
    }

    /// typevar returns a fresh type variable.
    fn typevar(self: *Sema) Variable {
        const v = self.next_var;
        self.next_var += 1;
        return v;
    }

    /// typeToTypeVar converts a Type to its corresponding TypeVar. In the case that a generic is hit then a fresh
    /// typevar is generated for it.
    fn typeToTypeVar(self: *Sema, t: Type) !TypeVar {
        var arena = std.heap.ArenaAllocator.init(self.gpa);
        defer arena.deinit();

        var generics = std.AutoHashMap(Variable, TypeVar).init(arena.allocator());
        return self.typeToTypeVarInner(t, &generics);
    }

    /// typeToTypeVar converts a Type to its corresponding TypeVar. In the case that a generic if a typevar has already
    /// been assigned for it (i.e. it is already in `generics`) then it is returned, otherwise a fresh typevar is
    /// assigned.
    fn typeToTypeVarInner(self: *Sema, t: Type, generics: *std.AutoHashMap(Variable, TypeVar)) !TypeVar {
        return switch (t) {
            .builtin => |b| TypeVar{ .builtin = b },
            .construct => |c| b: {
                var args = try self.gpa.alloc(TypeVar, c.args.len);
                errdefer self.gpa.free(args);

                for (c.args) |arg, i| {
                    args[i] = try self.typeToTypeVarInner(arg, generics);
                }

                break :b TypeVar{ .construct = .{ .constructor = c.constructor, .args = args } };
            },
            .generic => |g| b: {
                const gop = try generics.getOrPut(g);
                if (!gop.found_existing) gop.value_ptr.* = .{ .variable = self.typevar() };
                break :b gop.value_ptr.*;
            },
        };
    }

    /// generateApplyConstraints generates constraints for applying `f` to `f_args`. We generate two constraints. The
    /// first is that this expression has the same type as the return value of the function. The second is the function
    /// takes the same types as parameters as the args it is being given.
    fn generateApplyConstraints(
        self: *Sema,
        tv: Variable,
        f: Located(*Expression),
        f_args: []Located(*Expression),
    ) error{OutOfMemory}!Variable {
        const f_tv = try self.generateExprConstraints(f);

        var args = try std.ArrayListUnmanaged(TypeVar).initCapacity(self.gpa, f_args.len + 1);
        errdefer args.deinit(self.gpa);

        for (f_args) |arg| {
            const a_tv = try self.generateExprConstraints(arg);
            args.appendAssumeCapacity(.{ .variable = a_tv });
        }

        const ret_tv = self.typevar();
        args.appendAssumeCapacity(.{ .variable = ret_tv });

        try self.constraints.append(self.gpa, .{
            .lhs = .{ .variable = tv },
            .rhs = .{ .variable = ret_tv },
            .provenance = f,
        });

        try self.constraints.append(self.gpa, .{
            .lhs = .{ .variable = f_tv },
            .rhs = TypeVar{ .construct = .{ .constructor = "->", .args = try args.toOwnedSlice(self.gpa) } },
            .provenance = f,
        });

        return tv;
    }

    /// generateExprConstraints generates constraints for the given expression and returns the type variable of the
    /// expression.
    fn generateExprConstraints(self: *Sema, expr: Located(*Expression)) !Variable {
        const tv = self.typevar();
        try self.expr_type_vars.put(self.arena.allocator(), expr.inner, tv);

        switch (expr.inner.*) {
            // For builtin types we add a constraint that tv is that type.
            .integer, .boolean => try self.constraints.append(self.gpa, .{
                .lhs = .{ .variable = tv },
                .rhs = TypeVar{ .builtin = switch (expr.inner.*) {
                    .integer => BuiltinType.int,
                    .boolean => BuiltinType.boolean,
                    else => unreachable,
                } },
                .provenance = expr,
            }),

            // For identifiers we add a constraint that the type of the expression is the type given to that
            // identifier. The identifier may be polymorphic (i.e. generic), in which case we give each generic
            // parameter a new typevar.
            .identifier => |ident| {
                var i_tv = if (self.var_type_vars.get(ident)) |t| b: {
                    break :b TypeVar{ .variable = t };
                } else if (self.decl_types.get(ident)) |t| b: {
                    break :b try self.typeToTypeVar(t);
                } else b: {
                    const inner_tv = self.typevar();
                    try self.var_type_vars.put(self.arena.allocator(), ident, inner_tv);
                    break :b TypeVar{ .variable = inner_tv };
                };

                try self.constraints.append(self.gpa, .{
                    .lhs = .{ .variable = tv },
                    .rhs = i_tv,
                    .provenance = expr,
                });
            },

            // For an `if` we generate a constraint saying the condition must be a boolean; another constraint saying
            // the `then` and the `else` types must be the same and finally a constraint saying that tv must be the
            // type of the `then` branch.
            .@"if" => |i| {
                const c_tv = try self.generateExprConstraints(i.condition);
                const t_tv = try self.generateExprConstraints(i.then);
                const e_tv = try self.generateExprConstraints(i.@"else");

                try self.constraints.append(self.gpa, .{
                    .lhs = .{ .variable = c_tv },
                    .rhs = TypeVar{ .builtin = .boolean },
                    .provenance = i.condition,
                });

                try self.constraints.append(self.gpa, .{
                    .lhs = .{ .variable = t_tv },
                    .rhs = .{ .variable = e_tv },
                    .provenance = expr,
                });

                try self.constraints.append(self.gpa, .{
                    .lhs = .{ .variable = tv },
                    .rhs = .{ .variable = t_tv },
                    .provenance = expr,
                });
            },

            // For functions we generate a constraint that specifies that it must unify with an instance of the function
            // ("->") type and then generate a new type variable for every parameter and the return value.
            .function => |f| {
                try self.var_type_vars.beginScope(self.arena.allocator());

                var params = try std.ArrayListUnmanaged(TypeVar).initCapacity(self.gpa, f.params.len + 1);
                errdefer params.deinit(self.gpa);

                try self.var_type_vars.ensureUnusedCapacity(self.arena.allocator(), @intCast(u32, f.params.len));

                for (f.params) |param| {
                    const p_tv = self.typevar();
                    params.appendAssumeCapacity(.{ .variable = p_tv });
                    self.var_type_vars.putAssumeCapacity(param.inner.identifier, p_tv);
                }

                const b_tv = try self.generateExprConstraints(f.body);
                params.appendAssumeCapacity(.{ .variable = b_tv });

                try self.constraints.append(self.gpa, .{
                    .lhs = .{ .variable = tv },
                    .rhs = TypeVar{ .construct = .{ .constructor = "->", .args = try params.toOwnedSlice(self.gpa) } },
                    .provenance = expr,
                });

                try self.var_type_vars.endScope(self.arena.allocator());
            },

            // For apply we generate two constraints. The first is that this expression has the same type as the return
            // value of the function. The second is the function takes the same types as parameters as the args it is
            // being given.
            .apply => |a| return try self.generateApplyConstraints(tv, a.f, a.args),

            // Binops are handled as applications.
            .binop => |binop| return try self.generateApplyConstraints(
                tv,
                binopToExpr(binop.op),
                &[_]Located(*Expression){ binop.lhs, binop.rhs },
            ),

            // Binops are handled as applications.
            .unaryop => |unaryop| switch (unaryop.op) {
                .grouping => {
                    const g_tv = try self.generateExprConstraints(unaryop.e);
                    try self.constraints.append(self.gpa, .{
                        .lhs = .{ .variable = tv },
                        .rhs = .{ .variable = g_tv },
                        .provenance = expr,
                    });
                },
                else => return try self.generateApplyConstraints(
                    tv,
                    unaryopToExpr(unaryop.op),
                    &[_]Located(*Expression){unaryop.e},
                ),
            },

            .let => |l| {
                try self.var_type_vars.beginScope(self.arena.allocator());

                try self.var_type_vars.ensureUnusedCapacity(self.arena.allocator(), @intCast(u32, l.assignments.len));
                for (l.assignments) |a| {
                    const a_tv = try self.generateExprConstraints(a.inner.expression);
                    self.var_type_vars.putAssumeCapacity(a.inner.identifier, a_tv);
                }

                const in_tv = try self.generateExprConstraints(l.in);
                try self.constraints.append(self.gpa, .{
                    .lhs = .{ .variable = tv },
                    .rhs = .{ .variable = in_tv },
                    .provenance = expr,
                });

                try self.var_type_vars.endScope(self.arena.allocator());
            },
        }

        return tv;
    }

    /// printSubs prints the list of substitutions out.
    fn printSubs(self: *const Sema) void {
        std.debug.print("[", .{});
        for (self.substitutions.items) |sub, i| {
            std.debug.print("{}", .{sub});
            if (i < self.substitutions.items.len - 1) std.debug.print(", ", .{});
        }
        std.debug.print("]", .{});
    }

    /// printSub prints a debug message indicating we are substituting `to_be_replaced` with `with`.
    fn printSub(self: *const Sema, constraint: Constraint, to_be_replaced: TypeVar, with: TypeVar) void {
        std.debug.print("{}: Adding {}:={} to ", .{ constraint, to_be_replaced, with });
        self.printSubs();
        std.debug.print("\n", .{});
    }

    /// replaceTypeVar will replace `tv` with `with` if `tv` is the variable `to_be_replaced`. Otherwise it will
    /// recurse into `tv`'s children and replace them if needed.
    fn replaceTypeVar(self: *Sema, tv: *TypeVar, to_be_replaced: Variable, with: TypeVar) !bool {
        switch (tv.*) {
            .variable => |v| if (v == to_be_replaced) {
                tv.* = try with.clone(self.gpa);
                return true;
            } else {},
            .builtin => {},
            .construct => |c| {
                var ret = false;
                for (c.args) |*arg| {
                    ret = ret or try self.replaceTypeVar(arg, to_be_replaced, with);
                }
                return ret;
            },
        }

        return false;
    }

    /// replace replaced any occurrences of the variable `to_be_replaced` with `with`.
    fn replace(self: *Sema, to_be_replaced: Variable, with: TypeVar) !void {
        for (self.constraints.items) |*constraint| {
            var orig = constraint.*;

            var replaced = try self.replaceTypeVar(&constraint.lhs, to_be_replaced, with);
            replaced = try self.replaceTypeVar(&constraint.rhs, to_be_replaced, with) or replaced;

            if (replaced and self.debug) std.debug.print("  {} => {}\n", .{ orig, constraint.* });
        }
    }

    /// solve unifies the variables generated using the constraints.
    fn solve(self: *Sema) !void {
        if (self.debug) std.debug.print("============ UNIFICATION ===========\n", .{});

        var made_progress = true;
        while (made_progress) {
            made_progress = false;

            var to_add = std.ArrayListUnmanaged(Constraint){};

            for (self.constraints.items) |constraint| {
                if (!constraint.lhs.unifiableWith(constraint.rhs)) continue;

                // If one side is a variable then we can just use the other side whenever we see it.
                if (constraint.lhs == .variable) {
                    if (self.debug) self.printSub(constraint, constraint.lhs, constraint.rhs);

                    made_progress = true;
                    try self.replace(constraint.lhs.variable, constraint.rhs);
                    try self.substitutions.append(self.arena.allocator(), .{
                        .lhs = try constraint.lhs.clone(self.arena.allocator()),
                        .rhs = try constraint.rhs.clone(self.arena.allocator()),
                    });
                    continue;
                }

                if (constraint.rhs == .variable) {
                    if (self.debug) self.printSub(constraint, constraint.rhs, constraint.lhs);

                    made_progress = true;
                    try self.replace(constraint.rhs.variable, constraint.lhs);
                    try self.substitutions.append(self.arena.allocator(), .{
                        .lhs = try constraint.rhs.clone(self.arena.allocator()),
                        .rhs = try constraint.lhs.clone(self.arena.allocator()),
                    });
                    continue;
                }

                if (constraint.lhs == .builtin) continue;

                // If unifiableWith has passed then we know at this point both sides are constructs with unifiable
                // arguments
                std.debug.assert(constraint.lhs == .construct and constraint.rhs == .construct);
                var lhs = constraint.lhs.construct;
                var rhs = constraint.rhs.construct;

                if (self.debug) std.debug.print("{}: Expanding\n", .{constraint});

                for (lhs.args) |lhs_arg, i| {
                    var rhs_arg = rhs.args[i];
                    try to_add.append(self.arena.allocator(), .{
                        .lhs = try lhs_arg.clone(self.gpa),
                        .rhs = try rhs_arg.clone(self.gpa),
                        .provenance = constraint.provenance,
                    });

                    if (self.debug) std.debug.print("  {} ~ {}\n", .{ lhs_arg, rhs_arg });
                }

                made_progress = true;
            }

            try self.constraints.ensureUnusedCapacity(self.gpa, to_add.items.len);
            self.constraints.appendSliceAssumeCapacity(to_add.items);

            self.pruneConstraints();
        }
    }

    /// pruneConstraints removes any constraints that add no information, e.g. t0 ~ t0 or Int ~ Int.
    fn pruneConstraints(self: *Sema) void {
        var i: usize = 0;
        while (i < self.constraints.items.len) {
            const constraint = &self.constraints.items[i];
            if (constraint.lhs.eql(constraint.rhs)) {
                if (self.debug) std.debug.print("{}: Removing\n", .{constraint});

                constraint.deinit(self.gpa);

                _ = self.constraints.swapRemove(i);
                continue;
            }

            i += 1;
        }
    }

    /// typevarToType converts tv to a Type - substituting variables that are not bound in `substitutions` to generics.
    fn typevarToType(self: *Sema, tv: TypeVar, generics: *std.AutoHashMap(Variable, Variable)) error{OutOfMemory}!Type {
        return switch (tv) {
            .builtin => |b| Type{ .builtin = b },
            .variable => |v| b: {
                var t = try self.findAndGenerateTypeInner(v, generics) orelse {
                    const gop = try generics.getOrPut(v);
                    if (!gop.found_existing) gop.value_ptr.* = generics.unmanaged.size - 1;
                    break :b Type{ .generic = gop.value_ptr.* };
                };
                break :b t;
            },
            .construct => |c| b: {
                var args = try self.gpa.alloc(Type, c.args.len);
                errdefer self.gpa.free(args);

                for (c.args) |arg, i| {
                    args[i] = try self.typevarToType(arg, generics);
                }

                break :b Type{ .construct = .{ .constructor = c.constructor, .args = args } };
            },
        };
    }

    /// findAndGenerateTypeInner finds v in the substitutions and returns its type.
    fn findAndGenerateTypeInner(self: *Sema, v: Variable, generics: *std.AutoHashMap(Variable, Variable)) !?Type {
        for (self.substitutions.items) |sub| {
            if (sub.lhs != .variable or sub.lhs.variable != v) continue;
            return try self.typevarToType(sub.rhs, generics);
        }

        return null;
    }

    /// findAndGenerateType finds v in the substitutions and returns its type.
    fn findAndGenerateType(self: *Sema, v: Variable) !Type {
        var generics = std.AutoHashMap(Variable, Variable).init(self.arena.allocator());
        return (try self.findAndGenerateTypeInner(v, &generics)) orelse unreachable;
    }

    /// generateAssignmentConstraints generates constraints for the given assignment.
    fn generateAssignmentConstraints(self: *Sema, a: parser.Assignment) !Variable {
        const tv = self.typevar();
        try self.var_type_vars.put(self.arena.allocator(), a.identifier, tv);

        const e_tv = try self.generateExprConstraints(a.expression);
        try self.constraints.append(self.gpa, .{
            .lhs = .{ .variable = tv },
            .rhs = .{ .variable = e_tv },
            .provenance = a.expression,
        });

        return tv;
    }

    /// signatureTypeToTypeVar takes a type from a signature and converts it to a typevar.
    fn signatureTypeToTypeVar(self: *Sema, s: parser.SignatureType) !TypeVar {
        switch (s) {
            .mono => |m| return if (std.mem.eql(u8, m, "Int"))
                TypeVar{ .builtin = BuiltinType.int }
            else if (std.mem.eql(u8, m, "Bool"))
                TypeVar{ .builtin = BuiltinType.boolean }
            else if (std.ascii.isLower(m[0]))
                // FIXME: we need this to not be unifiable with any concrete type
                TypeVar{ .variable = self.typevar() }
            else
                return error.TypeNotFound,
            .construct => |c| {
                var args = try self.gpa.alloc(TypeVar, c.args.len);
                errdefer self.gpa.free(args);

                for (c.args) |arg, i| {
                    args[i] = try self.signatureTypeToTypeVar(arg);
                }

                return TypeVar{ .construct = .{ .constructor = c.constructor, .args = args } };
            },
        }
    }

    /// generateSignatureConstraints generates constraints for the given signature. We do this by generating a new
    /// typevar for whatever has the signature and then add constraints for it given the signature.
    fn generateSignatureConstraints(self: *Sema, s: parser.Signature) !Variable {
        const tv = self.typevar();
        const t = try self.signatureTypeToTypeVar(s.type);

        // FIXME: provenance
        try self.constraints.append(self.gpa, .{ .lhs = .{ .variable = tv }, .rhs = t, .provenance = undefined });

        return tv;
    }

    /// infer infers the type of all expressions in `program` and type checks them.
    pub fn infer(self: *Sema, program: *const parser.Program) !void {
        try self.prepopulate();

        var i: usize = 0;
        while (i < program.stmts.items.len) : (i += 1) {
            var stmt = program.stmts.items[i];

            self.var_type_vars = try VarTypeVars.init(self.arena.allocator());

            const res = switch (stmt.inner) {
                .assignment => |a| try self.generateAssignmentConstraints(a),
                .expression => |e| try self.generateExprConstraints(.{ .loc = stmt.loc, .inner = e }),
                .signature => |s| b: {
                    if (i == program.stmts.items.len - 1)
                        return error.FloatingBinding;

                    const a = switch (program.stmts.items[i + 1].inner) {
                        .expression, .signature, .typedef => return error.FloatingBinding,
                        .assignment => |a| a,
                    };

                    const a_tv = try self.generateAssignmentConstraints(a);
                    const tv = try self.generateSignatureConstraints(s);

                    try self.constraints.append(self.gpa, .{
                        .lhs = .{ .variable = a_tv },
                        .rhs = .{ .variable = tv },
                        .provenance = a.expression,
                    });

                    i += 1;

                    break :b a_tv;
                },
                .typedef => |t| switch (t.def) {
                    .@"union" => |u| {
                        var generics = std.StringArrayHashMapUnmanaged(Variable){};
                        try generics.ensureUnusedCapacity(self.arena.allocator(), u.variables.len);

                        var ty = b: {
                            var args = try self.gpa.alloc(Type, u.variables.len);
                            errdefer self.gpa.free(args);

                            for (u.variables) |v, j| {
                                generics.putAssumeCapacity(v, j);
                                args[j] = .{ .generic = j };
                            }

                            var ty = Type{ .construct = .{ .constructor = t.identifier, .args = args } };

                            try self.types.put(self.gpa, t.identifier, try ty.clone(self.gpa));
                            break :b ty;
                        };
                        defer ty.deinit(self.gpa);

                        for (u.variants) |variant| {
                            if (self.decl_types.contains(variant.name)) return error.NameInUse;

                            var args = try self.gpa.alloc(Type, 2);
                            errdefer self.gpa.free(args);

                            // TODO: What about user and invalid types?
                            args[0] = if (std.mem.eql(u8, variant.type, "Int"))
                                Type{ .builtin = .int }
                            else if (std.mem.eql(u8, variant.type, "Bool"))
                                Type{ .builtin = .boolean }
                            else b: {
                                var g = generics.get(variant.type) orelse return error.UndefinedVariabled;
                                break :b Type{ .generic = g };
                            };

                            args[1] = try ty.clone(self.gpa);

                            try self.decl_types.put(self.gpa, variant.name, .{ .construct = .{
                                .constructor = "->",
                                .args = args,
                            } });
                        }
                        continue;
                    },
                },
            };

            if (self.debug) {
                try self.printVars();
                try self.printConstraints();
            }

            try self.solve();

            if (self.debug) try self.printConstraints();

            if (self.debug) {
                std.debug.print("============== RESULT ==============\n", .{});
                std.debug.print("Looking for {}\n", .{res});
            }

            var ty = try self.findAndGenerateType(res);
            switch (stmt.inner) {
                .assignment => |a| try self.decl_types.put(self.gpa, a.identifier, ty),
                .signature => |s| try self.decl_types.put(self.gpa, s.identifier, ty),
                else => {},
            }

            if (self.debug) std.debug.print("{}\n", .{ty});

            _ = self.arena.reset(.retain_capacity);
            self.expr_type_vars = .{};
            self.substitutions = .{};
            self.var_type_vars = undefined;
        }

        if (self.constraints.items.len != 0) return error.TypeError;
    }

    fn allocDiag(self: *Sema, located: Located(*Expression), comptime fmt: []const u8, args: anytype) !lexer.Diag {
        return lexer.Diag{ .loc = located.loc, .msg = try std.fmt.allocPrint(self.gpa, fmt, args) };
    }

    pub fn getDiags(self: *Sema) ![]lexer.Diag {
        var al = try std.ArrayListUnmanaged(lexer.Diag).initCapacity(self.gpa, self.constraints.items.len);
        errdefer al.deinit(self.gpa);

        for (self.constraints.items) |constraint| {
            switch (constraint.lhs) {
                .builtin => |lhs| switch (constraint.rhs) {
                    .builtin => |rhs| al.appendAssumeCapacity(
                        try self.allocDiag(constraint.provenance, "Expected {} got {}", .{ lhs, rhs }),
                    ),
                    .construct => |rhs| al.appendAssumeCapacity(
                        try self.allocDiag(constraint.provenance, "Expected {}, got {s}", .{ lhs, rhs.constructor }),
                    ),
                    .variable => unreachable,
                },
                .construct => |lhs| switch (constraint.rhs) {
                    .builtin => |rhs| al.appendAssumeCapacity(
                        try self.allocDiag(constraint.provenance, "Expected {} got {}", .{ lhs, rhs }),
                    ),
                    .construct => |rhs| b: {
                        if (!std.mem.eql(u8, lhs.constructor, rhs.constructor)) {
                            al.appendAssumeCapacity(try self.allocDiag(
                                constraint.provenance,
                                "Expected a {s}, got a {s}",
                                .{ lhs.constructor, rhs.constructor },
                            ));
                            break :b;
                        }

                        if (lhs.args.len != rhs.args.len) {
                            var diff: usize = if (std.mem.eql(u8, "->", lhs.constructor)) 1 else 0;
                            al.appendAssumeCapacity(try self.allocDiag(
                                constraint.provenance,
                                "Expected {} arguments to {s}, got {}",
                                .{ lhs.args.len - diff, lhs.constructor, rhs.args.len - diff },
                            ));
                            break :b;
                        }

                        for (lhs.args) |lhs_arg, i| {
                            const rhs_arg = rhs.args[i];
                            if (lhs_arg.unifiableWith(rhs_arg)) continue;

                            al.appendAssumeCapacity(try self.allocDiag(
                                constraint.provenance,
                                "Expected a {}, got a {} in argument {} of {s}",
                                .{ lhs_arg, rhs_arg, i + 1, lhs.constructor },
                            ));

                            break :b;
                        }
                    },
                    .variable => unreachable,
                },
                .variable => unreachable,
            }
        }

        return try al.toOwnedSlice(self.gpa);
    }

    pub fn deinit(self: *Sema) void {
        self.arena.deinit();

        {
            var it = self.decl_types.iterator();
            while (it.next()) |entry| {
                entry.value_ptr.deinit(self.gpa);
            }

            self.decl_types.deinit(self.gpa);
        }

        {
            var it = self.types.iterator();
            while (it.next()) |entry| {
                entry.value_ptr.deinit(self.gpa);
            }

            self.types.deinit(self.gpa);
        }

        for (self.constraints.items) |*c| c.deinit(self.gpa);
        self.constraints.deinit(self.gpa);
    }
};

const testing = std.testing;

fn testNormalised(source: []const u8, expected: []const u8) !void {
    var l = parser.Lexer{ .real = lexer.Lexer.init(source) };

    var p = parser.Parser.init(testing.allocator, l);
    defer p.deinit();

    var program = try p.parse();
    defer program.deinit();

    try normaliseProgram(&program);

    var al = std.ArrayList(u8).init(testing.allocator);
    defer al.deinit();
    var w = al.writer();

    for (program.stmts.items) |stmt| {
        try stmt.inner.write(w);
    }

    try testing.expectEqualStrings(expected, al.items);
}

fn testErrorNormalised(source: []const u8, e: anyerror) !void {
    var l = parser.Lexer{ .real = lexer.Lexer.init(source) };

    var p = parser.Parser.init(testing.allocator, l);
    defer p.deinit();

    var program = try p.parse();
    defer program.deinit();

    try testing.expectError(e, normaliseProgram(&program));
}

const TypeMapping = struct { name: []const u8, type: []const u8 };

fn testTypeCheck(source: []const u8, mappings: []const TypeMapping) !void {
    var l = parser.Lexer{ .real = lexer.Lexer.init(source) };

    var p = parser.Parser.init(testing.allocator, l);
    defer p.deinit();

    var program = try p.parse();
    defer program.deinit();

    try normaliseProgram(&program);

    var sema = Sema.init(testing.allocator, false);
    defer sema.deinit();

    try sema.infer(&program);

    for (mappings) |m| {
        const t = sema.decl_types.get(m.name) orelse return error.DeclNotFound;
        const t_s = try std.fmt.allocPrint(testing.allocator, "{}", .{t});
        defer testing.allocator.free(t_s);

        try testing.expectEqualStrings(m.type, t_s);
    }
}

fn testErrorTypeCheck(source: []const u8) !void {
    var l = parser.Lexer{ .real = lexer.Lexer.init(source) };

    var p = parser.Parser.init(testing.allocator, l);
    defer p.deinit();

    var program = try p.parse();
    defer program.deinit();

    try normaliseProgram(&program);

    var sema = Sema.init(testing.allocator, false);
    defer sema.deinit();

    try testing.expectError(error.TypeError, sema.infer(&program));
}

test "reorder expressions and assignments" {
    try testNormalised("1; a = 1;",
        \\a = 1;
        \\1;
        \\
    );
}

test "reorder assignments" {
    try testNormalised("a = b; b = 1;",
        \\b = 1;
        \\a = b;
        \\
    );

    try testNormalised("a = let b = 10; in b+2; b = 1;",
        \\a = (let [b=10;] (+ b 2));
        \\b = 1;
        \\
    );
    try testNormalised("a = fn x y => x + y; x = 10;",
        \\a = (fn [x y] (+ x y));
        \\x = 10;
        \\
    );
}

test "reorder allows functions to refer to themselves" {
    try testNormalised("a = fn x y => a x y;",
        \\a = (fn [x y] (a x y));
        \\
    );
}

test "reorder signatures" {
    try testNormalised("c:Bool; b : Int; b = 5; a = 1;a : Int;",
        \\a : Int;
        \\a = 1;
        \\b : Int;
        \\b = 5;
        \\c : Bool;
        \\
    );
}

test "fail: reorder redefinition" {
    try testErrorNormalised("a=1;a=true;", error.Redefinition);
}

test "assignments" {
    try testTypeCheck("a = b; b = 1; c = true;", &.{
        TypeMapping{ .name = "a", .type = "Int" },
        TypeMapping{ .name = "b", .type = "Int" },
        TypeMapping{ .name = "c", .type = "Bool" },
    });
}

// test "fail: assignments" {
//     try testErrorTypeCheck("a=b;");
// }

test "maths" {
    try testTypeCheck("a=1+1;b=2-1;c=1*1;d=2/1;e=-1;", &.{
        TypeMapping{ .name = "a", .type = "Int" },
        TypeMapping{ .name = "b", .type = "Int" },
        TypeMapping{ .name = "c", .type = "Int" },
        TypeMapping{ .name = "d", .type = "Int" },
        TypeMapping{ .name = "e", .type = "Int" },
    });
}

test "fail: maths" {
    try testErrorTypeCheck("a=-true;");
    try testErrorTypeCheck("a=1+true;");
}

test "boolean binops" {
    try testTypeCheck("a=5==5; b=6*2; c=b>18; d=8-2*2<=3;", &.{
        TypeMapping{ .name = "a", .type = "Bool" },
        TypeMapping{ .name = "b", .type = "Int" },
        TypeMapping{ .name = "c", .type = "Bool" },
        TypeMapping{ .name = "d", .type = "Bool" },
    });
}

test "let..in" {
    try testTypeCheck("a = let x = 5; in x * 2; b = a + 1;", &.{
        .{ .name = "a", .type = "Int" },
        .{ .name = "b", .type = "Int" },
    });

    try testTypeCheck("a = true; b = let a = false; in let a = 10; in a * 2;", &.{
        .{ .name = "a", .type = "Bool" },
        .{ .name = "b", .type = "Int" },
    });
}

test "fail: let..in" {
    try testErrorTypeCheck("a=1;b=let a=true; in a+2;");
}

test "functions" {
    try testTypeCheck("f = fn x => -x; g = fn x y => x + y;", &.{
        TypeMapping{ .name = "f", .type = "(-> Int Int)" },
        TypeMapping{ .name = "g", .type = "(-> Int Int Int)" },
    });
}

test "function arguments shadowing decls/vars" {
    try testTypeCheck("x = true; f = fn x => x + 1; y = f 2;", &.{
        .{ .name = "x", .type = "Bool" },
        .{ .name = "f", .type = "(-> Int Int)" },
        .{ .name = "y", .type = "Int" },
    });

    try testTypeCheck("f = fn x => if x then (fn x => x + 1) else (fn x => x - 1); a = (f true) 1;", &.{
        .{ .name = "f", .type = "(-> Bool (-> Int Int))" },
        .{ .name = "a", .type = "Int" },
    });
}

test "function calls" {
    try testTypeCheck("f = fn x => -x;a = f 1;", &.{
        TypeMapping{ .name = "f", .type = "(-> Int Int)" },
        TypeMapping{ .name = "a", .type = "Int" },
    });

    try testTypeCheck("f = fn x y => x + y;a = f 1 2;", &.{
        TypeMapping{ .name = "f", .type = "(-> Int Int Int)" },
        TypeMapping{ .name = "a", .type = "Int" },
    });
}

test "fail: function calls" {
    try testErrorTypeCheck("f = fn x => -x; a = f true;");
}

test "if/then/elif/else" {
    try testTypeCheck("a = if true then 1 else 2;", &.{
        .{ .name = "a", .type = "Int" },
    });

    try testTypeCheck("a = if true then 1 elif false then 2 else 3;", &.{
        .{ .name = "a", .type = "Int" },
    });

    try testTypeCheck("a = if true then true else false;", &.{
        .{ .name = "a", .type = "Bool" },
    });
}

test "fail: if/then/elif/else" {
    try testErrorTypeCheck("if 1 then 1 else 2;");
    try testErrorTypeCheck("if true then 1 else false;");
    try testErrorTypeCheck("a=false;if true then 1 elif false then 2 else a;");
}

test "polymorphic functions" {
    try testTypeCheck(
        "id = fn x => x; const = fn a b => a; m = id 147; n = id true; o = const 1 2; p = const 1 true;",
        &.{
            .{ .name = "id", .type = "(-> g0 g0)" },
            .{ .name = "const", .type = "(-> g0 g1 g0)" },
            .{ .name = "m", .type = "Int" },
            .{ .name = "n", .type = "Bool" },
            .{ .name = "o", .type = "Int" },
            .{ .name = "p", .type = "Int" },
        },
    );
}

test "signatures" {
    try testTypeCheck("a : Int; a = 5; b : Bool; b = true;", &.{
        .{ .name = "a", .type = "Int" },
        .{ .name = "b", .type = "Bool" },
    });

    try testTypeCheck("id : fn Int => Int; id = fn a => a; x = id 1;", &.{
        .{ .name = "id", .type = "(-> Int Int)" },
        .{ .name = "x", .type = "Int" },
    });

    try testTypeCheck("id : fn a => a; id = fn a => a; x = id 1; y = id true;", &.{
        .{ .name = "id", .type = "(-> g0 g0)" },
        .{ .name = "x", .type = "Int" },
        .{ .name = "y", .type = "Bool" },
    });
}

test "fail: type signatures" {
    try testErrorTypeCheck("a : Int; a = true;");
    try testErrorTypeCheck("id : fn Int => Int; id = fn a => a; x = id true;");
}

test "unions" {
    try testTypeCheck(
        \\Option = union a => {
        \\    Some: a;
        \\    None: Bool;
        \\};
        \\x = Some 10;
        \\y = None true;
        \\z : Option Int;
        \\z = None true;
    , &.{
        .{ .name = "x", .type = "(Option Int)" },
        .{ .name = "y", .type = "(Option g0)" },
        .{ .name = "z", .type = "(Option Int)" },
    });
}
