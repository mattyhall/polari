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
    lhs: Located(*parser.Expression),
    rhs: Located(*parser.Expression),
    args: *std.ArrayList(Located(*parser.Expression)),
) !Located(*parser.Expression) {
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
fn normaliseExpression(arena: std.mem.Allocator, expr: *parser.Expression) error{OutOfMemory}!void {
    switch (expr.*) {
        .binop => |binop| {
            switch (binop.op) {
                .apply => {
                    var al = std.ArrayList(Located(*parser.Expression)).init(arena);
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
        .integer, .boolean, .identifier => return,
        .apply => unreachable,
    }
}

/// normaliseProgram normalises an AST into a standard, simpler form than that which comes out of the parser. Currently
/// it just changes chains of apply binops into a single Apply expression.
pub fn normaliseProgram(program: *parser.Program) !void {
    for (program.stmts.items) |stmt| {
        switch (stmt.inner) {
            .assignment => |a| try normaliseExpression(program.arena.allocator(), a.expression.inner),
            .expression => |e| try normaliseExpression(program.arena.allocator(), e),
        }
    }
}

fn hashFn(comptime Context: type) (fn (Context, Variable) u64) {
    return struct {
        fn hash(_: Context, v: Variable) u64 {
            return switch (v) {
                .id => |i| @intCast(u64, i),
                .expr => |e| @intCast(u64, @ptrToInt(e)),
            };
        }
    }.hash;
}

fn eqlFn(comptime Context: type) (fn (Context, Variable, Variable) bool) {
    return struct {
        fn eql(_: Context, lhs: Variable, rhs: Variable) bool {
            if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;

            return switch (lhs) {
                .id => |i| i == rhs.id,
                .expr => |e| e == rhs.expr,
            };
        }
    }.eql;
}

const MapContext = struct {
    pub const hash = hashFn(@This());
    pub const eql = eqlFn(@This());
};

const Type = union(enum) {
    bool,
    int,
    function: struct { params: []const Type, ret: *Type },
    unknown,

    pub fn eql(lhs: Type, rhs: Type) bool {
        if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;
        // FIXME: do we need equality for functions?
        return lhs != .function and lhs != .unknown;
    }

    fn write(self: Type, writer: anytype) !void {
        switch (self) {
            .bool => try writer.writeAll("Bool"),
            .int => try writer.writeAll("Int"),
            .function => |f| {
                try writer.writeAll("Fn [");
                for (f.params) |param, i| {
                    try param.write(writer);
                    if (i < f.params.len - 1) try writer.writeAll(" ");
                }
                try writer.writeAll("] ");

                try f.ret.write(writer);
            },
            .unknown => try writer.writeAll("???"),
        }
    }

    fn deinit(self: Type, gpa: std.mem.Allocator) void {
        if (self != .function) return;
        gpa.free(self.function.params);
        gpa.destroy(self.function.ret);
    }
};

const Id = u32;

/// Variable represents something that can be given a type.
const Variable = union(enum) {
    /// id is the Id of an identifier in the program.
    id: Id,
    expr: *const Expression,

    fn write(self: Variable, writer: anytype) !void {
        switch (self) {
            .id => |i| try writer.print("<{}>", .{i}),
            .expr => |e| try e.write(writer),
        }
    }
};

const TypeMap = std.HashMapUnmanaged(Variable, Type, MapContext, std.hash_map.default_max_load_percentage);
const VarMap = std.StringHashMapUnmanaged(Id);
const LocMap = std.HashMapUnmanaged(Variable, Loc, MapContext, std.hash_map.default_max_load_percentage);

/// Unification means that the type of lhs should be found by the rhs rule.
const Unification = struct { lhs: Variable, rhs: Rule };

/// A rule is some way of finding a type given other variables.
const Rule = union(enum) {
    /// f is a function and we should have the same type as the parameter with index param.
    parameter: struct { param: u32, f: Variable },

    /// Variable is a function and we should have the same type as its return.
    ret: Variable,

    /// We should have the same type as Variable.
    variable: Variable,

    fn write(self: Rule, writer: anytype) !void {
        switch (self) {
            .parameter => |p| {
                try writer.print("Parameter {} of ", .{p.param});
                try p.f.write(writer);
            },
            .ret => |v| {
                try writer.writeAll("Return type of ");
                try v.write(writer);
            },
            .variable => |v| try v.write(writer),
        }
    }
};

pub const Sema = struct {
    gpa: std.mem.Allocator,
    debug: bool,
    current_id: Id,
    made_progress: bool,
    /// types maps variables to their types.
    types: TypeMap,
    /// var_maps is a list of VarMaps: a map from an identifier to an Id.
    var_maps: std.ArrayListUnmanaged(VarMap),
    unifications: std.ArrayListUnmanaged(Unification),
    /// locations maps Variables to their locations.
    locations: LocMap,
    diags: std.ArrayListUnmanaged(Diag),

    const plusId = 0;
    const subId = 1;
    const multId = 2;
    const divId = 3;
    const negateId = 4;

    pub fn init(gpa: std.mem.Allocator, debug: bool) Sema {
        return .{
            .gpa = gpa,
            .debug = debug,
            .current_id = 0,
            .made_progress = true,
            .types = .{},
            .var_maps = .{},
            .unifications = .{},
            .locations = .{},
            .diags = .{},
        };
    }

    /// id generates a new Id for an identifier.
    fn id(self: *Sema) Id {
        self.current_id += 1;
        return self.current_id - 1;
    }

    fn currentVarMap(self: *Sema) *VarMap {
        return &self.var_maps.items[self.var_maps.items.len - 1];
    }

    fn allocDiag(self: *Sema, loc: lexer.Loc, comptime fmt: []const u8, args: anytype) error{OutOfMemory}!void {
        const msg = try std.fmt.allocPrint(self.gpa, fmt, args);
        try self.diags.append(self.gpa, Diag{ .msg = msg, .allocator = self.gpa, .loc = loc });
    }

    /// prepopulate adds the types of builtin functions.
    pub fn prepopulate(self: *Sema) error{OutOfMemory}!void {
        try self.var_maps.append(self.gpa, .{});

        const ops = [_][]const u8{ "+", "-", "*", "/" };

        try self.types.ensureUnusedCapacity(self.gpa, ops.len);
        try self.currentVarMap().ensureUnusedCapacity(self.gpa, ops.len);

        for (ops) |op| {
            var params = try self.gpa.alloc(Type, 2);
            errdefer self.gpa.free(params);
            params[0] = .int;
            params[1] = .int;

            var ret = try self.gpa.create(Type);
            errdefer self.gpa.destroy(ret);
            ret.* = .int;

            var t = .{ .function = .{ .ret = ret, .params = params } };

            const op_id = self.id();
            self.types.putAssumeCapacity(.{ .id = op_id }, t);

            self.currentVarMap().putAssumeCapacity(op, op_id);
        }

        {
            var params = try self.gpa.alloc(Type, 1);
            params[0] = .int;

            var ret = try self.gpa.create(Type);
            ret.* = .int;

            var t = .{ .function = .{ .ret = ret, .params = params } };

            const op_id = self.id();
            self.types.putAssumeCapacity(.{ .id = op_id }, t);

            self.currentVarMap().putAssumeCapacity("<negate>", op_id);
        }
    }

    /// findIdentifier finds the Id of ident.
    fn findIdentifier(self: *const Sema, ident: []const u8) ?Id {
        var i = self.var_maps.items.len - 1;
        while (true) : (i -= 1) {
            var map = self.var_maps.items[i];
            if (map.get(ident)) |ident_id| return ident_id;
            if (i == 0) return null;
        }
    }

    pub fn writeState(self: *const Sema, writer: anytype) !void {
        try writer.writeAll("============================\n");

        try writer.writeAll("=========== Types ==========\n");
        {
            var it = self.types.iterator();
            while (it.next()) |entry| {
                try entry.key_ptr.write(writer);
                try writer.writeAll(" => ");
                try entry.value_ptr.write(writer);
                try writer.writeAll("\n");
            }
        }

        try writer.writeAll("======= Unifications =======\n");
        for (self.unifications.items) |unif| {
            try unif.lhs.write(writer);
            try writer.writeAll(" : ");
            try unif.rhs.write(writer);
            try writer.writeAll("\n");
        }
    }

    fn generateRulesForApply(self: *Sema, f: Variable, args: []const Variable, expr: Variable) !void {
        try self.unifications.ensureUnusedCapacity(self.gpa, 1 + args.len);

        self.unifications.appendAssumeCapacity(.{ .lhs = expr, .rhs = .{ .ret = f } });

        for (args) |arg, i| {
            self.unifications.appendAssumeCapacity(.{
                .lhs = arg,
                .rhs = .{ .parameter = .{ .param = @intCast(u32, i), .f = f } },
            });
        }
    }

    /// generateRulesForExpression generates unifications for the given expression.
    fn generateRulesForExpression(self: *Sema, loc: Loc, expr: *const Expression) !void {
        const v = Variable{ .expr = expr };
        try self.locations.put(self.gpa, v, loc);

        switch (expr.*) {
            .integer => {
                try self.types.put(self.gpa, v, .int);
                return;
            },
            .boolean => {
                try self.types.put(self.gpa, v, .bool);
                return;
            },
            else => try self.types.put(self.gpa, v, .unknown),
        }

        switch (expr.*) {
            .integer, .boolean => unreachable,
            .identifier => |i| {
                const ident_id = self.findIdentifier(i) orelse {
                    try self.allocDiag(loc, "could not find variable {s}", .{i});
                    return error.CouldNotFindVariable;
                };

                try self.unifications.append(self.gpa, .{
                    .lhs = v,
                    .rhs = .{ .variable = .{ .id = ident_id } },
                });
            },
            .unaryop => |unaryop| {
                try self.generateRulesForExpression(unaryop.e.loc, unaryop.e.inner);

                switch (unaryop.op) {
                    .grouping => try self.unifications.append(
                        self.gpa,
                        .{ .lhs = v, .rhs = .{ .variable = .{ .expr = unaryop.e.inner } } },
                    ),
                    .negate => try self.generateRulesForApply(
                        .{ .id = negateId },
                        &[_]Variable{.{ .expr = unaryop.e.inner }},
                        v,
                    ),
                }
            },
            .binop => |binop| {
                try self.generateRulesForExpression(binop.lhs.loc, binop.lhs.inner);
                try self.generateRulesForExpression(binop.rhs.loc, binop.rhs.inner);

                var op_id: Id = switch (binop.op) {
                    .plus => plusId,
                    .minus => subId,
                    .multiply => multId,
                    .divide => divId,
                    .apply => @panic("not implemented"),
                };

                try self.generateRulesForApply(
                    .{ .id = op_id },
                    &[_]Variable{ .{ .expr = binop.lhs.inner }, .{ .expr = binop.rhs.inner } },
                    v,
                );
            },
            .let => unreachable,
            .function => unreachable,
            .apply => unreachable,
        }
    }

    /// generateRules generates unifications for the given program.
    pub fn generateRules(self: *Sema, program: *const parser.Program) !void {
        // Add top-level variables so that they don't need to be in a certain order to type check. E.g. a=b;b=1; should
        // be valid.
        for (program.stmts.items) |stmt| {
            var a = if (stmt.inner == .assignment) stmt.inner.assignment else continue;

            if (self.currentVarMap().contains(a.identifier)) {
                try self.allocDiag(stmt.loc, "redefinition of {s}", .{a.identifier});
                return error.Redefinition;
            }

            const a_id = self.id();
            try self.currentVarMap().put(self.gpa, a.identifier, a_id);
            try self.types.put(self.gpa, .{ .id = a_id }, .unknown);
        }

        for (program.stmts.items) |stmt| {
            switch (stmt.inner) {
                .expression => |e| try self.generateRulesForExpression(stmt.loc, e),
                .assignment => |a| {
                    const a_id = self.findIdentifier(a.identifier) orelse unreachable;
                    try self.currentVarMap().put(self.gpa, a.identifier, a_id);
                    const entry = self.types.getEntry(.{ .id = a_id }) orelse unreachable;

                    switch (a.expression.inner.*) {
                        .integer => entry.value_ptr.* = .int,
                        .boolean => entry.value_ptr.* = .bool,
                        else => {
                            try self.generateRulesForExpression(a.expression.loc, a.expression.inner);
                            try self.unifications.append(self.gpa, .{
                                .lhs = .{ .id = a_id },
                                .rhs = .{ .variable = .{ .expr = a.expression.inner } },
                            });
                        },
                    }
                },
            }
        }

        if (self.debug) {
            var w = std.io.getStdErr().writer();
            var it = self.currentVarMap().iterator();
            try w.writeAll("======================\n");
            while (it.next()) |entry| {
                try w.print("{s} => <{}>\n", .{ entry.key_ptr.*, entry.value_ptr.* });
            }
        }
    }

    /// setType sets the type of v to t if it was previously unknown, otherwise it checks that t is the same as v's
    /// type.
    fn setType(self: *Sema, v: Variable, t: Type) !void {
        var e = self.types.getEntry(v) orelse unreachable;
        if (e.value_ptr.* == .unknown) {
            e.value_ptr.* = t;
            self.made_progress = true;
            return;
        }

        if (!e.value_ptr.eql(t)) {
            const loc = self.locations.get(v) orelse Loc{};
            try self.allocDiag(loc, "type mismatch: expected {}, got {}", .{ t, e.value_ptr.* });
            return error.TypeMismatch;
        }

        self.made_progress = true;
    }

    pub fn solveIter(self: *Sema) !void {
        var i: usize = 0;
        while (i < self.unifications.items.len) {
            const unif = self.unifications.items[i];
            i += 1;

            switch (unif.rhs) {
                .variable => |v| {
                    const t = self.types.get(v) orelse unreachable;
                    switch (t) {
                        .unknown => continue,
                        else => {
                            try self.setType(unif.lhs, t);
                            i -= 1;
                            _ = self.unifications.swapRemove(i);
                        },
                    }
                },
                .parameter => |p| {
                    const t = switch (self.types.get(p.f) orelse unreachable) {
                        .unknown => continue,
                        .function => |f| f,
                        else => |t| {
                            const loc = self.locations.get(unif.lhs) orelse Loc{};
                            try self.allocDiag(loc, "expected function, got: {}", .{t});
                            return error.TypeMistmatch;
                        },
                    };

                    if (p.param >= t.params.len) {
                        const loc = self.locations.get(unif.lhs) orelse Loc{};
                        try self.allocDiag(loc, "too many arguments to function", .{});
                        return error.TypeMistmatch;
                    }

                    try self.setType(unif.lhs, t.params[p.param]);
                    i -= 1;
                    _ = self.unifications.swapRemove(i);
                },
                .ret => |r| {
                    const t = switch (self.types.get(r) orelse unreachable) {
                        .unknown => continue,
                        .function => |f| f,
                        else => |t| {
                            const loc = self.locations.get(unif.lhs) orelse Loc{};
                            try self.allocDiag(loc, "expected function, got: {}", .{t});
                            return error.TypeMistmatch;
                        },
                    };

                    try self.setType(unif.lhs, t.ret.*);
                    i -= 1;
                    _ = self.unifications.swapRemove(i);
                },
            }
        }
    }

    /// solve tries to find the type of all expressions.
    pub fn solve(self: *Sema) !void {
        var w = std.io.getStdOut().writer();

        while (self.made_progress) {
            if (self.debug) try self.writeState(w);

            self.made_progress = false;
            try self.solveIter();
        }

        if (self.debug) try self.writeState(w);

        {
            var it = self.types.iterator();
            while (it.next()) |entry| {
                switch (entry.value_ptr.*) {
                    .unknown => {
                        const loc = self.locations.get(entry.key_ptr.*) orelse Loc{};
                        try self.allocDiag(loc, "could not infer type", .{});
                    },
                    else => {},
                }
            }
        }

        if (self.diags.items.len > 0) return error.TypeCheckFailed;
    }

    pub fn deinit(self: *Sema) void {
        {
            var it = self.types.iterator();
            while (it.next()) |entry| {
                entry.value_ptr.deinit(self.gpa);
            }
            self.types.deinit(self.gpa);
        }

        for (self.var_maps.items) |*vm| {
            vm.deinit(self.gpa);
        }
        self.var_maps.deinit(self.gpa);

        for (self.diags.items) |*d| d.deinit();
        self.diags.deinit(self.gpa);

        self.unifications.deinit(self.gpa);
        self.locations.deinit(self.gpa);
    }
};

const testing = std.testing;

fn expectTypesEqual(source: []const u8, expected: []const struct { id: []const u8, t: Type }) !void {
    var l = parser.Lexer{ .real = lexer.Lexer.init(source) };

    var p = parser.Parser.init(testing.allocator, l);
    defer p.deinit();

    var program = try p.parse();
    defer program.deinit();

    var sema = Sema.init(testing.allocator, false);
    defer sema.deinit();

    try sema.prepopulate();
    try sema.generateRules(&program);
    try sema.solve();

    for (expected) |item| {
        const id = sema.currentVarMap().get(item.id) orelse return error.IdNotFound;
        const t = sema.types.get(.{ .id = id }) orelse return error.IdNotFound;
        if (!t.eql(item.t)) {
            std.debug.print("type of {s} incorrect; expected {}, got {}\n", .{ item.id, item.t, t });
        }
    }
}

fn expectTypeCheckFail(source: []const u8, err: anyerror) !void {
    var l = parser.Lexer{ .real = lexer.Lexer.init(source) };

    var p = parser.Parser.init(testing.allocator, l);
    defer p.deinit();

    var program = try p.parse();
    defer program.deinit();

    var sema = Sema.init(testing.allocator, false);
    defer sema.deinit();

    try sema.prepopulate();

    var e = sema.generateRules(&program);
    e catch {
        try testing.expectError(err, e);
        return;
    };

    var e2 = sema.solve();
    e2 catch {
        try testing.expectError(err, e2);
        return;
    };

    return error.TestExpectError;
}

test "assignments" {
    try expectTypesEqual("a=b;b=1;c=true;", &.{
        .{ .id = "a", .t = .int },
        .{ .id = "b", .t = .int },
        .{ .id = "c", .t = .bool },
    });
}

test "fail: assignments" {
    try expectTypeCheckFail("a=b;", error.CouldNotFindVariable);
    try expectTypeCheckFail("a=1;a=true;", error.Redefinition);
}

test "maths" {
    try expectTypesEqual("a=1+1;b=2-1;c=1*1;d=2/1;e=-1;", &.{
        .{ .id = "a", .t = .int },
        .{ .id = "b", .t = .int },
        .{ .id = "c", .t = .int },
        .{ .id = "d", .t = .int },
        .{ .id = "e", .t = .int },
    });
}

test "fail: maths" {
    try expectTypeCheckFail("a=-true;", error.TypeMismatch);
    try expectTypeCheckFail("a=1+true;", error.TypeMismatch);
}
